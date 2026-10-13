import AppKit
import Common
@preconcurrency import CoreGraphics

/// Key bindings: Lean holds the config's bindings and the active mode, and says which binding a key press triggers
/// (`Keymap.lookup` in Lean/DwmacCore/Keys.lean). This runs the commands, and repeats them while the key is held

@MainActor private var repeatingTasks: [Int: Task<Void, Never>] = [:]

/// The active mode, `nil` while dwmac is disabled. Lean's, mirrored for the menu bar
@MainActor var activeMode: String? = mainModeId

/// Give Lean the config's bindings
@MainActor func loadKeymap() {
    for task in repeatingTasks.values { task.cancel() }
    repeatingTasks = [:]
    _ = LeanState.runRaw("keymap\t" + config.keymapText)
}

@MainActor func activateMode(_ targetMode: String?) async throws {
    let changed = LeanState.runRaw("setMode\t" + (targetMode ?? "")).out == ["changed"]
    activeMode = targetMode
    if changed && !config.onModeChanged.isEmpty {
        guard let token: RunSessionGuard = .isServerEnabled else { return }
        try await runLightSession(.onModeChanged, token) {
            _ = try await config.onModeChanged.runCmdSeq(.defaultEnv, .emptyStdin)
        }
    }
}

/// The commands of the binding the key triggers in the active mode, if any
@MainActor private func boundCommands(_ keyCode: Int, _ modifiers: NSEvent.ModifierFlags) -> [any Command]? {
    let out = LeanState.runRaw("key\t\(keyCode)\t\(modifiers.rawValue)").out
    return out.isEmpty ? nil : out.dropFirst().compactMap { parseCommand($0.leanUnescaped).cmdOrNil }
}

// MARK: - CGEventTap Handling

@MainActor
func handleEventOnMainThread(type: CGEventType, keyCode: Int, modifiers: NSEvent.ModifierFlags, isRepeat: Bool) -> Bool {
    let commands = boundCommands(keyCode, modifiers)
    if type == .keyDown {
        guard let commands else { return false }
        // macOS's own repeats are swallowed: the binding repeats on its own schedule
        if isRepeat { return true }
        repeatingTasks[keyCode]?.cancel()
        repeatingTasks[keyCode] = Task {
            try? await runLightSession(.hotkeyBinding, .checkServerIsEnabledOrDie) { () throws in
                _ = try await commands.runCmdSeq(.defaultEnv, .emptyStdin)
            }
            try? await Task.sleep(nanoseconds: 300_000_000) // Initial delay
            while !Task.isCancelled {
                // The binding as of now: the mode may have changed
                guard let commands = boundCommands(keyCode, modifiers) else { return }
                try? await runLightSession(.hotkeyBinding, .checkServerIsEnabledOrDie) { () throws in
                    _ = try await commands.runCmdSeq(.defaultEnv, .emptyStdin)
                }
                try? await Task.sleep(nanoseconds: 100_000_000) // Repeat interval
            }
        }
        return true // Swallow event
    } else if type == .keyUp {
        let task = repeatingTasks.removeValue(forKey: keyCode)
        task?.cancel()
        return task != nil || commands != nil
    }
    return false
}
