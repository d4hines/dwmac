import AppKit
import Common

enum EffectiveLeaf {
    case window(Window)
    case emptyWorkspace(Workspace)
}
extension LiveFocus {
    var asLeaf: EffectiveLeaf {
        if let windowOrNil { .window(windowOrNil) } else { .emptyWorkspace(workspace) }
    }
}

/// This object should be only passed around but never memorized
/// Alternative name: ResolvedFocus
struct LiveFocus: AeroAny, Equatable {
    let windowOrNil: Window?
    var workspace: Workspace

    @MainActor var frozen: FrozenFocus {
        return FrozenFocus(
            windowId: windowOrNil?.windowId,
            workspaceName: workspace.name,
            monitorId: workspace.workspaceMonitor.monitorId ?? 0,
        )
    }
}

/// "old", "captured", "frozen in time" Focus
/// It's safe to keep a hard reference to this object.
/// Unlike in LiveFocus, information inside FrozenFocus isn't guaranteed to be self-consistent.
/// window - workspace - monitor relation could change since the object is created
struct FrozenFocus: AeroAny, Equatable, Sendable {
    let windowId: UInt32?
    let workspaceName: String
    // monitorId is not part of the focus. We keep it here only for 'on-monitor-changed' to work
    let monitorId: Int // 0-based

    @MainActor var live: LiveFocus { // Important: don't access focus.monitorId here. monitorId is not part of the focus. Always prefer workspace
        let window: Window? = windowId.flatMap { Window.get(byId: $0) }
        let workspace = Workspace.get(byName: workspaceName)

        let wsFocus = workspace.toLiveFocus()
        let wdFocus = window?.toLiveFocusOrNil() ?? wsFocus

        return wsFocus.workspace != wdFocus.workspace
            ? wsFocus // If window and workspace become separated prefer workspace
            : wdFocus
    }
}

/// Global focus: Lean's.
/// Commands must be cautious about accessing this property directly. There are legitimate cases.
/// But, in general, commands must firstly check --window-id, --workspace, DWMAC_WINDOW_ID env and
/// DWMAC_WORKSPACE env before accessing the global focus.
@MainActor var focus: LiveFocus { leanFocus.live }

@MainActor private var leanFocus: FrozenFocus {
    let snapshot = LeanState.last
    let workspace = snapshot.focusedWorkspace.isEmpty
        ? mainMonitor.activeWorkspace // Before Lean has heard of the monitors
        : Workspace.get(byName: snapshot.focusedWorkspace)
    return FrozenFocus(
        windowId: snapshot.focusedWindow,
        workspaceName: workspace.name,
        monitorId: workspace.workspaceMonitor.monitorId ?? 0,
    )
}

/// Ask Lean to focus `newFocus`. Returns whether it did. Focusing a hidden scratchpad window summons it onto the
/// focused workspace instead (Cmd-Tab, the Dock, `focus --window-id`); Lean never shows a scratchpad home
@MainActor func setFocus(to newFocus: LiveFocus) -> Bool {
    if let window = newFocus.windowOrNil {
        return leanApply(.focusWindow(window.windowId)).focusedWindow == window.windowId
    } else {
        return leanApply(.focusWorkspace(newFocus.workspace.name)).focusedWorkspace == newFocus.workspace.name
    }
}

extension Window {
    @MainActor func focusWindow() -> Bool {
        if let focus = toLiveFocusOrNil() {
            return setFocus(to: focus)
        } else {
            // todo We should also exit-native-hidden/unminimize[/exit-native-fullscreen?] window if we want to fix ID-B6E178F2
            //      and retry to focus the window. Otherwise, it's not possible to focus minimized/hidden windows
            return false
        }
    }

    /// nil for the windows Lean doesn't manage
    @MainActor func toLiveFocusOrNil() -> LiveFocus? { workspace.map { LiveFocus(windowOrNil: self, workspace: $0) } }
}
extension Workspace {
    @MainActor func focusWorkspace() -> Bool { setFocus(to: toLiveFocus()) }

    @MainActor func toLiveFocus() -> LiveFocus { LiveFocus(windowOrNil: mostRecentWindow, workspace: self) }
}

@MainActor private var lastKnownFocus: FrozenFocus? = nil

// Used by workspace-back-and-forth
@MainActor var _prevFocusedWorkspaceName: String? = nil {
    didSet {
        prevFocusedWorkspaceDate = .now
    }
}
@MainActor var prevFocusedWorkspaceDate: Date = .distantPast
@MainActor var prevFocusedWorkspace: Workspace? { _prevFocusedWorkspaceName.map { Workspace.get(byName: $0) } }

/// The focus before the last change (Lean keeps it, for focus-back-and-forth)
@MainActor var prevFocus: LiveFocus? {
    let snapshot = LeanState.last
    guard let workspace = snapshot.prevWorkspace else { return nil }
    let frozen = FrozenFocus(windowId: snapshot.prevWindow, workspaceName: workspace, monitorId: 0)
    return frozen.live.takeIf { $0 != focus }
}

@MainActor private var onFocusChangedRecursionGuard = false
// Should be called in refreshSession
@MainActor func checkOnFocusChangedCallbacks() {
    if refreshSessionEvent?.isStartup == true {
        return
    }
    let focus = focus
    let frozenFocus = focus.frozen
    let last = lastKnownFocus ?? frozenFocus
    var hasFocusChanged = false
    var hasFocusedWorkspaceChanged = false
    var hasFocusedMonitorChanged = false
    if frozenFocus != last {
        hasFocusChanged = true
    }
    if frozenFocus.workspaceName != last.workspaceName {
        _prevFocusedWorkspaceName = last.workspaceName
        hasFocusedWorkspaceChanged = true
    }
    if frozenFocus.monitorId != last.monitorId {
        hasFocusedMonitorChanged = true
    }
    lastKnownFocus = frozenFocus

    if onFocusChangedRecursionGuard { return }
    onFocusChangedRecursionGuard = true
    defer { onFocusChangedRecursionGuard = false }
    if hasFocusChanged {
        onFocusChanged(focus)
    }
    if let _prevFocusedWorkspaceName, hasFocusedWorkspaceChanged {
        onWorkspaceChanged(_prevFocusedWorkspaceName, frozenFocus.workspaceName)
    }
    if hasFocusedMonitorChanged {
        onFocusedMonitorChanged(focus)
    }
}

@MainActor private func onFocusedMonitorChanged(_ focus: LiveFocus) {
    if config.onFocusedMonitorChanged.isEmpty { return }
    guard let token: RunSessionGuard = .isServerEnabled else { return }
    // todo potential optimization: don't run runSession if we are already in runSession
    Task {
        try await runLightSession(.onFocusedMonitorChanged, token) {
            _ = try await config.onFocusedMonitorChanged.runCmdSeq(.defaultEnv.withFocus(focus), .emptyStdin)
        }
    }
}
@MainActor private func onFocusChanged(_ focus: LiveFocus) {
    if config.onFocusChanged.isEmpty { return }
    guard let token: RunSessionGuard = .isServerEnabled else { return }
    // todo potential optimization: don't run runSession if we are already in runSession
    Task {
        try await runLightSession(.onFocusChanged, token) {
            _ = try await config.onFocusChanged.runCmdSeq(.defaultEnv.withFocus(focus), .emptyStdin)
        }
    }
}

@MainActor private func onWorkspaceChanged(_ oldWorkspace: String, _ newWorkspace: String) {
    if let exec = config.execOnWorkspaceChange.first {
        let process = Process()
        process.executableURL = URL(filePath: exec)
        process.arguments = Array(config.execOnWorkspaceChange.dropFirst())
        var environment = config.execConfig.envVariables
        environment["DWMAC_FOCUSED_WORKSPACE"] = newWorkspace
        environment["DWMAC_PREV_WORKSPACE"] = oldWorkspace
        environment[DWMAC_WORKSPACE] = newWorkspace
        process.environment = environment
        _ = Result { try process.run() }
    }
}

/// Keep Lean's focus and macOS's in agreement (see `syncFocus` in Lean/DwmacCore/Command.lean): report the window
/// macOS says is focused, then do what Lean decides: ask macOS to focus Lean's window, or check again shortly
@MainActor func syncFocusWithMacos(_ nativeFocused: Window?) {
    if nativeFocused?.unmanaged == .popup { return }
    nativeFocused?.macAppUnsafe.lastNativeFocusedWindowId = nativeFocused?.windowId
    let now = Int(ProcessInfo.processInfo.systemUptime * 1000)
    for effect in leanRun(.syncFocus(nativeFocused?.windowId, now: now), CmdIo(stdin: .emptyStdin)).effects {
        switch effect {
            case .nativeFocus(let id): Window.get(byId: id)?.nativeFocus()
            case .recheck(let ms):
                DispatchQueue.main.asyncAfter(deadline: .now() + .milliseconds(ms)) {
                    if TrayMenuModel.shared.isEnabled { scheduleRefreshSession(.focusRecheck) }
                }
            case .launch, .prepareSummon: break
        }
    }
}
