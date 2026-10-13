import AppKit
import Common

// Scratchpads (xmonad's NamedScratchpad). Lean decides where their windows live (Lean/DwmacCore/Model.lean): in the
// scratchpad's hidden home workspace, or floating on the focused workspace while the scratchpad has focus, and what
// `scratchpad` does (Command.lean). It proves a home is never shown. This file does the physical side: launching
// apps, and sizing and placing summoned windows.

let scratchpadHomePrefix = "scratchpad:"

extension Workspace {
    var isScratchpadHome: Bool { name.hasPrefix(scratchpadHomePrefix) }
}

extension Scratchpad {
    @MainActor var home: Workspace { Workspace.get(byName: scratchpadHomePrefix + name) }
}

@MainActor func scratchpadOwning(_ window: Window) -> Scratchpad? {
    config.scratchpads.values.first { $0.appId == window.app.rawAppBundleId }
}

@MainActor
func toggleScratchpad(_ name: String, _ io: CmdIo) async throws -> Bool {
    // A window about to be summoned is resized while it's still hidden
    for case .prepareSummon(let id) in LeanState.preview(.scratchpad(name)).effects {
        if let window = Window.get(byId: id), let scratchpad = scratchpadOwning(window) {
            try await resizeWhileHidden(window, to: summonedFrame(scratchpad, on: focus.workspace).size)
        }
    }
    let reply = leanRun(.scratchpad(name), io)
    for case .launch(let appId) in reply.effects {
        // Lean summons the first window when it appears
        if !launchApp(appId, scratchpad: name, io) { return false }
    }
    return reply.ok
}

/// Called after Lean moved scratchpad windows out of their home onto `workspace`: put the focused one (the primary)
/// at its configured frame, and the rest (dialogs, second windows) back where they were before hideInCorner
@MainActor
func placeSummonedScratchpad(_ windows: [Window], on workspace: Workspace, focused: UInt32?) {
    guard let scratchpad = windows.first.flatMap(scratchpadOwning) else { return }
    let primary = windows.first { $0.windowId == focused } ?? primaryWindow(windows)
    for window in windows where window != primary {
        window.unhideFromCorner(monitor: workspace.workspaceMonitor.rect, restore: true)
    }
    if primary.isHiddenInCorner { primary.forgetHiddenInCorner() }
    let frame = summonedFrame(scratchpad, on: workspace)
    primary.lastFloatingSize = frame.size
    primary.setAxFrame(frame.topLeftCorner, frame.size)
}

/// The window that was focused last. It gets the configured size; the rest keep their size
@MainActor
private func primaryWindow(_ windows: [Window]) -> Window {
    windows.first { $0.parent?.mostRecentChild == $0 } ?? windows.last.orDie()
}

/// The primary window's frame when summoned: sized per the config, centered on the workspace's monitor
@MainActor
private func summonedFrame(_ scratchpad: Scratchpad, on workspace: Workspace) -> Rect {
    let rect = workspace.workspaceMonitor.visibleRect
    let size = CGSize(width: rect.width * scratchpad.width, height: rect.height * scratchpad.height)
    return Rect(
        topLeftX: rect.topLeftX + (rect.width - size.width) / 2,
        topLeftY: rect.topLeftY + (rect.height - size.height) / 2,
        width: size.width,
        height: size.height,
    )
}

/// Resize a window hidden in a corner before it's moved on screen, so the app reflows its content off-screen.
/// Resizing after the move is visible: the window shows up at the old size, then redraws left to right as it
/// shrinks (e.g. summoning on a smaller monitor than last time)
@MainActor
private func resizeWhileHidden(_ window: Window, to size: CGSize) async throws {
    guard window.isHiddenInCorner, let hidden = try await window.getAxRect() else { return }
    // Sizes from the config are fractional (0.9 * 3008 = 2707.2), but macOS reports whole points
    if abs(hidden.width - size.width) < 1 && abs(hidden.height - size.height) < 1 { return }
    // Move and resize as separate steps, ordered so the visible pixel column never leaves the screen (macOS refuses
    // such frames). In the bottom left corner, a shrinking window must move right first, a growing one must grow first
    let topLeft = hiddenTopLeft(resizing: hidden, to: size, monitors: monitors.map(\.rect))
    if size.width < hidden.width {
        try await window.setAxFrameBlocking(topLeft, nil)
        try await window.setAxFrameBlocking(nil, size)
    } else {
        try await window.setAxFrameBlocking(nil, size)
        try await window.setAxFrameBlocking(topLeft, nil)
    }
}

/// Where a window hidden in a corner (see hideInCorner) must be put when resized to `size`. Only a sliver of a
/// hidden window is on screen: its left edge in the bottom right corner, or its right edge in the bottom left one.
/// That edge must stay on screen, or macOS clamps the resize (it keeps ~24px of the window on screen). Hidden
/// windows sometimes drift a pixel further off screen, so keep a margin: only a 1px tall line is visible anyway
func hiddenTopLeft(resizing hidden: Rect, to size: CGSize, monitors: [Rect]) -> CGPoint {
    let isLeftEdgeVisible = monitors.contains { $0.contains(hidden.topLeftCorner) }
    let onScreenMargin: CGFloat = 32
    return isLeftEdgeVisible
        ? hidden.topLeftCorner
        : CGPoint(x: hidden.maxX + onScreenMargin - size.width, y: hidden.topLeftY)
}

@MainActor
private func launchApp(_ appId: String, scratchpad: String, _ io: CmdIo) -> Bool {
    guard let url = NSWorkspace.shared.urlForApplication(withBundleIdentifier: appId) else {
        return io.err("Can't find app '\(appId)' of scratchpad '\(scratchpad)'")
    }
    // If the app is already running without windows, macOS sends it a "reopen" event, which makes most apps
    // open a new window
    NSWorkspace.shared.openApplication(at: url, configuration: NSWorkspace.OpenConfiguration())
    return true
}
