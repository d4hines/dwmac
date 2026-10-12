import AppKit
import Common

// Scratchpads (xmonad's NamedScratchpad). Invariant: every window of a scratchpad's app lives either in the
// scratchpad's hidden home workspace or floating on the workspace it's summoned to. Windows of the app never
// tile into regular workspaces, and no other app's windows ever enter the home workspace.

private let scratchpadHomePrefix = "scratchpad:"

extension Workspace {
    var isScratchpadHome: Bool { name.hasPrefix(scratchpadHomePrefix) }
}

extension Scratchpad {
    @MainActor var home: Workspace { Workspace.get(byName: scratchpadHomePrefix + name) }

    /// Windows that dwmac lays out (popups, minimized, native fullscreen and hidden-app windows are skipped)
    @MainActor var windows: [Window] {
        Workspace.all.flatMap { $0.children.filterIsInstance(of: Window.self) }
            .filter { $0.app.rawAppBundleId == appId }
    }
}

@MainActor func scratchpadOwning(_ window: Window) -> Scratchpad? {
    config.scratchpads.values.first { $0.appId == window.app.rawAppBundleId }
}

private struct PendingSummon {
    let workspaceName: String
    let date: Date
}

/// Scratchpads launched by `scratchpad` command whose first window should be summoned once it appears
@MainActor private var pendingSummons: [String: PendingSummon] = [:]
private let pendingSummonTimeout: TimeInterval = 10

@MainActor
func toggleScratchpad(_ scratchpad: Scratchpad, _ io: CmdIo) async throws -> Bool {
    let workspace = focus.workspace
    let windows = scratchpad.windows
    if windows.isEmpty {
        return launchScratchpad(scratchpad, onto: workspace, io)
    }
    let windowsHere = windows.filter { $0.nodeWorkspace == workspace }
    if windowsHere.isEmpty {
        try await resizeWhileHidden(primaryWindow(windows), to: summonedFrame(scratchpad, on: workspace).size)
        summonScratchpad(scratchpad, windows, to: workspace)
        return primaryWindow(windows).focusWindow()
    }
    if let focused = focus.windowOrNil, windowsHere.contains(focused) {
        let home = scratchpad.home
        for window in windows where window.nodeWorkspace != home {
            window.bindAsFloatingWindow(to: home)
        }
        return workspace.focusWorkspace()
    }
    return primaryWindow(windowsHere).focusWindow()
}

/// Called for every newly detected window, before on-window-detected callbacks. Returns true if the window
/// belongs to a scratchpad (in which case on-window-detected callbacks must not run)
@MainActor
func adoptIntoScratchpadIfOwned(_ window: Window) -> Bool {
    guard window.parent is Workspace, let scratchpad = scratchpadOwning(window) else { return false }
    if let pending = pendingSummons.removeValue(forKey: scratchpad.name),
       pending.date.distance(to: .now) < pendingSummonTimeout
    {
        summonScratchpad(scratchpad, [window], to: Workspace.get(byName: pending.workspaceName))
        _ = window.focusWindow()
        return true
    }
    // New windows (dialogs, second windows) join their already summoned siblings. Otherwise, they stay hidden
    let home = scratchpad.home
    let summonedTo = scratchpad.windows.first { $0 != window && $0.nodeWorkspace != home }?.nodeWorkspace
    window.bindAsFloatingWindow(to: summonedTo ?? home)
    return true
}

/// Summon the scratchpad of a window hidden in its home onto the focused workspace. Called by setFocus, which never
/// focuses a home
@MainActor
func summonIfInScratchpadHome(_ window: Window) {
    guard window.nodeWorkspace?.isScratchpadHome == true, let scratchpad = scratchpadOwning(window),
          !focus.workspace.isScratchpadHome else { return }
    summonScratchpad(scratchpad, scratchpad.windows, to: focus.workspace)
}

/// A summoned scratchpad is only visible while it has focus: once focus moves to any other window (a click, a
/// focus command, another scratchpad), it goes back home. Otherwise, a tiled window raised by macOS could end up
/// on top of it. Called whenever the focus changes
@MainActor
func hideUnfocusedScratchpads() {
    let focused = focus.windowOrNil.flatMap(scratchpadOwning)
    for scratchpad in config.scratchpads.values where scratchpad.name != focused?.name {
        let summoned = scratchpad.windows.filter { $0.nodeWorkspace?.isScratchpadHome != true }
        if summoned.isEmpty { continue }
        let home = scratchpad.home
        for window in summoned {
            window.bindAsFloatingWindow(to: home)
        }
    }
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
private func summonScratchpad(_ scratchpad: Scratchpad, _ windows: [Window], to workspace: Workspace) {
    let primary = primaryWindow(windows)
    // Bind the primary window last, so it ends up the most recent child of the workspace
    for window in windows.filter({ $0 != primary }) + [primary] {
        window.bindAsFloatingWindow(to: workspace)
    }
    // Other windows (dialogs, second windows) go back where they were before hideInCorner. The primary window is
    // moved straight to its frame below; restoring its old position first would move it twice
    for window in windows where window != primary {
        (window as? MacWindow)?.unhideFromCorner()
    }
    if primary.isHiddenInCorner { primary.forgetHiddenInCorner() }
    let frame = summonedFrame(scratchpad, on: workspace)
    primary.lastFloatingSize = frame.size
    primary.setAxFrame(frame.topLeftCorner, frame.size)
}

@MainActor
private func launchScratchpad(_ scratchpad: Scratchpad, onto workspace: Workspace, _ io: CmdIo) -> Bool {
    guard let url = NSWorkspace.shared.urlForApplication(withBundleIdentifier: scratchpad.appId) else {
        return io.err("Can't find app '\(scratchpad.appId)' of scratchpad '\(scratchpad.name)'")
    }
    pendingSummons[scratchpad.name] = PendingSummon(workspaceName: workspace.name, date: .now)
    // If the app is already running without windows, macOS sends it a "reopen" event, which makes most apps
    // open a new window
    NSWorkspace.shared.openApplication(at: url, configuration: NSWorkspace.OpenConfiguration())
    return true
}
