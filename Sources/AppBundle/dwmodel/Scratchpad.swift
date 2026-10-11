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
func toggleScratchpad(_ scratchpad: Scratchpad, _ io: CmdIo) -> Bool {
    let workspace = focus.workspace
    let windows = scratchpad.windows
    if windows.isEmpty {
        return launchScratchpad(scratchpad, onto: workspace, io)
    }
    let windowsHere = windows.filter { $0.nodeWorkspace == workspace }
    if windowsHere.isEmpty {
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

/// The window that was focused last. It gets the configured size; the rest keep their size
@MainActor
private func primaryWindow(_ windows: [Window]) -> Window {
    windows.first { $0.parent?.mostRecentChild == $0 } ?? windows.last.orDie()
}

@MainActor
private func summonScratchpad(_ scratchpad: Scratchpad, _ windows: [Window], to workspace: Workspace) {
    let primary = primaryWindow(windows)
    // Bind the primary window last, so it ends up the most recent child of the workspace
    for window in windows.filter({ $0 != primary }) + [primary] {
        window.bindAsFloatingWindow(to: workspace)
        // Restore the position from before hideInCorner now, so that refresh doesn't override the frame below
        (window as? MacWindow)?.unhideFromCorner()
    }
    let rect = workspace.workspaceMonitor.visibleRect
    let size = CGSize(width: rect.width * scratchpad.width, height: rect.height * scratchpad.height)
    primary.lastFloatingSize = size
    primary.setAxFrame(
        CGPoint(x: rect.topLeftX + (rect.width - size.width) / 2, y: rect.topLeftY + (rect.height - size.height) / 2),
        size,
    )
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
