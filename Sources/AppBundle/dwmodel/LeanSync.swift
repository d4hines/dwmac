import AppKit
import Common

// Lean owns the window manager's state (which workspace each monitor shows, which workspace each window belongs to,
// window order and floating, focus, scratchpads). Swift's tree and focus are a projection of it:
// - Tree mutations (bind, unbind, isFloating) and focus/visibility changes become ops (see the hooks in Window.swift,
//   setFocus, setActiveWorkspace)
// - After every op, `project` makes Swift's tree, visible workspaces and focus match Lean's snapshot. If Lean refused
//   or changed a request (say, binding a window into a scratchpad home), Swift follows Lean
// Config and monitor changes are sent before each op.

@MainActor private var isProjecting = false

/// Stable Lean ids for monitors, by top left corner
@MainActor private var monitorIdsByPoint: [CGPoint: Int] = [:]
@MainActor private var lastEnvironment: (monitors: LeanOp, config: LeanOp)? = nil

@MainActor func leanMonitorId(_ point: CGPoint) -> Int {
    if let id = monitorIdsByPoint[point] { return id }
    let id = monitorIdsByPoint.count
    monitorIdsByPoint[point] = id
    return id
}

@MainActor func leanMonitorPoint(_ id: Int) -> CGPoint? { monitorIdsByPoint.first { $0.value == id }?.key }

@MainActor private func leanForcedAssignment() -> [String: Int] {
    let sorted = sortedMonitors
    var forced: [String: Int] = [:]
    for (workspace, descriptions) in config.workspaceToMonitorForceAssignment {
        if let monitor = descriptions.lazy.compactMap({ $0.resolveMonitor(sortedMonitors: sorted) }).first {
            forced[workspace] = leanMonitorId(monitor.rect.topLeftCorner)
        }
    }
    return forced
}

/// Send monitor and config changes, if any
@MainActor private func leanSyncEnvironment() {
    let mons = monitors.sorted { $0.isMain && !$1.isMain }.map { leanMonitorId($0.rect.topLeftCorner) }
    let forced = leanForcedAssignment()
    let monitorsOp = LeanOp.monitorsChanged(monitors: mons, forced: forced)
    let configOp = LeanOp.configChanged(
        scratchpads: config.scratchpads.values.sorted { $0.name < $1.name }.map { ($0.name, $0.appId) },
        forced: forced,
    )
    if lastEnvironment?.monitors != monitorsOp { LeanState.apply(monitorsOp) }
    if lastEnvironment?.config != configOp { LeanState.apply(configOp) }
    lastEnvironment = (monitorsOp, configOp)
}

/// Apply `op` and make Swift's state match the result
@MainActor @discardableResult
func leanApply(_ op: LeanOp) -> LeanSnapshot {
    leanSyncEnvironment()
    let before = LeanState.last
    let snapshot = LeanState.apply(op)
    project(snapshot, before: before)
    return snapshot
}

/// Pick up monitor and config changes
@MainActor func leanRefresh() {
    let before = LeanState.last
    leanSyncEnvironment()
    project(LeanState.last, before: before)
}

@MainActor func leanResetForTests() {
    monitorIdsByPoint = [:]
    lastEnvironment = nil
    LeanState.reset(scratchpads: [])
    leanRefresh()
}

// MARK: - Hooks, called by Window

/// A window was bound somewhere by Swift code
@MainActor func leanWindowBound(_ window: Window) {
    if isProjecting { return }
    let id = window.windowId
    let known = LeanState.last.window(id) != nil
    let app = window.app.rawAppBundleId ?? ""
    switch window.parent {
        case let workspace as Workspace:
            let index = workspace.allWindows.firstIndex(of: window) ?? 0
            leanApply(known
                ? .placeWindow(id, workspace: workspace.name, index: index, isFloating: window.isFloating)
                : .windowAppeared(id, appId: app, workspace: workspace.name, isFloating: window.isFloating, index: index))
        case let parent?:
            switch parent.cases {
                case .macosFullscreenWindowsContainer, .macosHiddenAppsWindowsContainer:
                    // Parked by macOS, but still part of its workspace
                    if !known, let workspace = parent.nodeWorkspace {
                        leanApply(.windowAppeared(id, appId: app, workspace: workspace.name, isFloating: window.isFloating,
                                                  index: Int(Int32.max)))
                    }
                default:
                    // Popups aren't laid out. Minimized windows leave too, so Lean never focuses one; they come back as
                    // new windows when unminimized
                    if known { leanApply(.windowClosed(id)) }
            }
        case nil: break
    }
}

@MainActor func leanWindowUnbound(_ id: UInt32) {
    if isProjecting || LeanState.last.window(id) == nil { return }
    leanApply(.windowClosed(id))
}

@MainActor func leanFloatingChanged(_ id: UInt32, _ isFloating: Bool) {
    if isProjecting || LeanState.last.window(id) == nil { return }
    leanApply(.setFloating(id, isFloating))
}

// MARK: - Projection

@MainActor private func project(_ snapshot: LeanSnapshot, before: LeanSnapshot) {
    isProjecting = true
    defer { isProjecting = false }

    projectLeanScreens(snapshot)

    // Windows: workspace, order and floating
    var summoned: [String: [Window]] = [:] // Scratchpad windows that left their home, by workspace
    for (name, leanWindows) in Dictionary(grouping: snapshot.windows, by: \.workspace) {
        let workspace = Workspace.get(byName: name)
        let windows = leanWindows.compactMap { Window.get(byId: $0.id) }
        for (window, lean) in zip(windows, leanWindows) where window.isFloating != lean.isFloating {
            window.isFloating = lean.isFloating
        }
        // Windows parked in macOS containers (native fullscreen, minimized, hidden apps) stay there
        let movable = windows.filter { $0.parent == nil || $0.parent is Workspace }
        if workspace.allWindows.map(\.windowId) != movable.map(\.windowId) {
            for window in movable {
                window.bind(to: workspace, index: INDEX_BIND_LAST)
            }
        }
        if !workspace.isScratchpadHome {
            for window in windows where scratchpadOwning(window) != nil {
                // Came out of its home, or is the first window of a scratchpad launched by `scratchpad` (Lean focuses
                // it). A dialog joining its summoned siblings keeps its size
                let wasHidden = before.window(window.windowId).map { $0.workspace.hasPrefix(scratchpadHomePrefix) }
                    ?? (snapshot.focusedWindow == window.windowId)
                if wasHidden { summoned[name, default: []].append(window) }
            }
        }
    }
    for id in snapshot.mru.reversed() {
        Window.get(byId: id)?.markAsMostRecentChild()
    }
    projectLeanFocus(snapshot)

    for (name, windows) in summoned {
        placeSummonedScratchpad(windows, on: Workspace.get(byName: name), focused: snapshot.focusedWindow)
    }
}
