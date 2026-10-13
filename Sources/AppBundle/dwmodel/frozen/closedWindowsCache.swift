import AppKit
import Common

/// First line of defence against lock screen: the layout from before windows started closing, to put them back
/// when they reappear with the same ids
@MainActor private var closedWindowsCache = FrozenWorld(workspaces: [], monitors: [], windowIds: [])

private struct FrozenWorld {
    let workspaces: [FrozenWorkspace]
    let monitors: [FrozenMonitor]
    let windowIds: Set<UInt32>
}

private struct FrozenMonitor {
    let topLeftCorner: CGPoint
    let visibleWorkspace: String

    @MainActor init(_ monitor: Monitor) {
        topLeftCorner = monitor.rect.topLeftCorner
        visibleWorkspace = monitor.activeWorkspace.name
    }
}

private struct FrozenWorkspace {
    let name: String
    let monitor: FrozenMonitor
    let layout: Layout
    let orientation: Orientation
    /// In tiling order
    let windows: [(id: UInt32, isFloating: Bool)]

    @MainActor init(_ workspace: Workspace) {
        name = workspace.name
        monitor = FrozenMonitor(workspace.workspaceMonitor)
        layout = workspace.layout
        orientation = workspace.orientation
        windows = LeanState.last.windows(in: workspace.name).map { ($0.id, $0.isFloating) }
    }
}

@MainActor func cacheClosedWindowIfNeeded() {
    let allWindowIds = LeanState.last.windows.map(\.id).toSet()
    if allWindowIds.isSubset(of: closedWindowsCache.windowIds) {
        return // already cached
    }
    closedWindowsCache = FrozenWorld(
        workspaces: Workspace.all.map { FrozenWorkspace($0) },
        monitors: monitors.map(FrozenMonitor.init),
        windowIds: allWindowIds,
    )
}

@MainActor func restoreClosedWindowsCacheIfNeeded(newlyDetectedWindow: Window) async throws -> Bool {
    if !closedWindowsCache.windowIds.contains(newlyDetectedWindow.windowId) {
        return false
    }
    let topLeftCornerToMonitor = monitors.grouped { $0.rect.topLeftCorner }

    for frozenWorkspace in closedWindowsCache.workspaces {
        let workspace = Workspace.get(byName: frozenWorkspace.name)
        _ = topLeftCornerToMonitor[frozenWorkspace.monitor.topLeftCorner]?
            .singleOrNil()?
            .setActiveWorkspace(workspace)
        workspace.layout = frozenWorkspace.layout
        workspace.orientation = frozenWorkspace.orientation
        for (index, window) in frozenWorkspace.windows.enumerated() where Window.get(byId: window.id) != nil {
            leanApply(.placeWindow(window.id, workspace: workspace.name, index: index, isFloating: window.isFloating))
        }
    }

    for monitor in closedWindowsCache.monitors {
        _ = topLeftCornerToMonitor[monitor.topLeftCorner]?
            .singleOrNil()?
            .setActiveWorkspace(Workspace.get(byName: monitor.visibleWorkspace))
    }
    return true
}

@MainActor func resetClosedWindowsCache() {
    closedWindowsCache = FrozenWorld(workspaces: [], monitors: [], windowIds: [])
}
