import AppKit

extension Workspace {
    @MainActor
    func layoutWorkspace() async throws {
        if isEffectivelyEmpty { return }
        let rect = workspaceMonitor.visibleRectPaddedByOuterGaps
        let context = LayoutContext(self)

        // Layout tiling windows
        try await layoutMasterStack(rect.topLeftCorner, width: rect.width, height: rect.height - 1, virtual: rect, context)

        // Layout floating windows
        for window in children.filterIsInstance(of: Window.self).filter({ $0.isFloating }) {
            window.lastAppliedLayoutPhysicalRect = nil
            window.lastAppliedLayoutVirtualRect = nil
            try await window.layoutFloatingWindow(context)
        }
    }

    @MainActor
    private func layoutMasterStack(_ point: CGPoint, width: CGFloat, height: CGFloat, virtual: Rect, _ context: LayoutContext) async throws {
        let windows = tilingWindows
        if windows.isEmpty || layout == .floating { return }

        let gaps = context.resolvedGaps.inner
        let tiles = masterStackTiles(
            width: width,
            height: height,
            gapH: gaps.horizontal,
            gapV: gaps.vertical,
            mfact: mfact,
            orientation: orientation,
            masterPosition: config.masterPosition,
            count: windows.count,
        )
        for (window, tile) in zip(windows, tiles) {
            let tilePoint = CGPoint(x: point.x + tile.topLeftX, y: point.y + tile.topLeftY)
            try await layoutWindow(window, tilePoint, tile.width, tile.height, virtual, context)
        }
    }

    @MainActor
    private func layoutWindow(_ window: Window, _ point: CGPoint, _ width: CGFloat, _ height: CGFloat, _ virtual: Rect, _ context: LayoutContext) async throws {
        let physicalRect = Rect(topLeftX: point.x, topLeftY: point.y, width: width, height: height)

        if window.windowId != currentlyManipulatedWithMouseWindowId {
            window.lastAppliedLayoutVirtualRect = virtual
            // In flat model, no rootTilingContainer. Check if window is fullscreen and matches criteria.
            // Assuming mostRecentWindowRecursive logic works on Workspace now.
            if window.isFullscreen && window == context.workspace.mostRecentWindowRecursive {
                window.lastAppliedLayoutPhysicalRect = nil
                window.layoutFullscreen(context)
            } else {
                window.lastAppliedLayoutPhysicalRect = physicalRect
                window.isFullscreen = false
                window.setAxFrame(point, CGSize(width: width, height: height))
            }
        }
    }
}

private struct LayoutContext {
    let workspace: Workspace
    let resolvedGaps: ResolvedGaps

    @MainActor
    init(_ workspace: Workspace) {
        self.workspace = workspace
        self.resolvedGaps = ResolvedGaps(gaps: config.gaps, monitor: workspace.workspaceMonitor)
    }
}

extension Window {
    @MainActor
    fileprivate func layoutFloatingWindow(_ context: LayoutContext) async throws {
        let workspace = context.workspace
        let targetMonitor = workspace.workspaceMonitor

        if config.centerFloatingWindows && windowId != currentlyManipulatedWithMouseWindowId {
            if let windowSize = try await getAxSize() ?? lastFloatingSize {
                let monitorRect = targetMonitor.visibleRect
                let topLeft = CGPoint(
                    x: monitorRect.topLeftX + (monitorRect.width - windowSize.width) / 2,
                    y: monitorRect.topLeftY + (monitorRect.height - windowSize.height) / 2
                )
                setAxFrame(topLeft, nil)
            }
        } else {
            // Optimization: Only check for monitor drift if the workspace's monitor has changed
            // since the last layout of this window.
            // This avoids expensive AX calls (getCenter/getAxTopLeftCorner) on every layout cycle.
            if lastLayoutMonitor?.rect.topLeftCorner != targetMonitor.rect.topLeftCorner {
                let currentMonitor = try await getCenter()?.monitorApproximation
                if let currentMonitor, let windowTopLeftCorner = try await getAxTopLeftCorner(), workspace != currentMonitor.activeWorkspace {
                    let xProportion = (windowTopLeftCorner.x - currentMonitor.visibleRect.topLeftX) / currentMonitor.visibleRect.width
                    let yProportion = (windowTopLeftCorner.y - currentMonitor.visibleRect.topLeftY) / currentMonitor.visibleRect.height

                    let moveTo = workspace.workspaceMonitor
                    setAxFrame(CGPoint(
                        x: moveTo.visibleRect.topLeftX + xProportion * moveTo.visibleRect.width,
                        y: moveTo.visibleRect.topLeftY + yProportion * moveTo.visibleRect.height,
                    ), nil)
                }
            }
        }
        lastLayoutMonitor = targetMonitor

        if isFullscreen {
            layoutFullscreen(context)
            isFullscreen = false
        }
    }

    @MainActor
    fileprivate func layoutFullscreen(_ context: LayoutContext) {
        let monitorRect = noOuterGapsInFullscreen
            ? context.workspace.workspaceMonitor.visibleRect
            : context.workspace.workspaceMonitor.visibleRectPaddedByOuterGaps
        setAxFrame(monitorRect.topLeftCorner, CGSize(width: monitorRect.width, height: monitorRect.height))
    }
}
