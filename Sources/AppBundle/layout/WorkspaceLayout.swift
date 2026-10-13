import AppKit

/// Put every window where Lean's layout says (Lean/DwmacCore/Layout.lean). Lean decides the frames, which windows
/// are hidden and in which corner; this only carries the effects out with AX calls
@MainActor
func layoutWorkspaces() async throws {
    leanRefresh() // Monitors or the config may have changed since the last op
    for effect in LeanState.layout(showAll: !TrayMenuModel.shared.isEnabled) {
        try await perform(effect)
    }
}

@MainActor
private func perform(_ effect: LeanEffect) async throws {
    switch effect {
        case .unhide(let id, let monitor, let restore):
            Window.get(byId: id)?.unhideFromCorner(monitor: monitor, restore: restore)
        case .tile(let id, let rect, let virtual, let fullscreen):
            guard let window = Window.get(byId: id), id != currentlyManipulatedWithMouseWindowId else { return }
            window.lastAppliedLayoutVirtualRect = virtual
            window.lastAppliedLayoutPhysicalRect = fullscreen ? nil : rect
            window.setAxFrame(rect.topLeftCorner, rect.size)
        case .float(let id, let monitor, let visible, let center):
            guard let window = Window.get(byId: id) else { return }
            window.lastAppliedLayoutPhysicalRect = nil
            window.lastAppliedLayoutVirtualRect = nil
            try await window.keepFloating(on: monitor, visible: visible, center: center)
        case .hide(let id, let corner, let visible):
            try await Window.get(byId: id)?.hideInCorner(corner, visible: visible)
    }
}

extension Window {
    @MainActor
    fileprivate func keepFloating(on monitor: Int, visible: Rect, center: Bool) async throws {
        if windowId == currentlyManipulatedWithMouseWindowId { return }
        if center {
            if let size = try await getAxSize() ?? lastFloatingSize {
                setAxFrame(CGPoint(x: visible.topLeftX + (visible.width - size.width) / 2,
                                   y: visible.topLeftY + (visible.height - size.height) / 2), nil)
                leanApply(.centered(windowId)) // Once: then it stays where the user puts it
            }
        } else if lastLayoutMonitor != monitor {
            // Its workspace moved to another monitor: take the window along, at the same relative position. Only then,
            // to save AX calls
            if let current = try await getCenter()?.monitorApproximation,
               leanMonitorId(current.rect.topLeftCorner) != monitor,
               let topLeft = try await getAxTopLeftCorner()
            {
                let from = current.visibleRect
                setAxFrame(CGPoint(
                    x: visible.topLeftX + (topLeft.x - from.topLeftX) / from.width * visible.width,
                    y: visible.topLeftY + (topLeft.y - from.topLeftY) / from.height * visible.height,
                ), nil)
            }
        }
        lastLayoutMonitor = monitor
    }
}
