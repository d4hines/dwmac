import AppKit
import Common

final class MacWindow: Window {
    let macApp: MacApp
    private var prevUnhiddenProportionalPositionInsideWorkspaceRect: CGPoint?

    private init(_ id: UInt32, _ actor: MacApp, lastFloatingSize: CGSize?) {
        self.macApp = actor
        super.init(id: id, actor, lastFloatingSize: lastFloatingSize)
    }

    @MainActor static var allWindows: [MacWindow] { Window.allWindowsMap.values.compactMap { $0 as? MacWindow } }

    @MainActor
    @discardableResult
    static func getOrRegister(windowId: UInt32, macApp: MacApp) async throws -> MacWindow {
        if let existing = allWindowsMap[windowId] as? MacWindow { return existing }
        let rect = try await macApp.getAxRect(windowId)
        let type = try await macApp.windowKind(windowId, getWindowLevel(for: windowId))?.type ?? .window

        // atomic synchronous section
        if let existing = allWindowsMap[windowId] as? MacWindow { return existing }
        let window = MacWindow(windowId, macApp, lastFloatingSize: rect?.size)
        allWindowsMap[windowId] = window
        let workspace = isStartup ? (rect?.center.monitorApproximation ?? mainMonitor).activeWorkspace : focus.workspace
        switch type {
            case .popup: window.unmanaged = .popup
            // Lean puts scratchpad windows in their home, or summons them
            case .dialog: window.manage(on: workspace, floating: true)
            case .window: window.manage(on: workspace, floating: false)
        }

        try await debugWindowsIfRecording(window)
        if try await !restoreClosedWindowsCacheIfNeeded(newlyDetectedWindow: window), scratchpadOwning(window) == nil {
            try await tryOnWindowDetected(window)
        }
        return window
    }

    @MainActor
    func windowKind(_ windowLevel: MacOsWindowLevel?) async throws -> (type: AxUiElementWindowType, isDialog: Bool)? {
        try await macApp.windowKind(windowId, windowLevel)
    }

    func dumpAxInfo() async throws -> [String: Json] {
        try await macApp.dumpWindowAxInfo(windowId: windowId)
    }

    func setNativeFullscreen(_ value: Bool) {
        macApp.setNativeFullscreen(windowId, value)
    }

    func setNativeMinimized(_ value: Bool) {
        macApp.setNativeMinimized(windowId, value)
    }

    @MainActor override var title: String { get async throws { try await macApp.getAxTitle(windowId) ?? "" } }
    @MainActor override var isMacosFullscreen: Bool { get async throws { try await macApp.isMacosNativeFullscreen(windowId) == true } }
    @MainActor override var isMacosMinimized: Bool { get async throws { try await macApp.isMacosNativeMinimized(windowId) == true } }

    @MainActor
    override func nativeFocus() {
        macApp.nativeFocus(windowId)
    }

    override func closeAxWindow() {
        garbageCollect(skipClosedWindowsCache: true)
        macApp.closeAndUnregisterAxWindow(windowId)
    }

    @MainActor
    override func hideInCorner(_ corner: OptimalHideCorner, visible: Rect) async throws {
        // Don't accidentally override prevUnhiddenEmulationPosition in case of subsequent
        // `hideEmulation` calls
        if !isHiddenInCorner {
            guard let windowRect = try await getAxRect() else { return }
            let topLeftCorner = windowRect.topLeftCorner
            let monitorRect = windowRect.center.monitorApproximation.rect // Similar to layoutFloatingWindow. Non idempotent
            let absolutePoint = topLeftCorner - monitorRect.topLeftCorner
            prevUnhiddenProportionalPositionInsideWorkspaceRect =
                CGPoint(x: absolutePoint.x / monitorRect.width, y: absolutePoint.y / monitorRect.height)
        }
        let p: CGPoint
        switch corner {
            case .bottomLeftCorner:
                guard let s = try await getAxSize() else { fallthrough }
                // Zoom will jump off if you do one pixel offset https://github.com/hillyu/Dwmac/issues/527
                // todo this ad hoc won't be necessary once I implement optimization suggested by Zalim
                let onePixelOffset = macApp.rawAppBundleId == "us.zoom.xos" ? .zero : CGPoint(x: 1, y: -1)
                p = visible.bottomLeftCorner + onePixelOffset + CGPoint(x: -s.width, y: 0)
            case .bottomRightCorner:
                // Zoom will jump off if you do one pixel offset https://github.com/hillyu/Dwmac/issues/527
                // todo this ad hoc won't be necessary once I implement optimization suggested by Zalim
                let onePixelOffset = macApp.rawAppBundleId == "us.zoom.xos" ? .zero : CGPoint(x: 1, y: 1)
                p = visible.bottomRightCorner - onePixelOffset
        }
        setAxFrame(p, nil)
    }

    @MainActor
    override func unhideFromCorner(monitor: Rect, restore: Bool) {
        guard let prevUnhiddenProportionalPositionInsideWorkspaceRect else { return }
        // Only floating windows are moved back: the layout places tiling ones
        if restore {
            let pointInsideWorkspace = CGPoint(
                x: monitor.width * prevUnhiddenProportionalPositionInsideWorkspaceRect.x,
                y: monitor.height * prevUnhiddenProportionalPositionInsideWorkspaceRect.y,
            )
            setAxFrame(monitor.topLeftCorner + pointInsideWorkspace, nil)
        }
        self.prevUnhiddenProportionalPositionInsideWorkspaceRect = nil
    }

    override var isHiddenInCorner: Bool {
        prevUnhiddenProportionalPositionInsideWorkspaceRect != nil
    }

    @MainActor
    override func forgetHiddenInCorner() {
        prevUnhiddenProportionalPositionInsideWorkspaceRect = nil
    }

    override func getAxSize() async throws -> CGSize? {
        try await macApp.getAxSize(windowId)
    }

    override func setAxFrame(_ topLeft: CGPoint?, _ size: CGSize?) {
        macApp.setAxFrame(windowId, topLeft, size)
    }

    override func setAxFrameBlocking(_ topLeft: CGPoint?, _ size: CGSize?) async throws {
        try await macApp.setAxFrameBlocking(windowId, topLeft, size)
    }

    override func getAxTopLeftCorner() async throws -> CGPoint? {
        try await macApp.getAxTopLeftCorner(windowId)
    }

    override func getAxRect() async throws -> Rect? {
        try await macApp.getAxRect(windowId)
    }
}

@MainActor
func tryOnWindowDetected(_ window: Window) async throws {
    if window.unmanaged != .popup {
        try await onWindowDetected(window)
    }
}

@MainActor
private func onWindowDetected(_ window: Window) async throws {
    for callback in config.onWindowDetected where try await callback.matches(window) {
        _ = try await callback.run.runCmdSeq(.defaultEnv.copy(\.windowId, window.windowId), .emptyStdin)
        if !callback.checkFurtherCallbacks {
            return
        }
    }
}

extension WindowDetectedCallback {
    @MainActor
    func matches(_ window: Window) async throws -> Bool {
        if let startupMatcher = matcher.duringDwmacStartup, startupMatcher != isStartup {
            return false
        }
        if let regex = matcher.windowTitleRegexSubstring, !(try await window.title).contains(regex) {
            return false
        }
        if let appId = matcher.appId, appId != window.app.rawAppBundleId {
            return false
        }
        if let regex = matcher.appNameRegexSubstring, !(window.app.name ?? "").contains(regex) {
            return false
        }
        if let workspace = matcher.workspace, workspace != window.workspace?.name {
            return false
        }
        return true
    }
}
