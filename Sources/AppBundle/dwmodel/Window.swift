import AppKit
import Common

open class Window: DwNode, Hashable {
    nonisolated let windowId: UInt32 // todo nonisolated keyword is no longer necessary?
    let app: any AbstractApp
    var lastFloatingSize: CGSize?
    var layoutReason: LayoutReason = .standard
    var isFloating: Bool = false {
        didSet {
            if oldValue != isFloating {
                let (id, isFloating) = (windowId, isFloating)
                MainActor.assumeIsolated { leanFloatingChanged(id, isFloating) }
            }
        }
    }
    /// The Lean monitor id the window was last laid out on, as a floating window
    var lastLayoutMonitor: Int? = nil

    @MainActor
    init(id: UInt32, _ app: any AbstractApp, lastFloatingSize: CGSize?, parent: NonLeafDwNodeObject, index: Int) {
        self.windowId = id
        self.app = app
        self.lastFloatingSize = lastFloatingSize
        super.init(parent: parent, index: index)
    }

    // Swift binding a window is a request to Lean, which owns where windows live (LeanSync.swift)
    @MainActor
    @discardableResult
    override func bind(to newParent: NonLeafDwNodeObject, index: Int) -> BindingData? {
        let result = super.bind(to: newParent, index: index)
        leanWindowBound(self)
        return result
    }

    @discardableResult
    override func unbindFromParent() -> BindingData {
        let result = super.unbindFromParent()
        let id = windowId
        MainActor.assumeIsolated { leanWindowUnbound(id) }
        return result
    }

    @MainActor static func get(byId windowId: UInt32) -> Window? { // todo make non optional
        isUnitTest
            ? Workspace.all.flatMap { $0.allLeafWindowsRecursive }.first(where: { $0.windowId == windowId })
            : MacWindow.allWindowsMap[windowId]
    }

    @MainActor
    func closeAxWindow() { die("Not implemented") }

    nonisolated public func hash(into hasher: inout Hasher) {
        hasher.combine(windowId)
    }

    func getAxTopLeftCorner() async throws -> CGPoint? { die("Not implemented") }
    func getAxSize() async throws -> CGSize? { die("Not implemented") }
    var title: String { get async throws { die("Not implemented") } }
    var isMacosFullscreen: Bool { get async throws { false } }
    var isMacosMinimized: Bool { get async throws { false } } // todo replace with enum MacOsWindowNativeState { normal, fullscreen, invisible }
    var isHiddenInCorner: Bool { die("Not implemented") }
    /// Forget the position from before hideInCorner without moving the window, for callers that set the frame
    /// themselves (otherwise the next refresh's unhideFromCorner would move it back there)
    @MainActor
    func forgetHiddenInCorner() { die("Not implemented") }
    @MainActor
    func nativeFocus() { die("Not implemented") }
    func getAxRect() async throws -> Rect? { die("Not implemented") }
    /// Move the window off screen, into `corner` of `visible` (a monitor's visible rect)
    @MainActor
    func hideInCorner(_ corner: OptimalHideCorner, visible: Rect) async throws { die("Not implemented") }
    /// Undo hideInCorner. `restore`: move it back where it was, relative to `monitor`
    @MainActor
    func unhideFromCorner(monitor: Rect, restore: Bool) { die("Not implemented") }
    func getCenter() async throws -> CGPoint? { try await getAxRect()?.center }

    func setAxFrameBlocking(_ topLeft: CGPoint?, _ size: CGSize?) async throws { die("Not implemented") }
    func setAxFrame(_ topLeft: CGPoint?, _ size: CGSize?) { die("Not implemented") }
}

enum LayoutReason: Equatable {
    case standard
    /// Reason for the cur temp layout is macOS native fullscreen, minimize, or hide
    case macos(prevParentKind: NonLeafDwNodeKind)
}

extension Window {
    /// dwmac's `fullscreen` (Lean shows it fullscreen while it's its workspace's most recent window)
    @MainActor var isFullscreen: Bool { LeanState.last.window(windowId)?.fullscreen != nil }
    @MainActor var noOuterGapsInFullscreen: Bool { LeanState.last.window(windowId)?.fullscreen == true }

    @discardableResult
    @MainActor
    func bindAsFloatingWindow(to workspace: Workspace) -> BindingData? {
        isFloating = true
        return bind(to: workspace, index: INDEX_BIND_LAST)
    }

    func asMacWindow() -> MacWindow { self as! MacWindow }
}
