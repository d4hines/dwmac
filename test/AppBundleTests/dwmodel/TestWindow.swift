@testable import AppBundle
import AppKit

final class TestWindow: Window, CustomStringConvertible {
    private var _rect: Rect?
    /// Every frame set through setAxFrame, in order
    var frameHistory: [Rect] = []
    var hiddenInCorner = false

    private init(_ id: UInt32, _ app: TestApp, _ rect: Rect?) {
        _rect = rect
        super.init(id: id, app, lastFloatingSize: nil)
    }

    /// A new window, last in `parent`
    @discardableResult
    @MainActor
    static func new(id: UInt32, parent: Workspace, rect: Rect? = nil, app: TestApp = .shared, floating: Bool = false) -> TestWindow {
        let wi = TestWindow(id, app, rect)
        app._windows.append(wi)
        allWindowsMap[id] = wi
        wi.manage(on: parent, floating: floating, index: Int(Int32.max))
        return wi
    }

    nonisolated var description: String { "TestWindow(\(windowId))" }

    @MainActor
    override func nativeFocus() {
        appForTests = TestApp.shared
        TestApp.shared.focusedWindow = self
    }

    override func closeAxWindow() {
        garbageCollect(skipClosedWindowsCache: true)
    }

    override var title: String {
        get async { // redundant async. todo create bug report to Swift
            description
        }
    }

    @MainActor override func getAxRect() async throws -> Rect? { // todo change to not Optional
        _rect
    }

    override func setAxFrame(_ topLeft: CGPoint?, _ size: CGSize?) {
        guard let old = _rect else { return }
        let newTopLeft = topLeft ?? old.topLeftCorner
        let newSize = size ?? old.size
        let rect = Rect(topLeftX: newTopLeft.x, topLeftY: newTopLeft.y, width: newSize.width, height: newSize.height)
        _rect = rect
        frameHistory.append(rect)
    }

    override func setAxFrameBlocking(_ topLeft: CGPoint?, _ size: CGSize?) async throws {
        setAxFrame(topLeft, size)
    }

    override var isHiddenInCorner: Bool { hiddenInCorner }

    @MainActor override func hideInCorner(_ corner: OptimalHideCorner, visible: Rect) async throws {
        hiddenInCorner = true
    }

    @MainActor override func unhideFromCorner(monitor: Rect, restore: Bool) {
        hiddenInCorner = false
    }

    @MainActor override func forgetHiddenInCorner() {
        hiddenInCorner = false
    }
}
