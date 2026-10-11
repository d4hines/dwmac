@testable import AppBundle
import AppKit

final class TestWindow: Window, CustomStringConvertible {
    private var _rect: Rect?
    /// Every frame set through setAxFrame, in order
    var frameHistory: [Rect] = []
    var hiddenInCorner = false

    @MainActor
    private init(_ id: UInt32, _ app: TestApp, _ parent: NonLeafDwNodeObject, _ rect: Rect?) {
        _rect = rect
        super.init(id: id, app, lastFloatingSize: nil, parent: parent, index: INDEX_BIND_LAST)
    }

    @discardableResult
    @MainActor
    static func new(id: UInt32, parent: NonLeafDwNodeObject, rect: Rect? = nil, app: TestApp = .shared) -> TestWindow {
        let wi = TestWindow(id, app, parent, rect)
        app._windows.append(wi)
        return wi
    }

    nonisolated var description: String { "TestWindow(\(windowId))" }

    @MainActor
    override func nativeFocus() {
        appForTests = TestApp.shared
        TestApp.shared.focusedWindow = self
    }

    override func closeAxWindow() {
        unbindFromParent()
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

    @MainActor override func forgetHiddenInCorner() {
        hiddenInCorner = false
    }
}
