@testable import AppBundle
import XCTest

@MainActor
final class LeanStateTest: XCTestCase {
    func testRoundTrip() {
        LeanState.reset(scratchpads: [("kitty", "net.kovidgoyal.kitty")])
        LeanState.apply(.monitorsChanged(monitors: [0, 1], forced: ["5": 1]))
        LeanState.apply(.windowAppeared(7, appId: "code", workspace: "1", isFloating: false, index: 0))
        let s = LeanState.apply(.windowAppeared(8, appId: "net.kovidgoyal.kitty", workspace: "1", isFloating: false, index: 0))
        assertEquals(s.screens, [.init(monitor: 0, workspace: "1"), .init(monitor: 1, workspace: "2")])
        assertEquals(s.window(7), .init(id: 7, workspace: "1", isFloating: false))
        assertEquals(s.window(8), .init(id: 8, workspace: "scratchpad:kitty", isFloating: true))
        assertEquals(s.focusedWindow, 7)
    }

    func testHomeIsNeverShown() {
        LeanState.reset(scratchpads: [("kitty", "net.kovidgoyal.kitty")])
        LeanState.apply(.windowAppeared(8, appId: "net.kovidgoyal.kitty", workspace: "1", isFloating: false, index: 0))
        for op: LeanOp in [.focusWorkspace("scratchpad:kitty"), .summonWorkspace("scratchpad:kitty"),
                           .showWorkspace(monitor: 0, workspace: "scratchpad:kitty")]
        {
            assertEquals(LeanState.apply(op).screens, [.init(monitor: 0, workspace: "1")])
        }
        // Focusing the hidden window summons it instead
        let s = LeanState.apply(.focusWindow(8))
        assertEquals(s.window(8)?.workspace, "1")
        assertEquals(s.focusedWindow, 8)
    }
}
