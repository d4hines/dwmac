@testable import AppBundle
import Common
import XCTest

@MainActor
final class CloseCommandTest: XCTestCase {
    override func setUp() async throws { setUpWorkspacesForTests() }

    func testSimple() async throws {
        Workspace.get(byName: name).apply {
            _ = TestWindow.new(id: 1, parent: $0).focusWindow()
            TestWindow.new(id: 2, parent: $0)
        }

        assertEquals(focus.windowOrNil?.windowId, 1)
        assertEquals(focus.workspace.allWindows.count, 2)

        try await cmd("close").run(.defaultEnv, .emptyStdin)

        assertEquals(focus.windowOrNil?.windowId, 2)
        assertEquals(focus.workspace.allWindows.count, 1)
    }

    func testCloseViaWindowIdFlag() async throws {
        Workspace.get(byName: name).apply {
            _ = TestWindow.new(id: 1, parent: $0).focusWindow()
            TestWindow.new(id: 2, parent: $0)
        }

        assertEquals(focus.windowOrNil?.windowId, 1)
        assertEquals(focus.workspace.allWindows.count, 2)

        try await cmd("close --window-id 2").run(.defaultEnv, .emptyStdin)

        assertEquals(focus.windowOrNil?.windowId, 1)
        assertEquals(focus.workspace.allWindows.count, 1)
    }
}
