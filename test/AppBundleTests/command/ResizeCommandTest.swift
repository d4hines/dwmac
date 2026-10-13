@testable import AppBundle
import Common
import XCTest

final class ResizeCommandTest: XCTestCase {
    @MainActor
    func testResizeFloating() async throws {
        setUpWorkspacesForTests()
        let workspace = Workspace.get(byName: "testResizeFloating")
        let window = TestWindow.new(id: 1, parent: workspace, rect: Rect(topLeftX: 100, topLeftY: 100, width: 200, height: 200), floating: true)
        _ = window.focusWindow()

        // Resize width +50
        try await cmd("resize width +50").run(.defaultEnv, .emptyStdin)

        var rect = try await window.getAxRect()
        assertEquals(rect?.width, 250)
        assertEquals(rect?.height, 200)

        // Resize height set 300
        try await cmd("resize height 300").run(.defaultEnv, .emptyStdin)

        rect = try await window.getAxRect()
        assertEquals(rect?.width, 250)
        assertEquals(rect?.height, 300)
    }

    @MainActor
    func testResizeTilingFails() async throws {
        setUpWorkspacesForTests()
        let workspace = Workspace.get(byName: "testResizeTilingFails")
        let window = TestWindow.new(id: 1, parent: workspace)
        _ = window.focusWindow()

        let result = try await cmd("resize width +50").run(.defaultEnv, .emptyStdin)
        assertEquals(result.exitCode, 1)
    }
}
