@testable import AppBundle
import Common
import XCTest

@MainActor
final class ScratchpadCommandTest: XCTestCase {
    override func setUp() async throws {
        setUpWorkspacesForTests()
        config.scratchpads = ["test": Scratchpad(name: "test", appId: TestApp.shared.rawAppBundleId.orDie(), width: 0.5, height: 0.5)]
    }

    private var scratchpad: Scratchpad { config.scratchpads["test"].orDie() }

    private func toggle() async throws {
        try await ScratchpadCommand(args: ScratchpadCmdArgs(rawArgs: []).copy(\.name, .initialized("test")))
            .run(.defaultEnv, .emptyStdin)
    }

    func testParse() {
        assertEquals(parseCommand("scratchpad").errorOrNil, "ERROR: Argument '<scratchpad>' is mandatory")
        XCTAssertNotNil(parseCommand("scratchpad signal").cmdOrNil)
    }

    func testUndeclared() async throws {
        let result = try await ScratchpadCommand(args: ScratchpadCmdArgs(rawArgs: []).copy(\.name, .initialized("nope")))
            .run(.defaultEnv, .emptyStdin)
        assertEquals(result.exitCode, 1)
        assertEquals(result.stderr, ["Scratchpad 'nope' isn't declared. Declare it in the config with [scratchpad.nope]"])
    }

    func testDetectedWindowGoesHome() {
        let window = TestWindow.new(id: 1, parent: focus.workspace)
        XCTAssertTrue(adoptIntoScratchpadIfOwned(window))
        assertEquals(window.nodeWorkspace, scratchpad.home)
        XCTAssertTrue(window.isFloating)

        // Other apps' windows are left alone
        let other = TestWindow.new(id: 2, parent: focus.workspace, app: .other)
        XCTAssertFalse(adoptIntoScratchpadIfOwned(other))
        assertEquals(other.nodeWorkspace, focus.workspace)
    }

    func testSummonFocusDismiss() async throws {
        let workspace = focus.workspace
        let tiled = TestWindow.new(id: 1, parent: workspace, app: .other)
        _ = tiled.focusWindow()
        let window = TestWindow.new(id: 2, parent: workspace, rect: Rect(topLeftX: 0, topLeftY: 0, width: 10, height: 10))
        window.bindAsFloatingWindow(to: scratchpad.home)

        // Summon: floats on the focused workspace, sized and centered, focused
        try await toggle()
        assertEquals(window.nodeWorkspace, workspace)
        XCTAssertTrue(window.isFloating)
        assertEquals(focus.windowOrNil, window)
        let monitorRect = workspace.workspaceMonitor.visibleRect
        assertEquals(try await window.getAxRect()?.width, monitorRect.width * 0.5)
        assertEquals(try await window.getAxRect()?.topLeftX, monitorRect.topLeftX + monitorRect.width * 0.25)

        // On the workspace but not focused: focus it
        _ = tiled.focusWindow()
        try await toggle()
        assertEquals(window.nodeWorkspace, workspace)
        assertEquals(focus.windowOrNil, window)

        // Focused: dismiss and give focus back to the workspace
        try await toggle()
        assertEquals(window.nodeWorkspace, scratchpad.home)
        assertEquals(focus.workspace, workspace)
        assertEquals(focus.windowOrNil, tiled)
    }

    func testSummonFromAnotherWorkspace() async throws {
        let window = TestWindow.new(id: 1, parent: Workspace.get(byName: "a"))
        window.isFloating = true
        let workspace = focus.workspace
        try await toggle()
        assertEquals(window.nodeWorkspace, workspace)
        assertEquals(focus.windowOrNil, window)
    }

    func testNewWindowJoinsSummonedSiblings() async throws {
        let first = TestWindow.new(id: 1, parent: focus.workspace)
        _ = adoptIntoScratchpadIfOwned(first)
        try await toggle()
        let dialog = TestWindow.new(id: 2, parent: Workspace.get(byName: "a"))
        XCTAssertTrue(adoptIntoScratchpadIfOwned(dialog))
        assertEquals(dialog.nodeWorkspace, focus.workspace)
        XCTAssertTrue(dialog.isFloating)
    }

    func testHomeIsSkippedByNextPrev() async throws {
        let window = TestWindow.new(id: 1, parent: focus.workspace)
        _ = adoptIntoScratchpadIfOwned(window)
        Workspace.get(byName: "a").apply { TestWindow.new(id: 2, parent: $0, app: .other) }
        let next = getNextPrevWorkspace(current: focus.workspace, isNext: true, wrapAround: true, stdin: nil, target: focus)
        XCTAssertFalse(next.orDie().isScratchpadHome)
    }

    func testParseConfig() {
        let (parsed, errors) = parseConfig(
            """
            [scratchpad.signal]
                app-id = 'org.whispersystems.signal-desktop'
                width = 0.8
            [scratchpad.noid]
                height = 2
            """,
        )
        assertEquals(parsed.scratchpads, ["signal": Scratchpad(name: "signal", appId: "org.whispersystems.signal-desktop", width: 0.8, height: 0.9)])
        assertEquals(errors.descriptions.sorted(), [
            "scratchpad.noid.height: Must be in (0, 1] range",
            "scratchpad.noid: 'app-id' is mandatory key",
        ])
    }

    func testParseConfigDuplicatedAppId() {
        let (parsed, errors) = parseConfig(
            """
            [scratchpad.a]
                app-id = 'org.whispersystems.signal-desktop'
            [scratchpad.b]
                app-id = 'org.whispersystems.signal-desktop'
            """,
        )
        // Which one wins depends on TOML table iteration order
        assertEquals(parsed.scratchpads.count, 1)
        assertEquals(errors.count, 1)
        XCTAssertTrue(errors.descriptions.first.orDie().contains("is already owned by scratchpad"))
    }
}
