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
        assertEquals(window.nodeWorkspace, scratchpad.home)
        XCTAssertTrue(window.isFloating)

        // Other apps' windows are left alone
        let other = TestWindow.new(id: 2, parent: focus.workspace, app: .other)
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

    func testHidesWhenFocusLeaves() async throws {
        let workspace = focus.workspace
        let tiled = TestWindow.new(id: 1, parent: workspace, app: .other)
        _ = tiled.focusWindow()
        let window = TestWindow.new(id: 2, parent: workspace)
        window.bindAsFloatingWindow(to: scratchpad.home)
        try await toggle()

        assertEquals(window.nodeWorkspace, workspace)

        // Focus moves to another window: the scratchpad goes home
        _ = tiled.focusWindow()
        assertEquals(window.nodeWorkspace, scratchpad.home)
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
        _ = TestWindow.new(id: 1, parent: focus.workspace)
        try await toggle()
        let dialog = TestWindow.new(id: 2, parent: Workspace.get(byName: "a"))
        assertEquals(dialog.nodeWorkspace, focus.workspace)
        XCTAssertTrue(dialog.isFloating)
    }

    func testNativeFocusSummonsInsteadOfShowingHome() {
        let workspace = focus.workspace
        let tiled = TestWindow.new(id: 1, parent: workspace, app: .other)
        _ = tiled.focusWindow()
        let window = TestWindow.new(id: 2, parent: workspace)
        window.bindAsFloatingWindow(to: scratchpad.home)

        // macOS focuses the hidden window (Cmd-Tab): it comes to the workspace, the home never becomes visible
        XCTAssertTrue(window.focusWindow())
        assertEquals(window.nodeWorkspace, workspace)
        XCTAssertTrue(window.isFloating)
        assertEquals(focus.workspace, workspace)
        assertEquals(focus.windowOrNil, window)
        XCTAssertFalse(scratchpad.home.isVisible)
    }

    func testHomeIsNeverShown() {
        let workspace = focus.workspace
        let home = scratchpad.home
        XCTAssertFalse(home.focusWorkspace())
        assertEquals(focus.workspace, workspace)
        XCTAssertFalse(home.isVisible)
        XCTAssertFalse(workspace.workspaceMonitor.setActiveWorkspace(home))
        XCTAssertFalse(home.isVisible)
    }

    func testHomeIsSkippedByNextPrev() async throws {
        _ = TestWindow.new(id: 1, parent: focus.workspace)
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

    func testHiddenTopLeft() {
        let monitor = Rect(topLeftX: 0, topLeftY: 0, width: 1920, height: 1080)
        let size = CGSize(width: 600, height: 400)
        // Hidden in the bottom left corner: only the right edge is on screen, so it stays put (plus a margin)
        let bottomLeft = Rect(topLeftX: -999, topLeftY: 1079, width: 1000, height: 800)
        assertEquals(hiddenTopLeft(resizing: bottomLeft, to: size, monitors: [monitor]), CGPoint(x: -599 + 32, y: 1079))
        // Hidden in the bottom right corner: only the left edge is on screen, so it stays put
        let bottomRight = Rect(topLeftX: 1919, topLeftY: 1079, width: 1000, height: 800)
        assertEquals(hiddenTopLeft(resizing: bottomRight, to: size, monitors: [monitor]), CGPoint(x: 1919, y: 1079))
    }

    /// Summoning a window hidden at another size resizes it while it's still hidden, then moves it on screen in
    /// one go, so the app never redraws at the new size on screen. A shrinking window hidden in the bottom left
    /// corner moves right first, so its right edge stays on screen
    func testSummonShrinksWhileHidden() async throws {
        let window = try await summonHidden(Rect(topLeftX: -999, topLeftY: 1079, width: 1000, height: 800))
        assertEquals(window.frameHistory.map(\.topLeftCorner), [CGPoint(x: -927, y: 1079), CGPoint(x: -927, y: 1079), summonedCenter])
        assertEquals(window.frameHistory.map(\.size), [CGSize(width: 1000, height: 800), summonedSize, summonedSize])
        XCTAssertFalse(window.hiddenInCorner)
    }

    /// A growing window hidden in the bottom left corner grows first, then moves left
    func testSummonGrowsWhileHidden() async throws {
        let window = try await summonHidden(Rect(topLeftX: -499, topLeftY: 1079, width: 500, height: 300))
        assertEquals(window.frameHistory.map(\.topLeftCorner), [CGPoint(x: -499, y: 1079), CGPoint(x: -927, y: 1079), summonedCenter])
        assertEquals(window.frameHistory.map(\.size), [summonedSize, summonedSize, summonedSize])
    }

    private let summonedSize = CGSize(width: 960, height: 540) // 0.5 x 0.5 of the 1920x1080 test monitor
    private let summonedCenter = CGPoint(x: 480, y: 270)

    private func summonHidden(_ hidden: Rect) async throws -> TestWindow {
        _ = TestWindow.new(id: 1, parent: focus.workspace, app: .other).focusWindow()
        let window = TestWindow.new(id: 2, parent: focus.workspace, rect: hidden)
        window.bindAsFloatingWindow(to: scratchpad.home)
        window.hiddenInCorner = true
        try await toggle()
        return window
    }
}
