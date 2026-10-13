@testable import AppBundle
import Common
import XCTest

@MainActor
final class FocusCommandTest: XCTestCase {
    override func setUp() async throws { setUpWorkspacesForTests() }

    func testParse() {
        XCTAssertTrue(parseCommand("focus --boundaries left").errorOrNil?.contains("Possible values") == true)
        var expected = FocusCmdArgs(rawArgs: ["--boundaries", "workspace", "next"].slice, nextPrev: .next)
        expected.rawBoundaries = .workspace
        testParseCommandSucc("focus --boundaries workspace next", expected)

        assertEquals(
            parseCommand("focus --boundaries workspace --boundaries workspace next").errorOrNil,
            "ERROR: Duplicated option '--boundaries'",
        )
        assertEquals(
            parseCommand("focus --window-id 42 --ignore-floating").errorOrNil,
            "--window-id is incompatible with other options",
        )
        assertEquals(
            parseCommand("focus --boundaries all-monitors-outer-frame next").errorOrNil,
            "(next|prev) only supports --boundaries workspace",
        )
    }
}

extension FocusCommand {
    static func new(relative: NextPrev) -> FocusCommand {
        FocusCommand(args: FocusCmdArgs(rawArgs: [], nextPrev: relative))
    }
}
