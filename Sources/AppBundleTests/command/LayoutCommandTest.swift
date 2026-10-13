@testable import AppBundle
import Common
import XCTest

final class LayoutCommandTest: XCTestCase {
    @MainActor
    override func setUp() async throws { setUpWorkspacesForTests() }

    func testParseCommand() {
        testParseCommandSucc("layout master-stack", LayoutCmdArgs(rawArgs: [], toggleBetween: [.master_stack]))
        testParseCommandSucc("layout h_master-stack", LayoutCmdArgs(rawArgs: [], toggleBetween: [.h_master_stack]))
        testParseCommandSucc("layout v_master-stack", LayoutCmdArgs(rawArgs: [], toggleBetween: [.v_master_stack]))
        testParseCommandSucc("layout tiling floating", LayoutCmdArgs(rawArgs: [], toggleBetween: [.tiling, .floating]))

        testParseCommandFail("layout foo", msg: """
            ERROR: Can't parse 'foo'
                   Possible values: (master-stack|horizontal|vertical|h_master-stack|v_master-stack|tiling|floating)
            """)
    }

}
