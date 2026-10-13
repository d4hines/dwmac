@testable import AppBundle
import Common
import XCTest

@MainActor
final class ModAliasTest: XCTestCase {
    func testModAlias() {
        let (config, errors) = parseConfig(
            """
            mod = 'cmd-alt'
            [mode.main.binding]
                mod-e = 'focus next'
            """,
        )
        XCTAssertEqual(errors, [])
        XCTAssertEqual(bound(config, "mod-e"), ["focus next"])
    }

    func testModAliasWithMultipleModifiers() {
        let (config, errors) = parseConfig(
            """
            mod = 'cmd-alt-ctrl-shift'
            [mode.main.binding]
                mod-e = 'focus next'
            """,
        )
        XCTAssertEqual(errors, [])
        XCTAssertEqual(bound(config, "mod-e"), ["focus next"])
    }

    func testNoModAlias() {
        let (config, errors) = parseConfig(
            """
            [mode.main.binding]
                alt-e = 'focus next'
            """,
        )
        XCTAssertEqual(errors, [])
        XCTAssertEqual(bound(config, "alt-e"), ["focus next"])
    }

    func testUnknownModAlias() {
        let (_, errors) = parseConfig(
            """
            [mode.main.binding]
                mod-e = 'focus next'
            """,
        )
        XCTAssertEqual(errors.descriptions, ["mode.main.binding.mod-e: Can't parse modifiers in 'mod-e' binding"])
    }
}
