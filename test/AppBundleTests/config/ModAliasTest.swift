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
        let binding = HotkeyBinding([.command, .option], .e, [cmd("focus next")])
        XCTAssertEqual(config.modes[mainModeId]?.bindings[binding.descriptionWithKeyCode], binding)
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
        let binding = HotkeyBinding([.command, .option, .control, .shift], .e, [cmd("focus next")])
        XCTAssertEqual(config.modes[mainModeId]?.bindings[binding.descriptionWithKeyCode], binding)
    }

    func testNoModAlias() {
        let (config, errors) = parseConfig(
            """
            [mode.main.binding]
                alt-e = 'focus next'
            """,
        )
        XCTAssertEqual(errors, [])
        let binding = HotkeyBinding(.option, .e, [cmd("focus next")])
        XCTAssertEqual(config.modes[mainModeId]?.bindings[binding.descriptionWithKeyCode], binding)
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
