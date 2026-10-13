@testable import AppBundle
import XCTest

@MainActor
final class MasterPositionTest: XCTestCase {
    func testParseMasterPosition() {
        let (config, errors) = parseConfig(
            """
            master-position = 'right'
            """
        )
        assertEquals(errors, [])
        assertEquals(config.masterPosition, .right)
    }

    func testParseMasterPositionDefault() {
        let (config, errors) = parseConfig("")
        assertEquals(errors, [])
        assertEquals(config.masterPosition, .left)
    }

    func testParseMasterPositionInvalid() {
        let (_, errors) = parseConfig(
            """
            master-position = 'foo'
            """
        )
        assertEquals(errors.descriptions, ["master-position: Can't parse master position 'foo'"])
    }
}
