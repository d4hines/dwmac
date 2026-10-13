@testable import AppBundle
import Common
import XCTest

final class ListModesTest: XCTestCase {
    @MainActor
    func testListModesOutput() async throws {
        config.modes = [
            "main": Mode(name: nil, bindings: [:]),
            "service": Mode(name: nil, bindings: [:]),
            "resize": Mode(name: nil, bindings: [:]),
        ]

        let defaultResult = try await cmd("list-modes").run(.defaultEnv, .emptyStdin)
        assertEquals(defaultResult.exitCode, 0)
        assertEquals(defaultResult.stdout, ["main", "resize", "service"])
        assertEquals(defaultResult.stderr, [])

        let currentResult = try await cmd("list-modes --current").run(.defaultEnv, .emptyStdin)
        assertEquals(currentResult.exitCode, 0)
        assertEquals(currentResult.stdout, ["main"])
        assertEquals(currentResult.stderr, [])

        let countResult = try await cmd("list-modes --count").run(.defaultEnv, .emptyStdin)
        assertEquals(countResult.exitCode, 0)
        assertEquals(countResult.stdout, ["3"])
        assertEquals(countResult.stderr, [])

        let jsonResult = try await cmd("list-modes --json").run(.defaultEnv, .emptyStdin)
        let expectedJson = JSONEncoder.dwmacDefault.encodeToString([
            ["mode-id": "main"],
            ["mode-id": "resize"],
            ["mode-id": "service"],
        ])
        assertEquals(jsonResult.exitCode, 0)
        assertEquals(jsonResult.stdout, [expectedJson])
        assertEquals(jsonResult.stderr, [])

        let currentJsonResult = try await cmd("list-modes --current --json").run(.defaultEnv, .emptyStdin)
        let expectedCurrentJson = JSONEncoder.dwmacDefault.encodeToString([
            ["mode-id": "main"],
        ])
        assertEquals(currentJsonResult.exitCode, 0)
        assertEquals(currentJsonResult.stdout, [expectedCurrentJson])
        assertEquals(currentJsonResult.stderr, [])
    }
}
