@testable import AppBundle
import Common
import XCTest

final class VerifiedLayoutTest: XCTestCase {
    /// Tiles as [x, y, width, height] (Rect isn't Equatable)
    private func tiles(_ width: CGFloat, _ height: CGFloat, _ count: Int, orientation: Orientation = .h, masterPosition: MasterPosition = .left) -> [[CGFloat]] {
        masterStackTiles(width: width, height: height, gapH: 10, gapV: 10, mfact: 0.67, orientation: orientation, masterPosition: masterPosition, count: count)
            .map { [$0.topLeftX, $0.topLeftY, $0.width, $0.height] }
    }

    func testMasterStack() {
        // Same numbers as `#eval masterStack ...` in Lean: the remainder pixel goes to the first stack window
        assertEquals(tiles(1512, 945, 4), [
            [0, 0, 1006, 945],
            [1016, 0, 496, 309],
            [1016, 319, 496, 308],
            [1016, 637, 496, 308],
        ])
        assertEquals(tiles(1512, 945, 2, masterPosition: .right), [
            [506, 0, 1006, 945],
            [0, 0, 496, 945],
        ])
        assertEquals(tiles(1000, 600, 3, orientation: .v), [
            [0, 0, 1000, 395],
            [0, 405, 495, 195],
            [505, 405, 495, 195],
        ])
    }

    func testEdgeCases() {
        assertEquals(tiles(1512, 945, 0), [])
        assertEquals(tiles(1512, 945, 1), [[0, 0, 1512, 945]])
        // Sizes are rounded; nonsense sizes become empty instead of crashing
        assertEquals(tiles(99.6, 50.2, 1), [[0, 0, 100, 50]])
        assertEquals(tiles(.nan, -5, 1), [[0, 0, 0, 0]])
        // Gaps that don't fit are dropped: tiles still never leave the workspace
        let crowded = tiles(100, 100, 50)
        assertEquals(crowded.count, 50)
        XCTAssertTrue(crowded.allSatisfy { $0[0] + $0[2] <= 100 && $0[1] + $0[3] <= 100 })
    }
}
