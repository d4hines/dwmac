import CDwmacCore
import Common
import CoreGraphics

/// Master-stack tiles of `count` windows in a `width` x `height` workspace, master first, relative to its top left
/// corner. Computed by the Lean implementation (Lean/DwmacCore/MasterStack.lean), which is proven to give one tile
/// per window, no two of them overlapping, all within the workspace. Sizes are rounded to whole points first
func masterStackTiles(
    width: CGFloat,
    height: CGFloat,
    gapH: Int,
    gapV: Int,
    mfact: CGFloat,
    orientation: Orientation,
    masterPosition: MasterPosition,
    count: Int,
) -> [Rect] {
    var tiles = [DwmacTile](repeating: DwmacTile(), count: count)
    let written = tiles.withUnsafeMutableBufferPointer { buffer in
        dwmac_master_stack(
            UInt32(clampingPoints: width),
            UInt32(clampingPoints: height),
            UInt32(clamping: gapH),
            UInt32(clamping: gapV),
            UInt32(clampingPoints: mfact * 1000),
            orientation == .v,
            masterPosition == .right,
            UInt32(clamping: count),
            buffer.baseAddress,
            buffer.count,
        )
    }
    check(written == count, "dwmac_master_stack returned \(written) tiles for \(count) windows")
    return tiles.map { Rect(topLeftX: CGFloat($0.x), topLeftY: CGFloat($0.y), width: CGFloat($0.w), height: CGFloat($0.h)) }
}

extension UInt32 {
    /// Rounded to the nearest whole point, clamped to UInt32's range (NaN counts as 0)
    fileprivate init(clampingPoints value: CGFloat) {
        let rounded = value.rounded()
        self = rounded.isNaN || rounded <= 0 ? 0 : rounded >= CGFloat(UInt32.max) ? .max : UInt32(rounded)
    }
}
