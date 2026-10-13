import AppKit
import Common

struct ResizeCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = true

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        guard let target = args.resolveTargetOrReportError(env, io) else { return false }
        guard let window = target.windowOrNil else { return false }

        if !window.isFloating {
            return io.err("resize command only supports floating windows")
        }

        guard let rect = try? await window.getAxRect() else { return false }
        var newSize = rect.size

        // `[+|-]<number>`: add, subtract, or set
        let units = args.str("units") ?? "0"
        let isRelative = units.starts(with: "+") || units.starts(with: "-")
        let number = CGFloat(Double(units.removePrefix("+").removePrefix("-")) ?? 0)
        let diff = units.starts(with: "-") ? -number : number
        func apply(_ current: CGFloat) -> CGFloat { isRelative ? current + diff : diff }

        switch args.str("dimension") {
            case "height", "smart-opposite": // smart-opposite is height for a floating window
                newSize.height = apply(newSize.height)
            default: // width, and smart
                newSize.width = apply(newSize.width)
        }

        // Prevent collapsing to zero/negative
        newSize.width = max(10, newSize.width)
        newSize.height = max(10, newSize.height)

        window.setAxFrame(rect.topLeftCorner, newSize)

        return true
    }
}
