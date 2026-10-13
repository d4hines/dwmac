import AppKit
import Common

struct ListModesCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        let modes: [String] = args.bool("current") ? [activeMode ?? mainModeId] : config.modes.keys.sorted()
        return switch true {
            case args.bool("outputOnlyCount"):
                io.out("\(modes.count)")
            case args.bool("json"):
                JSONEncoder.dwmacDefault.encodeToString(modes.map { ["mode-id": $0] }).map(io.out)
                    ?? io.err("Failed to encode JSON")
            default:
                io.out(modes)
        }
    }
}
