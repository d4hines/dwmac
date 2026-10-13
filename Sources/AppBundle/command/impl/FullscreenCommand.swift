import AppKit
import Common

struct FullscreenCommand: Command {
    let args: FullscreenCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        let toggle = switch args.toggle {
            case .on: "on"
            case .off: "off"
            case .toggle: "toggle"
        }
        let cmd = LeanCmd.fullscreen(args.leanTarget(env), toggle, noOuterGaps: args.noOuterGaps, failIfNoop: args.failIfNoop)
        return leanRun(cmd, io).ok
    }
}

let noWindowIsFocused = "No window is focused"
