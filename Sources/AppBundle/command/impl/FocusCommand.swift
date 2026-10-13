import AppKit
import Common

struct FocusCommand: Command {
    let args: FocusCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        let to: LeanCmd.FocusTo = switch args.target {
            case .windowId(let id): .window(id)
            case .index(let index): .index(Int(index))
            case .relative(let nextPrev):
                switch args.rawBoundariesAction ?? .wrapAroundTheWorkspace {
                    case .stop: .relative(nextPrev, boundary: "stop")
                    case .fail: .relative(nextPrev, boundary: "fail")
                    case .wrapAroundTheWorkspace: .relative(nextPrev, boundary: "wrap")
                    case .wrapAroundAllMonitors: dieT("Must be discarded by args parser")
                }
        }
        return leanRun(.focus(args.leanTarget(env), to), io).ok
    }
}
