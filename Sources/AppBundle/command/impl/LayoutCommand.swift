import AppKit
import Common

struct LayoutCommand: Command {
    let args: LayoutCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = true

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        leanRun(.layout(args.leanTarget(env), args.toggleBetween.val.map(\.rawValue)), io).ok
    }
}
