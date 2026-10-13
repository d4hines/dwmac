import AppKit
import Common

struct SwapCommand: Command {
    let args: SwapCmdArgs
    /*conforms*/ let shouldResetClosedWindowsCache: Bool = true

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        leanRun(.swap(args.leanTarget(env), args.target.val, wrap: args.wrapAround, swapFocus: args.swapFocus), io).ok
    }
}
