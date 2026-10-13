import AppKit
import Common

struct SummonWorkspaceCommand: Command {
    let args: SummonWorkspaceCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = true

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        leanRun(.summonWorkspace(args.target.val.raw, failIfNoop: args.failIfNoop), io).ok
    }
}
