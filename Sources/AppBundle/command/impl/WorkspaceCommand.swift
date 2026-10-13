import AppKit
import Common
import Foundation

struct WorkspaceCommand: Command {
    let args: WorkspaceCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        let to: LeanCmd.WsTo = switch args.target.val {
            case .relative(let nextPrev): .relative(nextPrev, wrap: args.wrapAround, among: args.useStdin ? stdinWorkspaces(io) : nil)
            case .direct(let name): .name(name.raw)
        }
        return leanRun(.workspace(args.leanTarget(env), to, autoBackAndForth: args.autoBackAndForth, failIfNoop: args.failIfNoop), io).ok
    }
}

/// Workspace names read from stdin, one per line, for `--stdin`
func stdinWorkspaces(_ io: CmdIo) -> [String] {
    io.readStdin().split(separator: "\n").map { String($0).trim() }.filter { !$0.isEmpty }
}
