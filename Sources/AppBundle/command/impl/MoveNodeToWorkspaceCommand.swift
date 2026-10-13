import Common

struct MoveNodeToWorkspaceCommand: Command {
    let args: MoveNodeToWorkspaceCmdArgs
    /*conforms*/ let shouldResetClosedWindowsCache: Bool = true

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        let to: LeanCmd.WsTo = switch args.target.val {
            case .relative(let nextPrev): .relative(nextPrev, wrap: args.wrapAround, among: args.useStdin ? stdinWorkspaces(io) : nil)
            case .direct(let name): .name(name.raw)
        }
        let cmd = LeanCmd.moveNodeToWorkspace(args.leanTarget(env), to, focusFollows: args.focusFollowsWindow, failIfNoop: args.failIfNoop)
        return leanRun(cmd, io).ok
    }
}
