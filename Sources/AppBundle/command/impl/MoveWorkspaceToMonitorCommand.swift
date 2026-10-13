import AppKit
import Common

struct MoveWorkspaceToMonitorCommand: Command {
    let args: MoveWorkspaceToMonitorCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = true

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        guard let target = args.resolveTargetOrReportError(env, io) else { return false }
        switch args.target.val.resolve(target.workspace.workspaceMonitor, wrapAround: args.wrapAround) {
            case .success(let targetMonitor):
                let monitor = leanMonitorId(targetMonitor.rect.topLeftCorner)
                return leanRun(.moveWorkspaceToMonitor(args.leanTarget(env), monitor: monitor), io).ok
            case .failure(let msg):
                return io.err(msg)
        }
    }
}
