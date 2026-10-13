import AppKit
import Common

struct MoveNodeToMonitorCommand: Command {
    let args: MoveNodeToMonitorCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = true

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        guard let target = args.resolveTargetOrReportError(env, io) else { return false }
        guard let window = target.windowOrNil else {
            return io.err(noWindowIsFocused)
        }
        guard let currentMonitor = window.nodeMonitor else {
            return io.err(windowIsntPartOfTree(window))
        }
        switch args.target.val.resolve(currentMonitor, wrapAround: args.wrapAround) {
            case .success(let targetMonitor):
                let cmd = LeanCmd.moveNodeToMonitor(
                    args.leanTarget(env),
                    monitor: leanMonitorId(targetMonitor.rect.topLeftCorner),
                    direction: args.target.val.directionOrNil,
                    focusFollows: args.focusFollowsWindow,
                    failIfNoop: args.failIfNoop,
                )
                return leanRun(cmd, io).ok
            case .failure(let msg):
                return io.err(msg)
        }
    }
}

func windowIsntPartOfTree(_ window: Window) -> String {
    "Window \(window.windowId) is not part of tree (minimized or hidden)"
}
