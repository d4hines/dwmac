import AppKit
import Common

struct ListMonitorsCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        let focus = focus
        var result = sortedMonitors
        if let focused = args.optBool("focused") {
            result = result.filter { (monitor) in (monitor.activeWorkspace == focus.workspace) == focused }
        }
        if let mouse = args.optBool("mouse") {
            let mouseWorkspace = mouseLocation.monitorApproximation.activeWorkspace
            result = result.filter { (monitor) in (monitor.activeWorkspace == mouseWorkspace) == mouse }
        }
        return args.printList(result.map { AeroObj.monitor($0) }, io, defaultFormat: [
            .interVar("monitor-id"), .interVar("right-padding"), .literal(" | "), .interVar("monitor-name"),
        ])
    }
}
