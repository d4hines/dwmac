import AppKit
import Common

struct ListWorkspacesCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        var result: [Workspace] = Workspace.all
        if let visible = args.optBool("visible") {
            result = result.filter { $0.isVisible == visible }
        }
        if args.has("onMonitors") {
            let monitors: Set<CGPoint> = resolveMonitorIds(args.strs("onMonitors"), io)
            if monitors.isEmpty { return false }
            result = result.filter { monitors.contains($0.workspaceMonitor.rect.topLeftCorner) }
        }
        if let empty = args.optBool("empty") {
            result = result.filter { $0.isEffectivelyEmpty == empty }
        }
        return args.printList(result.map { AeroObj.workspace($0) }, io, defaultFormat: [.interVar("workspace")])
    }
}

/// `--monitor`'s ids: 1-based indices, `focused`, `mouse`, `all`. Empty if one doesn't exist
@MainActor func resolveMonitorIds(_ ids: [String], _ io: CmdIo) -> Set<CGPoint> {
    var requested: Set<CGPoint> = []
    let sortedMonitors = sortedMonitors
    for id in ids {
        switch id {
            case "focused": requested.insert(focus.workspace.workspaceMonitor.rect.topLeftCorner)
            case "mouse": requested.insert(mouseLocation.monitorApproximation.rect.topLeftCorner)
            case "all": requested.formUnion(monitors.map(\.rect.topLeftCorner))
            default:
                guard let index = Int(id), let monitor = sortedMonitors.getOrNil(atIndex: index - 1) else {
                    io.err("Invalid monitor ID: \(id)")
                    return []
                }
                requested.insert(monitor.rect.topLeftCorner)
        }
    }
    return requested
}
