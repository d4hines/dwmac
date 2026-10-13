import AppKit
import Common

struct ListWindowsCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        let focus = focus
        var windows: [Window] = []

        if args.bool("focused") {
            if let window = focus.windowOrNil {
                windows = [window]
            } else {
                return io.err(noWindowIsFocused)
            }
        } else {
            let filters = args.strs("workspaces")
            var workspaces: Set<Workspace> = filters.isEmpty
                ? Workspace.all.toSet()
                : filters
                    .flatMap { filter in
                        switch filter {
                            case "focused": [focus.workspace]
                            case "visible": Workspace.all.filter(\.isVisible)
                            default: [Workspace.get(byName: filter)]
                        }
                    }
                    .toSet()
            if args.has("monitors") {
                let monitors: Set<CGPoint> = resolveMonitorIds(args.strs("monitors"), io)
                if monitors.isEmpty { return false }
                workspaces = workspaces.filter { monitors.contains($0.workspaceMonitor.rect.topLeftCorner) }
            }
            windows = workspaces.flatMap(\.allWindows)
            if let pid = args.str("pidFilter").flatMap(Int32.init) {
                windows = windows.filter { $0.app.pid == pid }
            }
            if let appId = args.str("appIdFilter") {
                windows = windows.filter { $0.app.rawAppBundleId == appId }
            }
        }

        if args.bool("outputOnlyCount") { return io.out("\(windows.count)") }
        var _list: [(window: Window, title: String)] = [] // todo cleanup
        for window in windows {
            _list.append((window, try await window.title))
        }
        _list = _list.filter { Window.get(byId: $0.window.windowId) != nil }
        _list = _list.sortedBy([{ $0.window.app.name ?? "" }, \.title])
        return args.printList(_list.map { AeroObj.window(window: $0.window, title: $0.title) }, io, defaultFormat: [
            .interVar("window-id"), .interVar("right-padding"), .literal(" | "),
            .interVar("app-name"), .interVar("right-padding"), .literal(" | "),
            .interVar("window-title"),
        ])
    }
}
