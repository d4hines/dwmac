import AppKit
import Common

struct ListAppsCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        var result = Array(MacApp.allAppsMap.values)
        if let hidden = args.optBool("macosHidden") {
            result = result.filter { $0.nsApp.isHidden == hidden }
        }
        return args.printList(result.map { AeroObj.app($0) }, io, defaultFormat: [
            .interVar("app-pid"), .interVar("right-padding"), .literal(" | "),
            .interVar("app-bundle-id"), .interVar("right-padding"), .literal(" | "),
            .interVar("app-name"),
        ])
    }
}
