import AppKit
import Common

struct EnableCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        let prevState = TrayMenuModel.shared.isEnabled
        let newState: Bool = switch args.str("targetState") {
            case "on": true
            case "off": false
            default: !TrayMenuModel.shared.isEnabled
        }
        if newState == prevState {
            if !args.bool("failIfNoop") {
                io.out((newState ? "Already enabled" : "Already disabled") +
                    "Tip: use --fail-if-noop to exit with non-zero code")
            }
            return !args.bool("failIfNoop")
        }

        TrayMenuModel.shared.isEnabled = newState
        if newState {
            for workspace in Workspace.all {
                for window in workspace.allWindows where window.isFloating {
                    window.lastFloatingSize = try await window.getAxSize() ?? window.lastFloatingSize
                }
            }
            try await activateMode(mainModeId)
        } else {
            try await activateMode(nil)
        }
        return true
    }
}
