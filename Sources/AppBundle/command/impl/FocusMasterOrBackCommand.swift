import AppKit
import Common

struct FocusMasterOrBackCommand: Command {
    let args: FocusMasterOrBackCmdArgs
    var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        leanRun(.focusMasterOrBack, io).ok
    }
}
