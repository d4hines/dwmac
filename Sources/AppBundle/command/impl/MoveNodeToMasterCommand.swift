import AppKit
import Common

struct MoveNodeToMasterCommand: Command {
    let args: MoveNodeToMasterCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = true

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        leanRun(.moveNodeToMaster, io).ok
    }
}
