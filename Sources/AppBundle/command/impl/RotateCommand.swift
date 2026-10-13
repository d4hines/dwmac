import AppKit
import Common

struct RotateCommand: Command {
    let args: RotateCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = true

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        leanRun(.rotate, io).ok
    }
}
