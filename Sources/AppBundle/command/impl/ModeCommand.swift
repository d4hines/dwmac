import AppKit
import Common

struct ModeCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        try await activateMode(args.str("targetMode") ?? "")
        return true
    }
}
