import AppKit
import Common

struct ScratchpadCommand: Command {
    let args: ScratchpadCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = true

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        try await toggleScratchpad(args.name.val, io)
    }
}
