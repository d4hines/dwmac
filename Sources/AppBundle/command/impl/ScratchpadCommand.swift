import AppKit
import Common

struct ScratchpadCommand: Command {
    let args: ScratchpadCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = true

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        let name = args.name.val
        guard let scratchpad = config.scratchpads[name] else {
            return io.err("Scratchpad '\(name)' isn't declared. Declare it in the config with [scratchpad.\(name)]")
        }
        return try await toggleScratchpad(scratchpad, io)
    }
}
