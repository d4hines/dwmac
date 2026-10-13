import AppKit
import Common

struct TriggerBindingCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        let reply = LeanState.runRaw("trigger\t\((args.str("_mode") ?? "").leanEscaped)\t\((args.str("binding") ?? "").leanEscaped)")
        guard reply.ok else { return io.err(reply.err.joined(separator: "\n")) }
        // refreshSession is not needed since commands are already run in refreshSession
        return try await reply.out.compactMap { parseCommand($0.leanUnescaped).cmdOrNil }.runCmdSeq(env, io)
    }
}
