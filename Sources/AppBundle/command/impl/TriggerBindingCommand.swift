import AppKit
import Common

struct TriggerBindingCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        let modeId = args.str("_mode") ?? ""
        let bindingName = args.str("binding") ?? ""
        return if let mode = config.modes[modeId] {
            if let binding = mode.bindings.values.first(where: { $0.descriptionWithKeyNotation == bindingName }) {
                // refreshSession is not needed since commands are already run in refreshSession
                try await binding.commands.runCmdSeq(env, io)
            } else {
                io.err("Binding '\(bindingName)' is not presented in mode '\(modeId)'")
            }
        } else {
            io.err("Mode '\(modeId)' doesn't exist. " +
                "Available modes: \(config.modes.keys.joined(separator: ","))")
        }
    }
}
