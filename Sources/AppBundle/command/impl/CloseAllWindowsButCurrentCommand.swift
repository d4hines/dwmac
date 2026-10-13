import AppKit
import Common

struct CloseAllWindowsButCurrentCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    /// `close`, with this command's `--quit-if-last-window`
    private var closeArgs: CmdFields {
        let quit = args.bool("quitIfLastWindow")
        return CmdFields(kind: "close", fields: quit ? ["quitIfLastWindow": .bool(true)] : [:],
                         args: ["close"] + (quit ? ["--quit-if-last-window"] : []), allowInConfig: true, isRunByLean: false)
    }

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        guard let target = args.resolveTargetOrReportError(env, io) else { return false }
        guard let focused = target.windowOrNil else {
            return io.err("Empty workspace")
        }
        guard let workspace = focused.workspace else {
            return io.err("Focused window '\(focused.windowId)' doesn't belong to workspace")
        }
        var result = true
        for window in workspace.allWindows where window != focused {
            result = try await CloseCommand(args: closeArgs).run(env.copy(\.windowId, window.windowId), io) && result
        }
        return result
    }
}
