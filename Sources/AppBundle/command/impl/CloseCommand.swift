import AppKit
import Common

struct CloseCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        try await allowOnlyCancellationError {
            guard let target = args.resolveTargetOrReportError(env, io) else { return false }
            guard let window = target.windowOrNil else {
                return io.err("Empty workspace")
            }
            // Access ax directly. Not cool :(
            if try await args.bool("quitIfLastWindow").andAsync({ @MainActor @Sendable in try await window.macAppUnsafe.getAxWindowsCount() == 1 }) {
                let app = window.macAppUnsafe
                if app.nsApp.terminate() {
                    for window in Window.allWindowsMap.values where window.app.pid == app.pid {
                        window.garbageCollect(skipClosedWindowsCache: true)
                    }
                    return true
                } else {
                    return io.err("Failed to quit '\(window.app.name ?? "Unknown app")'")
                }
            } else {
                window.closeAxWindow()
                return true
            }
        }
    }
}
