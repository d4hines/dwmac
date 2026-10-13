import AppKit
import Common

/// Problem ID-B6E178F2: It's not first-class citizen command in Dwmac model, since it interacts with macOS API directly.
/// Consecutive macos-native-fullscreen commands may not works as expected (because macOS may report correct state with a
/// delay), or may flicker
///
/// The same applies to macos-native-minimize command
struct MacosNativeFullscreenCommand: Command {
    let args: CmdFields
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        guard let target = args.resolveTargetOrReportError(env, io) else { return false }
        guard let window = target.windowOrNil else {
            return io.err(noWindowIsFocused)
        }
        let prevState = try await window.isMacosFullscreen
        let newState: Bool = switch args.str("toggle") {
            case "on": true
            case "off": false
            default: !prevState
        }
        if newState == prevState {
            if !args.bool("failIfNoop") {
                io.err((newState ? "Already fullscreen. " : "Already not fullscreen. ") +
                    "Tip: use --fail-if-noop to exit with non-zero exit code")
            }
            return !args.bool("failIfNoop")
        }
        window.asMacWindow().setNativeFullscreen(newState)
        guard window.lean != nil else {
            return io.err(windowIsntPartOfTree(window))
        }
        // Off the layout while in native fullscreen, back in its place after
        leanApply(.setParked(window.windowId, newState))
        return true
    }
}
