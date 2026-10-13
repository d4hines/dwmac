import AppKit
import Common

struct FocusBackAndForthCommand: Command {
    let args: FocusBackAndForthCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) -> Bool {
        leanRun(.focusBackAndForth, io).ok
    }
}
