import AppKit
import Common

struct MfactCommand: Command {
    let args: MfactCmdArgs
    /*conforms*/ var shouldResetClosedWindowsCache = false

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        guard let window = focus.windowOrNil else { return false }
        guard let workspace = window.nodeWorkspace else { return false }

        if workspace.layout != .masterStack { return false }

        func permille(_ value: Double) -> Int { Int((value * 1000).rounded()) }
        // Lean clamps it between 0.05 and 0.95
        let change: LeanOp.MfactChange = switch args.amount.val {
            case .set(let value): .set(permille: permille(value))
            case .add(let value): .add(permille: permille(value))
            case .subtract(let value): .sub(permille: permille(value))
        }
        leanApply(.mfact(workspace.name, change))
        return true
    }
}
