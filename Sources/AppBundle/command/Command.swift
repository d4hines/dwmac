import AppKit
import Common

protocol Command: AeroAny, Equatable, Sendable {
    /// What Lean parsed (Lean/DwmacCore/Cli.lean)
    var args: CmdFields { get }
    @MainActor
    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool

    /// We should reset closedWindowsCache when the command can potentiall change the tree
    var shouldResetClosedWindowsCache: Bool { get }
}

extension Command {
    static func == (lhs: Self, rhs: Self) -> Bool {
        return lhs.args == rhs.args
    }

    nonisolated func equals(_ other: any Command) -> Bool {
        (other as? Self).flatMap { self == $0 } ?? false
    }
}


extension Command {
    @MainActor
    @discardableResult
    func run(_ env: CmdEnv, _ stdin: CmdStdin) async throws -> CmdResult {
        return try await [self].runCmdSeq(env, stdin)
    }

    var isExec: Bool { self is ExecAndForgetCommand }
}

// There are 4 entry points for running commands:
// 1. config keybindings
// 2. CLI requests to server
// 3. on-window-detected callback
// 4. Tray icon buttons
extension [Command] {
    @MainActor
    func runCmdSeq(_ env: CmdEnv, _ io: sending CmdIo) async throws -> Bool {
        var isSucc = true
        for command in self {
            isSucc = try await command.run(env, io) && isSucc
            if command.shouldResetClosedWindowsCache { resetClosedWindowsCache() }
            refreshModel()
        }
        return isSucc
    }

    @MainActor
    func runCmdSeq(_ env: CmdEnv, _ stdin: CmdStdin) async throws -> CmdResult {
        let io: CmdIo = CmdIo(stdin: stdin)
        let isSucc = try await runCmdSeq(env, io)
        return CmdResult(stdout: io.stdout, stderr: io.stderr, exitCode: isSucc ? 0 : 1)
    }
}

let noWindowIsFocused = "No window is focused"

func windowIsntPartOfTree(_ window: Window) -> String {
    "Window \(window.windowId) is not part of tree (minimized or hidden)"
}
