import Common

/// A command parsed by Lean (Lean/DwmacCore/Cli.lean): its kind and the fields its flags and arguments set. A field
/// that's absent has its default
struct CmdFields: Equatable, Sendable {
    enum Value: Equatable, Sendable {
        case bool(Bool)
        case str(String)
        case strs([String])
    }

    let kind: String
    let fields: [String: Value]
    /// The arguments it was parsed from. The commands Lean runs are parsed again when they run
    let args: [String]
    let allowInConfig: Bool
    let isRunByLean: Bool

    func bool(_ key: String) -> Bool { if case .bool(let value)? = fields[key] { value } else { false } }
    /// nil if the flag wasn't given
    func optBool(_ key: String) -> Bool? { if case .bool(let value)? = fields[key] { value } else { nil } }
    func str(_ key: String) -> String? { if case .str(let value)? = fields[key] { value } else { nil } }
    func strs(_ key: String) -> [String] { if case .strs(let value)? = fields[key] { value } else { [] } }
    func has(_ key: String) -> Bool { fields[key] != nil }

    /// `--window-id`
    var windowId: UInt32? { str("windowId").flatMap(UInt32.init) }
    /// `--workspace`
    var workspaceName: String? { str("workspaceName") }

    var description: String {
        if kind == "exec-and-forget" { return "exec-and-forget" + (str("bashScript") ?? "") }
        // Quoted so that it splits back into the same arguments
        return ([kind] + args.dropFirst()).map { arg in
            let quote = arg.contains("'") ? "\"" : "'"
            return arg.contains(where: { $0.isWhitespace || $0 == "'" || $0 == "\"" }) ? quote + arg + quote : arg
        }.joined(separator: " ")
    }
}

enum ParsedCmd<T: Sendable>: Sendable {
    case cmd(T)
    case help(String)
    case failure(String)

    func map<R>(_ mapper: (T) -> R) -> ParsedCmd<R> {
        switch self {
            case .cmd(let cmd): .cmd(mapper(cmd))
            case .help(let help): .help(help)
            case .failure(let fail): .failure(fail)
        }
    }

    var cmdOrNil: T? {
        if case .cmd(let t) = self { t } else { nil }
    }

    /// (command, help, error)
    func unwrap() -> (T?, String?, String?) {
        switch self {
            case .cmd(let cmd): (cmd, nil, nil)
            case .help(let help): (nil, help, nil)
            case .failure(let error): (nil, nil, error)
        }
    }
}

/// A command string, as written in the config (`exec-and-forget` takes the rest of the line)
func parseCommand(_ raw: String) -> ParsedCmd<any Command> {
    decodeParsed(LeanState.query("parse\t" + raw.leanEscaped))
}

/// A command line, split into arguments
func parseCommand(_ args: [String]) -> ParsedCmd<any Command> {
    decodeParsed(LeanState.query("parseArgs\t" + args.map(\.leanEscaped).joined(separator: "\u{1f}")))
}

/// See `State.query` in Lean/DwmacCore/StateFfi.lean
private func decodeParsed(_ text: String) -> ParsedCmd<any Command> {
    var kind: String? = nil
    var allowInConfig = true
    var isRunByLean = false
    var fields: [String: CmdFields.Value] = [:]
    var args: [String] = []
    for line in text.split(separator: "\n", omittingEmptySubsequences: false) {
        let f = line.split(separator: "\t", maxSplits: 3, omittingEmptySubsequences: false).map(String.init)
        switch f.first {
            case "help": return .help(f.dropFirst().joined(separator: "\t").replacingOccurrences(of: "\u{1f}", with: "\n"))
            case "fail": return .failure(f.dropFirst().joined(separator: "\t").replacingOccurrences(of: "\u{1f}", with: "\n"))
            case "cmd" where f.count == 4:
                kind = f[1]
                allowInConfig = f[2] == "1"
                isRunByLean = f[3] == "1"
            case "field" where f.count >= 3:
                let raw = f[2...].joined(separator: "\t")
                fields[f[1]] = switch raw.first {
                    case "b": .bool(raw == "b1")
                    case "l": .strs(raw.dropFirst().isEmpty ? [] : raw.dropFirst().split(separator: "\u{1e}", omittingEmptySubsequences: false).map { String($0).leanUnescaped })
                    default: .str(String(raw.dropFirst()).leanUnescaped)
                }
            case "args":
                let raw = f.dropFirst().joined(separator: "\t")
                args = raw.isEmpty ? [] : raw.split(separator: "\u{1f}", omittingEmptySubsequences: false).map { String($0).leanUnescaped }
            default: break
        }
    }
    guard let kind else { return .failure("dwmac bug: Lean's parse reply is empty") }
    return .cmd(CmdFields(kind: kind, fields: fields, args: args, allowInConfig: allowInConfig, isRunByLean: isRunByLean).command)
}

extension String {
    /// Tabs and newlines travel as \u{1c} and \u{1d} (see `Cli.escape`)
    var leanEscaped: String { replacingOccurrences(of: "\t", with: "\u{1c}").replacingOccurrences(of: "\n", with: "\u{1d}") }
    var leanUnescaped: String { replacingOccurrences(of: "\u{1c}", with: "\t").replacingOccurrences(of: "\u{1d}", with: "\n") }
}

extension CmdFields {
    var command: any Command {
        if isRunByLean { return LeanCliCommand(args: self) }
        return switch kind {
            case "close": CloseCommand(args: self)
            case "close-all-windows-but-current": CloseAllWindowsButCurrentCommand(args: self)
            case "config": ConfigCommand(args: self)
            case "debug-windows": DebugWindowsCommand(args: self)
            case "enable": EnableCommand(args: self)
            case "exec-and-forget": ExecAndForgetCommand(args: self)
            case "list-apps": ListAppsCommand(args: self)
            case "list-exec-env-vars": ListExecEnvVarsCommand(args: self)
            case "list-modes": ListModesCommand(args: self)
            case "list-monitors": ListMonitorsCommand(args: self)
            case "list-windows": ListWindowsCommand(args: self)
            case "list-workspaces": ListWorkspacesCommand(args: self)
            case "macos-native-fullscreen": MacosNativeFullscreenCommand(args: self)
            case "macos-native-minimize": MacosNativeMinimizeCommand(args: self)
            case "mode": ModeCommand(args: self)
            case "move": MoveCommand(args: self)
            case "move-mouse": MoveMouseCommand(args: self)
            case "reload-config": ReloadConfigCommand(args: self)
            case "resize": ResizeCommand(args: self)
            case "trigger-binding": TriggerBindingCommand(args: self)
            case "volume": VolumeCommand(args: self)
            default: dieT("dwmac bug: Lean parsed an unknown command '\(kind)'")
        }
    }
}

/// A command Lean runs (Lean/DwmacCore/CliCmd.lean): it parses the arguments again, resolves them against the state
/// and the environment, and plans what to do
struct LeanCliCommand: Command {
    let args: CmdFields
    var shouldResetClosedWindowsCache: Bool {
        !["focus", "focus-back-and-forth", "focus-master-or-back", "focus-monitor", "fullscreen", "mfact", "workspace",
          "workspace-back-and-forth"].contains(args.kind)
    }

    func run(_ env: CmdEnv, _ io: CmdIo) async throws -> Bool {
        if args.kind == "scratchpad", let name = args.str("name") {
            return try await toggleScratchpad(name, io) // Resizes a window before it's summoned, launches apps
        }
        let stdin = args.optBool("explicitStdinFlag") == true ? io.readStdin().split(separator: "\n").map(String.init) : []
        return leanRunCli(args.args, env, stdin: stdin, io).ok
    }
}
