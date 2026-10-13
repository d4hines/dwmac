import AppKit
import Common
import OrderedCollections

@MainActor
func readConfig(forceConfigUrl: URL? = nil) -> Result<(Config, URL), String> {
    let customConfigUrl: URL
    switch findCustomConfigUrl() {
        case .file(let url): customConfigUrl = url
        case .noCustomConfigExists: customConfigUrl = defaultConfigUrl
        case .ambiguousConfigError(let candidates):
            let msg = """
                Ambiguous config error. Several configs found:
                \(candidates.map(\.path).joined(separator: "\n"))
                """
            return .failure(msg)
    }
    let configUrl: URL = forceConfigUrl ?? customConfigUrl
    let (parsedConfig, errors) = (try? String(contentsOf: configUrl, encoding: .utf8)).map { parseConfig($0) } ?? (defaultConfig, [])

    if errors.isEmpty {
        return .success((parsedConfig, configUrl))
    } else {
        let msg = """
            Failed to parse \(configUrl.absoluteURL.path)

            \(errors.joined(separator: "\n\n"))
            """
        return .failure(msg)
    }
}

/// Parse and interpret a config: Lean does both (Lean/DwmacCore/Config.lean); this decodes the result. With a syntax
/// error, the default config and that one error
@MainActor func parseConfig(_ rawToml: String) -> (config: Config, errors: [String]) {
    let envArg = processEnv.map { $0.key.leanEscaped + "\u{1f}" + $0.value.leanEscaped }.joined(separator: "\u{1e}")
    let text = LeanState.query("config\t" + envArg + "\t" + rawToml.leanEscaped)
    var config = Config()
    var errors: [String] = []
    var env: [String: String] = [:]
    var keymap: [String] = []
    func list(_ raw: String) -> [String] {
        raw.isEmpty ? [] : raw.split(separator: "\u{1e}", omittingEmptySubsequences: false).map { String($0).leanUnescaped }
    }
    func commands(_ raw: String) -> [any Command] { list(raw).compactMap { parseCommand($0).cmdOrNil } }
    func opt(_ raw: String) -> String? { raw.isEmpty ? nil : String(raw.dropFirst()).leanUnescaped }
    func monitors(_ raw: [String]) -> [MonitorDescription] { raw.compactMap { parseMonitorDescription($0).getOrNil() } }
    func regex(_ raw: String?) -> Regex<AnyRegexOutput>? { raw.flatMap { try? Regex($0).ignoresCase() } }
    for line in text.split(separator: "\n") {
        let f = line.split(separator: "\t", omittingEmptySubsequences: false).map(String.init)
        switch f[0] {
            case "syntax": return (defaultConfig, [f[1].leanUnescaped])
            case "error": errors.append(f[1].leanUnescaped)
            case "cmds":
                switch f[1] {
                    case "afterStartup": config.afterStartupCommand = commands(f[2])
                    case "onFocusChanged": config.onFocusChanged = commands(f[2])
                    case "onFocusedMonitorChanged": config.onFocusedMonitorChanged = commands(f[2])
                    case "onModeChanged": config.onModeChanged = commands(f[2])
                    default: break
                }
            case "opt":
                switch f[1] {
                    case "layout": config.defaultRootContainerLayout = f[2].parseLayout() ?? .masterStack
                    case "orientation": config.defaultRootContainerOrientation = .init(rawValue: f[2]) ?? .auto
                    case "masterPosition": config.masterPosition = .init(rawValue: f[2]) ?? .left
                    case "mfact": config.defaultMfact = Double(f[2]) ?? 0.5
                    default: break
                }
            case "flag":
                let on = f[2] == "1"
                switch f[1] {
                    case "startAtLogin": config.startAtLogin = on
                    case "autoUnhide": config.automaticallyUnhideMacosHiddenApps = on
                    case "attachBelow": config.attachBelow = on
                    case "centerFloating": config.centerFloatingWindows = on
                    default: break
                }
            case "persistent": config.persistentWorkspaces = list(f[1]).toOrderedSet()
            case "execOnWorkspaceChange": config.execOnWorkspaceChange = list(f[1])
            case "env": env[f[1].leanUnescaped] = f[2].leanUnescaped
            case "gap":
                let rules = list(f[3]).compactMap { (pair: String) -> PerMonitorValue<Int>? in
                    let p = pair.split(separator: "\u{1f}", omittingEmptySubsequences: false).map(String.init)
                    return parseMonitorDescription(p[0]).getOrNil().flatMap { d in Int(p[1]).map { PerMonitorValue(description: d, value: $0) } }
                }
                let value: DynamicConfigValue<Int> = rules.isEmpty ? .constant(Int(f[2]) ?? 0) : .perMonitor(rules, default: Int(f[2]) ?? 0)
                switch f[1] {
                    case "inner.horizontal": config.gaps.inner.horizontal = value
                    case "inner.vertical": config.gaps.inner.vertical = value
                    case "outer.left": config.gaps.outer.left = value
                    case "outer.bottom": config.gaps.outer.bottom = value
                    case "outer.top": config.gaps.outer.top = value
                    case "outer.right": config.gaps.outer.right = value
                    default: break
                }
            case "forced": config.workspaceToMonitorForceAssignment[f[1].leanUnescaped] = monitors(list(f[2]))
            case "scratchpad":
                let name = f[1].leanUnescaped
                config.scratchpads[name] = Scratchpad(name: name, appId: f[2].leanUnescaped,
                                                      width: Double(f[3]) ?? 0.9, height: Double(f[4]) ?? 0.9)
            case "mode":
                config.modes[f[1].leanUnescaped] = .zero
                keymap.append(String(line))
            case "binding":
                config.modes[f[1].leanUnescaped]?.bindings[f[2].leanUnescaped] = commands(f[6])
                keymap.append(String(line))
            case "detected":
                config.onWindowDetected.append(WindowDetectedCallback(
                    matcher: WindowDetectedCallbackMatcher(
                        appId: opt(f[1]), appNameRegexSubstring: regex(opt(f[2])), windowTitleRegexSubstring: regex(opt(f[3])),
                        workspace: opt(f[4]), duringDwmacStartup: f[5].isEmpty ? nil : f[5] == "1",
                    ),
                    checkFurtherCallbacks: f[6] == "1",
                    rawRun: commands(f[7]),
                ))
            default: break
        }
    }
    config.execConfig = ExecConfig(envVariables: env)
    config.keymapText = keymap.joined(separator: "\u{1f}")
    return (config, errors)
}
