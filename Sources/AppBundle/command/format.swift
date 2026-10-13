import Common

enum AeroObj {
    case window(window: Window, title: String)
    case workspace(Workspace)
    case app(any AbstractApp)
    case monitor(Monitor)

    var kind: AeroObjKind {
        switch self {
            case .window: .window
            case .workspace: .workspace
            case .app: .app
            case .monitor: .monitor
        }
    }
}

extension [AeroObj] {
    @MainActor
    func format(_ format: [StringInterToken]) -> Result<[String], String> {
        var cellTable: [[Cell<String>]] = []
        for obj in self {
            var line: [Cell<String>] = []
            var curCell: String = ""
            var errors: [String] = []
            for token in format {
                switch token {
                    case .interVar(PlainInterVar.rightPadding.rawValue):
                        line.append(Cell(value: curCell, rightPadding: true))
                        curCell = ""
                    case .literal(let literal):
                        curCell += literal
                    case .interVar(let value):
                        switch value.expandFormatVar(obj: obj) {
                            case .success(let expanded): curCell += expanded.toString()
                            case .failure(let error): errors.append(error)
                        }
                }
            }
            if !errors.isEmpty { return .failure(errors.joinErrors()) }
            line.append(Cell(value: curCell, rightPadding: false))
            cellTable.append(line)
        }
        let result = cellTable
            .transposed()
            .map { column in
                let columndWidth = column.map { $0.value.count }.max().orDie()
                return column.map {
                    $0.rightPadding
                        ? $0.value + String(repeating: " ", count: columndWidth - $0.value.count)
                        : $0.value
                }
            }
            .transposed()
            .map { line in line.joined(separator: "") }
        return .success(result)
    }
}

enum Primitive: Encodable {
    case bool(Bool)
    case int(Int)
    case int32(Int32)
    case uint32(UInt32)
    case string(String)

    func toString() -> String {
        switch self {
            case .bool(let x): x.description
            case .int(let x): x.description
            case .int32(let x): x.description
            case .uint32(let x): x.description
            case .string(let x): x
        }
    }

    func encode(to encoder: any Encoder) throws {
        let value: Encodable = switch self {
            case .bool(let x): x
            case .int(let x): x
            case .int32(let x): x
            case .uint32(let x): x
            case .string(let x): x
        }
        var container = encoder.singleValueContainer()
        try container.encode(value)
    }
}

private struct Cell<T> {
    let value: T
    let rightPadding: Bool
}

extension String {
    @MainActor
    func expandFormatVar(obj: AeroObj) -> Result<Primitive, String> {
        let formatVar = self.toFormatVar()
        switch (obj, formatVar) {
            case (_, .none): break

            case (.window(let w, _), .workspace):
                return w.workspace.flatMap(AeroObj.workspace).map(expandFormatVar) ?? .success(.string("NULL-WOKRSPACE"))
            case (.window(let w, _), .monitor):
                return w.workspace.map(\.workspaceMonitor).flatMap(AeroObj.monitor).map(expandFormatVar) ?? .success(.string("NULL-MONITOR"))
            case (.window(let w, _), .app):
                return expandFormatVar(obj: .app(w.app))
            case (.window(_, _), .window): break

            case (.workspace(let ws), .monitor):
                return expandFormatVar(obj: AeroObj.monitor(ws.workspaceMonitor))
            case (.workspace, _): break

            case (.app(_), _): break
            case (.monitor(_), _): break
        }
        switch (obj, formatVar) {
            case (.window(let w, let title), .window(let f)):
                return switch f {
                    case .windowId: .success(.uint32(w.windowId))
                    case .windowIsFullscreen: .success(.bool(w.isFullscreen))
                    case .windowTitle: .success(.string(title))
                    case .windowLayout, .windowParentContainerLayout: toLayoutResult(w: w)
                }
            case (.workspace(let w), .workspace(let f)):
                return switch f {
                    case .workspaceName: .success(.string(w.name))
                    case .workspaceVisible: .success(.bool(w.isVisible))
                    case .workspaceFocused: .success(.bool(focus.workspace == w))
                    case .workspaceRootContainerLayout: .success(.string(toLayoutString(w: w)))
                }
            case (.monitor(let m), .monitor(let f)):
                return switch f {
                    case .monitorId: .success(m.monitorId.map { .int($0 + 1) } ?? .string("NULL-MONITOR-ID"))
                    case .monitorAppKitNsScreenScreensId: .success(.int(m.monitorAppKitNsScreenScreensId))
                    case .monitorName: .success(.string(m.name))
                    case .monitorIsMain: .success(.bool(m.isMain))
                }
            case (.app(let a), .app(let f)):
                return switch f {
                    case .appBundleId: .success(.string(a.rawAppBundleId ?? "NULL-APP-BUNDLE-ID"))
                    case .appName: .success(.string(a.name ?? "NULL-APP-NAME"))
                    case .appPid: .success(.int32(a.pid))
                    case .appExecPath: .success(.string(a.execPath ?? "NULL-APP-EXEC-PATH"))
                    case .appBundlePath: .success(.string(a.bundlePath ?? "NULL-APP-BUNDLE-PATH"))
                }
            default: break
        }
        if self == PlainInterVar.newline.rawValue { return .success(.string("\n")) }
        if self == PlainInterVar.tab.rawValue { return .success(.string("\t")) }
        return .failure("Unknown interpolation variable '\(self)'. " +
            "Possible values:\n\(getAvailableInterVars(for: obj.kind).joined(separator: "\n").prependLines("  "))")
    }

    private func toFormatVar() -> FormatVar? {
        FormatVar.WindowFormatVar(rawValue: self).flatMap(FormatVar.window)
            ?? FormatVar.WorkspaceFormatVar(rawValue: self).flatMap(FormatVar.workspace)
            ?? FormatVar.AppFormatVar(rawValue: self).flatMap(FormatVar.app)
            ?? FormatVar.MonitorFormatVar(rawValue: self).flatMap(FormatVar.monitor)
    }
}

@MainActor private func toLayoutString(w: Workspace) -> String {
    switch (w.layout, w.orientation) {
        case (.masterStack, .h): return "h_master-stack"
        case (.masterStack, .v): return "v_master-stack"
        case (.floating, _): return "floating"
    }
}

@MainActor private func toLayoutResult(w: Window) -> Result<Primitive, String> {
    switch w.unmanaged {
        case .popup?: return .success(.string("NULL-WINDOW-LAYOUT"))
        case .minimized?: return .success(.string("macos_native_minimized"))
        case nil: break
    }
    guard let workspace = w.workspace else { return .failure("NULL-WORKSPACE") }
    if w.isParked {
        let isHiddenApp = (w.app as? MacApp)?.nsApp.isHidden == true
        return .success(.string(isHiddenApp ? "macos_native_window_of_hidden_app" : "macos_native_fullscreen"))
    }
    return .success(.string(w.isFloating ? "floating" : toLayoutString(w: workspace)))
}

// MARK: - Format variables (`%{...}`). Lean checks --format against them (Lean/DwmacCore/Cli.lean)

enum FormatVar: Equatable {
    case window(WindowFormatVar)
    case workspace(WorkspaceFormatVar)
    case app(AppFormatVar)
    case monitor(MonitorFormatVar)

    enum WindowFormatVar: String, Equatable, CaseIterable {
        case windowId = "window-id"
        case windowIsFullscreen = "window-is-fullscreen"
        case windowTitle = "window-title"
        case windowLayout = "window-layout" // An alias for windowParentContainerLayout
        case windowParentContainerLayout = "window-parent-container-layout"
    }

    enum WorkspaceFormatVar: String, Equatable, CaseIterable {
        case workspaceName = "workspace"
        case workspaceFocused = "workspace-is-focused"
        case workspaceVisible = "workspace-is-visible"
        case workspaceRootContainerLayout = "workspace-root-container-layout"
    }

    enum AppFormatVar: String, Equatable, CaseIterable {
        case appBundleId = "app-bundle-id"
        case appName = "app-name"
        case appPid = "app-pid"
        case appExecPath = "app-exec-path"
        case appBundlePath = "app-bundle-path"
    }

    enum MonitorFormatVar: String, Equatable, CaseIterable {
        case monitorId = "monitor-id"
        case monitorAppKitNsScreenScreensId = "monitor-appkit-nsscreen-screens-id"
        case monitorName = "monitor-name"
        case monitorIsMain = "monitor-is-main"
    }
}

enum PlainInterVar: String, CaseIterable {
    case rightPadding = "right-padding"
    case newline = "newline"
    case tab = "tab"
}

enum AeroObjKind: CaseIterable, Sendable {
    case window, workspace, app, monitor
}

func getAvailableInterVars(for kind: AeroObjKind) -> [String] {
    _getAvailableInterVars(for: kind) + PlainInterVar.allCases.map(\.rawValue)
}

private func _getAvailableInterVars(for kind: AeroObjKind) -> [String] {
    switch kind {
        case .app: FormatVar.AppFormatVar.allCases.map(\.rawValue)
        case .monitor: FormatVar.MonitorFormatVar.allCases.map(\.rawValue)
        case .workspace:
            FormatVar.WorkspaceFormatVar.allCases.map(\.rawValue) +
                _getAvailableInterVars(for: .monitor)
        case .window:
            FormatVar.WindowFormatVar.allCases.map(\.rawValue) +
                _getAvailableInterVars(for: .workspace) +
                _getAvailableInterVars(for: .app)
    }
}

extension CmdFields {
    /// The `--count`, `--json` or `--format` output of a `list-*` command
    @MainActor
    func printList(_ list: [AeroObj], _ io: CmdIo, defaultFormat: [StringInterToken]) -> Bool {
        if bool("outputOnlyCount") { return io.out("\(list.count)") }
        // Lean's tokens: `v:<variable>` and `l:<literal>`
        let custom: [StringInterToken] = strs("_format").map {
            $0.hasPrefix("v:") ? .interVar(String($0.dropFirst(2))) : .literal(String($0.dropFirst(2)))
        }
        let format = custom.isEmpty ? defaultFormat : custom
        if bool("json") {
            return switch list.formatToJson(format, ignoreRightPaddingVar: custom.isEmpty) {
                case .success(let json): io.out(json)
                case .failure(let msg): io.err(msg)
            }
        }
        return switch list.format(format) {
            case .success(let lines): io.out(lines)
            case .failure(let msg): io.err(msg)
        }
    }
}
