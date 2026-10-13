import AppKit
import Common

// Lean owns the window manager's state (which workspace each monitor shows, which workspace each window belongs to,
// window order and floating, focus, scratchpads). Swift reads it from `LeanState.last`, and asks for changes with ops
// (events: a window appeared, closed, was parked by macOS, was dragged) and commands. Config and monitor changes are
// sent before each.

/// Stable Lean ids for monitors, by top left corner
@MainActor private var monitorIdsByPoint: [CGPoint: Int] = [:]
/// The environment ops last sent, so only changes are sent
@MainActor private var lastEnvironment: [LeanOp] = []

@MainActor func leanMonitorId(_ point: CGPoint) -> Int {
    if let id = monitorIdsByPoint[point] { return id }
    let id = monitorIdsByPoint.count
    monitorIdsByPoint[point] = id
    return id
}

@MainActor func leanMonitorPoint(_ id: Int) -> CGPoint? { monitorIdsByPoint.first { $0.value == id }?.key }

@MainActor private func leanForcedAssignment() -> [String: Int] {
    let sorted = sortedMonitors
    var forced: [String: Int] = [:]
    for (workspace, descriptions) in config.workspaceToMonitorForceAssignment {
        if let monitor = descriptions.lazy.compactMap({ $0.resolveMonitor(sortedMonitors: sorted) }).first {
            forced[workspace] = leanMonitorId(monitor.rect.topLeftCorner)
        }
    }
    return forced
}

/// Send monitor and config changes, if any
@MainActor private func leanSyncEnvironment() {
    let monitors = monitors
    let mons = monitors.sorted { $0.isMain && !$1.isMain }.map { leanMonitorId($0.rect.topLeftCorner) }
    let forced = leanForcedAssignment()
    let environment: [LeanOp] = [
        .monitorsChanged(monitors: mons, forced: forced),
        .configChanged(
            scratchpads: config.scratchpads.values.sorted { $0.name < $1.name }.map { ($0.name, $0.appId) },
            forced: forced,
        ),
        .geometryChanged(monitors.map { monitor in
            let gaps = ResolvedGaps(gaps: config.gaps, monitor: monitor)
            return (leanMonitorId(monitor.rect.topLeftCorner), LeanGeometry(
                rect: monitor.rect,
                visible: monitor.visibleRect,
                padded: monitor.visibleRectPaddedByOuterGaps,
                gapH: gaps.inner.horizontal,
                gapV: gaps.inner.vertical,
                name: monitor.name,
            ))
        }),
        .settingsChanged(
            defaultLayout: config.defaultRootContainerLayout,
            defaultMfactPermille: Int((config.defaultMfact * 1000).rounded()),
            masterAtStop: config.masterPosition == .right,
            centerFloating: config.centerFloatingWindows,
            persistent: Array(config.persistentWorkspaces),
        ),
    ]
    for (index, op) in environment.enumerated() where lastEnvironment.getOrNil(atIndex: index) != op {
        LeanState.apply(op)
    }
    lastEnvironment = environment
}

/// Apply `op`
@MainActor @discardableResult
func leanApply(_ op: LeanOp) -> LeanSnapshot {
    leanSyncEnvironment()
    let before = LeanState.last
    let snapshot = LeanState.apply(op)
    placeSummoned(before: before)
    return snapshot
}

/// Run a command and pass on its output. The caller carries out the reply's effects
@MainActor
func leanRun(_ cmd: LeanCmd, _ io: CmdIo) -> LeanReply {
    leanSyncEnvironment()
    let before = LeanState.last
    let reply = LeanState.run(cmd)
    placeSummoned(before: before)
    io.out(reply.out)
    io.err(reply.err)
    return reply
}

/// Run a command Lean parses and resolves itself (see `execCli` in Lean/DwmacCore/StateFfi.lean) and pass on its output
@MainActor
func leanRunCli(_ args: [String], _ env: CmdEnv, stdin: [String], _ io: CmdIo) -> LeanReply {
    leanSyncEnvironment()
    let before = LeanState.last
    let input = [
        "cli",
        env.windowId.map(String.init) ?? "-",
        env.workspaceName?.leanEscaped ?? "-",
        stdin.map(\.leanEscaped).joined(separator: "\u{1e}"),
        args.map(\.leanEscaped).joined(separator: "\u{1f}"),
    ].joined(separator: "\t")
    let reply = LeanState.runRaw(input)
    placeSummoned(before: before)
    io.out(reply.out)
    io.err(reply.err)
    return reply
}

/// Pick up monitor and config changes
@MainActor func leanRefresh() {
    let before = LeanState.last
    leanSyncEnvironment()
    placeSummoned(before: before)
}

@MainActor func leanResetForTests() {
    monitorIdsByPoint = [:]
    lastEnvironment = []
    LeanState.reset(scratchpads: [])
    leanRefresh()
}

/// Size and place the scratchpad windows Lean just took out of their home: a scratchpad toggled on, a hidden window
/// focused, or the first window of a scratchpad `scratchpad` launched (Lean focuses it). A dialog joining its summoned
/// siblings keeps its size
@MainActor private func placeSummoned(before: LeanSnapshot) {
    let snapshot = LeanState.last
    var summoned: [String: [Window]] = [:]
    for lean in snapshot.windows where !lean.workspace.hasPrefix(scratchpadHomePrefix) {
        guard let window = Window.get(byId: lean.id), scratchpadOwning(window) != nil else { continue }
        let wasHidden = before.window(lean.id).map { $0.workspace.hasPrefix(scratchpadHomePrefix) }
            ?? (snapshot.focusedWindow == lean.id)
        if wasHidden { summoned[lean.workspace, default: []].append(window) }
    }
    for (name, windows) in summoned {
        placeSummonedScratchpad(windows, on: Workspace.get(byName: name), focused: snapshot.focusedWindow)
    }
}
