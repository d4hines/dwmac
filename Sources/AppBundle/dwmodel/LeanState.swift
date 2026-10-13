import AppKit
import CDwmacCore
import Common

/// An op of the window manager's state machine (Lean/DwmacCore/Model.lean). Monitors are `LeanMonitorId`s, windows
/// their window ids, workspaces their names
enum LeanOp: Equatable {
    case focusWindow(UInt32)
    case focusWorkspace(String)
    case summonWorkspace(String)
    case focusMonitor(Int)
    case moveWorkspaceToMonitor(Int)
    case showWorkspace(monitor: Int, workspace: String)
    case moveWindowToWorkspace(UInt32, workspace: String, focusFollows: Bool)
    case placeWindow(UInt32, workspace: String, index: Int, isFloating: Bool?)
    case setFloating(UInt32, Bool)
    case toggleScratchpad(String)
    case windowAppeared(UInt32, appId: String, workspace: String, isFloating: Bool, index: Int)
    case windowClosed(UInt32)
    case monitorsChanged(monitors: [Int], forced: [String: Int])
    case configChanged(scratchpads: [(name: String, appId: String)], forced: [String: Int])
    case setLayout(String, Layout?, Orientation?)
    case mfact(String, MfactChange)
    case setFullscreen(UInt32, on: Bool, noOuterGaps: Bool)
    case setParked(UInt32, Bool)
    case geometryChanged([(monitor: Int, geometry: LeanGeometry)])
    case settingsChanged(defaultLayout: Layout, defaultMfactPermille: Int, masterAtStop: Bool, centerFloating: Bool,
                         persistent: [String])

    enum MfactChange {
        case set(permille: Int), add(permille: Int), sub(permille: Int)
    }

    static func == (lhs: LeanOp, rhs: LeanOp) -> Bool { lhs.encoded == rhs.encoded }

    /// See Lean/DwmacCore/StateFfi.lean
    var encoded: String {
        func b(_ value: Bool) -> String { value ? "1" : "0" }
        func list(_ items: [String]) -> String { items.joined(separator: "\u{1e}") }
        func forcedList(_ forced: [String: Int]) -> String {
            list(forced.sorted { $0.key < $1.key }.map { "\($0.key)\u{1f}\($0.value)" })
        }
        let fields: [String] = switch self {
            case .focusWindow(let id): ["focusWindow", "\(id)"]
            case .focusWorkspace(let ws): ["focusWorkspace", ws]
            case .summonWorkspace(let ws): ["summonWorkspace", ws]
            case .focusMonitor(let mon): ["focusMonitor", "\(mon)"]
            case .moveWorkspaceToMonitor(let mon): ["moveWorkspaceToMonitor", "\(mon)"]
            case .showWorkspace(let mon, let ws): ["showWorkspace", "\(mon)", ws]
            case .moveWindowToWorkspace(let id, let ws, let follow): ["moveWindowToWorkspace", "\(id)", ws, b(follow)]
            case .placeWindow(let id, let ws, let index, let floating):
                ["placeWindow", "\(id)", ws, "\(max(index, 0))", floating.map(b) ?? "-"]
            case .setFloating(let id, let floating): ["setFloating", "\(id)", b(floating)]
            case .toggleScratchpad(let name): ["toggleScratchpad", name]
            case .windowAppeared(let id, let app, let ws, let floating, let index):
                ["windowAppeared", "\(id)", app, ws, b(floating), "\(max(index, 0))"]
            case .windowClosed(let id): ["windowClosed", "\(id)"]
            case .monitorsChanged(let mons, let forced): ["monitorsChanged", list(mons.map(String.init)), forcedList(forced)]
            case .configChanged(let sps, let forced):
                ["configChanged", list(sps.map { "\($0.name)\u{1f}\($0.appId)" }), forcedList(forced)]
            case .setLayout(let ws, let layout, let orientation):
                ["setLayout", ws, layout?.rawValue ?? "-", orientation.map { $0 == .h ? "h" : "v" } ?? "-"]
            case .mfact(let ws, let change): ["mfact", ws] + change.leanFields
            case .setFullscreen(let id, let on, let noOuterGaps): ["setFullscreen", "\(id)", b(on), b(noOuterGaps)]
            case .setParked(let id, let parked): ["setParked", "\(id)", b(parked)]
            case .geometryChanged(let geometry):
                ["geometryChanged", list(geometry.map { mon, g in
                    let fields: [String] = ["\(mon)", g.rect.leanEncoded, g.visible.leanEncoded, g.padded.leanEncoded,
                                            "\(max(g.gapH, 0))", "\(max(g.gapV, 0))", g.name.leanSafe]
                    return fields.joined(separator: "\u{1f}")
                })]
            case .settingsChanged(let layout, let mfact, let masterAtStop, let center, let persistent):
                ["settingsChanged", layout.rawValue, "\(max(mfact, 0))", b(masterAtStop), b(center), list(persistent)]
        }
        return fields.joined(separator: "\t")
    }
}

/// A monitor's geometry, with the config's gaps resolved for it
struct LeanGeometry {
    let rect: Rect
    let visible: Rect
    /// `visible` without the outer gaps
    let padded: Rect
    let gapH: Int
    let gapV: Int
    /// For messages
    let name: String
}

extension String {
    /// Without the protocol's separators
    var leanSafe: String {
        String(unicodeScalars.filter { !["\t", "\n", "\u{1e}", "\u{1f}"].contains($0) })
    }
}

/// A command (Lean/DwmacCore/Command.lean), with what only Swift knows (monitors, stdin) resolved
enum LeanCmd {
    /// What a command acts on: `--window-id`/`DWMAC_WINDOW_ID` (`fromEnv`), `--workspace`/`DWMAC_WORKSPACE`, else focus
    enum Target {
        case focused
        case window(UInt32, fromEnv: Bool)
        case workspace(String)
    }
    enum WsTo {
        case name(String)
        /// Among the workspaces on the target's monitor, or `among` (from stdin)
        case relative(NextPrev, wrap: Bool, among: [String]?)
    }
    enum FocusTo {
        case window(UInt32)
        case index(Int)
        case relative(NextPrev, boundary: String)
    }

    case focus(Target, FocusTo)
    case focusBackAndForth
    case focusMasterOrBack
    case focusMonitor(Int)
    case workspace(Target, WsTo, autoBackAndForth: Bool, failIfNoop: Bool)
    case workspaceBackAndForth
    case summonWorkspace(String, failIfNoop: Bool)
    case moveWorkspaceToMonitor(Target, monitor: Int)
    case moveNodeToWorkspace(Target, WsTo, focusFollows: Bool, failIfNoop: Bool)
    /// `direction`: the direction it was given as, if any. Moving right or down along the target workspace's
    /// orientation puts the window first
    case moveNodeToMonitor(Target, monitor: Int, direction: CardinalDirection?, focusFollows: Bool, failIfNoop: Bool)
    case moveNodeToMaster
    case rotate
    /// The window macOS says is focused (nil for a popup or no window), and the time in ms
    case syncFocus(UInt32?, now: Int)
    case swap(Target, NextPrev, wrap: Bool, swapFocus: Bool)
    case scratchpad(String)
    case layout(Target, [String])
    case mfact(LeanOp.MfactChange)
    case fullscreen(Target, String, noOuterGaps: Bool, failIfNoop: Bool)

    /// See `parseCmd` in Lean/DwmacCore/StateFfi.lean
    var encoded: String {
        func b(_ value: Bool) -> String { value ? "1" : "0" }
        func target(_ t: Target) -> String {
            switch t {
                case .focused: "-"
                case .window(let id, let fromEnv): (fromEnv ? "e" : "w") + "\(id)"
                case .workspace(let ws): "s" + ws
            }
        }
        func wsTo(_ to: WsTo) -> [String] {
            switch to {
                case .name(let ws): ["name", ws, "-", "-"]
                case .relative(let dir, let wrap, let among):
                    [dir == .next ? "next" : "prev", "-", b(wrap), among.map { $0.joined(separator: "\u{1e}") } ?? "-"]
            }
        }
        let fields: [String] = switch self {
            case .focus(let t, .window(let id)): ["focus", target(t), "window", "\(id)"]
            case .focus(let t, .index(let n)): ["focus", target(t), "index", "\(max(n, 0))"]
            case .focus(let t, .relative(let dir, let boundary)):
                ["focus", target(t), dir == .next ? "next" : "prev", boundary]
            case .focusBackAndForth: ["focusBackAndForth"]
            case .focusMasterOrBack: ["focusMasterOrBack"]
            case .focusMonitor(let mon): ["focusMonitor", "\(mon)"]
            case .workspace(let t, let to, let autoBackAndForth, let failIfNoop):
                ["workspace", target(t)] + wsTo(to) + [b(autoBackAndForth), b(failIfNoop)]
            case .workspaceBackAndForth: ["workspaceBackAndForth"]
            case .summonWorkspace(let ws, let failIfNoop): ["summonWorkspace", ws, b(failIfNoop)]
            case .moveWorkspaceToMonitor(let t, let mon): ["moveWorkspaceToMonitor", target(t), "\(mon)"]
            case .moveNodeToWorkspace(let t, let to, let focusFollows, let failIfNoop):
                ["moveNodeToWorkspace", target(t)] + wsTo(to) + [b(focusFollows), b(failIfNoop)]
            case .moveNodeToMonitor(let t, let mon, let dir, let focusFollows, let failIfNoop):
                ["moveNodeToMonitor", target(t), "\(mon)",
                 dir.map { ($0.orientation == .h ? "h" : "v") + b($0.isPositive) } ?? "-", b(focusFollows), b(failIfNoop)]
            case .moveNodeToMaster: ["moveNodeToMaster"]
            case .rotate: ["rotate"]
            case .syncFocus(let native, let now): ["syncFocus", native.map(String.init) ?? "-", "\(max(now, 0))"]
            case .swap(let t, let dir, let wrap, let swapFocus):
                ["swap", target(t), dir == .next ? "next" : "prev", b(wrap), b(swapFocus)]
            case .scratchpad(let name): ["scratchpad", name]
            case .layout(let t, let descriptions): ["layout", target(t), descriptions.joined(separator: "\u{1e}")]
            case .mfact(let change): ["mfact"] + change.leanFields
            case .fullscreen(let t, let toggle, let noOuterGaps, let failIfNoop):
                ["fullscreen", target(t), toggle, b(noOuterGaps), b(failIfNoop)]
        }
        return (["cmd"] + fields).joined(separator: "\t")
    }
}

extension LeanOp.MfactChange {
    var leanFields: [String] {
        switch self {
            case .set(let n): ["set", "\(max(n, 0))"]
            case .add(let n): ["add", "\(max(n, 0))"]
            case .sub(let n): ["sub", "\(max(n, 0))"]
        }
    }
}

/// A command's result: exit code, output, and what Swift should do besides laying out windows
struct LeanReply {
    enum Effect {
        /// Launch the app (or make it open a window)
        case launch(appId: String)
        /// A scratchpad window is about to be summoned: resize it while it's still hidden
        case prepareSummon(UInt32)
        /// Ask macOS to focus the window
        case nativeFocus(UInt32)
        /// Report macOS's focus again in `ms` (see syncFocusWithMacos)
        case recheck(ms: Int)
    }
    var ok = false
    var out: [String] = []
    var err: [String] = []
    var effects: [Effect] = []

    init(parsing text: String) {
        for line in text.split(separator: "\n") {
            let f = line.split(separator: "\t", maxSplits: 2, omittingEmptySubsequences: false).map(String.init)
            switch (f.first, f.count) {
                case ("ok", 2): ok = f[1] == "1"
                case ("out", 2): out.append(f[1])
                case ("err", 2): err.append(f[1])
                case ("effect", 3):
                    switch f[1] {
                        case "launch": effects.append(.launch(appId: f[2]))
                        case "prepareSummon": if let id = UInt32(f[2]) { effects.append(.prepareSummon(id)) }
                        case "nativeFocus": if let id = UInt32(f[2]) { effects.append(.nativeFocus(id)) }
                        case "recheck": if let ms = Int(f[2]) { effects.append(.recheck(ms: ms)) }
                        default: break
                    }
                default: break
            }
        }
    }
}

extension Rect {
    /// `x,y,w,h` in whole points
    fileprivate var leanEncoded: String {
        [topLeftX, topLeftY, width, height].map { String(Int($0.rounded())) }.joined(separator: ",")
    }

    fileprivate init?(leanEncoded: String) {
        let parts = leanEncoded.split(separator: ",").compactMap { Int($0) }
        guard parts.count == 4 else { return nil }
        self.init(topLeftX: CGFloat(parts[0]), topLeftY: CGFloat(parts[1]), width: CGFloat(parts[2]), height: CGFloat(parts[3]))
    }
}

/// What Lean says to do to put every window in its place (Lean/DwmacCore/Layout.lean)
enum LeanEffect {
    /// Bring the window back from its corner; `restore` puts it back where it was. `monitor` is its monitor's rect
    case unhide(UInt32, monitor: Rect, restore: Bool)
    /// Set the window's frame. `virtual` is the workspace's rect
    case tile(UInt32, Rect, virtual: Rect, fullscreen: Bool)
    /// Keep a floating window on monitor `monitor` (whose visible rect is `visible`), centering it if `center`
    case float(UInt32, monitor: Int, visible: Rect, center: Bool)
    case hide(UInt32, OptimalHideCorner, visible: Rect)

    init?(parsing line: Substring) {
        let f = line.split(separator: "\t", omittingEmptySubsequences: false).map(String.init)
        guard f.count >= 2, let id = UInt32(f[1]) else { return nil }
        switch (f[0], f.count) {
            case ("unhide", 4):
                guard let monitor = Rect(leanEncoded: f[2]) else { return nil }
                self = .unhide(id, monitor: monitor, restore: f[3] == "1")
            case ("tile", 5):
                guard let rect = Rect(leanEncoded: f[2]), let virtual = Rect(leanEncoded: f[3]) else { return nil }
                self = .tile(id, rect, virtual: virtual, fullscreen: f[4] == "1")
            case ("float", 5):
                guard let monitor = Int(f[2]), let visible = Rect(leanEncoded: f[3]) else { return nil }
                self = .float(id, monitor: monitor, visible: visible, center: f[4] == "1")
            case ("hide", 4):
                guard let visible = Rect(leanEncoded: f[3]) else { return nil }
                self = .hide(id, f[2] == "bottom-left" ? .bottomLeftCorner : .bottomRightCorner, visible: visible)
            default: return nil
        }
    }
}

/// What the Lean state machine says the world looks like
struct LeanSnapshot: Equatable {
    struct Screen: Equatable {
        let monitor: Int
        let workspace: String
    }
    struct Win: Equatable {
        let id: UInt32
        let workspace: String
        let isFloating: Bool
        /// dwmac's `fullscreen`: whether it has no outer gaps
        var fullscreen: Bool? = nil
        /// macOS took it off the layout (native fullscreen, or its app is hidden)
        var parked: Bool = false
    }
    struct WsProps: Equatable {
        var layout: Layout = .masterStack
        var orientation: Orientation = .h
        var mfactPermille: Int = 500
    }
    var focusedWorkspace: String = ""
    var focusedWindow: UInt32? = nil
    /// Main monitor first
    var screens: [Screen] = []
    /// In tiling order
    var windows: [Win] = []
    /// Most recently focused first. May hold closed windows
    var mru: [UInt32] = []
    /// The focus before the last change
    var prevWorkspace: String? = nil
    var prevWindow: UInt32? = nil
    /// The connected monitor each workspace was last shown on
    var assigned: [String: Int] = [:]
    var defaultWsProps = WsProps()
    /// Workspaces whose layout differs from the default
    var wsProps: [String: WsProps] = [:]

    init(parsing text: String) {
        for line in text.split(separator: "\n") {
            let f = line.split(separator: "\t", omittingEmptySubsequences: false).map(String.init)
            switch (f.first, f.count) {
                case ("focus", 3):
                    focusedWorkspace = f[1]
                    focusedWindow = UInt32(f[2])
                case ("screen", 3):
                    if let mon = Int(f[1]) { screens.append(Screen(monitor: mon, workspace: f[2])) }
                case ("window", 6):
                    let fullscreen: Bool? = f[4] == "-" ? nil : f[4] == "1"
                    if let id = UInt32(f[1]) {
                        windows.append(Win(id: id, workspace: f[2], isFloating: f[3] == "1", fullscreen: fullscreen,
                                           parked: f[5] == "1"))
                    }
                case ("mru", 2):
                    mru = f[1].split(separator: ",").compactMap { UInt32($0) }
                case ("prev", 3):
                    prevWorkspace = f[1] == "-" ? nil : f[1]
                    prevWindow = UInt32(f[2])
                case ("assigned", 2):
                    for item in f[1].split(separator: "\u{1e}") {
                        let pair = item.split(separator: "\u{1f}", omittingEmptySubsequences: false)
                        if pair.count == 2, let mon = Int(pair[1]) { assigned[String(pair[0])] = mon }
                    }
                case ("wsdefault", 4):
                    if let p = WsProps(f[1], f[2], f[3]) { defaultWsProps = p }
                case ("ws", 5):
                    if let p = WsProps(f[2], f[3], f[4]) { wsProps[f[1]] = p }
                default: break
            }
        }
    }

    func workspace(onMonitor monitor: Int) -> String? { screens.first { $0.monitor == monitor }?.workspace }
    func window(_ id: UInt32) -> Win? { windows.first { $0.id == id } }
    /// `workspace`'s windows, in tiling order
    func windows(in workspace: String) -> [Win] { windows.filter { $0.workspace == workspace } }
    /// The window of `workspace` to focus: the most recently focused one, else the first (Lean's `mruWin`)
    func mruWindow(in workspace: String) -> UInt32? {
        mru.first { window($0)?.workspace == workspace } ?? windows.first { $0.workspace == workspace }?.id
    }
    func monitor(showing workspace: String) -> Int? { screens.first { $0.workspace == workspace }?.monitor }
    func props(_ workspace: String) -> WsProps { wsProps[workspace] ?? defaultWsProps }
}

extension LeanSnapshot.WsProps {
    fileprivate init?(_ layout: String, _ orientation: String, _ mfact: String) {
        guard let layout = Layout(rawValue: layout), let mfact = Int(mfact) else { return nil }
        self.init(layout: layout, orientation: orientation == "v" ? .v : .h, mfactPermille: mfact)
    }
}

/// The window manager's state, owned by Lean. Swift sends it ops and reads back snapshots
@MainActor
enum LeanState {
    /// The snapshot after the last op
    private(set) static var last = LeanSnapshot(parsing: "")

    static func reset(scratchpads: [(name: String, appId: String)]) {
        dwmac_state_init(scratchpads.map { "\($0.name)\u{1f}\($0.appId)" }.joined(separator: "\u{1e}"))
        last = snapshot
    }

    @discardableResult
    static func apply(_ op: LeanOp) -> LeanSnapshot {
        _ = exec(op.encoded)
        return last
    }

    static func run(_ cmd: LeanCmd) -> LeanReply { exec(cmd.encoded) }

    /// The reply `run` would give, without changing anything
    static func preview(_ cmd: LeanCmd) -> LeanReply {
        guard let raw = dwmac_state_preview(cmd.encoded) else { return LeanReply(parsing: "") }
        defer { free(raw) }
        return LeanReply(parsing: String(cString: raw))
    }

    private static func exec(_ input: String) -> LeanReply {
        let raw = dwmac_state_exec(input)
        last = snapshot
        guard let raw else { return LeanReply(parsing: "") }
        defer { free(raw) }
        return LeanReply(parsing: String(cString: raw))
    }

    /// Where every window goes. With `showAll` (dwmac is disabled), every workspace is laid out and nothing is hidden
    static func layout(showAll: Bool) -> [LeanEffect] {
        guard let raw = dwmac_state_layout(showAll) else { return [] }
        defer { free(raw) }
        return String(cString: raw).split(separator: "\n").compactMap(LeanEffect.init(parsing:))
    }

    static var snapshot: LeanSnapshot {
        guard let raw = dwmac_state_snapshot() else { return LeanSnapshot(parsing: "") }
        defer { free(raw) }
        return LeanSnapshot(parsing: String(cString: raw))
    }
}
