import CDwmacCore
import Foundation

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
        }
        return fields.joined(separator: "\t")
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
    }
    var focusedWorkspace: String = ""
    var focusedWindow: UInt32? = nil
    /// Main monitor first
    var screens: [Screen] = []
    /// In tiling order
    var windows: [Win] = []
    /// Most recently focused first. May hold closed windows
    var mru: [UInt32] = []

    init(parsing text: String) {
        for line in text.split(separator: "\n") {
            let f = line.split(separator: "\t", omittingEmptySubsequences: false).map(String.init)
            switch (f.first, f.count) {
                case ("focus", 3):
                    focusedWorkspace = f[1]
                    focusedWindow = UInt32(f[2])
                case ("screen", 3):
                    if let mon = Int(f[1]) { screens.append(Screen(monitor: mon, workspace: f[2])) }
                case ("window", 4):
                    if let id = UInt32(f[1]) { windows.append(Win(id: id, workspace: f[2], isFloating: f[3] == "1")) }
                case ("mru", 2):
                    mru = f[1].split(separator: ",").compactMap { UInt32($0) }
                default: break
            }
        }
    }

    func workspace(onMonitor monitor: Int) -> String? { screens.first { $0.monitor == monitor }?.workspace }
    func window(_ id: UInt32) -> Win? { windows.first { $0.id == id } }
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
        dwmac_state_step(op.encoded)
        last = snapshot
        return last
    }

    static var snapshot: LeanSnapshot {
        guard let raw = dwmac_state_snapshot() else { return LeanSnapshot(parsing: "") }
        defer { free(raw) }
        return LeanSnapshot(parsing: String(cString: raw))
    }
}
