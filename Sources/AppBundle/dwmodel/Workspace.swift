import AppKit
import Common

@MainActor private var workspaceNameToWorkspace: [String: Workspace] = [:]

/// A workspace, by name. Lean owns what's in it, its layout, and the monitor it's shown on
/// (Lean/DwmacCore/Model.lean); these read and request those
final class Workspace: Hashable, Comparable, AeroAny {
    let name: String
    nonisolated private let nameLogicalSegments: StringLogicalSegments

    private init(_ name: String) {
        self.name = name
        self.nameLogicalSegments = name.toLogicalSegments()
    }

    /// The workspaces with windows, the shown ones, and the ones asked for by name since the last garbage collection
    @MainActor static var all: [Workspace] {
        let snapshot = LeanState.last
        for name in snapshot.windows.map(\.workspace) + snapshot.screens.map(\.workspace) {
            _ = get(byName: name)
        }
        return workspaceNameToWorkspace.values.sorted()
    }

    @MainActor static func get(byName name: String) -> Workspace {
        if let existing = workspaceNameToWorkspace[name] {
            return existing
        } else {
            let workspace = Workspace(name)
            workspaceNameToWorkspace[name] = workspace
            return workspace
        }
    }

    nonisolated static func < (lhs: Workspace, rhs: Workspace) -> Bool {
        lhs.nameLogicalSegments < rhs.nameLogicalSegments
    }

    @MainActor
    var description: String {
        let description = [
            ("name", name),
            ("isVisible", String(isVisible)),
            ("isEffectivelyEmpty", String(isEffectivelyEmpty)),
            ("doKeepAlive", String(config.persistentWorkspaces.contains(name))),
        ].map { "\($0.0): '\(String(describing: $0.1))'" }.joined(separator: ", ")
        return "Workspace(\(description))"
    }

    @MainActor
    static func garbageCollectUnusedWorkspaces() {
        for name in config.persistentWorkspaces {
            _ = get(byName: name) // Make sure that all persistent workspaces are "cached"
        }
        workspaceNameToWorkspace = workspaceNameToWorkspace.filter { (_, workspace: Workspace) in
            config.persistentWorkspaces.contains(workspace.name) ||
                !workspace.isEffectivelyEmpty ||
                workspace.isVisible ||
                workspace.name == focus.workspace.name
        }
    }

    nonisolated static func == (lhs: Workspace, rhs: Workspace) -> Bool { lhs.name == rhs.name }

    nonisolated func hash(into hasher: inout Hasher) { hasher.combine(name) }
}

// Owned by Lean (Lean/DwmacCore/Model.lean): setting one is a request to Lean
extension Workspace {
    @MainActor var layout: Layout {
        get { LeanState.last.props(name).layout }
        set { leanApply(.setLayout(name, newValue, nil)) }
    }
    @MainActor var orientation: Orientation {
        get { LeanState.last.props(name).orientation }
        set { leanApply(.setLayout(name, nil, newValue)) }
    }
    /// Master's share of the main axis. Lean keeps it between 0.05 and 0.95
    @MainActor var mfact: CGFloat {
        get { CGFloat(LeanState.last.props(name).mfactPermille) / 1000 }
        set { leanApply(.mfact(name, .set(permille: Int((newValue * 1000).rounded())))) }
    }
}

extension Workspace {
    /// In tiling order, parked ones included
    @MainActor var allWindows: [Window] { LeanState.last.windows(in: name).compactMap { Window.get(byId: $0.id) } }
    @MainActor var tilingWindows: [Window] { allWindows.filter { !$0.isFloating && !$0.isParked } }
    @MainActor var floatingWindows: [Window] { allWindows.filter { $0.isFloating && !$0.isParked } }
    @MainActor var isEffectivelyEmpty: Bool { LeanState.last.windows(in: name).isEmpty }
    /// The window focusing the workspace focuses
    @MainActor var mostRecentWindow: Window? { LeanState.last.mruWindow(in: name).flatMap { Window.get(byId: $0) } }

    @MainActor
    var isVisible: Bool { LeanState.last.monitor(showing: name) != nil }
    /// The monitor it's (or would be) shown on: its forced one, else its current one, else the last one, else the
    /// main one (Lean's `workspaceMonitor`)
    @MainActor
    var workspaceMonitor: Monitor {
        let snapshot = LeanState.last
        return forceAssignedMonitor
            ?? (snapshot.monitor(showing: name) ?? snapshot.assigned[name]).flatMap(leanMonitor)
            ?? mainMonitor
    }

    @MainActor var forceAssignedMonitor: Monitor? {
        guard let monitorDescriptions = config.workspaceToMonitorForceAssignment[name] else { return nil }
        let sortedMonitors = sortedMonitors
        return monitorDescriptions.lazy
            .compactMap { $0.resolveMonitor(sortedMonitors: sortedMonitors) }
            .first
    }
}

extension Monitor {
    @MainActor
    var activeWorkspace: Workspace {
        let id = leanMonitorId(rect.topLeftCorner)
        if LeanState.last.workspace(onMonitor: id) == nil {
            leanRefresh() // A monitor Lean hasn't heard of yet (monitor configuration changed)
        }
        let name = LeanState.last.workspace(onMonitor: id).orDie("Lean shows no workspace on monitor \(rect.topLeftCorner)")
        return Workspace.get(byName: name)
    }

    /// Lean refuses scratchpad homes, and workspaces forced onto another monitor
    @MainActor
    func setActiveWorkspace(_ workspace: Workspace) -> Bool {
        let monitor = leanMonitorId(rect.topLeftCorner)
        return leanApply(.showWorkspace(monitor: monitor, workspace: workspace.name)).workspace(onMonitor: monitor) == workspace.name
    }
}

@MainActor
func gcMonitors() {
    if LeanState.last.screens.count != monitors.count {
        leanRefresh()
    }
}

/// The connected monitor with Lean id `id`
@MainActor
func leanMonitor(_ id: Int) -> Monitor? {
    guard let point = leanMonitorPoint(id) else { return nil }
    return monitors.first { $0.rect.topLeftCorner == point }
}
