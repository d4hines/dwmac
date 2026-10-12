import AppKit
import Common

@MainActor private var workspaceNameToWorkspace: [String: Workspace] = [:]

@MainActor private var screenPointToVisibleWorkspace: [CGPoint: Workspace] = [:]
@MainActor private var visibleWorkspaceToScreenPoint: [Workspace: CGPoint] = [:]

final class Workspace: DwNode, NonLeafDwNodeObject, Hashable, Comparable {
    let name: String
    nonisolated private let nameLogicalSegments: StringLogicalSegments
    /// `assignedMonitorPoint` must be interpreted only when the workspace is invisible
    fileprivate var assignedMonitorPoint: CGPoint? = nil

    @MainActor
    private init(_ name: String) {
        self.name = name
        self.nameLogicalSegments = name.toLogicalSegments()
        super.init(parent: NilDwNode.instance, index: 0)
    }

    @MainActor static var all: [Workspace] {
        workspaceNameToWorkspace.values.sorted()
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

    nonisolated static func == (lhs: Workspace, rhs: Workspace) -> Bool {
        check((lhs === rhs) == (lhs.name == rhs.name), "lhs: \(lhs) rhs: \(rhs)")
        return lhs === rhs
    }

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
    @MainActor
    var isVisible: Bool { visibleWorkspaceToScreenPoint.keys.contains(self) }
    @MainActor
    var workspaceMonitor: Monitor {
        forceAssignedMonitor
            ?? visibleWorkspaceToScreenPoint[self]?.monitorApproximation
            ?? assignedMonitorPoint?.monitorApproximation
            ?? mainMonitor
    }
}

extension Monitor {
    @MainActor
    var activeWorkspace: Workspace {
        if let existing = screenPointToVisibleWorkspace[rect.topLeftCorner] {
            return existing
        }
        // A monitor Lean hasn't heard of yet (monitor configuration changed)
        leanRefresh()
        return screenPointToVisibleWorkspace[rect.topLeftCorner]
            .orDie("Lean shows no workspace on monitor \(rect.topLeftCorner)")
    }

    @MainActor
    func setActiveWorkspace(_ workspace: Workspace) -> Bool {
        rect.topLeftCorner.setActiveWorkspace(workspace)
    }
}

@MainActor
func gcMonitors() {
    if screenPointToVisibleWorkspace.count != monitors.count {
        leanRefresh()
    }
}

extension CGPoint {
    /// Lean refuses scratchpad homes, and workspaces forced onto another monitor
    @MainActor
    fileprivate func setActiveWorkspace(_ workspace: Workspace) -> Bool {
        let monitor = leanMonitorId(self)
        return leanApply(.showWorkspace(monitor: monitor, workspace: workspace.name)).workspace(onMonitor: monitor) == workspace.name
    }
}

/// Make the visible workspaces match Lean's
@MainActor
func projectLeanScreens(_ snapshot: LeanSnapshot) {
    var newScreenToWorkspace: [CGPoint: Workspace] = [:]
    for screen in snapshot.screens {
        guard let point = leanMonitorPoint(screen.monitor) else { continue }
        newScreenToWorkspace[point] = Workspace.get(byName: screen.workspace)
    }
    screenPointToVisibleWorkspace = newScreenToWorkspace
    visibleWorkspaceToScreenPoint = Dictionary(uniqueKeysWithValues: newScreenToWorkspace.map { ($0.value, $0.key) })
    for (point, workspace) in newScreenToWorkspace {
        workspace.assignedMonitorPoint = point
    }
}
