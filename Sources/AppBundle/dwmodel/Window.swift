import AppKit
import Common

/// A window dwmac knows of. Where it lives (workspace, order, floating, parked by macOS, focus) is Lean's
/// (Lean/DwmacCore/Model.lean); this holds what only Swift knows: how to reach it with AX, the frames it was given,
/// and the windows Lean doesn't manage
open class Window: Hashable, AeroAny {
    nonisolated let windowId: UInt32 // todo nonisolated keyword is no longer necessary?
    let app: any AbstractApp
    var lastFloatingSize: CGSize?
    /// Set for the windows Lean doesn't manage
    var unmanaged: Unmanaged? = nil
    // Usages:
    // - resize with mouse
    // - makeFloatingWindowsSeenAsTiling in focus command
    var lastAppliedLayoutVirtualRect: Rect? = nil // as if inner gaps were always zero
    // Usages:
    // - resize with mouse
    // - drag window with mouse
    // - move-mouse command
    var lastAppliedLayoutPhysicalRect: Rect? = nil // with real inner gaps
    /// The Lean monitor id the window was last laid out on, as a floating window
    var lastLayoutMonitor: Int? = nil

    enum Unmanaged: Equatable {
        /// Windows to AX, not to a human (e.g. the keyboard layout switch popup)
        case popup
        /// Lean forgets minimized windows, so it never focuses one. They come back as new windows
        case minimized(wasFloating: Bool)
    }

    init(id: UInt32, _ app: any AbstractApp, lastFloatingSize: CGSize?) {
        self.windowId = id
        self.app = app
        self.lastFloatingSize = lastFloatingSize
    }

    /// Every window dwmac knows of, managed by Lean or not
    @MainActor static var allWindowsMap: [UInt32: Window] = [:]

    @MainActor static func get(byId windowId: UInt32) -> Window? { allWindowsMap[windowId] }

    @MainActor
    func closeAxWindow() { die("Not implemented") }

    nonisolated public static func == (lhs: Window, rhs: Window) -> Bool { lhs === rhs }

    nonisolated public func hash(into hasher: inout Hasher) {
        hasher.combine(windowId)
    }

    func getAxTopLeftCorner() async throws -> CGPoint? { die("Not implemented") }
    func getAxSize() async throws -> CGSize? { die("Not implemented") }
    var title: String { get async throws { die("Not implemented") } }
    var isMacosFullscreen: Bool { get async throws { false } }
    var isMacosMinimized: Bool { get async throws { false } } // todo replace with enum MacOsWindowNativeState { normal, fullscreen, invisible }
    var isHiddenInCorner: Bool { die("Not implemented") }
    /// Forget the position from before hideInCorner without moving the window, for callers that set the frame
    /// themselves (otherwise the next refresh's unhideFromCorner would move it back there)
    @MainActor
    func forgetHiddenInCorner() { die("Not implemented") }
    @MainActor
    func nativeFocus() { die("Not implemented") }
    func getAxRect() async throws -> Rect? { die("Not implemented") }
    /// Move the window off screen, into `corner` of `visible` (a monitor's visible rect)
    @MainActor
    func hideInCorner(_ corner: OptimalHideCorner, visible: Rect) async throws { die("Not implemented") }
    /// Undo hideInCorner. `restore`: move it back where it was, relative to `monitor`
    @MainActor
    func unhideFromCorner(monitor: Rect, restore: Bool) { die("Not implemented") }
    func getCenter() async throws -> CGPoint? { try await getAxRect()?.center }

    func setAxFrameBlocking(_ topLeft: CGPoint?, _ size: CGSize?) async throws { die("Not implemented") }
    func setAxFrame(_ topLeft: CGPoint?, _ size: CGSize?) { die("Not implemented") }
}

// What Lean says about the window
extension Window {
    @MainActor var lean: LeanSnapshot.Win? { LeanState.last.window(windowId) }
    /// nil for the windows Lean doesn't manage
    @MainActor var workspace: Workspace? { lean.map { Workspace.get(byName: $0.workspace) } }
    @MainActor var isFloating: Bool { lean?.isFloating ?? false }
    /// macOS took it off the layout: it's in native fullscreen, or its app is hidden
    @MainActor var isParked: Bool { lean?.parked ?? false }
    /// dwmac's `fullscreen` (Lean shows it fullscreen while it's its workspace's most recent window)
    @MainActor var isFullscreen: Bool { lean?.fullscreen != nil }
    @MainActor var noOuterGapsInFullscreen: Bool { lean?.fullscreen == true }
    /// Its position among its workspace's windows
    @MainActor var ownIndex: Int? {
        lean.flatMap { lean in LeanState.last.windows(in: lean.workspace).firstIndex { $0.id == windowId } }
    }

    /// Hand the window to Lean as a new window of `workspace`. Tiling windows go first, or last with `attach-below`
    @MainActor func manage(on workspace: Workspace, floating: Bool, index: Int? = nil) {
        unmanaged = nil
        let index = index ?? (floating || config.attachBelow ? Int(Int32.max) : 0)
        leanApply(.windowAppeared(windowId, appId: app.rawAppBundleId ?? "", workspace: workspace.name,
                                  isFloating: floating, index: index))
    }

    /// Take the window from Lean
    @MainActor func unmanage(as reason: Unmanaged) {
        if lean != nil { leanApply(.windowClosed(windowId)) }
        unmanaged = reason
    }

    /// Forget a window that's gone. Focus stays on its workspace, unless that's the focused one, or the workspace
    /// focus just left (some apps switch workspace by closing their window)
    // skipClosedWindowsCache is an optimization when it's definitely not necessary to cache closed window.
    //                        If you are unsure, it's better to pass `false`
    @MainActor
    func garbageCollect(skipClosedWindowsCache: Bool) {
        if Window.allWindowsMap.removeValue(forKey: windowId) == nil {
            return
        }
        if !skipClosedWindowsCache { cacheClosedWindowIfNeeded() }
        let deadWindowWorkspace = workspace // nil for popups and minimized windows: focus doesn't switch back for them
        if lean != nil { leanApply(.windowClosed(windowId)) }
        let focus = focus
        if let deadWindowWorkspace, deadWindowWorkspace == focus.workspace ||
            deadWindowWorkspace == prevFocusedWorkspace && prevFocusedWorkspaceDate.distance(to: .now) < 1
        {
            let deadWindowFocus = deadWindowWorkspace.toLiveFocus()
            _ = setFocus(to: deadWindowFocus)
            // Guard against "Apple Reminders popup" bug: https://github.com/hillyu/Dwmac/issues/201
            if focus.windowOrNil?.app.pid != app.pid {
                // Force focus to fix macOS annoyance with focused apps without windows.
                //   https://github.com/hillyu/Dwmac/issues/65
                deadWindowFocus.windowOrNil?.nativeFocus()
            }
        }
    }

    func asMacWindow() -> MacWindow { self as! MacWindow }
}
