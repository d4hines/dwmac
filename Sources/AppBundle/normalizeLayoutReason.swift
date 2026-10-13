import AppKit

/// Follow macOS: windows it took off the layout (native fullscreen, a hidden app) are parked in Lean, minimized ones
/// leave Lean until they're unminimized, and popups that turn out to be windows join it
@MainActor
func normalizeLayoutReason() async throws {
    for window in Window.allWindowsMap.values.sorted(by: { $0.windowId < $1.windowId }) {
        switch window.unmanaged {
            case .popup?:
                try await validateStillPopup(window)
            case .minimized(let wasFloating)?:
                if try await !window.isMacosMinimized {
                    window.manage(on: focus.workspace, floating: wasFloating)
                }
            case nil:
                guard window.lean != nil else { continue }
                let isMacosFullscreen = try await window.isMacosFullscreen
                let isMacosMinimized = try await (!isMacosFullscreen).andAsync { @MainActor @Sendable in try await window.isMacosMinimized }
                if isMacosMinimized {
                    window.unmanage(as: .minimized(wasFloating: window.isFloating))
                    continue
                }
                let isMacosWindowOfHiddenApp = !isMacosFullscreen &&
                    !config.automaticallyUnhideMacosHiddenApps && window.macAppUnsafe.nsApp.isHidden
                let parked = isMacosFullscreen || isMacosWindowOfHiddenApp
                if parked != window.isParked {
                    leanApply(.setParked(window.windowId, parked))
                }
        }
    }
}

@MainActor
private func validateStillPopup(_ window: Window) async throws {
    guard let window = window as? MacWindow else { return }
    let windowLevel = getWindowLevel(for: window.windowId)
    guard let type = try await window.windowKind(windowLevel)?.type else { return }
    if type != .popup {
        window.manage(on: focus.workspace, floating: type == .dialog)
        try await tryOnWindowDetected(window)
    }
}
