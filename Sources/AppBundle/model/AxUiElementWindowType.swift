import AppKit

enum AxUiElementWindowType: String {
    case window
    case dialog
    /// Not even a real window
    case popup
}

/// What Lean's window/dialog/popup heuristic (Lean/DwmacCore/WindowKind.lean) decides from, encoded for its
/// `windowKind` query. Read on the app's AX thread; covered by tests in test/axDumps
extension AxUiElementMock {
    func windowFacts(
        axApp: AxUiElementMock,
        _ bundleId: String?,
        _ activationPolicy: NSApplication.ActivationPolicy,
        _ windowLevel: MacOsWindowLevel?,
    ) -> String {
        func button(_ attr: Ax.ReadableAttrImpl<any AxUiElementMock>) -> String {
            guard let button = get(attr) else { return "-" }
            return button.get(Ax.enabledAttr) == true ? "1" : "0"
        }
        func flag(_ value: Bool?) -> String { value.map { $0 ? "1" : "0" } ?? "-" }
        let level: String = switch windowLevel {
            case .normalWindow: "0"
            case .alwaysOnTopWindow: "3"
            case .unknown(let level): "\(level)"
            case nil: "-"
        }
        let fields: [String] = [
            (bundleId ?? "").leanSafe,
            activationPolicy == .accessory ? "1" : "0",
            level,
            get(Ax.subroleAttr)?.leanSafe ?? "-",
            (get(Ax.titleAttr) ?? "").leanSafe,
            get(Ax.identifierAttr)?.leanSafe ?? "-",
            button(Ax.closeButtonAttr),
            button(Ax.fullscreenButtonAttr),
            button(Ax.zoomButtonAttr),
            button(Ax.minimizeButtonAttr),
            flag(get(Ax.isFocused)),
            flag(get(Ax.isMainAttr)),
            axApp.get(Ax.focusedWindowAttr)?.windowId == containingWindowId() ? "1" : "0",
        ]
        return fields.joined(separator: "\t")
    }
}

extension AxUiElementWindowType {
    /// Lean's verdict on `facts` (see `windowFacts`)
    @MainActor
    static func classify(_ facts: String) -> (type: AxUiElementWindowType, isDialog: Bool) {
        let answer = LeanState.query("windowKind\t" + facts).split(separator: "\t").map(String.init)
        guard answer.count == 2, let type = AxUiElementWindowType(rawValue: answer[0]) else {
            return (.window, false) // dwmac bug: the facts didn't parse
        }
        return (type, answer[1] == "1")
    }
}
