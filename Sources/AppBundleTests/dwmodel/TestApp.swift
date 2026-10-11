@testable import AppBundle
import Common

final class TestApp: AbstractApp {
    let pid: Int32
    let rawAppBundleId: String?
    let name: String?
    let execPath: String? = nil
    let bundlePath: String? = nil
    @MainActor
    static let shared = TestApp()
    /// For tests that need windows of two different apps
    @MainActor
    static let other = TestApp(pid: 1, bundleId: "hillyu.Dwmac.other-test-app")

    private init(pid: Int32 = 0, bundleId: String = "hillyu.Dwmac.test-app") {
        self.pid = pid
        self.rawAppBundleId = bundleId
        self.name = rawAppBundleId
    }

    var _windows: [Window] = []
    var windows: [Window] {
        get { _windows }
        set {
            if let focusedWindow {
                check(newValue.contains(focusedWindow))
            }
            _windows = newValue
        }
    }
    @MainActor func detectNewWindowsAndGetIds() async throws -> [UInt32] {
        return windows.map { $0.windowId }
    }

    private var _focusedWindow: Window? = nil
    var focusedWindow: Window? {
        get { _focusedWindow }
        set {
            if let window = newValue {
                check(windows.contains(window))
            }
            _focusedWindow = newValue
        }
    }
    @MainActor func getFocusedWindow() -> Window? { _focusedWindow }
}
