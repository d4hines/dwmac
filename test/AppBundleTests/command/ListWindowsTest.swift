@testable import AppBundle
import Common
import XCTest

@MainActor
final class ListWindowsTest: XCTestCase {
    override func setUp() async throws { setUpWorkspacesForTests() }

    func testInterpolationVariablesConsistency() {
        for kind in AeroObjKind.allCases {
            switch kind {
                case .window:
                    assertTrue(FormatVar.WindowFormatVar.allCases.allSatisfy { $0.rawValue.starts(with: "window-") })
                case .app:
                    assertTrue(FormatVar.AppFormatVar.allCases.allSatisfy { $0.rawValue.starts(with: "app-") })
                case .workspace:
                    assertTrue(FormatVar.WorkspaceFormatVar.allCases.allSatisfy { $0.rawValue.starts(with: "workspace") })
                case .monitor:
                    assertTrue(FormatVar.MonitorFormatVar.allCases.allSatisfy { $0.rawValue.starts(with: "monitor-") })
            }
        }
    }

    func testFormat() {
        Workspace.get(byName: name).apply {
            let windows = [
                AeroObj.window(window: TestWindow.new(id: 2, parent: $0), title: "non-empty"),
                AeroObj.window(window: TestWindow.new(id: 1, parent: $0), title: ""),
            ]
            assertEquals(windows.format([.interVar("window-title")]), .success(["non-empty", ""]))
        }

        Workspace.get(byName: name).apply {
            let windows = [
                AeroObj.window(window: TestWindow.new(id: 3, parent: $0), title: "non-empty"),
                AeroObj.window(window: TestWindow.new(id: 10, parent: $0), title: ""),
            ]
            assertEquals(windows.format([.interVar("window-id"), .interVar("right-padding"), .interVar("window-title")]), .success(["3 non-empty", "10"]))
        }

        Workspace.get(byName: name).apply {
            let windows = [
                AeroObj.window(window: TestWindow.new(id: 4, parent: $0), title: "title1"),
                AeroObj.window(window: TestWindow.new(id: 11, parent: $0), title: "title2"),
            ]
            assertEquals(windows.format([.interVar("window-id"), .interVar("right-padding"), .literal(" | "), .interVar("window-title")]), .success(["4  | title1", "11 | title2"]))
        }
    }
}
