@testable import AppBundle
import Common
import XCTest

@MainActor
final class AxWindowKindTest: XCTestCase {
    func test() throws {
        try checkAxDumpsRecursive(projectRoot.appending(path: "test/axDumps"))
    }
}

@MainActor
func checkAxDumpsRecursive(_ dir: URL) throws {
    for file in try FileManager.default.contentsOfDirectory(at: dir, includingPropertiesForKeys: nil) {
        if file.isDirectory {
            try checkAxDumpsRecursive(file)
            continue
        }
        if file.pathExtension == "md" { continue }

        let rawJson = try JSONSerialization.jsonObject(with: Data.init(contentsOf: file), options: [.json5Allowed]) as! [String: Any]
        let json = Json.newOrDie(rawJson).asDictOrDie
        let app = json["Aero.AXApp"]!.asDictOrDie
        let appBundleId = rawJson["Aero.App.appBundleId"] as? String
        let windowLevel = json["Aero.windowLevel"].map { MacOsWindowLevel.fromJson($0) ?? dieT() }
        let activationPolicy: NSApplication.ActivationPolicy = .from(string: rawJson["Aero.App.nsApp.activationPolicy"] as! String)
        let kind = AxUiElementWindowType.classify(json.windowFacts(axApp: app, appBundleId, activationPolicy, windowLevel))
        assertEquals(
            kind.type,
            AxUiElementWindowType(rawValue: rawJson["Aero.AxUiElementWindowType"] as? String ?? dieT()),
            additionalMsg: "\(file.path()):0:0: AxUiElementWindowType doesn't match",
        )
        assertEquals(
            kind.isDialog,
            rawJson["Aero.AxUiElementWindowType_isDialogHeuristic"] as? Bool ?? dieT(),
            additionalMsg: "\(file.path()):0:0: AxUiElementWindowType_isDialogHeuristic doesn't match",
        )
    }
}

extension [String: Json]: AxUiElementMock {
    public func get<Attr>(_ attr: Attr) -> Attr.T? where Attr: ReadableAttr {
        // Swift reads every fact up front, including some of a synthetic element's (a button dumped as a reference)
        // that the heuristic never looks at: unknown, rather than a test failure
        guard let value = self[attr.key] else { return nil }
        if let value = value.rawValue {
            return attr.getter(value as AnyObject)
                ?? dieT("Value \(value) (of type \(Swift.type(of: value))) isn't convertible to \(attr.key)")
        } else {
            return nil
        }
    }

    public func containingWindowId() -> CGWindowID? { _containingWindowId() }

    private func _containingWindowId() -> CGWindowID {
        let windowId = self["Aero.axWindowId"]?.rawValue ?? dieT()
        if let windowId = windowId as? Int {
            return UInt32.init(windowId)
        } else if let windowId = windowId as? String { // Some dumps record it as a string
            return UInt32(windowId) ?? dieT()
        } else {
            return windowId as? UInt32 ?? dieT()
        }
    }
}

extension NSApplication.ActivationPolicy {
    static func from(string: String) -> NSApplication.ActivationPolicy {
        switch string {
            case "regular": .regular
            case "accessory": .accessory
            case "prohibited": .prohibited
            default: dieT("Unknown ActivationPolicy \(string)")
        }
    }
}

extension URL {
    var isDirectory: Bool {
        (try? resourceValues(forKeys: [.isDirectoryKey]))?.isDirectory == true
    }
}
