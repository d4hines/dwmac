@testable import AppBundle
import Common
import Foundation
import AppKit
import HotKey

extension [String] {
    var descriptions: [String] { self }
}

extension HotkeyBinding {
    init(_ modifiers: NSEvent.ModifierFlags, _ keyCode: Key, _ commands: [any Command], descriptionWithKeyNotation: String? = nil) {
        let desc = modifiers.isEmpty ? keyCode.toString() : modifiers.toString() + "-" + keyCode.toString()
        self.init(modifiers, keyCode, commands, descriptionWithKeyNotation: descriptionWithKeyNotation ?? desc, descriptionWithKeyCode: desc)
    }
}
