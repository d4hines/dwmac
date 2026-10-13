import Common

struct Mode: ConvenienceCopyable, Equatable, Sendable {
    /// User visible name. Optional. todo drop it?
    var name: String?
    var bindings: [String: HotkeyBinding]

    static let zero = Mode(name: nil, bindings: [:])
}
