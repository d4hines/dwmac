import Common

struct Mode: ConvenienceCopyable {
    /// User visible name. Optional. todo drop it?
    var name: String?
    /// Each binding's commands, by the binding as the config writes it (`alt-shift-j`)
    var bindings: [String: [any Command]]

    static let zero = Mode(name: nil, bindings: [:])
}
