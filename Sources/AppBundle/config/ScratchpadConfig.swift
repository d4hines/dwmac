import Common

struct Scratchpad: ConvenienceCopyable, Equatable {
    var name: String = ""
    var appId: String = ""
    /// Fractions of the monitor's visible rect
    var width: Double = 0.9
    var height: Double = 0.9
}
