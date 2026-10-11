import Common
import TOMLKit

struct Scratchpad: ConvenienceCopyable, Equatable {
    var name: String = ""
    var appId: String = ""
    /// Fractions of the monitor's visible rect
    var width: Double = 0.9
    var height: Double = 0.9
}

private let scratchpadParser: [String: any ParserProtocol<Scratchpad>] = [
    "app-id": Parser(\.appId, parseString),
    "width": Parser(\.width, parseFraction),
    "height": Parser(\.height, parseFraction),
]

func parseScratchpads(_ raw: TOMLValueConvertible, _ backtrace: TomlBacktrace, _ errors: inout [TomlParseError]) -> [String: Scratchpad] {
    guard let rawTable = raw.table else {
        errors += [expectedActualTypeError(expected: .table, actual: raw.type, backtrace)]
        return [:]
    }
    var result: [String: Scratchpad] = [:]
    var appIdToName: [String: String] = [:]
    for (name, rawScratchpad) in rawTable {
        let backtrace = backtrace + .key(name)
        var myErrors: [TomlParseError] = []
        let scratchpad = parseTable(rawScratchpad, Scratchpad(name: name), scratchpadParser, backtrace, &myErrors)
        if scratchpad.appId.isEmpty {
            myErrors.append(.semantic(backtrace, "'app-id' is mandatory key"))
        } else if let other = appIdToName[scratchpad.appId] {
            myErrors.append(.semantic(backtrace, "app-id '\(scratchpad.appId)' is already owned by scratchpad '\(other)'"))
        }
        if myErrors.isEmpty {
            result[name] = scratchpad
            appIdToName[scratchpad.appId] = name
        } else {
            errors += myErrors
        }
    }
    return result
}

private func parseFraction(_ raw: TOMLValueConvertible, _ backtrace: TomlBacktrace) -> ParsedToml<Double> {
    (raw.double ?? raw.int.map(Double.init))
        .orFailure(expectedActualTypeError(expected: [.double, .int], actual: raw.type, backtrace))
        .filter(.semantic(backtrace, "Must be in (0, 1] range")) { $0 > 0 && $0 <= 1 }
}
