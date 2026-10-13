public typealias Parsed<T> = Result<T, String>
extension String: @retroactive Error {} // Make it possible to use String in Result. todo migrate to self written Result monad
extension Array: @retroactive Error where Element: Error {} // Make it possible to use [String] in Result. todo migrate to self written Result monad

extension String {
    public func trim() -> String {
        self.trimmingCharacters(in: .whitespacesAndNewlines)
    }

    public func prefixLines(with: String) -> String {
        split(separator: "\n", omittingEmptySubsequences: false).map { with + $0 }.joined(separator: "\n")
    }

    public func quoted(with char: String) -> String { char + self + char }
    public var singleQuoted: String { "'" + self + "'" }
    public var doubleQuoted: String { "\"" + self + "\"" }
}

extension [String] {
    public func joinErrors() -> String { // todo reuse in config parsing?
        map { (error: String) -> String in
            error.split(separator: "\n").enumerated()
                .map { (i, line) in
                    i == 0
                        ? "ERROR: " + line
                        : "       " + line
                }
                .joined(separator: "\n")
        }
        .joined(separator: "\n")
    }

    public func joinTruncating(separator: String, length maxLength: Int, trailing: String = "…") -> String {
        if isEmpty {
            return ""
        }
        var remainingLen = maxLength
        let separatorCount = separator.count
        var result: String = first.orDie()
        for _elem in self.dropFirst() {
            let elemCount = separatorCount + _elem.count
            if remainingLen < elemCount / 2 {
                return result + separator + trailing
            }
            let elem = separator + _elem
            if elemCount < remainingLen {
                result += elem
                remainingLen -= elemCount
            } else {
                return result + elem.prefix(remainingLen) + trailing
            }
        }
        return result
    }
}

extension [[String]] {
    public func toPaddingTable(columnSeparator: String = " | ") -> [String] {
        let pads: [Int] = transposed().map { column in column.map { $0.count }.max().orDie() }
        return self.map { (row: [String]) in
            zip(row.enumerated(), pads)
                .map { (elem: (Int, String), pad: Int) in
                    elem.0 != row.count - 1 ? elem.1.padding(toLength: pad, withPad: " ", startingAt: 0) : elem.1
                }
                .joined(separator: columnSeparator)
        }
    }
}

extension Array { // todo move to ArrayEx.swift
    public func transposed<T>() -> [[T]] where Self.Element == [T] {
        if isEmpty {
            return []
        }
        let table: [[T]] = self
        var result: [[T]] = []
        for columnIndex in 0... {
            if columnIndex < table.first.orDie().count {
                result += [table.map { row in row.getOrNil(atIndex: columnIndex).orDie() }]
            } else {
                break
            }
        }
        return result
    }
}


public enum StringInterToken: Equatable, Sendable {
    case literal(String)
    case interVar(String) // "interpolation variable"
}
