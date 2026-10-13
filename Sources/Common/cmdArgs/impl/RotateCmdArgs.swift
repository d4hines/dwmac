public struct RotateCmdArgs: CmdArgs {
    public var commonState: CmdArgsCommonState

    public init(rawArgs: StrArrSlice) {
        self.commonState = .init(rawArgs)
    }

    public static let parser: CmdParser<Self> = cmdParser(
        kind: .rotate,
        allowInConfig: true,
        help: rotate_help_generated,
        flags: [:],
        posArgs: [],
    )
}
