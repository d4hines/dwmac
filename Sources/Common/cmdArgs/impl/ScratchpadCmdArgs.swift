public struct ScratchpadCmdArgs: CmdArgs {
    /*conforms*/ public var commonState: CmdArgsCommonState
    public init(rawArgs: StrArrSlice) { self.commonState = .init(rawArgs) }
    public static let parser: CmdParser<Self> = cmdParser(
        kind: .scratchpad,
        allowInConfig: true,
        help: scratchpad_help_generated,
        flags: [:],
        posArgs: [newArgParser(\.name, consumeStrCliArg, mandatoryArgPlaceholder: "<scratchpad>")],
    )

    public var name: Lateinit<String> = .uninitialized
}
