import AppKit
import Common

let testEnv = ["PATH": "DWMAC_TEST_PATH", "DWMAC_INHERITED_TEST_ENV": "inherited"]
var processEnv: [String: String] {
    isUnitTest ? testEnv : ProcessInfo.processInfo.environment
}

/// The environment `exec-and-forget` runs commands in. The config decides it (`Config.env` in Lean/DwmacCore/Config.lean)
struct ExecConfig: Equatable {
    var envVariables: [String: String] = processEnv
}
