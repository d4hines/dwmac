# dwmac (beth fork)

A dwm-style master-stack tiling window manager for macOS, forked from [hillyu/dwmac](https://github.com/hillyu/dwmac)
(itself an [AeroSpace](https://github.com/nikitabobko/AeroSpace) fork). This fork is built by
[beth](https://github.com/d4hines/beth) (`overlays/dwmac`), not on its own.

Its state and decisions live in Lean, with proofs (see [Lean/README.md](Lean/README.md)); Swift is the shell that
talks to macOS. `lake build` (at the root) checks the proofs and the Lean tests; only `Lean/` ships.

| Path | What |
|---|---|
| `Lean/` | The core, in Lean: the state machine, commands and layout (compiled into the app) |
| `proofs/` | The theorems about the core |
| `Sources/AppBundle/` | The app: AX events and CLI commands in, Lean's effects carried out with AX calls |
| `Sources/CDwmacCore/` | The C shim around Lean's generated code (`generated/` is filled at build time) |
| `Sources/Cli/`, `Sources/Common/` | The `dwmac` CLI, and what it shares with the app (argument parsing, help) |
| `test/DwmacTest/` | Scenarios for the core's commands and protocol, checked with `#guard` at build time |
| `test/AppBundleTests/` | Swift tests for argument parsing and the effect interpreter |
| `test/axDumps/` | AX dumps of real windows, fixtures for the window/dialog/popup heuristic's tests |
| `docs/config-examples/default-config.toml` | The default config, bundled into the app |
| `resources/` | The app icon |

From beth: `overlays/dwmac/dev.sh lean|build|test|install`; `darwin-rebuild` builds the committed submodule.
