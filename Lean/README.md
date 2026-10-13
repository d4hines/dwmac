# DwmacCore: the verified core of dwmac

dwmac's state and decisions live here, in Lean, with proofs of the properties the window manager relies on. Lean
compiles them to C, which the `CDwmacCore` Swift target links (see `Sources/CDwmacCore`). Swift is the shell
around it: it turns AX events into `Op`s and CLI commands into `Cmd`s, prints Lean's replies, and carries out the
`Effect`s Lean computes with AX calls.

Three Lean libraries, all built by `lake build` at the repo root (`lakefile.toml`). Only `DwmacCore` is compiled
into the app; the other two are checked at build time.

| Code (`Lean/DwmacCore/`) | What | Proofs (`proofs/DwmacProofs/`) |
|---|---|---|
| `Split.lean` | Cut a length into `n` segments with gaps, in whole pixels | segments are disjoint and within the length |
| `MasterStack.lean` | Master-stack tiling | `masterStack_length`, `masterStack_pairwise_disjoint`, `masterStack_within` |
| `Model.lean` | The state machine: monitors, workspaces, windows, focus, scratchpads, layout settings, and an `Op` per command or event | its invariant `Inv`, preserved by every op: `reachable_inv`, `home_never_shown`, `scratchpad_topmost_or_hidden` |
| `Layout.lean` | The layout as effects: tile, float, hide in a corner, unhide | `layout_shows_visible`, `layout_never_shows_home` |
| `Command.lean` | The CLI commands (`focus`, `workspace`, `swap`, `scratchpad`, ...): each is planned as a list of ops plus a reply | `exec_inv`, `reachable_exec_inv`, `layout_never_shows_home_exec` |
| `StateFfi.lean` | The text protocol and `@[export]`ed C entry points | |

The tests (`test/DwmacTest/`) are scenarios checked with `#guard` at build time: `Model.lean` for the state
machine and the text protocol, `Command.lean` for each command's behaviour and messages.

The pipeline:

```
Lean/DwmacCore/*.lean ──lake build──▶ .lake/build/ir/DwmacCore/*.c ──copy-lean-c.sh──▶ Sources/CDwmacCore/generated/
   (checks proofs/ and test/ too)                                                          │
Sources/CDwmacCore/shim.c (runtime init, holds the state) ◀─────────────────────────────────┘
          ▲
Sources/AppBundle/dwmodel/LeanState.swift (ops, commands, replies, snapshots, effects)
          ▲
Sources/AppBundle/dwmodel/LeanSync.swift (config and monitor changes → ops)
Sources/AppBundle/dwmodel/{Window,Workspace}.swift, normalizeLayoutReason.swift (snapshot queries; AX events → ops)
Sources/AppBundle/command/impl/*.swift (parse arguments → `leanRun`)
Sources/AppBundle/layout/WorkspaceLayout.swift (effects → AX calls)
```

## Working on it

The toolchain is the flake's `lean4` (`lean-toolchain` names the same version for editors). From the beth repo:

```sh
overlays/dwmac/dev.sh lean   # build, check proofs and tests, regenerate C
overlays/dwmac/dev.sh test   # ...then the Swift tests
```

For an editor, point the Lean extension at the flake's Lean, e.g. `nix shell <beth>#darwinConfigurations.<host>.pkgs.lean4`.

To check that a theorem doesn't secretly rely on `sorry`, `#print axioms masterStack_pairwise_disjoint` should list
only `propext`, `Classical.choice` and `Quot.sound`.
