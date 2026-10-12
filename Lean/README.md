# DwmacCore: verified window manager algorithms

dwmac algorithms rewritten in Lean, with proofs of the properties the window manager relies on. Lean compiles
them to C, which the `CDwmacCore` Swift target links (see `Sources/CDwmacCore`).

| File | What | Theorems |
|---|---|---|
| `DwmacCore/Split.lean` | Cut a length into `n` segments with gaps, in whole pixels | segments are disjoint and within the length |
| `DwmacCore/MasterStack.lean` | Master-stack tiling | `masterStack_length`, `masterStack_pairwise_disjoint`, `masterStack_within` |
| `DwmacCore/Ffi.lean` | `@[export]`ed C entry points | |
| `DwmacCore/Model.lean` | The state machine: monitors, workspaces, windows, focus, scratchpads, and an `Op` per command or event | |
| `DwmacCore/ModelProofs.lean` | Its invariant, preserved by every op | `reachable_inv`, `home_never_shown`, `scratchpad_topmost_or_hidden` |
| `DwmacCore/ModelTest.lean` | Scenarios, checked with `#guard` at build time | |

The model isn't wired into Swift yet: it's the spec the Swift code should match. It already settles two questions
Swift answers differently: `move-node-to-workspace` refuses to move a window into a scratchpad home, or a scratchpad
window out of one.

The pipeline:

```
Lean/DwmacCore/*.lean ──lake build──▶ .lake/build/ir/*.c ──copy-lean-c.sh──▶ Sources/CDwmacCore/generated/
                     (checks proofs)                                              │
Sources/CDwmacCore/shim.c (runtime init, plain C API) ◀───────────────────────────┘
          ▲
Sources/AppBundle/layout/VerifiedLayout.swift (Swift wrapper)
```

## Working on it

The toolchain is the flake's `lean4` (`lean-toolchain` names the same version for editors). From the beth repo:

```sh
overlays/dwmac/dev.sh lean   # build, check proofs, regenerate C
overlays/dwmac/dev.sh test   # ...then the Swift tests
```

For an editor, point the Lean extension at the flake's Lean, e.g. `nix shell ~/repos/beth#darwinConfigurations.yachal.pkgs.lean4`.

To check that a theorem doesn't secretly rely on `sorry`, `#print axioms masterStack_pairwise_disjoint` should list
only `propext`, `Classical.choice` and `Quot.sound`.
