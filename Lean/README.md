# DwmacCore: verified window manager algorithms

dwmac algorithms rewritten in Lean, with proofs of the properties the window manager relies on. Lean compiles
them to C, which the `CDwmacCore` Swift target links (see `Sources/CDwmacCore`).

| File | What | Theorems |
|---|---|---|
| `DwmacCore/Split.lean` | Cut a length into `n` segments with gaps, in whole pixels | segments are disjoint and within the length |
| `DwmacCore/MasterStack.lean` | Master-stack tiling | `masterStack_length`, `masterStack_pairwise_disjoint`, `masterStack_within` |
| `DwmacCore/Ffi.lean` | `@[export]`ed C entry points | |

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
