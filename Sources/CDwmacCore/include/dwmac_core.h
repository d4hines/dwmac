// Formally verified layout algorithms, written in Lean (Lean/DwmacCore) and compiled to C.
// Plain C types only: the shim keeps Lean's runtime and objects out of Swift's sight.
#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// A tile, relative to the workspace's top left corner, in whole points
typedef struct {
    uint32_t x, y, w, h;
} DwmacTile;

/// Master-stack tiles of `n` windows in a `w` x `h` workspace, master first. `mfact_permille` is master's share of
/// the main axis in thousandths. Writes up to `cap` tiles to `out` and returns the number of tiles (always `n`).
/// Proven (Lean/DwmacCore/MasterStack.lean): no two tiles overlap and every tile lies within the workspace.
size_t dwmac_master_stack(uint32_t w, uint32_t h, uint32_t gap_h, uint32_t gap_v, uint32_t mfact_permille,
                          bool vertical, bool master_at_stop, uint32_t n, DwmacTile *out, size_t cap);
