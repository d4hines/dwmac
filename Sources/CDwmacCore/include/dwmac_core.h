// The window manager's state and layout, formally verified in Lean (Lean/DwmacCore) and compiled to C.
// Plain C types only: the shim keeps Lean's runtime and objects out of Swift's sight.
#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// The window manager's state machine (Lean/DwmacCore/Model.lean), held here. Proven: whatever ops are applied, no
/// scratchpad home is ever shown, a scratchpad is either hidden or focused, and focus points at a shown workspace.
/// Main thread only. Ops and snapshots are text; see Lean/DwmacCore/StateFfi.lean for the format.
void dwmac_state_init(const char *scratchpads);
void dwmac_state_step(const char *op);
/// The caller frees the result
char *dwmac_state_snapshot(void);
/// Where every window goes, as effects for Swift to carry out (one per line, see `Effect.render`). Proven: unless
/// `show_all` (dwmac is disabled), a window in a scratchpad's home is only ever hidden. The caller frees the result
char *dwmac_state_layout(bool show_all);
