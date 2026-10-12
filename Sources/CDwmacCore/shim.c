#include <lean/lean.h>
#include <pthread.h>
#include <stdlib.h>

#include "dwmac_core.h"

// Lean's runtime (libleanrt) and the generated code (generated/, from Lean/.lake/build/ir)
void lean_initialize_runtime_module(void);
void lean_initialize_thread(void);
void lean_io_mark_end_initialization(void);
void lean_io_result_show_error(lean_object *result);
lean_object *initialize_DwmacCore_DwmacCore(uint8_t builtin);
lean_object *dwmac_lean_master_stack(uint32_t w, uint32_t h, uint32_t gap_h, uint32_t gap_v, uint32_t mfact_permille,
                                     uint8_t vertical, uint8_t master_at_stop, uint32_t n);

static pthread_once_t runtime_once = PTHREAD_ONCE_INIT;
static _Thread_local bool thread_ready = false;

static void initialize_runtime(void) {
    lean_initialize_runtime_module();
    lean_object *result = initialize_DwmacCore_DwmacCore(1);
    if (!lean_io_result_is_ok(result)) {
        lean_io_result_show_error(result);
        abort();
    }
    lean_dec_ref(result);
    lean_io_mark_end_initialization();
    thread_ready = true;
}

/// The runtime is initialized on first use, from whichever thread gets there first. Lean wants every other thread
/// that calls into it to register itself
static void ensure_ready(void) {
    pthread_once(&runtime_once, initialize_runtime);
    if (!thread_ready) {
        lean_initialize_thread();
        thread_ready = true;
    }
}

static uint32_t read_u32_le(const uint8_t *p) {
    return (uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 | (uint32_t)p[3] << 24;
}

size_t dwmac_master_stack(uint32_t w, uint32_t h, uint32_t gap_h, uint32_t gap_v, uint32_t mfact_permille,
                          bool vertical, bool master_at_stop, uint32_t n, DwmacTile *out, size_t cap) {
    ensure_ready();
    lean_object *bytes = dwmac_lean_master_stack(w, h, gap_h, gap_v, mfact_permille, vertical, master_at_stop, n);
    size_t count = lean_sarray_size(bytes) / 16;
    const uint8_t *p = lean_sarray_cptr(bytes);
    for (size_t i = 0; i < count && i < cap; i++, p += 16) {
        out[i] = (DwmacTile){read_u32_le(p), read_u32_le(p + 4), read_u32_le(p + 8), read_u32_le(p + 12)};
    }
    lean_dec_ref(bytes);
    return count;
}
