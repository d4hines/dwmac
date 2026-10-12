#include <lean/lean.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

#include "dwmac_core.h"

// Lean's runtime (libleanrt) and the generated code (generated/, from Lean/.lake/build/ir)
void lean_initialize_runtime_module(void);
void lean_initialize_thread(void);
void lean_io_mark_end_initialization(void);
void lean_io_result_show_error(lean_object *result);
lean_object *initialize_DwmacCore_DwmacCore(uint8_t builtin);
lean_object *dwmac_lean_state_init(lean_object *scratchpads);
lean_object *dwmac_lean_state_step(lean_object *state, lean_object *op);
lean_object *dwmac_lean_state_snapshot(lean_object *state);
lean_object *dwmac_lean_state_layout(lean_object *state, uint8_t show_all);

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

// The window manager's state (Lean/DwmacCore/Model.lean). Only touched from the main thread
static lean_object *state = NULL;

void dwmac_state_init(const char *scratchpads) {
    ensure_ready();
    if (state) lean_dec_ref(state);
    state = dwmac_lean_state_init(lean_mk_string(scratchpads));
}

void dwmac_state_step(const char *op) {
    ensure_ready();
    if (!state) dwmac_state_init("");
    state = dwmac_lean_state_step(state, lean_mk_string(op)); // Both arguments are consumed
}

static char *take_string(lean_object *str) {
    char *copy = strdup(lean_string_cstr(str));
    lean_dec_ref(str);
    return copy;
}

char *dwmac_state_snapshot(void) {
    ensure_ready();
    if (!state) dwmac_state_init("");
    lean_inc_ref(state); // Consumed
    return take_string(dwmac_lean_state_snapshot(state));
}

char *dwmac_state_layout(bool show_all) {
    ensure_ready();
    if (!state) dwmac_state_init("");
    lean_inc_ref(state); // Consumed
    return take_string(dwmac_lean_state_layout(state, show_all));
}
