#ifndef CN_BI_ABDUCTION_H
#define CN_BI_ABDUCTION_H

/* Bi-abductive execution support (paper: "Concrete Bi-Abduction").

   The runtime keeps:
   - a global *event log* of abduction events (a, size, o, d): address range
     [a, a+size) was acquired at accessor depth d while owned at depth o
     (o = 0 encodes "environment" / no ownership history).  This is the lazy
     representation of Definition acq's interval write: activation i's
     anti-frame A_i is materialised at pop-frame time as
       { a | event (a,_,o,d) in i's span, o < depth_i <= d }.
   - a stack of activation frames (function name, activation id, ghost
     stack depth, span start in the event log, recorded variables, the
     ownership snapshot after the user precondition, and the leak set).
   - a data-point log, one entry per completed activation, dumped as JSON
     at exit. */

#ifdef __CN_INSTRUMENT
/* In instrumented code, system headers conflict with cerberus types.
   Use cerberus types directly and void* for FILE*. */
#include "cerb_types.h"

#ifdef __cplusplus
extern "C" {
#endif

void cn_abd_init(void *heap_out);
void cn_abd_destroy(void);
_Bool cn_abd_is_enabled(void);
void cn_abd_push_frame(const char *func_name);
void cn_abd_pop_frame(void);
void cn_abd_record_event(
    __cerbty_uintptr_t addr, __cerbty_size_t size, long owner_depth, long accessor_depth);
void cn_abd_record_post_remaining(__cerbty_uintptr_t addr, __cerbty_size_t size);
void cn_abd_record_var(
    const char *name, __cerbty_uintptr_t value, __cerbty_size_t size);
void cn_abd_record_post_var(
    const char *name, __cerbty_uintptr_t value, __cerbty_size_t size);
void cn_abd_mark_post(void);
void cn_abd_leak_check_and_release(long caller_depth);
void cn_abd_dump_summary(void *out);

#ifdef __cplusplus
}
#endif

#else /* !__CN_INSTRUMENT: normal compilation of runtime */

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

void cn_abd_init(FILE *heap_out);
void cn_abd_destroy(void);
bool cn_abd_is_enabled(void);
void cn_abd_push_frame(const char *func_name);
void cn_abd_pop_frame(void);
/* Record one abduction event.  [owner_depth] is the ghost-state reading at
   the access (may be UNMAPPED_VAL < 0; normalised to 0 = environment);
   [accessor_depth] is the depth the acquisition happened at (caller depth
   for precondition takes, current depth for body accesses and
   postcondition takes). */
void cn_abd_record_event(
    uintptr_t addr, size_t size, long owner_depth, long accessor_depth);
void cn_abd_record_post_remaining(uintptr_t addr, size_t size);
void cn_abd_record_var(const char *name, uintptr_t value, size_t size);
void cn_abd_record_post_var(const char *name, uintptr_t value, size_t size);
void cn_abd_mark_post(void);
/* Collect the current activation's leak set Lambda (ghost entries deeper
   than [caller_depth]), record it as post_remaining, and release those
   ranges to [caller_depth] (paper rule B-Ret: G_3 = ...[Lambda -> d-1]).
   Called from cn_postcondition_leak_check after ghost_stack_depth_decr. */
void cn_abd_leak_check_and_release(long caller_depth);
void cn_abd_dump_summary(FILE *out);

#ifdef __cplusplus
}
#endif

#endif /* __CN_INSTRUMENT */

#endif /* CN_BI_ABDUCTION_H */
