#ifndef CN_BI_ABDUCTION_H
#define CN_BI_ABDUCTION_H

#include "hash_table.h"
#include "fulminate_alloc.h"

#ifdef __CN_INSTRUMENT
/* In instrumented code, system headers conflict with cerberus types.
   Use cerberus types directly and void* for FILE*. */
#include "cerb_types.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct cn_abd_var_entry {
  const char *name;
  __cerbty_uintptr_t value;
  __cerbty_size_t size;
  const char *type_name;
} cn_abd_var_entry;

typedef struct cn_abd_frame {
  const char *function_name;
  hash_table *missing;
  hash_table *vars;
  int var_count;
  hash_table *pre_missing;
  hash_table *pre_vars;
  int pre_var_count;
  struct cn_abd_frame *prev;
} cn_abd_frame;

void cn_abd_init(void *heap_out);
void cn_abd_destroy(void);
_Bool cn_abd_is_enabled(void);
void cn_abd_push_frame(const char *func_name);
void cn_abd_pop_frame(void);
void cn_abd_record_missing(__cerbty_uintptr_t addr, __cerbty_size_t size);
void cn_abd_record_var(
    const char *name, __cerbty_uintptr_t value, __cerbty_size_t size, const char *type_name);
void cn_abd_mark_post(void);
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

typedef struct cn_abd_var_entry {
  const char *name;
  uintptr_t value;
  size_t size;
  const char *type_name;
} cn_abd_var_entry;

typedef struct cn_abd_frame {
  const char *function_name;
  hash_table *missing;        /* M: address (int64_t) -> size (int64_t*) */
  hash_table *vars;           /* V: sequential index (int64_t) -> cn_abd_var_entry* */
  int var_count;
  /* Snapshot of pre-state (set by cn_abd_mark_post) */
  hash_table *pre_missing;
  hash_table *pre_vars;
  int pre_var_count;
  struct cn_abd_frame *prev;  /* caller's frame */
} cn_abd_frame;

void cn_abd_init(FILE *heap_out);
void cn_abd_destroy(void);
bool cn_abd_is_enabled(void);
void cn_abd_push_frame(const char *func_name);
void cn_abd_pop_frame(void);
void cn_abd_record_missing(uintptr_t addr, size_t size);
void cn_abd_record_var(
    const char *name, uintptr_t value, size_t size, const char *type_name);
void cn_abd_mark_post(void);
void cn_abd_dump_summary(FILE *out);

#ifdef __cplusplus
}
#endif

#endif /* __CN_INSTRUMENT */

#endif /* CN_BI_ABDUCTION_H */
