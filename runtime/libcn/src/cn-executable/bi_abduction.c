#include <assert.h>
#include <inttypes.h>
#include <setjmp.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-executable/bi_abduction.h>

/* Allocator: use stdlib malloc (not bump allocator) because bi-abductive state
   must persist beyond cn_bump_free_after which reclaims per-function bump memory. */
static void *abd_malloc(size_t sz) { return malloc(sz); }
static void *abd_calloc(size_t n, size_t sz) { return calloc(n, sz); }
static void abd_free_noop(void *p) { (void)p; /* leaked intentionally; short-lived process */ }
static allocator abd_alloc = (allocator){
    .malloc = &abd_malloc, .calloc = &abd_calloc, .free = &abd_free_noop};

/* Global state */
static bool abd_enabled = false;
static cn_abd_frame *current_frame = NULL;
static FILE *heap_output = NULL;

/* Linked list of collected data points (lightweight, kept in memory) */
typedef struct abd_data_point {
  const char *function_name;
  hash_table *pre_missing;
  hash_table *pre_vars;
  int pre_var_count;
  hash_table *body_missing;     /* body auto-grants = precondition needs */
  hash_table *post_remaining;   /* leak check remainder = postcondition */
  struct abd_data_point *next;
} abd_data_point;

static abd_data_point *data_points_head = NULL;
static abd_data_point *data_points_tail = NULL;

static hash_table *abd_new_table(void) {
  return ht_create(&abd_alloc);
}

static void abd_reset_missing_state(cn_abd_frame *frame) {
  frame->missing = abd_new_table();
}

static cn_abd_frame *abd_new_frame(const char *func_name, cn_abd_frame *parent) {
  cn_abd_frame *frame = malloc(sizeof(cn_abd_frame));
  frame->function_name = func_name;
  abd_reset_missing_state(frame);
  frame->pre_missing = NULL;
  frame->pre_vars = abd_new_table();
  frame->pre_var_count = 0;
  frame->post_remaining = NULL;
  frame->prev = parent;
  return frame;
}

static void abd_record_addr_size(hash_table **table, uintptr_t addr, size_t size) {
  int64_t key = (int64_t)addr;
  if (*table == NULL)
    *table = abd_new_table();
  if (ht_get(*table, &key) != NULL)
    return;

  int64_t *heap_key = malloc(sizeof(int64_t));
  *heap_key = (int64_t)addr;
  int64_t *size_val = malloc(sizeof(int64_t));
  *size_val = (int64_t)size;
  ht_set(*table, heap_key, size_val);
}

static void abd_merge_missing(hash_table *dst, hash_table *src) {
  if (src == NULL)
    return;

  hash_table_iterator it = ht_iterator(src);
  while (ht_next(&it)) {
    if (ht_get(dst, it.key) == NULL)
      ht_set(dst, it.key, it.value);
  }
}

static void abd_append_data_point(cn_abd_frame *frame) {
  abd_data_point *dp = malloc(sizeof(abd_data_point));
  dp->function_name = frame->function_name;
  dp->pre_missing = frame->pre_missing;
  dp->pre_vars = frame->pre_vars;
  dp->pre_var_count = frame->pre_var_count;
  dp->body_missing = frame->missing;        /* body auto-grants = precondition */
  dp->post_remaining = frame->post_remaining; /* leak check remainder = postcondition */
  dp->next = NULL;

  if (data_points_tail != NULL)
    data_points_tail->next = dp;
  else
    data_points_head = dp;
  data_points_tail = dp;
}

void cn_abd_init(FILE *heap_out) {
  abd_enabled = true;
  current_frame = NULL;
  heap_output = heap_out;
  data_points_head = NULL;
  data_points_tail = NULL;
}

void cn_abd_destroy(void) {
  abd_enabled = false;
  current_frame = NULL;
  heap_output = NULL;
  data_points_head = NULL;
  data_points_tail = NULL;
}

bool cn_abd_is_enabled(void) {
  return abd_enabled;
}

void cn_abd_push_frame(const char *func_name) {
  if (!abd_enabled)
    return;

  current_frame = abd_new_frame(func_name, current_frame);
}

void cn_abd_pop_frame(void) {
  if (!abd_enabled || current_frame == NULL)
    return;

  cn_abd_frame *frame = current_frame;

  abd_append_data_point(frame);

  /* Merge callee's missing addresses into parent's M (RETURN rule: M'' = M ∪ M')
     Only body_missing (precondition needs) propagates to caller.
     post_remaining (postcondition) is recorded in the data point but NOT merged. */
  cn_abd_frame *parent = frame->prev;
  if (parent != NULL) {
    /* Merge pre_missing into parent */
    abd_merge_missing(parent->missing, frame->pre_missing);
    /* Merge body_missing into parent (M'' = M ∪ M') */
    abd_merge_missing(parent->missing, frame->missing);
    /* NOTE: post_remaining is NOT merged — it's f's postcondition, not caller's obligation */
  }

  current_frame = parent;
}

/* Signal handler state for safe memory reads (file scope) */
static sigjmp_buf jmp_env;
static volatile sig_atomic_t in_safe_read = 0;

static void safe_read_handler(int sig) {
  (void)sig;
  if (in_safe_read)
    siglongjmp(jmp_env, 1);
}

/* Dump heap neighborhood around an address to heap_output (JSONL).
   phase: "pre"  = H_entry snapshot (at cn_abd_mark_post, before body)
          "post" = H_exit snapshot (at cn_abd_record_post_remaining, after body) */
static void dump_heap_neighborhood(
    const char *func_name, const char *phase, uintptr_t addr) {
  if (heap_output == NULL)
    return;

  /* Radius: 64 bytes in each direction, 8-byte aligned */
  const size_t radius = 64;
  uintptr_t aligned = addr & ~(uintptr_t)7;
  uintptr_t base = (aligned >= radius) ? aligned - radius : 0;
  uintptr_t end = (addr & ~(uintptr_t)7) + radius;

  struct sigaction sa_new, sa_old_segv, sa_old_bus;
  sa_new.sa_handler = safe_read_handler;
  sa_new.sa_flags = 0;
  sigemptyset(&sa_new.sa_mask);
  sigaction(SIGSEGV, &sa_new, &sa_old_segv);
  sigaction(SIGBUS, &sa_new, &sa_old_bus);

  fprintf(heap_output,
      "{\"function\":\"%s\",\"phase\":\"%s\",\"addr\":\"0x%" PRIxPTR "\",\"words\":{",
      func_name, phase, addr);

  bool first = true;
  for (uintptr_t a = base; a <= end; a += 8) {
    in_safe_read = 1;
    if (sigsetjmp(jmp_env, 1) == 0) {
      uint64_t val = *(volatile uint64_t *)a;
      in_safe_read = 0;
      if (!first)
        fprintf(heap_output, ",");
      fprintf(heap_output, "\"0x%" PRIxPTR "\":\"0x%016" PRIx64 "\"", a, val);
      first = false;
    } else {
      in_safe_read = 0;
      /* SIGSEGV/SIGBUS: skip this address */
    }
  }

  fprintf(heap_output, "}}\n");
  fflush(heap_output);

  /* Restore original signal handlers */
  sigaction(SIGSEGV, &sa_old_segv, NULL);
  sigaction(SIGBUS, &sa_old_bus, NULL);
}

void cn_abd_record_missing(uintptr_t addr, size_t size) {
  if (!abd_enabled || current_frame == NULL)
    return;

  if (current_frame->missing != NULL) {
    int64_t key = (int64_t)addr;
    if (ht_get(current_frame->missing, &key) != NULL)
      return;
  }
  abd_record_addr_size(&current_frame->missing, addr, size);
}

void cn_abd_record_post_remaining(uintptr_t addr, size_t size) {
  if (!abd_enabled || current_frame == NULL)
    return;

  abd_record_addr_size(&current_frame->post_remaining, addr, size);

  /* Dump H_exit heap neighborhood around the leaked address */
  dump_heap_neighborhood(current_frame->function_name, "post", addr);
}

void cn_abd_record_var(
    const char *name, uintptr_t value, size_t size, const char *type_name) {
  if (!abd_enabled || current_frame == NULL)
    return;

  cn_abd_var_entry *entry = malloc(sizeof(cn_abd_var_entry));
  entry->name = name;
  entry->value = value;
  entry->size = size;
  entry->type_name = type_name;

  int64_t *heap_idx = malloc(sizeof(int64_t));
  *heap_idx = current_frame->pre_var_count;
  ht_set(current_frame->pre_vars, heap_idx, entry);
  current_frame->pre_var_count++;
}

void cn_abd_mark_post(void) {
  if (!abd_enabled || current_frame == NULL)
    return;

  /* Snapshot pre-state */
  current_frame->pre_missing = current_frame->missing;

  /* Start fresh for post-state */
  abd_reset_missing_state(current_frame);

  /* Dump H_entry: heap neighborhoods of all pointer-sized function arguments.
     This captures the heap structure visible at function entry (before the body
     executes), which is what pre-condition inference needs. */
  for (int i = 0; i < current_frame->pre_var_count; i++) {
    int64_t idx = i;
    cn_abd_var_entry *entry = ht_get(current_frame->pre_vars, &idx);
    if (entry != NULL && entry->size == 8 && entry->value != 0) {
      dump_heap_neighborhood(current_frame->function_name, "pre", (uintptr_t)entry->value);
    }
  }
}

/* JSON output helpers */

static void dump_vars_json(FILE *out, hash_table *vars, int count) {
  fprintf(out, "[");
  for (int i = 0; i < count; i++) {
    int64_t idx = i;
    cn_abd_var_entry *entry = ht_get(vars, &idx);
    if (entry == NULL)
      continue;
    if (i > 0)
      fprintf(out, ",");
    fprintf(out, "{\"name\":\"%s\",\"value\":\"0x%" PRIxPTR "\",\"size\":%zu,\"type\":\"%s\"}",
        entry->name, entry->value, entry->size, entry->type_name);
  }
  fprintf(out, "]");
}

static void dump_missing_json(FILE *out, hash_table *missing) {
  fprintf(out, "[");
  if (missing != NULL) {
    hash_table_iterator it = ht_iterator(missing);
    bool first = true;
    while (ht_next(&it)) {
      if (!first)
        fprintf(out, ",");
      int64_t *size_val = it.value;
      fprintf(out, "{\"addr\":\"0x%" PRIx64 "\",\"size\":%" PRId64 "}",
          *it.key, *size_val);
      first = false;
    }
  }
  fprintf(out, "]");
}

void cn_abd_dump_summary(FILE *out) {
  if (!abd_enabled || out == NULL)
    return;

  fprintf(out, "{\"version\":2,\"data_points\":[");

  abd_data_point *dp = data_points_head;
  bool first = true;
  while (dp != NULL) {
    if (!first)
      fprintf(out, ",");

    fprintf(out, "{\"function\":\"%s\",\"pre\":{\"vars\":", dp->function_name);
    dump_vars_json(out, dp->pre_vars, dp->pre_var_count);
    fprintf(out, ",\"missing\":");
    dump_missing_json(out, dp->pre_missing);
    fprintf(out, "},\"body\":{\"missing\":");
    dump_missing_json(out, dp->body_missing);
    fprintf(out, "},\"post\":{\"remaining\":");
    dump_missing_json(out, dp->post_remaining);
    fprintf(out, "}}");

    first = false;
    dp = dp->next;
  }

  fprintf(out, "]}\n");
  fflush(out);
}
