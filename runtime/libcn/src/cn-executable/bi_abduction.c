#include <assert.h>
#include <inttypes.h>
#include <setjmp.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cn-executable/bi_abduction.h>
#include <cn-executable/rmap.h>
#include <cn-executable/hash_table.h>
#include <cn-executable/fulminate_alloc.h>

/* Ownership map G, owned by utils.c. */
struct cn_error_message_info;
extern struct rmap *cn_ownership_global_ghost_state;
extern signed long get_cn_stack_depth(void);
void ownership_ghost_state_set(int64_t address,
    size_t size,
    int stack_depth_val,
    struct cn_error_message_info *error_msg_info);

/* Allocator: use stdlib malloc (not bump allocator) because bi-abductive state
   must persist beyond cn_bump_free_after which reclaims per-function bump memory. */
static void *abd_malloc(size_t sz) { return malloc(sz); }
static void *abd_calloc(size_t n, size_t sz) { return calloc(n, sz); }
static void abd_free_noop(void *p) { (void)p; /* leaked intentionally; short-lived process */ }
static allocator abd_alloc = (allocator){
    .malloc = &abd_malloc, .calloc = &abd_calloc, .free = &abd_free_noop};

/* ---------- Event log (lazy representation of the interval write) ---------- */

typedef struct abd_event {
  uint64_t addr;
  uint64_t size;
  long o; /* owner depth at the access; 0 = environment */
  long d; /* accessor depth (see header) */
} abd_event;

static abd_event *abd_events = NULL;
static size_t abd_events_n = 0;
static size_t abd_events_cap = 0;

static void abd_events_push(uint64_t addr, uint64_t size, long o, long d) {
  if (abd_events_n == abd_events_cap) {
    size_t cap = abd_events_cap ? abd_events_cap * 2 : 256;
    abd_event *grown = realloc(abd_events, cap * sizeof(abd_event));
    if (!grown)
      return; /* drop event on OOM; inference degrades, execution continues */
    abd_events = grown;
    abd_events_cap = cap;
  }
  abd_events[abd_events_n++] = (abd_event){.addr = addr, .size = size, .o = o, .d = d};
}

/* ---------- Frames (live activations) ---------- */

typedef struct cn_abd_frame {
  const char *function_name;
  int dp_idx;              /* activation id, in call order */
  signed long depth;       /* ghost stack depth of this activation */
  size_t start_event;      /* event-log index at push */
  hash_table *pre_vars;    /* idx -> cn_abd_var_entry* */
  int pre_var_count;
  hash_table *post_vars;   /* idx -> cn_abd_var_entry* */
  int post_var_count;
  hash_table *owned_pre;   /* addr -> size: G-entries at own depth at mark_post */
  hash_table *post_remaining; /* leak set Lambda: addr -> size */
  struct cn_abd_frame *prev;
} cn_abd_frame;

typedef struct cn_abd_var_entry {
  const char *name;
  uintptr_t value;
  size_t size;
} cn_abd_var_entry;

/* Global state */
static bool abd_enabled = false;
static cn_abd_frame *current_frame = NULL;
static FILE *heap_output = NULL;
static int abd_next_dp_idx = 0;

/* Data points: one per completed activation. */
typedef struct abd_data_point {
  int dp_idx;
  const char *function_name;
  hash_table *pre_vars;
  int pre_var_count;
  hash_table *post_vars;
  int post_var_count;
  hash_table *owned_pre;
  hash_table *body_missing;   /* materialised anti-frame A */
  hash_table *post_remaining; /* leak set Lambda = frame L */
  struct abd_data_point *next;
} abd_data_point;

static abd_data_point *data_points_head = NULL;
static abd_data_point *data_points_tail = NULL;

/* Dedup table for pre-phase heap dumps: key = (dp_idx << 48) | aligned addr. */
static hash_table *abd_dumped_pre = NULL;

static hash_table *abd_new_table(void) {
  return ht_create(&abd_alloc);
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

void cn_abd_init(FILE *heap_out) {
  abd_enabled = true;
  current_frame = NULL;
  heap_output = heap_out;
  data_points_head = NULL;
  data_points_tail = NULL;
  abd_next_dp_idx = 0;
  abd_events_n = 0;
  abd_dumped_pre = NULL;
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

  cn_abd_frame *frame = malloc(sizeof(cn_abd_frame));
  frame->function_name = func_name;
  frame->dp_idx = abd_next_dp_idx++;
  /* push runs before this activation's ghost_stack_depth_incr */
  frame->depth = get_cn_stack_depth() + 1;
  frame->start_event = abd_events_n;
  frame->pre_vars = abd_new_table();
  frame->pre_var_count = 0;
  frame->post_vars = abd_new_table();
  frame->post_var_count = 0;
  frame->owned_pre = NULL;
  frame->post_remaining = NULL;
  frame->prev = current_frame;
  current_frame = frame;
}

/* Materialise the anti-frame from the event log (paper, closed form /
   Definition acq): A_i = { a | event (a,_,o,d) in i's span, o < depth_i <= d }. */
static hash_table *abd_materialise_missing(cn_abd_frame *frame) {
  hash_table *missing = abd_new_table();
  for (size_t i = frame->start_event; i < abd_events_n; i++) {
    abd_event *e = &abd_events[i];
    if (e->o < frame->depth && frame->depth <= e->d)
      abd_record_addr_size(&missing, (uintptr_t)e->addr, (size_t)e->size);
  }
  return missing;
}

static void abd_append_data_point(cn_abd_frame *frame, hash_table *missing) {
  abd_data_point *dp = malloc(sizeof(abd_data_point));
  dp->dp_idx = frame->dp_idx;
  dp->function_name = frame->function_name;
  dp->pre_vars = frame->pre_vars;
  dp->pre_var_count = frame->pre_var_count;
  dp->post_vars = frame->post_vars;
  dp->post_var_count = frame->post_var_count;
  dp->owned_pre = frame->owned_pre;
  dp->body_missing = missing;
  dp->post_remaining = frame->post_remaining;
  dp->next = NULL;

  if (data_points_tail != NULL)
    data_points_tail->next = dp;
  else
    data_points_head = dp;
  data_points_tail = dp;
}

void cn_abd_pop_frame(void) {
  if (!abd_enabled || current_frame == NULL)
    return;

  cn_abd_frame *frame = current_frame;
  abd_append_data_point(frame, abd_materialise_missing(frame));
  /* No merge into the parent: ancestors materialise their own records from
     the shared event log (interval semantics), which is what makes the
     per-activation solutions least (paper, Theorem canonicity). */
  current_frame = frame->prev;
}

/* Signal handler state for safe memory reads (file scope) */
static sigjmp_buf jmp_env;
static volatile sig_atomic_t in_safe_read = 0;

static void safe_read_handler(int sig) {
  (void)sig;
  if (in_safe_read)
    siglongjmp(jmp_env, 1);
}

/* Dump heap neighborhood around an address to heap_output (JSONL), tagged
   with the owning activation's dp index.
   phase: "pre"  = entry-heap approximation (at mark_post / first miss)
          "post" = exit heap (at the leak check) */
static void dump_heap_neighborhood(const char *phase, int dp_idx, uintptr_t addr) {
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

  fprintf(heap_output, "{\"dp\":%d,\"phase\":\"%s\",\"words\":{", dp_idx, phase);

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

/* Dump the pre-phase neighborhood of [addr] for activation [dp_idx], once
   per (dp, 8-byte window).  Called on abduction events so that pointer
   chains reachable from missing cells are visible to the footprint harness
   even when they lie outside the argument neighborhoods. */
static void abd_dump_pre_once(int dp_idx, uintptr_t addr) {
  if (heap_output == NULL)
    return;
  if (abd_dumped_pre == NULL)
    abd_dumped_pre = abd_new_table();
  int64_t key = ((int64_t)dp_idx << 48) | (int64_t)(addr & ~(uintptr_t)7);
  if (ht_get(abd_dumped_pre, &key) != NULL)
    return;
  int64_t *heap_key = malloc(sizeof(int64_t));
  *heap_key = key;
  static int64_t present = 1;
  ht_set(abd_dumped_pre, heap_key, &present);
  dump_heap_neighborhood("pre", dp_idx, addr);
}

void cn_abd_record_event(
    uintptr_t addr, size_t size, long owner_depth, long accessor_depth) {
  if (!abd_enabled || current_frame == NULL)
    return;

  long o = owner_depth < 0 ? 0 : owner_depth; /* UNMAPPED_VAL -> environment */
  abd_events_push((uint64_t)addr, (uint64_t)size, o, accessor_depth);
  /* The event lands in the anti-frame of every live activation in the
     interval (o, d] — so each of those activations' pre-heap snapshots
     must contain this neighborhood too (a recursive callee's misses are
     part of the root's footprint). */
  for (cn_abd_frame *f = current_frame; f != NULL; f = f->prev) {
    if (o < f->depth && f->depth <= accessor_depth)
      abd_dump_pre_once(f->dp_idx, addr);
  }
}

void cn_abd_record_post_remaining(uintptr_t addr, size_t size) {
  if (!abd_enabled || current_frame == NULL)
    return;

  abd_record_addr_size(&current_frame->post_remaining, addr, size);

  /* Dump exit-heap neighborhood around the leaked range */
  dump_heap_neighborhood("post", current_frame->dp_idx, addr);
  if (size > 64)
    dump_heap_neighborhood("post", current_frame->dp_idx, addr + size - 1);
}

/* ---------- Leak check + release (B-Ret) ---------- */

typedef struct abd_range {
  uint64_t k0;
  uint64_t k1;
} abd_range;

typedef struct abd_leak_ctx {
  long caller_depth;
  abd_range *ranges;
  size_t n;
  size_t cap;
} abd_leak_ctx;

static void abd_leak_collect_cb(rmap_key_t k0, rmap_key_t k1, rmap_value_t v, void *ctx_) {
  abd_leak_ctx *ctx = (abd_leak_ctx *)ctx_;
  if ((long)v <= ctx->caller_depth)
    return;
  if (ctx->n == ctx->cap) {
    size_t cap = ctx->cap ? ctx->cap * 2 : 64;
    abd_range *grown = realloc(ctx->ranges, cap * sizeof(abd_range));
    if (!grown)
      return;
    ctx->ranges = grown;
    ctx->cap = cap;
  }
  ctx->ranges[ctx->n++] = (abd_range){.k0 = k0, .k1 = k1};
}

void cn_abd_leak_check_and_release(long caller_depth) {
  if (!abd_enabled || current_frame == NULL)
    return;

  /* Collect first (mutating the rmap during foreach is not safe), ... */
  abd_leak_ctx ctx = {.caller_depth = caller_depth, .ranges = NULL, .n = 0, .cap = 0};
  rmap_foreach(cn_ownership_global_ghost_state, abd_leak_collect_cb, &ctx);
  /* ... then record Lambda and release it to the caller. */
  for (size_t i = 0; i < ctx.n; i++) {
    uint64_t k0 = ctx.ranges[i].k0;
    uint64_t k1 = ctx.ranges[i].k1;
    cn_abd_record_post_remaining((uintptr_t)k0, (size_t)(k1 - k0 + 1));
    ownership_ghost_state_set((int64_t)k0, (size_t)(k1 - k0 + 1), (int)caller_depth, NULL);
  }
  free(ctx.ranges);
}

/* ---------- Variables ---------- */

static void abd_record_var_in(
    hash_table *vars, int *count, const char *name, uintptr_t value, size_t size) {
  cn_abd_var_entry *entry = malloc(sizeof(cn_abd_var_entry));
  entry->name = name;
  entry->value = value;
  entry->size = size;

  int64_t *heap_idx = malloc(sizeof(int64_t));
  *heap_idx = *count;
  ht_set(vars, heap_idx, entry);
  (*count)++;
}

void cn_abd_record_var(const char *name, uintptr_t value, size_t size) {
  if (!abd_enabled || current_frame == NULL)
    return;
  abd_record_var_in(
      current_frame->pre_vars, &current_frame->pre_var_count, name, value, size);
}

void cn_abd_record_post_var(const char *name, uintptr_t value, size_t size) {
  if (!abd_enabled || current_frame == NULL)
    return;
  abd_record_var_in(
      current_frame->post_vars, &current_frame->post_var_count, name, value, size);
}

/* ---------- mark_post: end of user precondition evaluation ---------- */

static void abd_owned_pre_cb(rmap_key_t k0, rmap_key_t k1, rmap_value_t v, void *ctx_) {
  cn_abd_frame *frame = (cn_abd_frame *)ctx_;
  if ((long)v != frame->depth)
    return;
  abd_record_addr_size(&frame->owned_pre, (uintptr_t)k0, (size_t)(k1 - k0 + 1));
}

void cn_abd_mark_post(void) {
  if (!abd_enabled || current_frame == NULL)
    return;

  /* Snapshot the ownership this activation holds after evaluating the user
     precondition (user footprint + parameter/local cells).  This is what
     the sandwich upper bound B_j subtracts from the heap. */
  rmap_foreach(cn_ownership_global_ghost_state, abd_owned_pre_cb, current_frame);

  /* Dump entry-heap neighborhoods of all pointer-sized function arguments. */
  for (int i = 0; i < current_frame->pre_var_count; i++) {
    int64_t idx = i;
    cn_abd_var_entry *entry = ht_get(current_frame->pre_vars, &idx);
    if (entry != NULL && entry->size == 8 && entry->value != 0) {
      dump_heap_neighborhood("pre", current_frame->dp_idx, (uintptr_t)entry->value);
    }
  }
}

/* ---------- JSON output ---------- */

static void dump_vars_json(FILE *out, hash_table *vars, int count) {
  fprintf(out, "[");
  for (int i = 0; i < count; i++) {
    int64_t idx = i;
    cn_abd_var_entry *entry = ht_get(vars, &idx);
    if (entry == NULL)
      continue;
    if (i > 0)
      fprintf(out, ",");
    fprintf(out, "{\"name\":\"%s\",\"value\":\"0x%" PRIxPTR "\",\"size\":%zu}",
        entry->name, entry->value, entry->size);
  }
  fprintf(out, "]");
}

static void dump_ranges_json(FILE *out, hash_table *ranges) {
  fprintf(out, "[");
  if (ranges != NULL) {
    hash_table_iterator it = ht_iterator(ranges);
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

  fprintf(out, "{\"data_points\":[");

  abd_data_point *dp = data_points_head;
  bool first = true;
  while (dp != NULL) {
    if (!first)
      fprintf(out, ",");

    fprintf(out, "{\"dp\":%d,\"function\":\"%s\",\"pre\":{\"vars\":",
        dp->dp_idx, dp->function_name);
    dump_vars_json(out, dp->pre_vars, dp->pre_var_count);
    fprintf(out, ",\"owned\":");
    dump_ranges_json(out, dp->owned_pre);
    fprintf(out, "},\"body\":{\"missing\":");
    dump_ranges_json(out, dp->body_missing);
    fprintf(out, "},\"post\":{\"vars\":");
    dump_vars_json(out, dp->post_vars, dp->post_var_count);
    fprintf(out, ",\"remaining\":");
    dump_ranges_json(out, dp->post_remaining);
    fprintf(out, "}}");

    first = false;
    dp = dp->next;
  }

  fprintf(out, "]}\n");
  fflush(out);
}
