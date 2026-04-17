# Bi-Abduction Inference Framework

**Summarized by Claude**

## What this is

OCaml inference framework for bi-abductive specification inference in CN/Fulminate.
Takes runtime data (missing ownership addresses + heap dumps) from Phase 1 (C runtime
in `runtime/libcn/src/cn-executable/bi_abduction.c`) and suggests CN specifications.

Pipeline: **parse → group by function → build memory graph → enumerate qualifiers → compute footprints → disjoint set cover → pretty-print suggestions**.

Entry points:
- Push-button CLI: `bin/bi_abd_infer.ml` (`cn bi-abd file.c` — instruments, compiles, runs, infers)
- Library: `Bi_abduction.Infer.infer_from_files` (just the inference, given pre-collected data)

## Module map

| Module | Role |
|---|---|
| `data_point.ml` | Parse `cn_abd_summary.json` (lightweight: missing addrs + var bindings) and `cn_abd_heap.jsonl` (raw memory words). Types: `var_binding`, `missing_entry`, `data_point`, `heap_dump`. |
| `memory_graph.ml` | Build a directed graph from heap data. Nodes = addresses, edges = struct field offsets or pointer dereferences. BFS from anchors (function args) through pointer-sized fields to discover linked structures. |
| `qualifier.ml` | Thin wrapper around `Request.t`. A qualifier = a candidate `take _ = ...` binding. Uses CN's existing `Request.t`, `IndexTerms.t`, `Sctypes.t` — no parallel type hierarchy. |
| `enumerator.ml` | Generate candidate qualifiers from the current function scope: exact `Owned<T>(arg)` for pointer args, plus recursive predicate candidates rooted at arguments whose pointee type matches the predicate's root cell and whose concrete address connects to missing bytes. Pointer iargs may reuse in-scope pointers or `NULL`. |
| `footprint.ml` | Compute which byte-addresses a qualifier covers. `Owned` → `[base, base+size)`. Predicates → `reachable_struct_bytes` from the memory graph. |
| `cover.ml` | Greedy disjoint cover: pick a small qualifier subset covering as many missing bytes as possible without overlapping footprints. |
| `infer.ml` | Top-level orchestrator. Picks best representative data point, wires everything together, formats output. |

## Key design decisions

- **Reuse CN types** — qualifiers are `Request.t`, terms are `IndexTerms.t`. No separate AST.
- **Byte-level granularity** — the runtime records missing addresses per-byte. Footprints must also be per-byte. `owned_footprint` strides by 1, not 8.
- **BFS graph expansion** — the memory graph follows pointer chains through intermediate nodes (e.g. n2 in `p→n1→n2→n3`) that aren't themselves missing. Without this, linked structure inference doesn't work.
- **Representative execution baseline** — inference currently works from one representative data point, chosen as the call with the richest missing set. Multi-run generalisation is intentionally left to `TODO.md`.
- **Struct byte expansion** — `reachable_struct_bytes` expands struct base addresses to full `[base, base+struct_total_size)` byte ranges. This bridges the gap between graph-level nodes and byte-level missing sets.
- **Root-type filtering** — a recursive predicate is only considered at an argument whose pointee type matches the predicate's first owned root cell. This avoids suggestions like `IntList(b)` when `b` is really a wrapper struct pointer.

## CN/Cerberus conventions to know

- CN's `List`, `Option`, `String` modules shadow `Stdlib`. Always use `module StdList = Stdlib.List` etc.
- `BaseTypes.t = unit t_gen` so `Loc` carries `unit`: `Loc ()`. Surface `BaseTypes.Surface.t = Sctypes.t option t_gen` has `Loc (Some ct)`. Don't mix them.
- `Pp.debug level (lazy doc)` prints to stderr when `-p level` is ≥ the given level. Use levels 2–5 for inference debug output.
- `Cerb_colour.without_colour` has type `(unit -> 'a) -> unit -> 'a` (curried). You must apply the trailing `()`.
- `<>` is shadowed by CN's integer comparison. Use `String.length s > 0` instead of `s <> ""` for string checks.
- `Request.t` does not support polymorphic `=`. Pattern match instead of `x = []`.

## Runtime data format

**`cn_abd_summary.json`** — written once at exit:
```json
{"data_points":[{
  "function":"f",
  "pre":{"vars":[{"name":"p","value":"0x...","size":8}]},
  "body":{"missing":[{"addr":"0x...","size":1}]},
  "post":{"remaining":[{"addr":"0x...","size":1}]}
}]}
```
Note: the callee's pre_missing (addresses its declared precondition could not
claim) is not recorded in its own data point. Per IDEA.md's CALL/return rules,
pre_missing is propagated to the caller's missing set at pop_frame time, so
the caller's body_missing already reflects it transitively.

**`cn_abd_heap.jsonl`** — one JSON object per line, written incrementally:
```json
{"phase":"pre","words":{"0x...":"0x...",...}}
```
`phase` is `"pre"` (H_entry snapshot) or `"post"` (H_exit snapshot). Words are 8-byte-aligned address → 8-byte hex value. Used by `heap_lookup` for pointer chasing.

## Debug output (`-p N`)

The inference modules use `Pp.debug` at these levels:
- **2**: High-level pipeline stages (parsing, per-function inference start/end)
- **3**: Data point details (vars, missing counts, representative selection)
- **4**: Graph construction (nodes, edges, BFS expansion), enumeration results
- **5**: Footprint computation per qualifier, cover algorithm steps

## Known limitations / TODOs

See **[TODO.md](TODO.md)** for detailed analysis. The most critical:

- **Only postconditions are inferred** — `body_missing` captures all body ownership failures, which conflates precondition and postcondition needs
- **Multiple executions are not generalised yet** — the baseline uses one representative run and ignores the others
- **No partial spec awareness** — suggested qualifiers may overlap with existing takes
- **No iterated resources, free/malloc, loop invariants, qualifier chains, return value**

## Testing

```bash
# Curated bi-abduction regression suite
tests/run-bi-abd.sh

# Simple struct (Owned inference)
cn bi-abd tests/bi-abd/simple_struct_nospec.c

# Linked list (recursive predicate inference)
cn bi-abd tests/bi-abd/list_example_nospec.c

# Binary tree (recursive predicate inference)
cn bi-abd tests/bi-abd/tree_example_nospec.c
```

In development, set `CN_RUNTIME_PREFIX` to a directory containing `include/` and `libcn_exec.a`.

## Related files outside this directory

- `bin/bi_abd_infer.ml` — CLI subcommand (`cn bi-abd`)
- `runtime/libcn/src/cn-executable/bi_abduction.c` — C runtime state (missing set, heap dumps)
- `runtime/libcn/include/cn-executable/bi_abduction.h` — C runtime header
- `lib/fulminate/internal.ml` — codegen for `cn_abd_record_var`, frame push/pop, mark_post
- `runtime/libcn/src/cn-executable/utils.c` — ownership check interception (`c_ownership_check`)
