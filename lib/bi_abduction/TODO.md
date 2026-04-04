# Bi-Abduction: Limitations, TODOs, and Future Plans

## Critical: Pre-condition inference is broken

**Severity: the system only infers postconditions.**

The codegen order is: `abd_push → record_args → precondition_eval → entry → abd_mark_post → [body] → exit → postcondition_eval → abd_pop`.

With `requires true`, the precondition eval does nothing, so `pre_missing = {}`. After `mark_post`, all body ownership failures go into `post_missing`. The postcondition leak check also adds to `post_missing`. So `post_missing` conflates two distinct things:

1. Addresses the body **needed at entry** (should be precondition)
2. Addresses **still owned at exit** (should be postcondition)

For functions that borrow and return everything (e.g., `list_length`), pre = post so conflation is harmless. But for functions that consume or produce ownership (e.g., `free`, constructors), this distinction matters.

**Possible fixes:**
- (a) **Heuristic**: For `requires true`, assume pre = post (body needs = leaked). Works for read-only traversals but not for mutation.
- (b) **Two-pass**: First run to discover body's needs (→ pre), second run with those addresses pre-granted to discover what leaks (→ post).
- (c) **Semantic split**: Track *why* each address is missing — was it a body access (pre need) or a leak check (post residue)? This requires tagging in `c_ownership_check` vs `cn_postcondition_leak_check`.

Approach (c) is cleanest. In `bi_abduction.c`, add a `source` tag to each missing entry: `BODY_ACCESS` vs `LEAK_CHECK`. Then in the OCaml side, split `post_missing` into `pre_candidates` (body accesses) and `post_candidates` (leaks).


## Critical: Multiple executions are not generalised

**`baseline_multi_call_list.c` produces 0 selected, 32 uncovered.**

Two independent issues:

### Issue 1: `must_cover_set` unions addresses across calls

`post_must_cover_set` computes `⋃ᵢ missing(dpᵢ)`. When different calls have different stack addresses (e.g., call 1 at `0x100`, call 2 at `0x200`), the union mixes addresses from incompatible concrete heaps. The representative data point's candidates can only cover its own addresses, leaving the other call's addresses uncovered.

**Fix**: Compute must-cover *per data point*, run cover per data point, then take the intersection (or vote) of selected qualifiers across data points. A qualifier is "confirmed" if it's selected in a majority of non-trivial data points.

### Issue 2: `exact_cover` only tracks complete covers

The exact cover algorithm (used for ≤20 candidates) updates `best` only when `Int64Set.is_empty remaining` — i.e., only when ALL must-cover addresses are explained. If no complete cover exists (common with mixed-address must-cover sets), it returns `{ selected = []; uncovered = must_cover }`.

**Fix**: Track best partial cover too. When a subset covers more must-cover addresses than `best`, update `best` even if some addresses remain uncovered. The greedy algorithm already handles this correctly.

### Issue 3: Single representative for graph construction

The memory graph is built from one data point. Different calls exercise different structure shapes (e.g., length-1 list vs length-3 list). Using only one misses patterns visible in others.

**Fix (longer term)**: Build graphs from multiple data points and merge/intersect them. Or abstract the graph into a symbolic shape (e.g., "chain of struct node of unknown length").


## Partial specifications

The missing set *implicitly* respects existing specs: if the user has `take X = Owned<struct foo>(p)`, Fulminate grants ownership for those bytes, so they don't appear as missing. This is correct for exclusion.

However, the *suggested* qualifiers may **overlap** with existing takes. For example, if the user has `take X = Owned<struct node>(p)` and the function traverses a list from `p`, the inference might suggest `IntList(p)` which *also* owns `struct node` at `p`.

**Fix**: After cover selection, subtract the footprint of existing spec takes from each suggested qualifier's footprint. If a qualifier's remaining footprint is empty, drop it. If it partially overlaps, warn or suggest only the non-overlapping part. Requires parsing the existing spec's resource bindings and computing their footprints — this could reuse Fulminate's existing ownership tracking.


## Iterated separating conjunctions (`each`)

`Request.Q` (quantified resources, i.e., `each (u64 i; ...) { ... }`) is not handled anywhere:
- Enumerator doesn't generate `each` candidates
- Footprint returns `None` for `Request.Q`
- Cover ignores them

This is needed for arrays: `each (u64 i; 0u64 <= i && i < n) { Owned<int>(array_shift<int>(p, i)) }`.

**Approach**: Detect array-like access patterns in the missing set (contiguous addresses at regular stride from a pointer argument). Generate `each` candidates with stride matching the element type size.


## `free` and `malloc` (dynamic allocation)

Not tracked. Issues:
- `malloc` returns addresses not visible in the pre-state. The inference can't attribute ownership of newly-allocated memory to a pre-condition qualifier.
- `free` releases ownership. Freed addresses should NOT appear in post-conditions but currently they'd show up as "not leaked" (silently correct) or "leaked" (wrong).
- Heap fragmentation means the memory graph can't follow pointer chains through malloc'd nodes unless those addresses happen to be in the heap dump radius.

**Fix**: Intercept `malloc`/`free` in bi-abductive mode. Record allocations and deallocations separately. Malloc'd addresses should be excluded from pre-condition inference. Free'd addresses should be excluded from post-condition suggestions.


## Loop invariants

Not supported. The runtime doesn't record per-iteration data. The codegen uses `skip_label_inlining:true` to avoid errors but doesn't enable invariant inference.

Loop invariants are structurally different from pre/post specs — they must hold at every iteration entry, and typically involve the loop variable and some resource that's peeled off or accumulated.

**Approach**: Record missing addresses at each loop iteration boundary (would need a `cn_abd_loop_iter()` hook). Generalise across iterations to find the invariant pattern. This is hard: the invariant must relate to the loop counter symbolically, not just concrete addresses.


## Qualifier chains

The `Qualifier.chain_step` and `Qualifier.chain` types exist but are never used. Currently each qualifier is independent — flat, not chained.

Example of what's missing: `take X = Owned<struct node>(p); take Y = IntList(X.next);`. The second qualifier depends on the value bound by the first. The enumerator only generates top-level qualifiers from function arguments, not from intermediate bindings.

**Approach**: After initial coverage, check if uncovered addresses are reachable via struct fields of already-selected qualifiers. Generate follow-up qualifiers rooted at those fields. This is essentially the "unfolding" step from IDEA.md section 4.5.


## Argument type inference

`v.size = 8 → Loc ()` is a crude heuristic. An 8-byte integer would be misclassified as a pointer. This could generate spurious `Owned`/predicate candidates for non-pointer arguments.

**Fix**: Use the actual C type from the function signature. The type information is available in `sigm.declarations` — thread it through to `cn_abd_record_var` and include it in the JSON output.


## Return value

The post-condition doesn't reference `return`. We don't capture the function's return value or suggest specs like `ensures take R = IntList(return);`.

**Fix**: Record the return value in `cn_abd_record_var` at the end of the function. Make it available as an additional variable for post-condition qualifier generation. The Fulminate codegen already knows the return variable.


## Global variables

Only function arguments are used as anchors. Global variables that the function accesses are invisible to the inference.

**Fix**: Record global variable addresses in `cn_abd_record_var`. Add them as additional anchors in the memory graph and as base terms in the enumerator.


## `traversal_fields` stub

`enumerator.ml:81-96` has a TODO to walk `LogicalArgumentTypes` (the predicate clause bodies) and extract which struct fields a recursive predicate traverses. Currently predicates are matched purely via memory graph connectivity, which works but is imprecise — it can't distinguish between a `List` predicate (follows `next`) and a hypothetical `ListBackward` predicate (follows `prev`) if both are defined.

**Fix**: Pattern-match the predicate definition's `ResourceTypes.request` terms to find `MemberShift` / field access patterns. Build a "traversal signature" per predicate. Compare against the graph's actual pointer chain structure to pick the right predicate.


## Negative examples

The cover algorithm only uses positive data (missing addresses). Addresses that are NOT missing could eliminate spurious candidates that cover too much.

**Example**: If address `X` is NOT missing but candidate `Q` covers it, then `Q` is over-approximate and might be wrong (it would claim ownership of memory that isn't actually needed).

**Fix**: For each candidate, check that its footprint doesn't cover addresses the function demonstrably does NOT need. Define "not needed" as addresses in the heap dump neighborhood that are not in the missing set. This requires care — absence from the missing set could mean "already owned" or "truly not needed".


## Ranking / candidate preference

Some problematic examples are better understood as **ranking** issues rather than
baseline inference bugs:

- `extra_nonrecursive_predicate_ignored.c`: preferring `PairCell(p)` over the
  lower-level `RW<struct pair>(p)` requires a policy for ranking equivalent
  candidates, not just better enumeration.
- `extra_wrong_struct_type.c`: when several candidates cover the same concrete
  bytes, preferring the one whose type best matches the source-level signature
  is also a ranking / disambiguation problem.

These are still important, but they should be tracked separately from the core
baseline issues like pre/post splitting, multi-run generalisation, or invalid
out-of-scope predicate arguments.


## Interprocedural inference

Each function is inferred independently. The plan (IDEA.md) describes propagating specs across call boundaries: if `f` calls `g`, and we've inferred `g`'s spec, use that to refine `f`'s inference (the callee's pre becomes the caller's obligation, the callee's post becomes the caller's available resources).

This is a significant extension. The current frame push/pop/merge mechanism in the runtime provides the raw data (callee missing merges into parent), but the OCaml side doesn't exploit cross-function relationships.


## PBT integration

Property-based testing (Bennet) could generate diverse inputs, producing many data points with varied structure shapes. This would improve generalisation: seeing lists of length 1, 3, 5, 10 makes it clear that `IntList(p)` is the right predicate, not `Owned<struct node>(p)` for some fixed number of nodes.

**Current blocker**: The inference doesn't generalise across data points (see "Multiple executions" above). Fix that first, then PBT integration becomes straightforward — just use Bennet's input generation to produce more `cn_abd_summary.json` data points.


## Concrete vs. symbolic

The entire pipeline is concrete: suggestions like `IntList(p)` are made because concrete addresses match, not because of symbolic reasoning. There is no guarantee that the suggested spec is valid for all inputs — only that it explains the observed execution(s).

This is by design (see IDEA.md §1 on concrete vs. symbolic trade-offs), but means:
- Suggestions should be presented as *candidates*, not proven specs
- Verification (running `cn verify` with the suggested spec) is the ground truth
- Multiple diverse executions increase confidence


## Output integration

Suggestions are printed as comments, not integrated back into the source file. Ideally, `cn bi-abd` should be able to rewrite the source file with the suggested specs inserted, or at least produce a diff/patch.


## Summary: priority ordering

1. **Pre/post splitting** — without this, half the spec is always empty
2. **Per-data-point cover + exact_cover partial fix** — without this, multiple calls break inference
3. **Qualifier chains** — needed for most real predicates (the predicate unfolds through struct fields)
4. **Return value** — needed for post-conditions like `ensures take R = List(return)`
5. **Argument type from C signature** — low effort, fixes false positives
6. **`traversal_fields` implementation** — improves predicate selection precision
7. **Iterated separating conjunctions** — needed for arrays
8. **PBT integration** — multiplies data quality
9. **Free/malloc tracking** — needed for allocator-heavy code
10. **Loop invariants** — hard, separate research problem
11. **Interprocedural** — significant extension, depends on 1-3
12. **Source rewriting** — quality of life
