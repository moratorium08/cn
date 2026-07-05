# Bi-Abduction: Limitations, TODOs, and Future Plans

*(Step 0 of PLAN.md — interval-propagation event log, Λ release at return,
dp-keyed wire schema with `owned`/`post.vars`, the `B` overlap filter, and the
chain-shaped `Qualifier.t` — is done; sections below have been pruned
accordingly.)*

## Multiple executions — done (PLAN.md Step 2)

Inference is now data-relative across all activations: candidates must
evaluate on every data point (`F ≠ ⊥`), respect every data point's upper
bound, and Cover works on per-dp footprint maps with a least
over-approximation tie-break (`step2_null_guard.c`,
`baseline_multi_call_list.c`).

Remaining refinement: candidate *scoring* could reward agreement across runs
explicitly (today more runs only constrain; they don't rank), and PBT/Bennet
integration would multiply data-point diversity.


## Partial specifications

Mostly handled by Step 0: the runtime snapshots `owned_pre` (ghost entries at
the activation's depth right after the user precondition evaluated) and
`infer.ml` rejects candidates whose footprint intersects it — the sandwich
upper bound `B_j` (see `tests/bi-abd/step0_partial_spec_b.c`).

Remaining gaps:
- at **post**, the filter reuses the *pre* snapshot as an approximation of the
  user's postcondition footprint; for consuming/transferring functions the
  right subtrahend is the post footprint, which is not yet recorded;
- rejected overlapping candidates currently just disappear — suggesting the
  non-overlapping *remainder* needs qualifier chains (PLAN.md Step 3).


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

Currently each qualifier is independent — flat, not chained.

Example of what's missing: `take X = Owned<struct node>(p); take Y = IntList(X.next);`. The second qualifier depends on the value bound by the first. The enumerator only generates top-level qualifiers from function arguments, not from intermediate bindings.

**Approach**: After initial coverage, check if uncovered addresses are reachable via struct fields of already-selected qualifiers. Generate follow-up qualifiers rooted at those fields. This is essentially the "unfolding" step from IDEA.md section 4.5.


## Argument type inference

The baseline enumerator now uses the function signature to recover parameter
types, so pointer arguments can generate `Owned` candidates from their actual
pointee type (e.g. `int *p` -> `RW<int>(p)` rather than only struct-based
candidates).

Remaining gaps:
- fallback inference still uses `v.size = 8 → Loc ()` when signature metadata is
  unavailable,
- predicate iarg matching still only looks at coarse `BaseTypes.t`, not full C
  types.

**Further work**: Thread precise C types more systematically through the runtime
and inference pipeline, and use them for richer ranking/disambiguation.


## Return value

The post-condition doesn't reference `return`. We don't capture the function's return value or suggest specs like `ensures take R = IntList(return);`.

**Fix**: Record the return value in `cn_abd_record_var` at the end of the function. Make it available as an additional variable for post-condition qualifier generation. The Fulminate codegen already knows the return variable.


## Global variables

Only function arguments are used as anchors. Global variables that the function accesses are invisible to the inference.

**Fix**: Record global variable addresses in `cn_abd_record_var`. Add them as additional anchors in the memory graph and as base terms in the enumerator.


## Predicate footprints are not checked semantically

Current recursive-predicate footprint computation does **not** evaluate the
predicate body on the concrete heap. In `footprint.ml`, predicate candidates are
handled by `predicate_footprint_from_graph`, which:

- resolves only the root pointer,
- ignores predicate iargs,
- ignores clause guards and `assert(...)`,
- ignores return equations,
- and approximates the footprint as "reachable struct bytes intersected with the
  missing set".

This means a predicate can be suggested even when it is semantically impossible
on the observed heap. Example: `extra_predicate_body_ignored.c` defines
`NegList(p)` with `assert(H.val < 0)`, but the observed list has positive
values; the current machinery still suggests `NegList(p)` because the heap shape
matches.

In the terms of IDEA.md, this means the implementation is not really checking
whether `F(q, d) ≠ ⊥` for predicate candidates; it is using a shape heuristic
instead.

**Fix options:**
- Re-evaluate candidate predicates with the executable CN/Fulminate semantics on
  the recorded concrete heap and variable environment, and reject candidates
  whose execution fails.
- Or implement a dedicated concrete predicate interpreter in OCaml that follows
  the predicate clauses, checks guards/assertions, and returns the precise
  consumed footprint.

Without this, recursive predicate suggestions remain only shape-based guesses.


## Predicate traversal matching

Predicates are currently matched purely via memory graph connectivity, which is imprecise — it can't distinguish between a `List` predicate (follows `next`) and a hypothetical `ListBackward` predicate (follows `prev`) if both are defined.

**Fix**: Walk `LogicalArgumentTypes` (the predicate clause bodies) to extract which struct fields a recursive predicate traverses. Pattern-match the predicate definition's `ResourceTypes.request` terms to find `MemberShift` / field access patterns. Build a "traversal signature" per predicate. Compare against the graph's actual pointer chain structure to pick the right predicate.


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


## Runtime memory cleanup

The current bi-abduction runtime intentionally leaks its bookkeeping state until
process exit. In `runtime/libcn/src/cn-executable/bi_abduction.c`,
`cn_abd_destroy()` just drops global pointers; it does not walk and free the
frame/data-point lists or destroy their hash tables.

Relatedly, the runtime currently uses a custom allocator wrapper with a no-op
`free` for bi-abduction tables. This is only tolerable because the
instrumented process is assumed to be short-lived.

**Fix**:
- Switch the bi-abduction tables to a normal allocator (`fulm_default_alloc`
  should be enough unless there is a stronger lifetime reason not to).
- Make `cn_abd_destroy()` actually free the collected frames/data points and
  call `ht_destroy()` on owned hash tables.
- Keep the ownership/missing-state lifetime explicit, rather than relying on
  process exit to reclaim memory.


## Output integration

Suggestions are printed as comments, not integrated back into the source file. Ideally, `cn bi-abd` should be able to rewrite the source file with the suggested specs inserted, or at least produce a diff/patch.


## Summary: priority ordering

1. **Pre/post splitting** — without this, half the spec is always empty
2. **Per-data-point cover / multi-run consensus** — needed to learn from several executions instead of one representative run
3. **Qualifier chains** — needed for most real predicates (the predicate unfolds through struct fields)
4. **Return value** — needed for post-conditions like `ensures take R = List(return)`
5. **Argument type from C signature** — low effort, fixes false positives
6. **Predicate traversal matching** — improves predicate selection precision
7. **Iterated separating conjunctions** — needed for arrays
8. **PBT integration** — multiplies data quality
9. **Free/malloc tracking** — needed for allocator-heavy code
10. **Loop invariants** — hard, separate research problem
11. **Interprocedural** — significant extension, depends on 1-3
12. **Source rewriting** — quality of life
