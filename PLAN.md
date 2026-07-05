# Implementation Plan: Concrete Bi-Abduction for CN

This plan turns the algorithm described in `IDEA.md` (Sections 3 and 4) and
`main.tex` into a working, incrementally-built implementation on top of the
existing `bi-abd` branch. It was produced by comparing the branch against
`main` (`git diff main`) and reading every module under `lib/bi_abduction/`,
the runtime support in `runtime/libcn/`, the Fulminate integration in
`lib/fulminate/`, and the test suite in `tests/bi-abd/`.

*Note: `main.tex` only includes the abstract in this repository (the
`sec-*.tex` files it `\input`s are absent), so the algorithmic detail is taken
from `IDEA.md` Sections 3–4, which the abstract of `main.tex` matches
(bi-abductive operational semantics; footprint functions; learning from
sandwich constraints `A_j ⊆ F(d_j) ⊆ B_j`; disjoint cover).*

---

## 1. The intended algorithm (IDEA.md §3–4, main.tex)

The system has two halves.

**(a) Bi-abductive execution semantics (IDEA.md §3, "per-trace bi-abduction"
in main.tex).** Run the Fulminate-instrumented program, but instead of failing
on a missing-ownership error, *grant* the ownership, record the address in a
per-activation missing set `M`, and continue (`ABD_UNOP_DEREFERENCE`,
`ABD_SPEC_OWNED`, and their `_UNSAFE` variants). At each function return,
record a *data point* and propagate the activation's missing set to the caller
(the `CALL`/`return` rules). Per main.tex, this computes the unique *least*
anti-frame/frame pair per activation: the anti-frame is exactly what the
activation's subtree touched but was not given; the frame (leftover
postcondition) is what remains owned at the leak check.

Each data point for a function `f` is

```
d_j = (V_j, H_j, A_j, B_j)
```

- `V_j`: concrete environment (spec variables → values),
- `H_j`: the concrete heap at the point of interest (entry heap for
  preconditions, exit heap for postconditions),
- `A_j`: **lower bound** — addresses that *must* be owned (the missing set),
- `B_j`: **upper bound** — addresses that *may* be owned (everything not
  already owned by the user's existing partial specification or by other
  frames), with `A_j ⊆ B_j`.

**(b) Inference from data points (IDEA.md §4, "learning from sandwich
constraints" in main.tex).** Given data points, a set of user-defined
predicates, and a set of candidate *qualifiers* `Q` (candidate `take` bindings:
`RW<T>(t)`, `P(t, t1..tk)`, `each(...){...}`, over terms `t` built from
in-scope variables), find `Q' ⊆ Q` such that **for every data point** `d_j`:

1. `F(q, d_j) ≠ ⊥` for all `q ∈ Q'` — the qualifier *evaluates successfully*
   under the standard operational semantics of the CN specification language
   on the concrete heap `H_j` and environment `V_j`;
2. `A_j ⊆ ⋃ F(q_i, d_j) ⊆ B_j` — the sandwich constraint;
3. the footprints `F(q_i, d_j)` are pairwise disjoint.

The algorithm is: **Enumerate** qualifiers → **Filter**
(`F(q,d) ≠ ⊥ ∧ F(q,d) ⊆ B_j ∧ F(q,d) ∩ A_j ≠ ∅` for all `j`) → **Cover**
(disjoint set cover, NP-hard in general; greedy/ILP acceptable) → **Rank**.
Two refinements are essential:

- **Qualifier chains (§4.4):** qualifiers may depend on values bound by
  earlier ones (`take X = RW<struct node>(p); take Q = RW<int>(X.ptr);`), and
  chains sharing a prefix must be merged rather than declared non-disjoint.
- **Guided enumeration (§4.5):** rather than enumerating all well-typed
  instantiations, use the concrete heap as a *memory graph*, use
  *explainable* values (values of terms over `V_j`) as anchors, extract a
  *traversal summary* from each predicate definition (which fields its
  recursive unfolding follows), and only propose predicate instances whose
  traversal connects an explainable anchor to the missing region. Remaining
  arguments (e.g. a list segment's end pointer) are recovered from where the
  concrete unfolding must *stop*, not guessed up front.

---

## 2. Intended algorithm vs. current implementation

The current pipeline (`lib/bi_abduction/`, runtime
`runtime/libcn/src/cn-executable/bi_abduction.c`, driver
`bin/bi_abd_infer.ml`) is:

parse `cn_abd_summary.json` + `cn_abd_heap.jsonl` → group data points by
function → pick **one representative data point** (largest missing set) →
build memory graph → enumerate qualifiers **naively** (every well-typed iarg
assignment from args + small constants) → compute footprints → greedy disjoint
cover → print `take _ = ...;` suggestions.

| Intended (IDEA.md / main.tex) | Current implementation | Gap |
|---|---|---|
| §3 semantics: per-activation `M`, propagate to caller at return, leak check gives frame | `bi_abduction.c`: frame stack with `missing`/`pre_missing`/`post_remaining`; `pop_frame` merges into parent; `cn_postcondition_leak_check` collects the remainder (`utils.c: abd_leak_collect_cb`) | **Mostly implemented.** But auto-granted ghost-state entries are never cleaned up at frame pop, so later calls in the same run see stale ownership; leak sets from distinct calls contaminate each other (`baseline_multi_call_list.c` fails for this reason). |
| `H_j`: the concrete heap per data point | 64-byte neighborhoods around pointer args (`pre`, dumped in `cn_abd_mark_post`) and around leaked addresses (`post`); **all dumps of all activations merged into one global table** (`Data_point.heap_lookup`) | Heap is not per-data-point (stack addresses reused across calls collide); entry heap misses anything > 64 bytes from an argument, so pointer chains that leave the neighborhood are invisible. |
| `F(q, d)` computed by the *operational semantics of the CN spec language* | `Owned` footprint: correct byte range when the pointer is a bare variable (`footprint.ml: owned_footprint`). Predicate footprint: `predicate_footprint_from_graph` = "reachable struct bytes ∩ missing set". Guards, `assert`s, iargs, clause structure all **ignored** | **The central gap.** `F(q,d) ≠ ⊥` is never actually checked for predicates; `extra_predicate_body_ignored.c` deliberately pins the wrong answer (`NegList(p)` suggested for a positive-value list). Iargs don't affect footprints, so `IntListSeg(xs, end)` vs `IntListSeg(xs, xs)` are indistinguishable. |
| `B_j` upper bound; `F(q,d) ⊆ B_j` filter; respect for partial specs | `B` is implicitly "everything". Nothing records what the user's spec already owns; suggestions may double-own | Not implemented. |
| Cover over **all** data points simultaneously | Cover over one representative data point; other executions discarded | Not implemented (explicitly deferred in `TODO.md`). |
| Qualifier chains with prefix merging (§4.4) | Only flat top-level qualifiers rooted at function arguments | Not implemented (`baseline_wrapper_lists.c` fails for this reason). |
| Guided enumeration: anchors, traversal summaries (§4.5) | `memory_graph.ml` exists (BFS over struct layouts + pointer derefs) but `enumerator.ml` **ignores it** (`ignore graph; ignore var_addrs`) and enumerates all well-typed combinations | Graph is built but unused by enumeration; no traversal summaries; combinatorial iarg blowup for predicates with several args. |
| Iterated resources (`each`) | `Request.Q` returns `None` everywhere | Not implemented. |
| Ranking (§5) | Greedy cover order only | Not implemented (`extra_nonrecursive_predicate_ignored.c` pins `RW<struct pair>` over the semantically nicer `PairCell(p)`). |
| Least-solution minimality (main.tex: least anti-frame) | Cover only checks footprints intersect the missing set; a candidate strictly larger than needed wins ties arbitrarily | Partially: `A` is the least per-trace answer, but qualifier selection doesn't prefer minimal over-approximation. |

## 3. Currently supported fragment

Programs for which `cn bi-abd file.c` produces a correct suggestion today
(verified against `tests/run-bi-abd.sh` expectations):

- A single translation unit, `main` marked `trusted`, target functions with
  `requires true; ensures true` (or trivial specs).
- Function arguments of pointer-to-scalar, pointer-to-struct, or integer type
  only. Anything else (struct-by-value, arrays, function pointers) makes
  `Infer.infer_function` fail loudly (`arg_of_var` → `failwith`).
- Missing ownership directly rooted at a **function argument**:
  - `RW<T>(p)` for scalar and struct pointees
    (`extra_scalar_pointer_missing.c`, `baseline_pair_pre_post.c`,
    `extra_wrong_struct_type.c` — the *type-correct* struct is chosen because
    footprints are byte-accurate for `Owned`).
  - A recursive predicate instance `P(p, ...)` rooted at an argument, when the
    heap shape (pointer-chase reachability) covers the missing bytes and the
    relevant nodes happen to sit within the 64-byte dump radius of an argument
    (stack-allocated lists/trees in the tests:
    `list_example_nospec.c`, `tree_example_nospec.c`,
    `extra_null_boundary_argument.c`, `extra_iarg_name_capture.c`).
- Precondition/postcondition split: body accesses → `requires` candidates;
  leak-check remainder → `ensures` candidates (this *is* implemented, contrary
  to the stale first section of `TODO.md`).
- One call per function per run is the reliable case; a single "representative"
  call is used when there are several.

## 4. Currently unsupported cases

- **Semantic validity of predicate candidates**: guards, `assert`s, and iargs
  are ignored — `NegList(p)` is wrongly suggested (`extra_predicate_body_ignored.c`).
- **Multiple calls / multiple executions**: state pollution across calls makes
  even the easy multi-call case fail (`baseline_multi_call_list.c` expects
  `inference failed`); no generalisation across data points.
- **Qualifier chains**: ownership reached through a field of an owned struct
  (`baseline_wrapper_lists.c` — wrapper struct holding two list heads) cannot
  be expressed; inference fails.
- **Partial specifications**: existing `take`s are respected for *collection*
  (Fulminate grants them, so they don't show up as missing) but not for
  *suggestion* (candidates may overlap what the user already owns); `B_j` is
  not tracked.
- **Return values**: `ensures take R = P(return)` can never be suggested.
- **Global variables** as anchors.
- **Arrays / `each`** iterated resources.
- **Integer iargs that must be *derived*** (lengths, counts, bounds not equal
  to any in-scope variable or 0/1/−1).
- **malloc/free**, **loop invariants**, **interprocedural spec reuse**,
  **ranking**, **source rewriting**.
- Heap capture beyond 64 bytes of an argument (heap-allocated or long
  structures silently escape the dump, making predicate unfolding blind).

---

## 5. Incremental plan overview

> **Status note (updated after the `bi-abd/rewrite-footprint` branch and the
> Step 0 work below):** Step 1's goal — semantic footprints — is implemented,
> not by an OCaml interpreter but by a generated C harness
> (`fp_codegen.ml` / `fp_runner.ml`) that runs each candidate through
> Fulminate's own compiled predicate semantics against the recorded heap
> (`cn_load_hook` + failure callback ⇒ ⊥).  **Step 0 (done)** then aligned
> the data structures with the paper:
>
> - **Runtime, interval semantics**: `bi_abduction.c` now keeps a global
>   *event log* of abduction triples `(a, size, o, d)` (the paper's lazy
>   representation, Cor. 3.14) and materialises each activation's anti-frame
>   at pop as `{a | event in span, o < depth_i ≤ d}`.  The wholesale
>   merge-to-parent and the `pre_missing` workaround are gone; precondition
>   takes already acquire at caller depth (`cn_get_ownership` checks at
>   `cn_stack_depth - 1`), so the interval rule needs no special cases.
> - **Runtime, Λ release (B-Ret)**: `cn_abd_leak_check_and_release` records
>   the leak set and releases it to the caller, eliminating cross-activation
>   ghost-state contamination.
> - **Wire schema, dp-keyed**: summary entries and heap-dump lines carry a
>   `dp` activation id; data points carry `pre.owned` (complement of the
>   sandwich upper bound `B_j`) and `post.vars` (incl. `return`); heap
>   neighborhoods are additionally dumped at each first-missing address.
> - **`B_j` filter**: `infer.ml` rejects candidates whose footprint
>   intersects `owned_pre` — partial-spec-aware suggestion.
> - **Chain-shaped `Qualifier.t`**: `step list` (name × `Request.t`);
>   singletons today, unblocking Step 3 without interface churn.
>
> Regression tests: `step0_interval_owner.c` (interval rule; forbidden-output
> support added to `run-bi-abd.sh`), `step0_partial_spec_b.c` (B filter),
> and `baseline_multi_call_list.c` flipped from `inference failed` to
> `IntList(p)`.  The remaining steps below should be read with Step 1
> reinterpreted as harness-based (done).
>
> **Step 2 (done):** inference is data-relative across all activations —
> the representative-dp collapse is gone.  The harness sweeps every dp
> against its own heap snapshot; condition (†) is applied per candidate
> (`F ≠ ⊥` on every dp, `F ∩ owned_pre_j = ∅` on every dp); `Cover`
> works on per-dp footprint maps with per-dp disjointness and a least
> over-approximation tie-break (`Σ_j |F_j \ A_j|`).  `Owned` footprints
> are ⊥ at NULL, so base-case activations reject unguarded `RW`
> candidates in favour of guarded predicates (`step2_null_guard.c`).
>
> **Step 3 (done, depth 2):** qualifier chains.  The enumerator emits
> `take W = RW<struct S>(arg); take _ = Q(W.field)` for pointer-typed
> fields of struct-pointer arguments, with one stable prefix per
> argument; the harness renders chains as sequential calls (generating
> `owned_<S>` functions that predicate bodies alone did not induce);
> Cover decomposes candidates into canonical *steps* so chains share
> prefixes (§4.4 — a shared step's footprint is counted once), and the
> printer merges them.  `baseline_wrapper_lists.c` now yields
> `take b_W = RW<struct list_pair>(b); take _ = IntList(b_W.xs);
> take _ = IntList(b_W.ys);`; depth ≥ 3 fails honestly
> (`step3_chain_depth_limit.c`).  Deferred within Step 3: `return` as a
> post anchor (needs per-phase candidate sets; the value is recorded),
> chains rooted at user-spec bindings, deeper chains.

Five steps, each independently landable, each leaving `tests/run-bi-abd.sh`
green (with deliberate expectation updates) and the standard suites
(`tests/run-cn.sh`, `tests/run-cn-exec.sh`, fulminate CI) untouched. Runtime
changes stay behind `cn_abd_is_enabled()`; OCaml changes stay inside
`lib/bi_abduction/` plus its existing integration points
(`bin/bi_abd_infer.ml`, the bi-abd hooks in `lib/fulminate/internal.ml`,
`runtime/libcn/src/cn-executable/bi_abduction.c`).

1. **Semantic footprints** — implement `F(q, d)` as a concrete evaluator of CN
   predicate bodies (the paper's core object), replacing the shape heuristic.
2. **Faithful data points and multi-execution Cover** — per-activation heaps,
   per-frame ghost-state hygiene, `B_j` from already-owned addresses, cover
   over *all* data points, minimality tie-breaking.
3. **Qualifier chains and return values** — dependent `take`s with prefix
   merging (§4.4), `return` as a postcondition anchor.
4. **Guided enumeration and ranking** — anchors + traversal summaries (§4.5)
   replacing naive enumeration; simple ranking policy.
5. **Iterated resources (`each`) for arrays** — contiguous-stride detection
   with bounds restricted to explainable terms.

Deliberately deferred (see §7): derived integer iargs, loop invariants,
malloc/free, interprocedural propagation, symbolic post-validation.

---

### Step 1 — Semantic footprints: a concrete evaluator for qualifiers

**Feature summary.** Implement `F(q, d)` per IDEA.md §4.2: evaluate a
candidate qualifier by the operational semantics of the CN spec language on
the recorded concrete heap and environment, returning either the exact set of
consumed byte addresses or ⊥. This makes guards, `assert`s, clause selection,
and iargs semantically meaningful, and is the prerequisite for every later
step.

**Relevant files / modules / data structures.**
- New: `lib/bi_abduction/concrete_eval.ml{,i}`.
- Modified: `lib/bi_abduction/footprint.ml` (delegate predicate case to the
  evaluator; delete `predicate_footprint_from_graph`),
  `lib/bi_abduction/infer.ml` (`infer_function_inner` calls the new
  footprint), `lib/bi_abduction/memory_graph.ml` (no longer on the footprint
  path; retained for Step 4).
- Runtime: `runtime/libcn/src/cn-executable/bi_abduction.c` — in
  `cn_abd_record_missing`, also `dump_heap_neighborhood("pre", addr)` (first
  recording of an address only), so the evaluator can chase pointer chains
  that leave the argument neighborhoods. No format change to
  `cn_abd_heap.jsonl`.
- CN core consumed read-only (no modification): `Definition.Predicate.t`
  (`clauses`, `instantiate`), `Definition.Clause.t` (`guard`, `packing_ft`),
  `LogicalArgumentTypes.t` (`Define`/`Resource`/`Constraint`/`I`),
  `Request.t`, `IndexTerms`, `Memory.size_of_ctype`, struct layouts already
  computed by `Infer.build_struct_layouts`.

**Implementation strategy.**
- Define a concrete value type in `concrete_eval.ml`:

  ```ocaml
  type cval =
    | VInt of Z.t                       (* integers, bits *)
    | VPtr of int64                     (* Loc; VPtr 0L = NULL *)
    | VBool of bool
    | VStruct of Sym.t * (Id.t * cval) list
    | VUnit
  ```

- `eval_term : env:(Sym.t -> cval option) -> IndexTerms.t -> cval option` for
  the subset that occurs in predicate bodies and iargs: `Sym`, `Const` (Z,
  Bits, Null, Bool), `Binop` (arith, comparisons, and/or), `Unop` (not,
  negation), `ITE`, `StructMember`, `MemberShift`, `ArrayShift`, `EQ`/`LT`/…,
  `IsNull` (encoded as `EQ` with null), `Cast`. Anything else → `None` (⊥),
  never a crash — unsupported term forms must degrade to "candidate rejected".
- Heap reads: build a byte-precise reader on top of the 8-byte-aligned word
  dumps (`Data_point.heap_lookup`): `read_bytes : heap -> addr:int64 ->
  size:int -> Z.t option` slices words; struct values are assembled field-by-
  field from the layout (`(Id.t * int * int) list Sym.Map.t`).
- `eval_request : fuel:int -> state -> Request.Predicate.t -> (footprint * cval) option`:
  - `Owned (ct, _)`: footprint = `[addr, addr+size)`, value read from heap
    (needed because later bindings like `H.next` consume it). Fail (⊥) if any
    byte is unreadable (outside the dump) or already in the accumulated
    footprint (disjointness within a single qualifier's unfolding — catches
    cyclic structures together with the fuel bound).
  - `PName p`: look up `Definition.Predicate.t`, instantiate clauses with
    `Definition.Predicate.instantiate` (concrete pointer + iarg values lifted
    back to `IndexTerms` constants, or evaluate clause-by-clause with an
    environment — prefer the environment approach to avoid constructing
    terms), select the **first clause whose guard evaluates to true** (CN
    clause semantics), then walk its `packing_ft`:
    `Define` extends the environment; `Resource` recurses (consuming
    footprint); `Constraint (LC.T it)` evaluates `it` — false or unevaluable ⇒
    ⊥; `Constraint (LC.Forall _)` ⇒ ⊥ (unsupported, Step 5 territory);
    `I it` evaluates the return value (the predicate's oarg, needed by
    enclosing `Define`s/guards).
  - `Request.Q _` ⇒ ⊥ (until Step 5).
  - `fuel` (e.g. 4096 unfoldings) guards non-termination; exhaustion ⇒ ⊥.
- `Footprint.compute` becomes: evaluate the qualifier's pointer and iarg
  terms in the data-point environment (`dp.pre_vars`), then
  `Concrete_eval.eval_request`. The existing filter in
  `Infer.infer_function_inner` (`F ∩ must_cover ≠ ∅`) and `Cover.greedy_cover`
  stay as they are.

**Algorithmic content.** This is the footprint function `F(q,d)` of §4.2 /
main.tex (iii) computed by direct interpretation; determinism of the CN spec
language makes it a function. Filtering now genuinely checks `F(q,d) ≠ ⊥`.

**Tests to add / update** (all in `tests/bi-abd/`, expectations in
`tests/run-bi-abd.sh: expectations_for`).
- Update `extra_predicate_body_ignored.c`: expectation flips from
  `take _ = NegList(p);` to *no* `NegList` suggestion (with only `RW` available
  for the first node and chains not yet implemented, the run should report
  `/* inference failed */` — that honest failure is the correct baseline
  answer here).
- Update `extra_iarg_name_capture.c` / `extra_null_boundary_argument.c`:
  same expected strings, but now they must hold for *semantic* reasons; add a
  sibling test `step1_wrong_iarg_rejected.c` where `IntListSeg(xs, xs)` (empty
  footprint) and `IntListSeg(mid, end)` (doesn't start at the anchor) must
  lose to `IntListSeg(xs, end)`.
- New `step1_guarded_clause.c`: predicate with a non-trivial guard (e.g.
  `EvenList` requiring `H.val % 2 == 0` via `assert`) on a satisfying heap —
  must be suggested; and on a violating heap — must not be.
- New failure-mode test `step1_heap_escape.c`: heap-allocated list via a
  `trusted` malloc wrapper; documents (and after the runtime dump extension,
  fixes) the "outside dump radius ⇒ candidate rejected, inference fails"
  behavior.

**Success criteria.** `tests/run-bi-abd.sh` green with updated expectations;
`NegList` never suggested; `dune build` and `dune runtest` clean; no change to
non-bi-abd runtime behavior (all new C code behind `cn_abd_is_enabled()`).

**Risks / dependencies / limitations.**
- The `IndexTerms` constructor surface is large; mitigate by total
  pattern-matching with a `None` default and a `Pp.debug 5` note when a term
  form is rejected, so gaps are visible rather than silent misbehavior.
- Struct layout duplication: `Infer.build_struct_layouts` re-derives offsets
  (already flagged risky — it re-implements alignment). Before relying on it
  for byte-precise reads, cross-check against `Memory.struct_layout`/
  `Memory.member_offset` from CN core and switch to those if available;
  this is read-only use of core, not a core change.
- Evaluation reads the *entry* heap; for postcondition candidates the *exit*
  heap table is used (already split as `pre_heap_lookup`/`post_heap_lookup`).
  Mutation between entry and the first access can skew `pre` dumps recorded
  mid-body; accepted as a documented approximation until Step 2's snapshots.

---

### Step 2 — Faithful data points and multi-execution Cover

**Feature summary.** Make the collected data match the paper's
`d_j = (V_j, H_j, A_j, B_j)`: per-activation heap snapshots, per-frame
ghost-state hygiene so calls don't contaminate each other, an explicit `B_j`
(complement of already-owned), and Cover run over **all** data points of a
function simultaneously with a minimality tie-break.

**Relevant files / modules / functions.**
- Runtime `runtime/libcn/src/cn-executable/bi_abduction.c` +
  `include/cn-executable/bi_abduction.h`:
  - add a monotonically increasing `activation_id` per `cn_abd_push_frame`;
    tag every heap-dump line and every data point with it
    (`{"phase":"pre","dp":7,"words":{...}}`);
  - record `already_owned` per frame: on `cn_abd_mark_post` (i.e. after the
    user's precondition has been evaluated), snapshot the ghost-state entries
    at the current stack depth. This needs a read-only walk of
    `cn_ownership_global_ghost_state`, exposed via the existing
    `rmap_foreach` (`runtime/libcn/src/cn-executable/rmap.c`) — same pattern
    as `abd_leak_collect_cb` in `utils.c`;
  - frame-pop hygiene: on `cn_abd_pop_frame`, downgrade/remove the
    ghost-state entries that were *auto-granted* by
    `report_and_correct_missing_ownership` during this activation, so
    subsequent calls start clean. Track auto-granted ranges in the frame
    (they are exactly the `missing` entries) and reset them to unmapped via
    `ownership_ghost_state_remove` unless they were leaked on purpose.
    *Investigate first*: reproduce `baseline_multi_call_list.c`'s failure and
    confirm this is the contamination mechanism before changing state
    handling (per the repo's own caution about state-changing fixes).
- `lib/bi_abduction/data_point.ml`: parse `dp` ids; give each `data_point`
  its own `pre_heap : int64 -> int64 option`, `post_heap`, and
  `already_owned : Int64Set.t`; delete the global merged `heap_lookup` path.
- `lib/bi_abduction/infer.ml`: drop the representative-selection fold;
  `infer_function` now computes, per candidate `q`,
  `F(q, d_j)` for **every** `d_j`, applies the paper's filter
  (`∀j. F ≠ ⊥ ∧ F ⊆ B_j ∧ F ∩ A_j ≠ ∅`), and calls Cover with per-data-point
  footprints.
- `lib/bi_abduction/cover.ml`: generalise `candidate` to
  `{ qualifier; footprints : Int64Set.t list (* per dp *) }`; greedy selection
  now scores by total newly-covered `A` bytes across data points, maintains
  disjointness per data point, and tie-breaks by minimal over-approximation
  `Σ_j |F(q,d_j) \ A_j|` (the concrete analogue of main.tex's *least*
  anti-frame).

**Implementation strategy.**
- `B_j` is realised as `B_j = Addr \ already_owned_j` and the filter
  `F(q,d_j) ⊆ B_j` implemented as `F(q,d_j) ∩ already_owned_j = ∅`. This is
  exactly the "do not overlap user-provided partial specifications" design
  constraint from IDEA.md §1, and it makes partial-spec-aware suggestion work
  without parsing user specs on the OCaml side.
- Keep the `A_j ⊆ ⋃F` acceptance check per data point; if any data point is
  left uncovered, report `inference failed` as today (`qualifiers = None`).
- The NULL-call data point (e.g. `list_length(NULL)`) has `A_j = ∅`; the
  filter clause `F ∩ A_j ≠ ∅` must be applied only to *non-empty* `A_j`
  (IDEA.md marks it as a removable heuristic); `IntList(p)` evaluates on it to
  the empty footprint via the `is_null` guard — a good end-to-end check that
  Step 1's evaluator handles base cases.

**Algorithmic content.** Implements Cover exactly as §4.2–4.3 (all-`j`
filtering + disjoint cover), plus least-solution tie-breaking; realises the
sandwich constraint with a concrete `B_j`.

**Tests to add / update.**
- Update `baseline_multi_call_list.c`: expectation flips from
  `/* inference failed */` to `take _ = IntList(p);` — the flagship win of
  this step (length-1, length-3 and NULL calls must all be explained by one
  qualifier).
- New `step2_partial_spec_no_overlap.c`: `requires take X = RW<struct node>(p);`
  with the body traversing the full list; suggested candidates must not
  include anything whose footprint covers `p`'s first node (with chains not
  yet available the honest outcome is `inference failed`; the assertion is
  specifically that `IntList(p)` is *not* suggested).
- New `step2_two_shapes_consensus.c`: two calls with different shapes where a
  single-run answer would be over-specific (e.g. `RW<struct node>(p)` fits a
  1-node run but not a 3-node run) — must produce the predicate, not the
  `RW`.
- Negative case `step2_incompatible_runs.c`: two calls whose missing sets
  cannot be explained by any common qualifier (e.g. one run passes a struct
  pointer, another an int pointer via a union-ish setup) — expect
  `inference failed`, not a wrong spec.

**Success criteria.** Multi-call tests green; all Step-1 tests still green;
`tests/run-cn-exec.sh` (Fulminate suite) unaffected — in particular the
ghost-state hygiene change must be a no-op when `cn_abd_is_enabled()` is
false.

**Risks / dependencies / limitations.**
- Ghost-state cleanup at pop is the riskiest change (it touches the shared
  rmap); guard it entirely behind `cn_abd_is_enabled()` and land it with the
  reproduced-bug evidence.
- Heap snapshot volume grows with activation count; the dump-on-record scheme
  from Step 1 keeps it proportional to missing bytes, which is acceptable for
  test-sized programs. Whole-heap snapshots and the "re-running" scheme from
  IDEA.md's thinking log are explicitly out of scope.
- Depends on Step 1 (semantic `F` is what makes cross-data-point filtering
  meaningful).

---

### Step 3 — Qualifier chains and return values

**Feature summary.** Support dependent qualifiers (§4.4): candidates rooted at
values bound by earlier `take`s (`take W = RW<struct wrap>(w); take L =
IntList(W.head);`), with prefix sharing during cover; and make `return`
available as a postcondition anchor (`ensures take R = IntList(return);`).

**Relevant files / modules / functions.**
- `lib/bi_abduction/qualifier.ml`: a chain type replaces the bare alias:

  ```ocaml
  type step = { name : Sym.t; req : Request.t }   (* take name = req *)
  type t = step list   (* dependency-ordered; later steps may mention earlier names *)
  ```

  `pp` prints named takes (`take W = RW<struct wrap>(w); take _ =
  IntList(W.head);`); equality is modulo bound-name alpha-renaming keyed by
  the request they're bound to.
- `lib/bi_abduction/concrete_eval.ml`: evaluate a chain by threading the
  environment (each step's oarg value is bound to its `name`) and the
  accumulated footprint; the chain footprint is the disjoint union, per-step
  footprints retained for prefix merging.
- `lib/bi_abduction/enumerator.ml`: iterative deepening — round 0 enumerates
  the current root terms (arguments; plus `return` for the post phase); after
  evaluating round-`k` candidates, extend the term pool with `X.field` for
  every struct oarg `X` bound in a surviving candidate whose field is
  pointer-typed, and enumerate one more round; bound the depth
  (`config.max_chain_depth`, default 2) and total candidates
  (`config.max_qualifiers`, already present).
- `lib/bi_abduction/cover.ml`: prefix merging per §4.4 — canonicalise each
  chain step by the pair (evaluated footprint of the step, request-with-
  concrete-root); two chains sharing the canonical first step are *mergeable*,
  and disjointness is checked on the union of distinct canonical steps rather
  than naively per chain. Concretely: cover selects a set of canonical steps
  (a DAG), then reconstructs the printed chains from the DAG.
- Return value: `lib/fulminate/internal.ml` (`generate_c_specs_internal`,
  where `abd_push`/`abd_record_args`/`abd_mark_post`/`abd_pop` strings are
  built) — emit `cn_abd_record_var("return", (uintptr_t)__cn_ret, ...)` into
  the post-injection (`post_strs`, before `cn_abd_pop_frame`); Fulminate's
  epilogue already has the return value in scope (see how `exit_strs` are
  spliced). Runtime and `data_point.ml` need no schema change (`return` is
  just another var binding, but must be attached to the post environment).

**Algorithmic content.** §4.4's dependency-aware Cover. The key data
structure change is from "set of qualifiers" to "DAG of canonical steps";
disjoint set cover runs over DAG nodes, which resolves the duplicated-prefix
problem (`take X = RW<struct node>(node)` shared by two dependent takes)
without weakening disjointness.

**Tests to add / update.**
- Update `baseline_wrapper_lists.c`: flips from `/* inference failed */` to a
  chain expectation
  (`take W = RW<struct wrapper>(w);` + `take _ = IntList(W.head);` +
  `take _ = IntList(W.tail);` — exact printed names normalised by the test to
  avoid symbol-number noise; extend `run-bi-abd.sh` matching if needed).
- Update `step2_partial_spec_no_overlap.c`: now expects
  `take _ = IntList(??.next)`-style completion instead of failure — i.e. the
  partial-spec case becomes a *positive* test.
- New `step3_return_list.c`: constructor function building and returning a
  fresh list; expect `ensures`-side `IntList(return)` (this also exercises the
  leak-check → post pipeline on heap the caller never passed in).
- Failure case `step3_chain_depth_limit.c`: nesting deeper than
  `max_chain_depth` — expect honest `inference failed` plus a debug note, not
  a blow-up.

**Success criteria.** Wrapper and constructor tests green; enumeration stays
bounded (assert candidate-count in a unit test with a many-field struct).

**Risks / dependencies / limitations.**
- Candidate growth is multiplicative in chain depth; the round-based
  enumeration *only from surviving candidates* (those with `F ≠ ⊥` and
  `F ∩ A ≠ ∅`) is the containment mechanism — this is where Step 4's guided
  enumeration will eventually take over.
- Printing chains requires stable, readable bound names (`W`, `L1`…); keep a
  per-function counter, and alpha-equivalence in `Qualifier.equal` to avoid
  duplicate suggestions.
- The `return` hook is the one codegen touch; it reuses the existing bi-abd
  injection point in `internal.ml` and is a no-op without `~bi_abductive`.

---

### Step 4 — Guided enumeration (memory graph + traversal summaries) and ranking

**Feature summary.** Replace naive predicate-instance enumeration with the
§4.5 heuristic: anchors = explainable values; traversal summaries extracted
from predicate definitions; propose only instances whose summary-directed walk
on the concrete memory graph connects an anchor to the missing region; recover
boundary iargs from where the walk stops. Add a small ranking pass.

**Relevant files / modules / functions.**
- New: `lib/bi_abduction/traversal.ml` — extract, per
  `Definition.Predicate.t`, a summary
  `{ root_cell : Sctypes.t option; next_offsets : int list; stop_iargs : (int (* iarg index *) * stop_role) list }`
  by walking each `Clause.t`'s `packing_ft`: find the `Resource` whose pointer
  is the predicate's own `pointer` sym (root cell type); find recursive
  `Resource`s naming the same predicate and pattern-match their pointer terms
  as `StructMember`/`MemberShift` of the root binding (traversal offsets);
  match guards of non-recursive clauses of the shape `pointer == t` where `t`
  is an iarg or `NULL` (stop condition → which iarg is the boundary).
  Unrecognised shapes degrade to "no summary" ⇒ fall back to Step-3
  enumeration for that predicate.
- `lib/bi_abduction/enumerator.ml`: rewrite `predicate_qualifiers` to:
  1. compute the explainable-anchor set: evaluate each pool term (args, chain
     fields, `return`) to a concrete address;
  2. for each predicate with a summary and each anchor whose pointee type
     matches `root_cell`: walk `Memory_graph.t` (`adj` with `Offset`/`Deref`
     edges) following `next_offsets` from the anchor; require the walk to
     touch `A`;
  3. the address where the walk first leaves the region it may own (reaches
     `already_owned`, leaves the dump, or hits NULL) is the *boundary value*;
     instantiate the stop iarg with an explainable term evaluating to that
     value (`NULL`, an argument like `end`); if no term explains it, drop the
     candidate (deferred: derived iargs);
  4. remaining iargs still come from `choices_for_bt`, but only for predicates
     whose summary left them unconstrained.
  Keep the naive path behind `config.naive_enumeration : bool` for
  differential testing.
- `lib/bi_abduction/memory_graph.ml`: minor extension — record *which* struct
  layout produced each `Offset` edge so the walk can be typed (today every
  layout is overlaid on every node), and expose
  `walk : t -> start:int64 -> offsets:int list -> stop:(int64 -> bool) -> int64 list`.
- New: `lib/bi_abduction/rank.ml` — order multiple valid covers / equal-
  footprint candidates: fewer qualifiers ≺ more; named predicate ≺ `RW` chain
  at equal footprint; smaller `Σ|F \ A|` first (from Step 2). `infer.ml`
  runs cover on the ranked candidate order and reports the best cover.

**Algorithmic content.** §4.5's anchor/traversal heuristic; candidate
generation becomes O(anchors × predicates × walk length) instead of
O(args^iargs × predicates). Ranking implements a minimal §5.

**Tests to add / update.**
- Update `extra_nonrecursive_predicate_ignored.c`: flips to expect
  `take _ = PairCell(p);` (ranking prefers the named predicate at equal
  footprint).
- New `step4_two_traversals.c`: struct with `next` and `prev`, predicates
  `FwdList` (follows `next`) and `BwdList` (follows `prev`), heap linked only
  forward — must pick `FwdList` (today both look identical to reachability).
- New `step4_enumeration_scale.c`: predicate with 3 pointer iargs and a
  function with 6 pointer args; assert (via candidate-count debug output or a
  timeout budget in `run-bi-abd.sh`) that guided enumeration stays small
  where the naive product explodes.
- Failure case `step4_unexplainable_boundary.c`: list segment whose boundary
  address corresponds to no in-scope term — expect no segment suggestion and
  honest failure (this is the doorway to deferred derived-iarg work).

**Success criteria.** All earlier tests green; scale test within budget;
`FwdList`/`BwdList` disambiguation correct.

**Risks / dependencies / limitations.**
- Traversal-summary extraction is pattern-matching over `IndexTerms` shapes
  and will not recognise every predicate style (multi-cell nodes, boolean
  flag guards). The fallback to Step-3 enumeration keeps this safe: guided
  enumeration must only ever *shrink* the candidate set on predicates it
  understands.
- Depends on Steps 1–3 (semantic `F` validates what the heuristic proposes;
  chains supply the anchor pool).

---

### Step 5 — Iterated resources (`each`) for arrays

**Feature summary.** Suggest
`each (u64 i; lo <= i && i < hi) { RW<T>(array_shift<T>(p, i)) }` when the
missing region is a contiguous, regularly-strided block anchored at an
explainable pointer, with `lo`/`hi` restricted to explainable terms
(constants like `0`, integer arguments like `n`).

**Relevant files / modules / functions.**
- `lib/bi_abduction/enumerator.ml`: array-candidate detection — for each
  pointer anchor `p` with pointee type `T` (`sizeof T = s`), find the maximal
  run `[p + lo*s, p + hi*s)` inside `A_j`; propose bounds only when `lo` and
  `hi` are explainable in `V_j` **for every data point** (typically `lo = 0`,
  `hi = n` for an `int n` argument — two runs with different `n` disambiguate
  `n` from a constant).
- `lib/bi_abduction/qualifier.ml` / `concrete_eval.ml`: represent the
  candidate as `Request.Q { name = Owned (ct,Init); pointer; q = (i, u64);
  permission = lo <= i && i < hi; step; ... }`; evaluation iterates `i` over
  the concrete bound values, consuming `[p + i*s, p + (i+1)*s)` per index —
  this finally makes `Request.Q` a first-class citizen of `F(q,d)`.
- `lib/bi_abduction/footprint.ml`, `cover.ml`: no structural change —
  `Request.Q` footprints flow through the same per-data-point sets.
- `lib/fulminate` / runtime: none.

**Algorithmic content.** Extends the qualifier grammar to the `each` form of
IDEA.md §4.1 (`each(u64 i; t1 <= i && i < t2) { RW<ty>(p+i) }`); bound
selection is the same explainability discipline as Step 4's boundary iargs;
multi-data-point filtering (Step 2) is what pins bounds to `n` rather than to
the concrete length of one run.

**Tests to add / update.**
- New `step5_array_fill.c`: `void fill(int *p, int n)` writing `p[0..n)`,
  called with two different `n` — expect the `each` qualifier with bound `n`.
- New `step5_array_suffix.c`: function touching `p[1..n)` — expect
  `lo = 1` (constant lower bound).
- Failure cases: `step5_strided_gap.c` (touches every other element — no
  contiguous run ⇒ no `each`; falls back to failure or individual `RW`s) and
  `step5_unexplainable_bound.c` (length derived arithmetically, e.g. `n*2`,
  with term synthesis disabled — honest failure; documents the derived-iarg
  deferral).

**Success criteria.** Array tests green; `each` candidates never suggested
when a single-cell or predicate candidate covers `A` more tightly (ranking
from Step 4 applies).

**Risks / dependencies / limitations.**
- Depends on Step 2 (multi-run bound disambiguation) and Step 4 (ranking).
- Only unit stride over `sizeof T` and conjunctive `lo <= i < hi` bounds;
  nested `each`, matrices, and `each` inside predicate bodies remain ⊥.

---

## 6. Cross-cutting testing and CI

- `tests/run-bi-abd.sh` is the regression harness; every step lands with its
  expectation updates in the same commit as the behavior change, so the
  suite is always green at every commit. New tests follow the
  `stepN_*.c` naming used above and are appended to the default `tests` array.
- The suite is not yet wired into GitHub CI (`.github/workflows/` has no
  bi-abd job). Add a job to `fulminate.yml` (or a new `bi-abd.yml`) that runs
  `dune build && tests/run-bi-abd.sh` — do this in Step 1 so later steps are
  guarded.
- Non-bi-abd suites (`tests/run-cn.sh`, `tests/run-cn-exec.sh`,
  `tests/run-cn-vip.sh`, testgen suites) must stay green untouched; the only
  shared surfaces are `utils.c`/`rmap.c` (changes stay behind
  `cn_abd_is_enabled()`) and `internal.ml` codegen (changes stay behind
  `~bi_abductive`).
- `lib/bi_abduction/CLAUDE.md` and `TODO.md` are updated at each step (the
  pre/post-split section of `TODO.md` is already stale and should be corrected
  in Step 1's commit).

## 7. Intentionally deferred features

- **Derived integer predicate arguments** (lengths, counts, bounds that equal
  no in-scope term — e.g. `ListSeg` length iargs, `n*2` array bounds).
  Requires synthesising arithmetic terms whose value matches an observed
  concrete number across all data points: a program-synthesis search
  (IDEA.md compares it to programming-by-examples) with a large, noisy
  hypothesis space. Every earlier step is designed so that its absence
  degrades to "candidate dropped / honest failure", never a wrong spec.
  Revisit after Step 4, where the boundary-value machinery gives natural
  entry points.
- **Loop invariants.** Structurally different (per-iteration data points,
  symbolic relation to the loop counter); IDEA.md itself marks it TODO.
  Needs a new runtime hook (`cn_abd_loop_iter`) and a generalisation story —
  a separate project.
- **malloc/free tracking.** Requires allocation-event interception and
  ownership-transfer semantics for pre vs post attribution (IDEA.md §3
  "Malloc/free: TODO"). Constructor-style tests in Step 3 dodge this by
  relying on the leak check only.
- **Interprocedural spec propagation** (using an inferred callee spec to
  refine the caller). The runtime already merges callee missing sets into the
  caller (the `return` rule), so the data is there, but consuming inferred
  specs during inference introduces ordering/fixed-point questions —
  post-Step-4 work.
- **Symbolic validation / integration with `cn verify`** of suggested specs,
  and **source rewriting**. Suggestions remain explicitly *candidates*
  (concrete evidence only); auto-inserting and verifying them is quality-of-
  life work once precision is trustworthy.
- **Broader architectural risks flagged, not planned:** whole-heap snapshots
  or deterministic re-running (IDEA.md thinking log) if the
  dump-neighborhood scheme proves too lossy; provenance-aware addresses
  (main.tex notes the core is parametric in address structure — the current
  runtime collapses to raw `uintptr_t`, which is fine for the concrete tool
  but would need care for CHERI-style targets).
