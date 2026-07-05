# Bi-Abduction Implementation Plan (v2)

**Audience.** This plan is an implementation handoff: the work items below are
specified so that they can be implemented without further design input (target
implementer: Claude Opus 4.8).  Before touching code, read
`lib/bi_abduction/CLAUDE.md` (architecture, wire formats, CN/Cerberus
conventions — especially the module-shadowing pitfalls) and skim
`sec-biabduction.tex` / `sec-inference.tex` for the semantics this
implementation realises.  `lib/bi_abduction/TODO.md` tracks fine-grained
leftovers per area.

**The test suite is the specification.**  Every work item below is driven by
named tests in `tests/bi-abd/`, run by `tests/run-bi-abd.sh`.  Tests currently
expecting `/* ... inference failed */` are *honest failures* pinning the
supported-fragment boundary; a work item "flips" a test when it turns that
failure into a correct suggestion.  A work item is done when (a) its driving
tests flip to the specified outputs, (b) every other test still passes, and
(c) no test ever flips from honest failure to a *wrong* suggestion.

---

## 1. Current status

Implemented (Steps 0–3 of the original plan; commits `01fc8f733`,
`7dd39eb7f`, `984fc6041`, `35c8159da`, `db8b01554`):

- **Runtime** (`runtime/libcn/src/cn-executable/bi_abduction.c`): global
  abduction-event log `(a, size, o, d)` (the paper's lazy interval
  representation); per-activation anti-frames materialised at pop as
  `A_i = {a | event in span, o < depth_i ≤ d}`; leak set Λ recorded and
  *released to the caller* at return (B-Ret); per-activation (`dp`-keyed)
  wire schema with `pre.owned` (complement of the sandwich upper bound `B`)
  and `post.vars` (including `return` for scalar/pointer returns); pre-heap
  neighborhoods dumped for *every* activation in an event's interval.
- **Footprints**: `F(σ,V,H)` computed by a generated C harness
  (`fp_codegen.ml` / `fp_runner.ml`) that compiles candidates with
  Fulminate's own predicate codegen and evaluates them against the recorded
  per-dp heap snapshot (`cn_load_hook`; failures → ⊥ via a `siglongjmp`
  callback).  Never re-implement CN semantics in OCaml — extend the harness.
- **Inference** (`infer.ml`, `cover.ml`): data-relative across *all*
  activations (condition (†)): candidates must evaluate on every dp, avoid
  every dp's `owned_pre`, and Cover works over per-dp footprint maps with
  per-dp disjointness, greedy by newly-covered `A` bytes, ties broken by
  least over-approximation `Σ_j |F_j \ A_j|`.  Pre/post phases succeed or
  fail independently.
- **Qualifiers** (`qualifier.ml`): chains of named `take` steps.  Enumerated
  today: `RW<T>(arg)`; `P(arg, iargs)` with iargs from in-scope args + small
  constants; depth-2 chains `take W = RW<struct S>(arg); take _ = Q(W.field)`
  with one stable prefix per argument, shared between chains (Cover works on
  canonical *steps*; shared steps counted once; printer merges).

Working end-to-end (see `example_*.c`): list/tree/dlist traversals and
mutations including recursion (one dp per activation), wrapper chains, mixed
covers (`IntList(p)` + `RW` out-parameters), early-exit traversals,
multi-call generalisation, NULL-guarded predicates, partial-spec `B`
filtering, cyclic-list rejection-not-divergence.

## 2. Currently-failing tests → work items

| Test (function) | Needed capability | Work item |
|---|---|---|
| `adversarial_array_two_cells` (`sum2`) | shifted Owned anchors, constant index | WI-1 |
| `adversarial_array_arg_index` (`load_at`) | shifted Owned anchors, argument index | WI-1 |
| `adversarial_struct_array_element` (`second_pair_sum`) | shifted Owned anchors, struct element | WI-1 |
| `adversarial_void_cast_scalar` (`load_void`) | pointee-type hypotheses for `void*` args | WI-1 |
| `adversarial_nested_scalar_chain` (`nested_load`) | depth-3 chains, scalar leaf | WI-2 |
| `adversarial_list_via_pointer_pointer` (`indirect_length`) | chains through scalar-pointer pointees | WI-2 |
| `example_hard_ptr_chain` (`deref2`) | same | WI-2 |
| `step3_chain_depth_limit` (`deep_length`) | depth-3 chains | WI-2 |
| `adversarial_global_list_anchor` (`global_length`) | globals as anchors | WI-3 |
| `example_hard_global_counter` (`tick`) | globals as anchors | WI-3 |
| `example_hard_listseg_boundary` (`walk3`) | globals as anchors (boundary `&far_tail1` becomes in-scope) | WI-3 |
| `example_list_reverse` post (`list_reverse`) | `return` as post anchor | WI-4 |
| `adversarial_field_boundary_segment` (`segment_sum`) | user-spec bindings as anchors/iargs | WI-5 |
| `step0_partial_spec_b` (`list_length`) | same (flips from honest failure to completion) | WI-5 |
| `adversarial_sized_list_constant` (`list_sum3`) | derived integer iargs | WI-6 |
| `adversarial_sized_list_arg_plus_one` (`length_plus_one_case`) | derived integer iargs (arith over args) | WI-6 |
| `example_hard_array_fill` (`array_fill`) | `each` iterated resources | WI-7 |
| `example_hard_malloc_constructor` (`mk_node`) | allocation tracking + WI-4 | WI-8 |
| `example_hard_cycle` (`cycle_length`) | none — predicate-vocabulary gap, stays honest failure | — |

Recommended order: **WI-1 → WI-2 → WI-3 → WI-4 → WI-5 → WI-6 → WI-7 → WI-8**.
WI-1..WI-5 are independent enough to reorder; WI-7 reuses WI-1's shifted-cell
machinery; WI-8 requires WI-4.

## 3. Invariants — do not break these

1. **(†) on every data point.**  A candidate must have `F ≠ ⊥` on *every* dp
   of the function and `F ∩ owned_pre_j = ∅` on every dp.  Never weaken this
   to "some dp" — it is what prevents unsound suggestions
   (`step2_null_guard.c` is the sentinel).
2. **Honest failure beats wrong suggestion.**  When extending enumeration,
   every new candidate class must be validated by the harness (or an exact
   analytic footprint); heuristic shape-matching without semantic evaluation
   is how the pre-rewrite code suggested `NegList(p)` for a positive list.
3. **Footprints come from Fulminate's own semantics** (the harness), not
   from a parallel OCaml interpreter.  The only analytic footprints are
   `Owned` at concretely-evaluable addresses, and they must be exact.
4. **All runtime changes behind `cn_abd_is_enabled()`**; all codegen changes
   behind `~bi_abductive`.  The plain Fulminate suites must be unaffected.
5. **Ties prefer less over-approximation, then earlier enumeration order.**
   Enumeration order is semantic: flat before chains, predicates before RW
   in chain leaves.  When adding candidate classes, place them deliberately
   and document the choice.
6. Suite must be green at every commit (`tests/run-bi-abd.sh`); expectation
   updates land in the same commit as the behavior change, with exact
   observed output strings; `dune fmt` clean.

## 4. Work items

### WI-1: Shifted and typed Owned anchors

**Flips** (expected new outputs; capture exact `Request.pp` rendering when
setting expectations):
- `adversarial_array_two_cells`: pre/post
  `take _ = RW<signed int>(a);` and
  `take _ = RW<signed int>(array_shift<signed int>(a, 1));` (two disjoint
  cells selected by Cover).
- `adversarial_array_arg_index`: `take _ = RW<signed int>(array_shift<signed int>(a, i))`.
  With one call, the constant form `array_shift(a, 2)` is equally
  data-relatively valid — enumerate **argument-indexed shifts before
  constant-indexed** so the arg form wins the tie (document this in the
  test's comment; if the constant form is preferred, swap the order).
- `adversarial_struct_array_element`:
  `take _ = RW<struct pair>(array_shift<struct pair>(ps, 1));`.
- `adversarial_void_cast_scalar`: `take _ = RW<signed int>(p);`.

**Files / functions.**
- `lib/bi_abduction/enumerator.ml`:
  - `owned_qualifiers`: for each pointer arg with `owned_ct = Some ct`
    (`ct` not void/function), additionally emit
    `RW<ct>(array_shift<ct>(arg, k))` for `k ∈ int_args @ [1; 2; 3]`
    (index 0 is the existing plain candidate).  Build the term with
    `IT.arrayShift_ ~base ~index ct loc` (`lib/indexTerms.ml:714`); it
    casts the index to `Memory.uintptr_bt` itself, so plain `IT.sym_` for
    int args and `IT.int_ k loc` for constants are fine as inputs.
  - For `void*` args (`owned_ct = None` today): give them *hypothesised*
    pointee types: emit `RW<T>(arg)` for
    `T ∈ {Sctypes.Integer (Signed Int_)} ∪ all struct tags in struct_defs`.
    Keep the hypothesis set small; the tie-break (least `|F \ A|`) picks
    the size matching the observed access.  `arg_of_var` in `infer.ml`
    currently maps `Pointer Void` to `owned_ct = None` — keep that, and add
    the hypothesis expansion in the enumerator so it is clearly marked as
    guessing.
- `lib/bi_abduction/footprint.ml`:
  - `eval_pointer_term` must evaluate `ArrayShift` terms:
    `addr(base) + index * Memory.size_of_ctype ct`, where `base` is a `Sym`
    resolved from `pre_vars` and `index` is a constant or a `Sym` resolved
    from `pre_vars` (int args are recorded there).  Return `None` for base
    value `0` (NULL) as for plain pointers, and `None` for negative
    resulting addresses.
  - `compute` for these candidates stays analytic (exact byte range).
- No harness changes: shifted candidates are `Owned`, computed analytically.
  (Do **not** add shifted roots for *predicate* candidates in this WI; if a
  test ever needs `P(a+k)`, that requires `render_raw_value` to support
  `ArrayShift` — note it in TODO.md instead.)

**Acceptance.**  Four adversarial tests flip; `example_hard_array_fill`
still fails (its 8-cell block exceeds the constant range `k ≤ 3` — this is
deliberate: `each` must stay necessary; do not raise the constant bound to
make it pass).

**Risks.**  The `arrayShift_` self-cast means the printed term may carry a
cast (e.g. `array_shift<signed int>(a, (u64)i)`); set the test expectations
from the *observed* rendering, not from the comment sketches above.

### WI-2: Generalised chains (any pointer pointee, depth 3) with per-step harness footprints

**Flips:**
- `example_hard_ptr_chain`: `take P = RW<signed int*>(pp); take _ = RW<signed int>(P);`
  (bound-name rendering: the leaf's pointer term is the prefix's bound sym
  directly).
- `adversarial_list_via_pointer_pointer`:
  `take P = RW<struct node*>(pp); take _ = IntList(P);`.
- `adversarial_nested_scalar_chain`:
  `take o_W = RW<struct outer>(o); take I = RW<struct inner>(o_W.in); take _ = RW<signed int>(I.p);`.
- `step3_chain_depth_limit`: flips to the depth-3 chain ending in
  `IntList(...)` — update the test's header comment (it documents the old
  limit) and its expectation.

**Design.**  Replace the hardcoded two-step chain enumeration with a
worklist over *roots*:

```
root = { term : IT.t;            (* pointer-valued term *)
         pointee : Sctypes.t;    (* what it points to *)
         prefix : Qualifier.step list;  (* steps binding it, [] for args *) }
```

Seed: one root per pointer argument (and, after WI-3/WI-4/WI-5, per global /
return / spec-binding anchor).  For a root at depth `< max_chain_depth`
(new `Enumerator.config` field, default 3):
- emit leaf candidates `prefix @ [leaf]` for each predicate (iargs as today)
  and for `RW<pointee>` — predicates before RW (tie order);
- extend the worklist:
  - `pointee = Struct S`: bind `W` via `Owned(Struct S)(term)` (one stable
    `W` sym per root, as today); new roots
    `{ term = IT.member_ ~member_bt:(Loc ()) (w_term, fid); pointee = pointee_of_field; prefix = prefix @ [W-step] }`
    for each pointer-typed field;
  - `pointee = Pointer U` (scalar pointer): bind `P` via
    `Owned(Pointer U)(term)`; new root
    `{ term = IT.sym_ (P, Loc (), loc); pointee = U; prefix = prefix @ [P-step] }`.
  - other pointees: leaf only.

Cap total candidates with the existing `config.max_qualifiers`.

**Per-step harness footprints (replaces the analytic prefix diff).**
`infer.ml:steps_of` currently reconstructs the prefix footprint analytically
and only supports length ≤ 2.  Replace with per-step measurement in the
harness:
- `fp_codegen.ml`: in the per-qualifier function, after *each* rendered step
  call `fp_emit_step(out, ...)` variants that snapshot the ghost-state delta
  (addresses at `FP_PREDICATE_DEPTH`) — emit JSON
  `{"q":k,"dp":j,"steps":[[...],[...]]}` where `steps[i]` is the *cumulative*
  set after step `i` (cumulative is simpler to emit; OCaml takes successive
  differences).  On failure at any step emit `"steps":null`.
- `fp_runner.ml:parse_results_json` and `fp_table.ml`: value type becomes
  `Int64Set.t list option` (cumulative sets).  `Footprint.lookup` returns the
  final set; a new `Footprint.lookup_steps` returns the per-step sets.
- `infer.ml:steps_of`: for a chain of length n, step-unit `i`'s footprint =
  `steps[i] \ steps[i-1]`; keys as today (`Pp.plain (Request.pp req)`).
  Singleton candidates unchanged (one step, final set).
- Chain rendering (`render_qualifier_stmts`): generalise from the fixed
  two-step match to a fold over steps, maintaining a binding environment
  `Sym.t -> C expression`:
  - struct prefix: `struct <tag>_cn *<name> = owned_struct_<tag>(<ptr>, PRE, (void*)0);`
    → later `StructMember(sym, fid)` renders as `<name>-><field>`;
  - scalar-pointer prefix: `cn_pointer *<name> = owned_<u>_pointer(<ptr>, PRE, (void*)0);`
    → later `Sym name` at Loc renders as `<name>` directly.
    Owned-function naming: `owned_fn_name` already produces
    `owned_struct_node_pointer` for `struct node*`
    (`Utils.str_of_ctype (Pointer t) = str_of_ctype t ^ " pointer"`); the
    *return type* of the generated owned function for a pointer ctype is
    `cn_pointer*` — verify once against `Internal.generate_ownership_functions`
    output before relying on it.
  - Collect `extra_cts` for every Owned step (prefix and leaf), as today.

**Cycle safety.**  Deeper chains whose leaf wraps to a consumed prefix cell
already fail via the ownership re-claim (⊥) — no new handling needed, but
keep `example_hard_cycle` green (honest failure).

**Acceptance.**  Four flips above; `baseline_wrapper_lists`,
`example_stack_peek`, `example_list_append`, `extra_predicate_body_ignored`
outputs unchanged (their depth-2 chains re-derive under the new enumeration —
if bound-name rendering changes (`b_W` etc.), keep the naming scheme
`<root>_W` for struct prefixes to avoid churning expectations).

**Risks.**  Candidate growth at depth 3 (struct with many pointer fields ×
predicates): acceptable at test scale; the guided enumeration of the original
Step 4 remains the long-term fix — record actual candidate counts in the
commit message.  Per-step JSON is a wire-format change internal to the
harness: update `fp_codegen.mli` docs and `CLAUDE.md` in the same commit.

### WI-3: Globals as anchors

**Flips:**
- `adversarial_global_list_anchor`: `take _ = IntList(head);` pre/post.
- `example_hard_global_counter`: `take _ = RW<signed int>(&counter);` pre/post.
- `example_hard_listseg_boundary` (expected — verify): with `&far_tail1` an
  in-scope Loc term, the iarg pool for `IntListSeg` gains the boundary:
  `take _ = IntListSeg(p, &far_tail1);`.  If it flips, rewrite that test's
  comment (it currently claims the boundary is underivable) and keep a truly
  underivable variant if desired (heap-allocated tail).

**Design.**
- **Runtime data**: globals must appear in each dp's `pre_vars`, so that the
  harness (`fp_var_value`) and analytic evaluation resolve them, uniformly
  with arguments.  Codegen in `lib/fulminate/internal.ml`
  (`generate_c_specs_internal`, next to `abd_record_args`): for each global
  `g` from `Cn_to_ail.extract_global_variables cabs_tunit prog5` (already
  computed in scope — hoist if needed), emit
  `cn_abd_record_var("&g", (uintptr_t)&g, sizeof(g));` and, when `g` has
  pointer type, also `cn_abd_record_var("g", (uintptr_t)g, sizeof(g));`
  (the *entry value*, recorded at push).
- **Types to OCaml**: `bin/bi_abd_infer.ml` — add
  `extract_global_defs : sigma -> (string * Sctypes.t) list` from
  `sigm.A.declarations` (`Decl_object`), thread through `Infer.infer` as
  `~global_defs`.  In `infer_function`, extend the `args` list with
  pseudo-args: for global `g : T`, an anchor `{sym = Sym.fresh "&g";
  bt = Loc (); owned_ct = Some T}`; for `T = Pointer U` additionally
  `{sym = Sym.fresh "g"; bt = Loc (); owned_ct = Some U}`.  Everything
  downstream (flat candidates, chains, iarg pools) then works unchanged —
  the name-keyed variable lookup is what makes this work, and printing a
  `Sym` named `"&counter"` renders the desired `&counter`.
  `arg_of_var` must *not* be applied to globals (they are not in
  `signature_args`); build the pseudo-args separately and filter dp
  `pre_vars` entries whose names start with `&`/match globals when building
  argument-derived anchors.
- **Spec-syntax caveat**: a CN spec using a global needs an `accesses g;`
  clause (or explicit takes of `&g`).  The suggestions are still just
  `take` lines; add a one-line hint to the printed output when a suggestion
  mentions a global (`/* requires 'accesses g;' or ownership of &g */`) —
  cosmetic, do not gate acceptance on it.

**Acceptance.**  Two (likely three) flips; `pre.owned` interaction: globals
are owned at depth 0, so they are *not* in `owned_pre` and the B filter does
not reject them — confirm with `step0_interval_owner` still green.

**Risks.**  `sizeof(g)` for array globals records the whole array — fine for
the value entry being skipped (arrays are not pointer-typed); the `&g` anchor
with `owned_ct = Some (array T)` — `Sctypes` arrays should be skipped
(only record scalars/structs/pointers; guard on the ctype).

### WI-4: `return` as a postcondition anchor (per-phase candidate sets)

**Flips:** `example_list_reverse` post: `take _ = IntList(return);`.

**Design.**  The value is already recorded (`post.vars` carries `return`).
- `bin/bi_abd_infer.ml`: extend `function_args` to carry the return ctype:
  `(string * ((string * Sctypes.t) list * Sctypes.t option)) list`
  (from `Decl_function`'s return type; `None` for void/unsupported).
- `infer.ml`: split candidate enumeration per phase.  `candidates_pre` =
  enumeration over arg+global anchors (as now).  `candidates_post` =
  the same plus, when the return type is `Pointer T`, an anchor
  `{sym = Sym.fresh "return"; bt = Loc (); owned_ct = Some T}` (participates
  in flat candidates *and* chains).  Keep one global `q_idx` space: index
  pre candidates first, then post-only candidates, so `Fp_table` keys stay
  unambiguous; run the pre harness with pre candidates and the post harness
  with post candidates.
- `fp_codegen.ml`: dp variable tables must include post vars for name
  resolution — emit `e.dp.pre_vars @ e.dp.post_vars` into `VARS_DP%d` (the
  pre harness never receives return-rooted candidates, so the extra entries
  are inert there); `dp_has_all_syms` similarly checks both lists.
- Footprint analytic path (`eval_pointer_term`): resolve names against
  `pre_vars @ post_vars`.

**Acceptance.**  `list_reverse` flips (pre unchanged); `example_list_append`
post output may change if a return-rooted candidate ties — it should not
(void return); assert unchanged.  All Owned-at-NULL and (†) checks apply to
post candidates only on post dps — note: `F(q, d_j)` for a post-only
candidate is still required on **every** dp (each dp has its own return
value; a NULL return on some dp correctly ⊥-rejects `RW<T>(return)` but not
guarded predicates).

**Risks.**  Functions where `return` aliases an argument (e.g. identity)
create equal-footprint candidates; tie order (args enumerated before
`return`) keeps current outputs stable.

### WI-5: User-spec bindings as anchors and iarg sources

**Flips:**
- `adversarial_field_boundary_segment`: pre
  `take L = IntListSeg(box.start, box.stop);` and post
  `take L2 = IntListSeg(box2.start, box2.stop);` (names bound by the user's
  own `take box = ...` / `take box2 = ...`).
- `step0_partial_spec_b`: flips from double honest failure to
  `take _ = IntList(First.next);` pre / `take _ = IntList(First2.next);`
  post — rewrite that test's comment and expectations (it currently
  documents the failure), and keep the *forbidden* `IntList(p)` check.

**Design.**  Record the concrete values of user `take`-bound names and
expose them as anchors, per phase.
- **Codegen** — the robust place is inside the spec-to-C translation, where
  binder names and types are known: `lib/fulminate/cn_to_ail.ml`
  (`cn_to_ail_pre_post` internals, where each precondition/postcondition
  `Resource` binder `X` is bound to a C local).  Behind the `bi_abductive`
  flag (thread it in from `internal.ml` — `cn_to_ail_pre_post` gains an
  optional `~bi_abd_record:(string -> unit)`-style parameter or a boolean
  that makes it also emit recording statements):
  - binder `X : Loc` (a pointer take, e.g. `RW<τ*>` or a pointer-returning
    predicate): emit `cn_abd_record_var("X", (uintptr_t)cn_pointer_unwrap(X_cn), 8);`
    (find the actual accessor for `cn_pointer`'s raw value — `->ptr` per the
    generated code);
  - binder `X : Struct S` (an `RW<struct S>` take): for each pointer-typed
    field `f`: `cn_abd_record_var("X.f", (uintptr_t)X_cn->f->ptr, 8);`.
  - Precondition binders → `cn_abd_record_var` (pre vars); postcondition
    binders → `cn_abd_record_post_var`.
  Use the binder's *source name* (what `Sym.pp_string` gives for the
  user-written name) — this is what makes the suggestion `IntListSeg(box.start, …)`
  read as CN the user can paste (pre/post are one scope in CN, so pre
  binders are legal in ensures too).
- **OCaml**: dp `pre_vars`/`post_vars` now contain entries named `X` and
  `X.f`.  Add them as anchors (Loc pseudo-args) in the respective phase's
  candidate set; they also enter `choices_for_bt (Loc ())`, which is what
  puts `box.stop` in the `IntListSeg` iarg pool.  Types: unknown from the
  signature — treat as `owned_ct = None` anchors (predicate roots and iargs
  only) in the first cut; typed chains from spec bindings can follow later.
- **B-filter sanity**: candidates rooted at spec bindings cover memory
  *adjacent to* but not inside the user footprint (`IntListSeg(box.start,
  box.stop)` stops exactly where the user's `IntList(box.stop)` begins) —
  the per-dp `owned_pre` check enforces this; no changes needed, but this is
  the acceptance check that matters.

**Acceptance.**  Two flips; `extra_*` and `example_*` outputs unchanged
(functions without user takes get no new anchors).

**Risks.**  The generated C local name for a binder (`X_cn` vs a
sym-numbered variant) — do the recording inside `cn_to_ail` where the Ail
symbol is in hand, not by string-guessing in `internal.ml`.  This is the WI
with the deepest Fulminate integration; budget review time for the
`cn_to_ail_pre_post` change and keep it minimal (one optional parameter).

### WI-6: Derived integer iargs (search + term fitting)

**Flips:**
- `adversarial_sized_list_constant`: `take _ = SizedList(p, 3i32);`.
- `adversarial_sized_list_arg_plus_one`: `take _ = SizedList(p, n + 1i32);`
  — with a single call, the constant `3i32` also satisfies (†); prefer
  arg-based terms over constants in the fitting order so the general form
  wins.  (Better: add a second call with a different length to the test to
  make the constant unfittable — coordinate with the test owner.)

**Design.**  For predicates with integer iargs, don't enumerate values —
*search* them in the harness and *fit terms* afterwards.
- **Enumeration** (`enumerator.ml`): for each predicate with exactly one
  integer iarg (limit the first cut to one), and each root, emit a candidate
  whose iarg is a distinguished *hole* — represent as a fresh sym named
  `"?iarg"` in the `Request.t` (the chain/step machinery is untouched; mark
  the qualifier as holed in a side table `(q_idx -> hole info)` passed to
  the codegen, or detect the sym name).
- **Harness** (`fp_codegen.ml`): for a holed qualifier, wrap the per-dp body
  in `for (long v = 0; v <= 8; v++) { fp_setup(dp); ... call with
  convert_to_cn_bits_i32((int32_t)v) ...; on success: emit
  {"q":k,"dp":j,"iarg":v,"steps":[...]} and break; fp_teardown(); }`; if no
  `v` succeeds emit null.  (Search bound 8 is a config constant; document.)
- **Fitting** (`infer.ml`): for a holed candidate with per-dp solutions
  `v_j`: try terms in order (1) each integer argument `n` with
  `eval n = v_j` on every dp; (2) `n + c` / `n - c` for `c ∈ {1, 2}`;
  (3) the constant, if all `v_j` are equal.  First fit wins; instantiate the
  qualifier (substitute the hole sym via `IT.subst` /
  `Request.Predicate.subst`) and proceed as a normal candidate with the
  recorded footprints.  No fit → drop the candidate.
- Footprints per dp come from the successful runs, so (†) and Cover are
  unchanged.

**Acceptance.**  Two flips; `SizedList`'s `assert (0i32 < n)` and NULL base
case are exercised by the search (v=0 fails on non-null, correct v
succeeds) — which is exactly the take-semantics validation doing the
filtering.  Guard the cost: holed candidates multiply harness work by the
search bound; only predicates that *have* integer iargs pay.

**Risks.**  Two-int-iarg predicates (unsupported: emit no holed candidate —
honest failure); holes in chain leaves (allowed by construction, but test
coverage is via the flat cases first).

### WI-7: Iterated resources (`each`) for arrays

**Flips:** `example_hard_array_fill`:
`each (u64 i; 0u64 <= i && i < 8u64) { RW<signed int>(array_shift<signed int>(a, i)) }`
— or with the bound `n`: generalising the bound to the argument requires the
multi-dp fit (same machinery as WI-6): with one call, the constant bound is
what (†) supports; **extend the test with a second call `array_fill(buf2, 4)`**
so the fitted bound must be `(u64)n` — then expect
`each (u64 i; 0u64 <= i && i < (u64)n) { ... }`.  Capture the exact
`Request.pp` rendering of the `Q` request for the expectation.

**Design.**
- **Detection** (`enumerator.ml` or a small new module `each_detect.ml`):
  per pointer arg `a` with element type `T` (`s = size_of T`), per dp: find
  the maximal `lo ≤ i < hi` with `[addr(a)+lo*s, addr(a)+hi*s) ⊆ A_j` and
  the run non-empty.  Candidate bounds: fit `lo` and `hi` across dps with
  the WI-6 term grammar (constants; int args; arg±1).  Emit a
  `Request.Q` candidate:
  `{ name = Owned (T, Init); pointer = arg term; q = (i_sym, BT.Bits (Unsigned, 64));
     q_loc = loc; step = Sctypes.to_ctype T; permission = lo ≤ i && i < hi; iargs = [] }`
  (see `Request.QPredicate` in `lib/request.ml:73` for the exact record; the
  `step` field is a `Sctypes.ctype`).
- **Footprint**: analytic (`footprint.ml`): evaluate `lo`/`hi` per dp
  (constants or args from `pre_vars`), footprint =
  `⋃_{i∈[lo,hi)} [addr + i*s, addr + (i+1)*s)`; ⊥ if the base is NULL or
  bounds are unevaluable/negative.  (No harness involvement in the first
  cut; `Request.Q` remains un-rendered there — `needs_harness` must return
  `false` for these.)
- **Printing**: `Request.pp` handles `Q` already; check the suggestion reads
  as valid CN (`each (u64 i; ...) { ... }`) and adjust `pp_takes` framing if
  needed.

**Acceptance.**  `array_fill` flips; `adversarial_array_two_cells` must
*still* produce the two individual cells, not a 2-element `each` — tie/order:
enumerate `each` candidates *after* flat and chain candidates, and only emit
them when the run length is ≥ 3 (a documented heuristic constant), so small
fixed blocks keep their exact per-cell form.

**Risks.**  `Request.Q`'s permission term construction (use `IT.le_`/`IT.lt_`
over the right bit-vector type; match on how CN parses `each` specs by
round-tripping one through `cn verify` manually).

### WI-8: Allocation tracking (malloc/free) — requires WI-4

**Flips:** `example_hard_malloc_constructor`: pre — *no additions* (the
fresh cell must drop out of the anti-frame per the closed form
`A* = (T \ N) \ Own`); post — `take _ = RW<struct node>(return);`
(ties with `IntList(return)` on a single node; owned-before-predicates flat
order keeps `RW` — either is correct; pin whichever is produced).

**Design.**
- **Interposition**: in `lib/fulminate/fulminate.ml`, where the bi-abductive
  prelude is emitted (`#include <cn-executable/bi_abduction.h>`), also emit
  `#define malloc(sz) cn_abd_malloc(sz)` and
  `#define free(p) cn_abd_free(p)` (behind `bi_abductive`; the defines are
  inert for declarations and correct for calls in the single translation
  unit Fulminate instruments).
- **Runtime** (`bi_abduction.c` + header):
  - `void *cn_abd_malloc(size_t sz)`: call libc malloc, and (B-New) map the
    range in the ghost state at the *current* depth
    (`ownership_ghost_state_set(ptr, sz, get_cn_stack_depth(), NULL)`) so
    subsequent accesses are owned (no events → excluded from `A`).  Record
    the range in the current frame (an `allocs` table) for diagnostics.
  - `void cn_abd_free(void *p)`: look up the allocation size (keep a global
    table `ptr -> size` filled by `cn_abd_malloc`), remove the range from
    the ghost state (`ownership_ghost_state_remove`) so it neither leaks nor
    re-abduces (B-Free), then call libc free.
  - Λ at return then naturally contains still-owned fresh cells (they are at
    the callee's depth) → released to the caller and covered by
    return-anchored post candidates (WI-4).
- No OCaml-side changes beyond expectations.

**Acceptance.**  `mk_node` flips with an *empty pre* (this is the paper's
`N_i` subtraction observable end-to-end); heap dumps around the leaked
malloc'd cell already happen (leak-time dumps), so the post harness can
evaluate `RW<struct node>(return)`.

**Risks.**  The `#define` approach only covers the instrumented TU (fine —
that is Fulminate's scope) and standard names (`malloc`/`calloc`/`free`;
add `calloc` if a test needs it).  `cn_stack_depth` visibility in
`bi_abduction.c`: use `get_cn_stack_depth()` as elsewhere.

## 5. Explicitly deferred (unchanged rationale)

- **Loop invariants** (per-iteration data points; symbolic relation to the
  loop counter) — separate research track.
- **Interprocedural spec reuse** (consuming inferred callee specs during the
  caller's inference; over-approximation propagation across activations —
  `sec-inference.tex`'s open problem, visible today in
  `example_list_append`'s shape-specific post).
- **Guided enumeration / ranking beyond tie-breaks** (traversal summaries,
  memory-graph anchors — original Step 4): re-evaluate after WI-2/WI-6 land;
  candidate counts in commit messages are the tripwire.
- **Deeper derived terms** (arbitrary arithmetic, unexplainable pointer
  boundaries with heap-allocated targets): WI-6's grammar is deliberately
  {args, args±1..2, constants}.
- **Provenance-aware addresses** (`Prop. parametricity` in the paper): the
  runtime collapses to `uintptr_t`; fine for the concrete tool.

## 6. Workflow (how to land each WI)

```
# build              dune build bin/main.exe runtime/libcn/lib @install
# full suite         tests/run-bi-abd.sh          (uses _build/install runtime)
# one test           tests/run-bi-abd.sh <name>.c
# inspect a run      cd $(mktemp -d) && CN_RUNTIME_PREFIX=$REPO/_build/install/default/lib/cn/runtime \
#                      $REPO/_build/default/bin/main.exe bi-abd <file> [-p 3..5]
# artifacts          cn_abd_summary.json, cn_abd_heap.jsonl, bi_abd_fp_<fn>_<phase>.c in the temp dir
# format             dune fmt && dune build @fmt
```

- One commit per WI (plus a separate commit if `dune fmt` reflows untouched
  files).  Commit message: what flipped, why it is sound, candidate-count
  or runtime observations if relevant.  Trailers as in recent history.
- Update, in the same commit: `tests/run-bi-abd.sh` expectations (exact
  observed strings; add `forbidden_for` entries whenever a WI creates a new
  way to be wrong), the flipped tests' header comments,
  `lib/bi_abduction/CLAUDE.md` (module map / limitations), and
  `lib/bi_abduction/TODO.md`.
- If a WI produces an unexpected *wrong* suggestion on any test, stop and
  fix the candidate-validation path before adjusting expectations —
  invariant 2 outranks progress.
