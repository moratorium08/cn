#!/usr/bin/env bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
TEST_DIR="$ROOT/tests/bi-abd"
CN_BIN="${CN_BIN:-$ROOT/_build/default/bin/main.exe}"
TMP_RUNTIME_PREFIX=""

build_cn() {
  if [ ! -x "$CN_BIN" ]; then
    (cd "$ROOT" && dune build bin/main.exe >/dev/null)
  fi
}

resolve_runtime_prefix() {
  if [ -n "${CN_RUNTIME_PREFIX:-}" ]; then
    printf '%s\n' "$CN_RUNTIME_PREFIX"
    return 0
  fi

  local installed_prefix="$ROOT/_build/install/default/lib/cn/runtime"
  if [ -f "$installed_prefix/libcn_exec.a" ] && [ -d "$installed_prefix/include" ]; then
    printf '%s\n' "$installed_prefix"
    return 0
  fi

  local built_prefix="$ROOT/_build/default/runtime/libcn"
  if [ -f "$built_prefix/lib/libcn_exec.a" ] && [ -d "$built_prefix/include" ]; then
    TMP_RUNTIME_PREFIX=$(mktemp -d)
    mkdir -p "$TMP_RUNTIME_PREFIX/include"
    cp -R "$built_prefix/include/." "$TMP_RUNTIME_PREFIX/include/"
    cp "$built_prefix/lib/libcn_exec.a" "$TMP_RUNTIME_PREFIX/libcn_exec.a"
    printf '%s\n' "$TMP_RUNTIME_PREFIX"
    return 0
  fi

  printf 'Could not locate CN runtime artifacts.\n' >&2
  printf 'Set CN_RUNTIME_PREFIX or build/install the runtime first.\n' >&2
  exit 1
}

cleanup() {
  if [ -n "$TMP_RUNTIME_PREFIX" ] && [ -d "$TMP_RUNTIME_PREFIX" ]; then
    rm -rf "$TMP_RUNTIME_PREFIX"
  fi
}

expectations_for() {
  case "$1" in
    baseline_multi_call_list.c)
      printf '%s\n' \
        '/* Function: list_length */' \
        'take _ = IntList(p);'
      ;;
    step0_interval_owner.c)
      printf '%s\n' \
        '/* Function: read_cell */' \
        'take _ = RW<signed int>(p);' \
        '/* Function: caller */'
      ;;
    step0_partial_spec_b.c)
      printf '%s\n' \
        '/* Function: list_length */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    step2_null_guard.c)
      printf '%s\n' \
        '/* Function: first_val */' \
        'take _ = IntList(p);'
      ;;
    step3_chain_depth_limit.c)
      printf '%s\n' \
        '/* Function: deep_length */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    example_list_sum.c)
      printf '%s\n' \
        '/* Function: list_sum */' \
        'take _ = IntList(p);'
      ;;
    example_list_find.c)
      printf '%s\n' \
        '/* Function: list_contains */' \
        'take _ = IntList(p);'
      ;;
    example_list_append.c)
      printf '%s\n' \
        '/* Function: list_append */' \
        'take _ = IntList(a);' \
        'take a_W = RW<struct node>(a);' \
        'take _ = RW<struct node>(a_W.next);'
      ;;
    example_list_reverse.c)
      printf '%s\n' \
        '/* Function: list_reverse */' \
        'take _ = IntList(p);' \
        '/* postcondition inference failed */'
      ;;
    example_tree_sum.c)
      printf '%s\n' \
        '/* Function: tree_sum */' \
        'take _ = Tree(t);'
      ;;
    example_tree_search.c)
      printf '%s\n' \
        '/* Function: bst_contains */' \
        'take _ = Tree(t);'
      ;;
    example_tree_mirror.c)
      printf '%s\n' \
        '/* Function: tree_mirror */' \
        'take _ = Tree(t);'
      ;;
    example_stack_peek.c)
      printf '%s\n' \
        '/* Function: stack_size */' \
        'take s_W = RW<struct stack>(s);' \
        'take _ = IntList(s_W.top);'
      ;;
    example_dlist_length.c)
      printf '%s\n' \
        '/* Function: dlist_length */' \
        'take _ = DList(p);'
      ;;
    example_rotate3.c)
      printf '%s\n' \
        '/* Function: rotate3 */' \
        'take _ = RW<signed int>(a);' \
        'take _ = RW<signed int>(b);' \
        'take _ = RW<signed int>(c);'
      ;;
    example_list_min_max.c)
      printf '%s\n' \
        '/* Function: list_min_max */' \
        'take _ = IntList(p);' \
        'take _ = RW<signed int>(out_min);' \
        'take _ = RW<signed int>(out_max);'
      ;;
    example_hard_array_fill.c)
      printf '%s\n' \
        '/* Function: array_fill */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    example_hard_listseg_boundary.c)
      printf '%s\n' \
        '/* Function: walk3 */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    example_hard_ptr_chain.c)
      printf '%s\n' \
        '/* Function: deref2 */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    example_hard_malloc_constructor.c)
      printf '%s\n' \
        '/* Function: mk_node */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    example_hard_global_counter.c)
      printf '%s\n' \
        '/* Function: tick */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    example_hard_cycle.c)
      printf '%s\n' \
        '/* Function: cycle_length */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_array_two_cells.c)
      printf '%s\n' \
        '/* Function: sum2 */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_global_list_anchor.c)
      printf '%s\n' \
        '/* Function: global_length */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_nested_scalar_chain.c)
      printf '%s\n' \
        '/* Function: nested_load */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_sized_list_constant.c)
      printf '%s\n' \
        '/* Function: list_sum3 */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_void_cast_scalar.c)
      printf '%s\n' \
        '/* Function: load_void */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_array_arg_index.c)
      printf '%s\n' \
        '/* Function: load_at */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_field_boundary_segment.c)
      printf '%s\n' \
        '/* Function: segment_sum */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_list_via_pointer_pointer.c)
      printf '%s\n' \
        '/* Function: indirect_length */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_sized_list_arg_plus_one.c)
      printf '%s\n' \
        '/* Function: length_plus_one_case */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    adversarial_struct_array_element.c)
      printf '%s\n' \
        '/* Function: second_pair_sum */' \
        '/* precondition inference failed */' \
        '/* postcondition inference failed */'
      ;;
    baseline_pair_pre_post.c)
      printf '%s\n' \
        '/* Function: sum_pair */' \
        'take _ = RW<struct pair>(p);'
      ;;
    baseline_wrapper_lists.c)
      printf '%s\n' \
        '/* Function: total_length */' \
        'take b_W = RW<struct list_pair>(b);' \
        'take _ = IntList(b_W.xs);' \
        'take _ = IntList(b_W.ys);'
      ;;
    extra_iarg_name_capture.c)
      printf '%s\n' \
        '/* Function: seg_length */' \
        'take _ = IntListSeg(xs, end);'
      ;;
    extra_nonrecursive_predicate_ignored.c)
      printf '%s\n' \
        '/* Function: sum_pair */' \
        'take _ = RW<struct pair>(p);'
      ;;
    extra_null_boundary_argument.c)
      printf '%s\n' \
        '/* Function: list_length */' \
        'take _ = IntListSeg(xs, NULL);'
      ;;
    extra_predicate_body_ignored.c)
      printf '%s\n' \
        '/* Function: list_length */' \
        'take p_W = RW<struct node>(p);' \
        'take _ = RW<struct node>(p_W.next);'
      ;;
    extra_scalar_pointer_missing.c)
      printf '%s\n' \
        '/* Function: load_int */' \
        'take _ = RW<signed int>(p);'
      ;;
    extra_wrong_struct_type.c)
      printf '%s\n' \
        '/* Function: sum_alpha */' \
        'take _ = RW<struct alpha>(p);'
      ;;
    *)
      printf 'No expectations registered for %s\n' "$1" >&2
      return 1
      ;;
  esac
}

# Multi-line fixed strings that must NOT appear in the output (matched as a
# plain substring, so patterns may span lines).  Empty for most tests.
forbidden_for() {
  case "$1" in
    extra_predicate_body_ignored.c)
      # the observed list has positive values, so the take semantics
      # rejects NegList (assert H.val < 0 fails)
      printf 'NegList(p);'
      ;;
    step0_interval_owner.c)
      # caller already owns *p via its own precondition; the interval rule
      # must not abduce it into caller's anti-frame (no suggestions at all).
      printf '/* Function: caller */\n  /* Suggested'
      ;;
    step0_partial_spec_b.c)
      # IntList(p) overlaps the node the user's spec already owns
      # (sandwich upper bound B); it must not be suggested.
      printf 'take _ = IntList(p);'
      ;;
    step2_null_guard.c)
      # RW<struct node>(p) is undefined on the NULL activation, so (†)
      # rejects it; only the guarded predicate may be suggested.
      printf 'take _ = RW<struct node>(p);'
      ;;
    *)
      ;;
  esac
}

run_case() {
  local test_name="$1"
  local test_path="$TEST_DIR/$test_name"
  if [ ! -f "$test_path" ]; then
    printf 'Missing test case: %s\n' "$test_path" >&2
    return 1
  fi

  local tmp_dir
  tmp_dir=$(mktemp -d)
  local output=""
  local status=0
  if ! output=$(cd "$tmp_dir" && CN_RUNTIME_PREFIX="$RUNTIME_PREFIX" "$CN_BIN" bi-abd "$test_path" 2>&1); then
    status=$?
  fi
  rm -rf "$tmp_dir"

  if [ "$status" -ne 0 ]; then
    printf '[FAIL] %s\n' "$test_name"
    printf '%s\n' "$output"
    return 1
  fi

  local missing=0
  while IFS= read -r needle; do
    if ! grep -Fq "$needle" <<<"$output"; then
      if [ "$missing" -eq 0 ]; then
        printf '[FAIL] %s\n' "$test_name"
      fi
      missing=1
      printf '  missing: %s\n' "$needle"
    fi
  done < <(expectations_for "$test_name")

  local forbidden
  forbidden=$(forbidden_for "$test_name")
  if [ -n "$forbidden" ] && [[ "$output" == *"$forbidden"* ]]; then
    if [ "$missing" -eq 0 ]; then
      printf '[FAIL] %s\n' "$test_name"
    fi
    missing=1
    printf '  forbidden output present: %s\n' "$forbidden"
  fi

  if [ "$missing" -ne 0 ]; then
    printf '%s\n' "$output"
    return 1
  fi

  printf '[PASS] %s\n' "$test_name"
}

main() {
  build_cn
  RUNTIME_PREFIX=$(resolve_runtime_prefix)
  trap cleanup EXIT

  local -a tests
  if [ "$#" -gt 0 ]; then
    tests=("$@")
  else
    tests=(
      baseline_multi_call_list.c
      baseline_pair_pre_post.c
      baseline_wrapper_lists.c
      extra_iarg_name_capture.c
      extra_nonrecursive_predicate_ignored.c
      extra_null_boundary_argument.c
      extra_predicate_body_ignored.c
      extra_scalar_pointer_missing.c
      extra_wrong_struct_type.c
      step0_interval_owner.c
      step0_partial_spec_b.c
      step2_null_guard.c
      step3_chain_depth_limit.c
      example_list_sum.c
      example_list_find.c
      example_list_append.c
      example_list_reverse.c
      example_tree_sum.c
      example_tree_search.c
      example_tree_mirror.c
      example_stack_peek.c
      example_dlist_length.c
      example_rotate3.c
      example_list_min_max.c
      example_hard_array_fill.c
      example_hard_listseg_boundary.c
      example_hard_ptr_chain.c
      example_hard_malloc_constructor.c
      example_hard_global_counter.c
      example_hard_cycle.c
      adversarial_array_two_cells.c
      adversarial_global_list_anchor.c
      adversarial_nested_scalar_chain.c
      adversarial_sized_list_constant.c
      adversarial_void_cast_scalar.c
      adversarial_array_arg_index.c
      adversarial_field_boundary_segment.c
      adversarial_list_via_pointer_pointer.c
      adversarial_sized_list_arg_plus_one.c
      adversarial_struct_array_element.c
    )
  fi

  local failed=0
  local test_name
  for test_name in "${tests[@]}"; do
    if ! run_case "$test_name"; then
      failed=1
    fi
  done

  exit "$failed"
}

main "$@"
