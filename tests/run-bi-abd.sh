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
        '/* inference failed */'
      ;;
    baseline_pair_pre_post.c)
      printf '%s\n' \
        '/* Function: sum_pair */' \
        'take _ = RW<struct pair>(p);'
      ;;
    baseline_wrapper_lists.c)
      printf '%s\n' \
        '/* Function: total_length */' \
        'take _ = IntList(b);'
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
        'take _ = IntListSeg(xs, xs);'
      ;;
    extra_predicate_body_ignored.c)
      printf '%s\n' \
        '/* Function: list_length */' \
        'take _ = NegList(p);'
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
