#!/usr/bin/env bash
# CN_SWITCH and ROCQ_SWITCH allow CN and Rocq to live in different opam switches.
set -euo pipefail
repo=$(cd "$(dirname "$0")/.." && pwd)
cn=${CN:-"$repo/_build/default/bin/main.exe"}
work=$(mktemp -d /tmp/cn-memory-tests.XXXXXX)
printf 'Memory test artifacts: %s\n' "$work"
mkdir -p "$work/lib" "$work/proof"

run_cn() {
  if [[ -n ${CN_SWITCH:-} ]]; then opam exec --switch="$CN_SWITCH" -- "$cn" "$@";
  else "$cn" "$@"; fi
}
run_rocq() {
  if [[ -n ${ROCQ_SWITCH:-} ]]; then opam exec --switch="$ROCQ_SWITCH" -- rocq "$@";
  else rocq "$@"; fi
}

for lib in CN_Lib CN_Memory CN_Memory_Iris CN_Lib_Iris_Fixpoint; do
  cp "$repo/coq/CN_Lemmas/$lib.v" "$work/lib/"
  run_rocq compile -Q "$work/lib" CN_Lemmas "$work/lib/$lib.v" >"$work/$lib.log" 2>&1 || {
    cat "$work/$lib.log"; exit 1;
  }
done
cp "$repo/tests/rocq_lemmas/unit/memory/Memory_Pointer_Tests.v" "$work/"
run_rocq compile -Q "$work/lib" CN_Lemmas "$work/Memory_Pointer_Tests.v" >"$work/pointer.log" 2>&1

run_cn verify --lemmata_coq "$work/proof/Gen_Spec.v" \
  "$repo/tests/rocq_lemmas/cases/memory/memory.c" >"$work/export.log" 2>&1
cp "$repo/tests/rocq_lemmas/proofs/memory/Inst_Spec.v" "$work/proof/"
for file in Gen_Spec Inst_Spec; do
  run_rocq compile -Q "$work/lib" CN_Lemmas -Q "$work/proof" MemoryExport \
    "$work/proof/$file.v" >"$work/$file.log" 2>&1 || {
      cat "$work/$file.log"; exit 1;
    }
done

# A rejected export must preserve an existing destination, not truncate it.
cp "$work/proof/Gen_Spec.v" "$work/protected.v"
if run_cn verify --lemmata_coq "$work/protected.v" \
    "$repo/tests/rocq_lemmas/cases/memory/container_of.c" >"$work/negative-index.log" 2>&1; then
  echo 'Expected unsupported unary index export to fail'; exit 1
fi
rg -q 'Unsupported unop' "$work/negative-index.log"
cmp "$work/proof/Gen_Spec.v" "$work/protected.v"
if run_cn verify --lemmata_coq "$work/copy.v" \
    "$repo/tests/rocq_lemmas/cases/memory/copy_alloc_id.c" >"$work/copy.log" 2>&1; then
  echo 'Expected unavailable annotation builtin to be rejected'; exit 1
fi
rg -q "copy_alloc_id.*not declared" "$work/copy.log"
test ! -e "$work/copy.v"

test "$(rg -c 'Closed under the global context' "$work/pointer.log")" -eq 23
test "$(rg -c 'Closed under the global context' "$work/Inst_Spec.log")" -eq 8
printf 'PASS: 23 model theorems, 8 exported lemmas, 2 rejection regressions.\n'
