#!/usr/bin/env bash
# Memory-model regression for the cp526/integers baseline: C integer types are
# mathematical integers, so every exported fixture here is integer-mode.
# CN_SWITCH and ROCQ_SWITCH allow CN and Rocq to live in different opam switches.
set -euo pipefail
repo=$(cd "$(dirname "$0")/.." && pwd)
cn=${CN:-"$repo/_build/default/bin/main.exe"}
cases=$repo/tests/rocq_lemmas/cases/memory
proofs=$repo/tests/rocq_lemmas/proofs/memory
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
compile() {
  local file=$1
  run_rocq compile -Q "$work/lib" CN_Lemmas -Q "$work/proof" MemoryExport \
    "$file" >"${file%.v}.log" 2>&1 || { cat "${file%.v}.log"; exit 1; }
}
closed() {
  test "$(grep -c '^Closed under the global context$' "$1")" -eq "$2"
}

for lib in CN_Lib CN_Memory CN_Memory_Iris CN_Lib_Iris_Fixpoint; do
  cp "$repo/coq/CN_Lemmas/$lib.v" "$work/lib/"
  compile "$work/lib/$lib.v"
done
cp "$repo/tests/rocq_lemmas/unit/memory/Memory_Pointer_Tests.v" "$work/"
compile "$work/Memory_Pointer_Tests.v"
closed "$work/Memory_Pointer_Tests.log" 23

# Pointer model, struct ownership and qualified struct projections.
run_cn verify "$cases/memory.c" --lemmata_coq "$work/proof/Gen_Spec.v" \
  >"$work/export.log" 2>&1
grep -q 'Definition bitvectors := false' "$work/proof/Gen_Spec.v"
grep -q 'memory_probe_only_node' "$work/proof/Gen_Spec.v"
cp "$proofs/Inst_Spec.v" "$work/proof/"
compile "$work/proof/Gen_Spec.v"
compile "$work/proof/Inst_Spec.v"
closed "$work/proof/Inst_Spec.log" 9

# Integer arithmetic follows lib/solver.ml (SMT-LIB div/mod, Z3 rem, shifts).
run_cn verify "$cases/integer_arith.c" --lemmata_coq "$work/proof/Arith_Spec.v" \
  >"$work/arith-export.log" 2>&1
grep -q 'CN_Lib.div_smt' "$work/proof/Arith_Spec.v"
if grep -qE '\(\(a / b\)|\(a mod b\)' "$work/proof/Arith_Spec.v"; then
  echo 'Integer division exported with Rocq floor semantics'; exit 1
fi
cp "$proofs/Arith_Proof.v" "$work/proof/"
compile "$work/proof/Arith_Spec.v"
compile "$work/proof/Arith_Proof.v"
closed "$work/proof/Arith_Proof.log" 5

# A rejected export must preserve an existing destination, not truncate it.
cp "$work/proof/Gen_Spec.v" "$work/protected.v"
if run_cn verify "$cases/reject_clz.c" --lemmata_coq "$work/protected.v" \
    >"$work/reject-clz.log" 2>&1; then
  echo 'Expected unsupported bitvector clz export to fail'; exit 1
fi
grep -q 'Unsupported unop' "$work/reject-clz.log"
cmp "$work/proof/Gen_Spec.v" "$work/protected.v"
if run_cn verify "$cases/copy_alloc_id.c" --lemmata_coq "$work/copy.v" \
    >"$work/copy.log" 2>&1; then
  echo 'Expected unavailable annotation builtin to be rejected'; exit 1
fi
grep -q "copy_alloc_id.*not declared" "$work/copy.log"
test ! -e "$work/copy.v"

printf 'PASS: 23 model theorems, 9 memory + 5 arithmetic exported lemmas, 2 rejection regressions.\n'
