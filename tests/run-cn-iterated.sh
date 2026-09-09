#!/usr/bin/env bash
# The optional INTEGER_CN must be cp526/integers with this exporter overlay,
# not a main checkout with only cnBV toggled. All build products stay in /tmp.
set -euo pipefail
repo=$(cd "$(dirname "$0")/.." && pwd)
cn=${CN:-"$repo/_build/default/bin/main.exe"}
work=$(mktemp -d /tmp/cn-iterated-tests.XXXXXX)
printf 'Iterated test artifacts: %s\n' "$work"
mkdir -p "$work/lib" "$work/proof"
run_cn() {
  if [[ -n ${CN_SWITCH:-} ]]; then opam exec --switch="$CN_SWITCH" -- "$@";
  else "$@"; fi
}
run_rocq() {
  if [[ -n ${ROCQ_SWITCH:-} ]]; then opam exec --switch="$ROCQ_SWITCH" -- rocq "$@";
  else rocq "$@"; fi
}
compile() {
  local file=$1
  run_rocq compile -Q "$work/lib" CN_Lemmas -Q "$work/proof" IteratedExport \
    "$file" >"${file%.v}.log" 2>&1 || { cat "${file%.v}.log"; exit 1; }
}
closed() {
  test "$(rg -c '^Closed under the global context$' "$1")" -eq "$2"
}
for lib in CN_Lib CN_Memory CN_Memory_Iris CN_Lib_Iris_Fixpoint; do
  cp "$repo/coq/CN_Lemmas/$lib.v" "$work/lib/"
  compile "$work/lib/$lib.v"
done
for test in Iterated_Resource_Tests Integer_Branch_Tests; do
  cp "$repo/tests/rocq_lemmas/unit/memory/$test.v" "$work/"
  compile "$work/$test.v"
done
closed "$work/Iterated_Resource_Tests.log" 9
closed "$work/Integer_Branch_Tests.log" 4

run_cn "$cn" verify "$repo/tests/rocq_lemmas/cases/memory/iterated.c" \
  --lemmata_coq "$work/proof/Iterated.v" >"$work/export.log" 2>&1
cp "$repo/tests/rocq_lemmas/proofs/memory/Iterated_Proof.v" "$work/proof/"
compile "$work/proof/Iterated.v"
compile "$work/proof/Iterated_Proof.v"
closed "$work/proof/Iterated_Proof.log" 5
# Stride and element size are independent; identity proofs alone could miss
# an exporter that made the same wrong substitution on both sides.
rg -U -q 'BlockSized 4%nat\s+\(CN_Lib_Iris.Memory.arrayshift p 8 i\)' "$work/proof/Iterated.v"
rg -q 'i <= 18446744073709551615' "$work/proof/Iterated.v"
if rg -q 'each_int|\bnth\b|Block_before|Block_after' "$work/proof/Iterated.v"; then
  echo 'Unexpected legacy iterated encoding'; exit 1
fi
cp "$work/proof/Iterated.v" "$work/protected.v"
if run_cn "$cn" verify "$repo/tests/rocq_lemmas/cases/memory/iterated_w_output.c" \
    --lemmata_coq "$work/protected.v" >"$work/rejected.log" 2>&1; then
  echo 'Expected iterated W output use to be rejected'; exit 1
fi
rg -q 'Unsupported use of iterated W output' "$work/rejected.log"
cmp "$work/proof/Iterated.v" "$work/protected.v"
printf 'PASS: 13 model theorems, 5 bitvector exported proofs, W-output rejection.\n'

if [[ -n ${INTEGER_CN:-} ]]; then
  run_cn "$INTEGER_CN" verify "$repo/tests/rocq_lemmas/cases/memory/iterated_integer.c" \
    --lemmata_coq "$work/proof/IteratedInteger.v" >"$work/integer-export.log" 2>&1
  rg -q 'Definition bitvectors := false' "$work/proof/IteratedInteger.v"
  cp "$repo/tests/rocq_lemmas/proofs/memory/IteratedInteger_Proof.v" "$work/proof/"
  compile "$work/proof/IteratedInteger.v"
  compile "$work/proof/IteratedInteger_Proof.v"
  closed "$work/proof/IteratedInteger_Proof.log" 3
  printf 'PASS: 3 integer-mode exported proofs (negative sparse indices and signed RW).\n'
else
  echo 'SKIP: integer frontend export checks (set INTEGER_CN).'
fi
