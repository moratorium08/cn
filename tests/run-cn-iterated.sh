#!/usr/bin/env bash
# Iterated-ownership regression for the cp526/integers baseline. The integer
# fixtures are mandatory. The bitvector fixtures need a frontend with
# BaseTypes.cnBV=true carrying this exporter; pass it as BITVECTOR_CN.
# All build products stay in /tmp.
set -euo pipefail
repo=$(cd "$(dirname "$0")/.." && pwd)
cn=${CN:-"$repo/_build/default/bin/main.exe"}
cases=$repo/tests/rocq_lemmas/cases/memory
proofs=$repo/tests/rocq_lemmas/proofs/memory
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
  test "$(grep -c '^Closed under the global context$' "$1")" -eq "$2"
}
one_line() { tr -s '[:space:]' ' ' <"$1"; }

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

run_cn "$cn" verify "$cases/iterated_integer.c" \
  --lemmata_coq "$work/proof/IteratedInteger.v" >"$work/integer-export.log" 2>&1
grep -q 'CN_Memory.IntegerAddress CN_ExportWidth' "$work/proof/IteratedInteger.v"
grep -q 'CN_ExportProvenance := CN_Memory.VIP' "$work/proof/IteratedInteger.v"
# Stride and element size are independent; identity proofs alone could miss
# an exporter that made the same wrong substitution on both sides.
one_line "$work/proof/IteratedInteger.v" |
  grep -q 'BlockSized 4%nat (CN_Lib_Iris.Memory.arrayshift p 8 i)'
if grep -qE 'each_int|\bnth\b|Block_before|Block_after|0 <= i\)?\)? ∧ \(i <= 18446744073709551615' \
    "$work/proof/IteratedInteger.v"; then
  echo 'Unexpected legacy or artificially bounded iterated encoding'; exit 1
fi
cp "$proofs/IteratedInteger_Proof.v" "$work/proof/"
compile "$work/proof/IteratedInteger.v"
compile "$work/proof/IteratedInteger_Proof.v"
closed "$work/proof/IteratedInteger_Proof.log" 3

# W maps must be bound in all three resource contexts, without constraining
# their values to zero or relating them to readable initialized contents.
run_cn "$cn" verify "$cases/iterated_w_ghost.c" \
  --lemmata_coq "$work/proof/WGhost.v" >"$work/w-ghost-export.log" 2>&1
cp "$proofs/WGhost_Proof.v" "$work/proof/"
compile "$work/proof/WGhost.v"
compile "$work/proof/WGhost_Proof.v"
closed "$work/proof/WGhost_Proof.log" 4
run_cn "$cn" verify "$cases/iterated_w_output.c" \
  --lemmata_coq "$work/proof/WOutput.v" >"$work/w-output-export.log" 2>&1
cp "$proofs/WOutput_Obstruction.v" "$work/proof/"
compile "$work/proof/WOutput.v"
compile "$work/proof/WOutput_Obstruction.v"
closed "$work/proof/WOutput_Obstruction.log" 1
printf 'PASS: 13 model theorems, 3 integer-mode + 4 W ghost-map proofs, zero-output obstruction.\n'

# --no-vip is how the pKVM allocator is verified: the export must select the
# NoVIP provenance module (AllocId = unit) and the same proofs must still close.
mkdir -p "$work/novip"
run_cn "$cn" verify "$cases/iterated_integer.c" --no-vip \
  --lemmata_coq "$work/novip/IteratedInteger.v" >"$work/novip-export.log" 2>&1
grep -q 'CN_ExportProvenance := CN_Memory.NoVIP' "$work/novip/IteratedInteger.v"
cp "$proofs/IteratedInteger_Proof.v" "$work/novip/"
run_rocq compile -Q "$work/lib" CN_Lemmas -Q "$work/novip" IteratedExport \
  "$work/novip/IteratedInteger.v" >"$work/novip/IteratedInteger.log" 2>&1 ||
  { cat "$work/novip/IteratedInteger.log"; exit 1; }
run_rocq compile -Q "$work/lib" CN_Lemmas -Q "$work/novip" IteratedExport \
  "$work/novip/IteratedInteger_Proof.v" >"$work/novip/IteratedInteger_Proof.log" 2>&1 ||
  { cat "$work/novip/IteratedInteger_Proof.log"; exit 1; }
closed "$work/novip/IteratedInteger_Proof.log" 3
printf 'PASS: 3 integer-mode exported proofs under --no-vip.\n'

run_cn "$cn" verify "$cases/iterated_w_ghost.c" --no-vip \
  --lemmata_coq "$work/novip/WGhost.v" >"$work/novip-w-ghost-export.log" 2>&1
cp "$proofs/WGhost_Proof.v" "$work/novip/"
for file in WGhost WGhost_Proof; do
  run_rocq compile -Q "$work/lib" CN_Lemmas -Q "$work/novip" IteratedExport \
    "$work/novip/$file.v" >"$work/novip/$file.log" 2>&1 ||
    { cat "$work/novip/$file.log"; exit 1; }
done
closed "$work/novip/WGhost_Proof.log" 4
printf 'PASS: 4 W ghost-map proofs under --no-vip.\n'

# Named predicates may own nothing at NULL. Recursive calls inside each
# must also satisfy the generated monotonicity and induction obligations.
for mode in vip novip; do
  flags=()
  if [[ $mode == novip ]]; then flags=(--no-vip); fi
  run_cn "$cn" verify "${flags[@]}" "$cases/iterated_named.c" \
    --lemmata_coq "$work/proof/IteratedNamed.v" >"$work/$mode-named-export.log" 2>&1
  cp "$proofs/IteratedNamed_Proof.v" "$work/proof/"
  compile "$work/proof/IteratedNamed.v"
  compile "$work/proof/IteratedNamed_Proof.v"
  closed "$work/proof/IteratedNamed_Proof.log" 5
  printf 'PASS: 5 iterated named-predicate proofs (%s).\n' "$mode"
done

if [[ -n ${BITVECTOR_CN:-} ]]; then
  run_cn "$BITVECTOR_CN" verify "$cases/iterated_bitvector.c" \
    --lemmata_coq "$work/proof/IteratedBitvector.v" >"$work/bitvector-export.log" 2>&1
  grep -q 'CN_Memory.BoundedAddress CN_ExportWidth' "$work/proof/IteratedBitvector.v"
  one_line "$work/proof/IteratedBitvector.v" |
    grep -q 'BlockSized 4%nat (CN_Lib_Iris.Memory.arrayshift p 8 i)'
  grep -q 'i <= 18446744073709551615' "$work/proof/IteratedBitvector.v"
  cp "$proofs/IteratedBitvector_Proof.v" "$work/proof/"
  compile "$work/proof/IteratedBitvector.v"
  compile "$work/proof/IteratedBitvector_Proof.v"
  closed "$work/proof/IteratedBitvector_Proof.log" 5
  for mode in vip novip; do
    flags=()
    if [[ $mode == novip ]]; then flags=(--no-vip); fi
    run_cn "$BITVECTOR_CN" verify "${flags[@]}" "$cases/iterated_w_ranges_bitvector.c" \
      --lemmata_coq "$work/proof/WBitvector.v" >"$work/$mode-w-bitvector-export.log" 2>&1
    cp "$proofs/WBitvector_Proof.v" "$work/proof/"
    compile "$work/proof/WBitvector.v"
    compile "$work/proof/WBitvector_Proof.v"
    closed "$work/proof/WBitvector_Proof.log" 4
    run_cn "$BITVECTOR_CN" verify "${flags[@]}" "$cases/iterated_w_output_bitvector.c" \
      --lemmata_coq "$work/proof/WBitvectorOutput.v" >"$work/$mode-bitvector-w-output.log" 2>&1
    cp "$proofs/WBitvector_Obstruction.v" "$work/proof/"
    compile "$work/proof/WBitvectorOutput.v"
    compile "$work/proof/WBitvector_Obstruction.v"
    closed "$work/proof/WBitvector_Obstruction.log" 1
    printf 'PASS: 4 bitvector W range proofs and zero-output obstruction (%s).\n' "$mode"
  done
  printf 'PASS: 5 existing bitvector-mode exported proofs.\n'
else
  echo 'SKIP: bitvector frontend export checks (set BITVECTOR_CN).'
fi
