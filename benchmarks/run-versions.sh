#!/bin/sh
# Benchmark stagedtrees across the three versions discussed in
# paper/ai-assisted-maintenance.Rmd.
#
#   v0  state before this work began
#   v1  after the defect fixes (PR #150, #139)
#   v2  after the optimisations (PR #151)
#
# Repetitions are the OUTER loop and versions the INNER loop, so the
# versions are interleaved in time. Running all repetitions of one version
# before the next confounds machine drift with the version under test; that
# mistake produced a spurious 13.7% result once already. Do not "optimise"
# this by hoisting the worktree setup out of the repetition loop in a way
# that reintroduces blocking.
#
# Usage:  sh benchmarks/run-versions.sh [reps]

# Each version is INSTALLED with R CMD INSTALL into its own library and
# benchmarked from there. Do not switch this back to pkgload::load_all on the
# worktree: load_all compiles with the debug flags (-O0) while R CMD INSTALL
# uses -O2, a 3.9x difference on this package's compiled kernels, so a
# load_all harness would understate every version carrying src/ -- which is
# precisely the version the benchmark exists to measure.
#
# Results are written to a staging directory and moved into place only once
# the whole run completes, so a run in progress never leaves the working tree
# holding a partial, internally inconsistent set of results. A partial set is
# worse than none: the manuscript reads these files, and half a run mixes
# repetitions across versions unevenly.

set -e
REPS="${1:-8}"
ROOT=$(git rev-parse --show-toplevel)
WT=$(mktemp -d)
OUT="$ROOT/benchmarks/results"
STAGE="$WT/staged"
mkdir -p "$OUT" "$STAGE"

V0=654e8c5
V1=0e3c152
V2=6703428

cleanup() {
  for v in v0 v1 v2; do
    git -C "$ROOT" worktree remove "$WT/$v" --force 2>/dev/null || true
  done
  rm -rf "$WT"
}
trap cleanup EXIT

# worktrees are created and installed once; only the measurement order is
# interleaved. Installing inside the repetition loop would put compilation
# time into the same drift budget the interleaving exists to control.
for pair in "v0 $V0" "v1 $V1" "v2 $V2"; do
  set -- $pair
  git -C "$ROOT" worktree add "$WT/$1" "$2" --detach >/dev/null 2>&1
  mkdir -p "$WT/lib-$1"
  echo "installing $1 ($2)"
  R CMD INSTALL "$WT/$1" -l "$WT/lib-$1" --preclean > "$WT/install-$1.log" 2>&1 || {
    echo "FAILED to install $1 ($2); see $WT/install-$1.log" >&2
    tail -20 "$WT/install-$1.log" >&2
    exit 1
  }
done

r=1
while [ "$r" -le "$REPS" ]; do
  echo "=== repetition $r/$REPS ==="
  for v in v0 v1 v2; do
    printf '%s\n' "-- $v"
    Rscript "$ROOT/benchmarks/version-benchmark.R" \
      "$WT/lib-$v" "$v" "$STAGE/bench-$v.csv" "$r"
  done
  r=$((r + 1))
done

# only now that every repetition succeeded, publish as one set
rm -f "$OUT"/bench-v*.csv
mv "$STAGE"/bench-v*.csv "$OUT"/

echo
echo "Results in $OUT"
