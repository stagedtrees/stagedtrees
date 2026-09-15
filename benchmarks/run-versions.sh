#!/bin/sh
# Benchmark stagedtrees across the three versions discussed in
# paper/ai-assisted-maintenance.Rmd.
#
#   v0  state before this work began
#   v1  after the defect fixes (PR #150, #139)
#   v2  after the optimisations (PR #151)
#
# Creates detached worktrees, benchmarks each, writes CSVs to
# benchmarks/results/, then removes the worktrees.
#
# Usage:  sh benchmarks/run-versions.sh [reps]

set -e
REPS="${1:-5}"
ROOT=$(git rev-parse --show-toplevel)
WT=$(mktemp -d)
OUT="$ROOT/benchmarks/results"
mkdir -p "$OUT"

V0=654e8c5
V1=0e3c152
V2=c3f5856

cleanup() {
  for v in v0 v1 v2; do
    git -C "$ROOT" worktree remove "$WT/$v" --force 2>/dev/null || true
  done
  rm -rf "$WT"
}
trap cleanup EXIT

for pair in "v0 $V0" "v1 $V1" "v2 $V2"; do
  set -- $pair
  label=$1; rev=$2
  git -C "$ROOT" worktree add "$WT/$label" "$rev" --detach >/dev/null 2>&1
  echo "=== $label ($rev) ==="
  Rscript "$ROOT/benchmarks/version-benchmark.R" \
    "$WT/$label" "$label" "$OUT/bench-$label.csv" "$REPS"
done

echo
echo "Results in $OUT"
