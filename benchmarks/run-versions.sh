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

# worktrees are created once; only the measurement order is interleaved
for pair in "v0 $V0" "v1 $V1" "v2 $V2"; do
  set -- $pair
  git -C "$ROOT" worktree add "$WT/$1" "$2" --detach >/dev/null 2>&1
done

r=1
while [ "$r" -le "$REPS" ]; do
  echo "=== repetition $r/$REPS ==="
  for v in v0 v1 v2; do
    printf '%s\n' "-- $v"
    Rscript "$ROOT/benchmarks/version-benchmark.R" \
      "$WT/$v" "$v" "$STAGE/bench-$v.csv" "$r"
  done
  r=$((r + 1))
done

# only now that every repetition succeeded, publish as one set
rm -f "$OUT"/bench-v*.csv
mv "$STAGE"/bench-v*.csv "$OUT"/

echo
echo "Results in $OUT"
