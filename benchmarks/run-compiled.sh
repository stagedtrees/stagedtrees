#!/bin/sh
# Measure the two search functions before and after the compiled selection
# kernels, across model dimension. Produces benchmarks/results/compiled.csv,
# which paper/ai-assisted-maintenance.Rmd reads.
#
#   rver  the last pure-R version (v1, after the defect fixes)
#   cver  the version with the compiled kernels (v2)
#
# Each cell runs as its own process under timeout(1), because the pure-R
# searches do not complete at the larger sizes and that non-completion is
# itself the result being reported. A cell that is killed is recorded with
# seconds = NA and the cap in the `cap` column, so the manuscript can say
# "did not finish within N seconds" rather than silently dropping the row.
#
# Usage: sh benchmarks/run-compiled.sh [cap-seconds]

set -e
CAP="${1:-600}"
ROOT=$(git rev-parse --show-toplevel)
WT=$(mktemp -d)
OUT="$ROOT/benchmarks/results"
STAGE="$WT/compiled.csv"
mkdir -p "$OUT"

RVER=0e3c152
CVER=07e14af

cleanup() {
  for v in rver cver; do
    git -C "$ROOT" worktree remove "$WT/$v" --force 2>/dev/null || true
  done
  rm -rf "$WT"
}
trap cleanup EXIT

for pair in "rver $RVER" "cver $CVER"; do
  set -- $pair
  git -C "$ROOT" worktree add "$WT/$1" "$2" --detach >/dev/null 2>&1
  mkdir -p "$WT/lib-$1"
  echo "installing $1 ($2)"
  R CMD INSTALL "$WT/$1" -l "$WT/lib-$1" --preclean > "$WT/install-$1.log" 2>&1 || {
    echo "FAILED to install $1 ($2)" >&2; tail -20 "$WT/install-$1.log" >&2; exit 1
  }
done

record_na() {
  # $1 label  $2 fun  $3 p
  if [ ! -f "$STAGE" ]; then
    echo '"version","fun","p","nobs","lv","seconds"' > "$STAGE"
  fi
  echo "\"$1\",\"$2\",$3,5000,3,NA" >> "$STAGE"
  echo "  $1 $2 p=$3  did not finish within ${CAP}s"
}

for cell in "stages_bhc 4" "stages_bhc 5" "stages_bhc 6" "stages_bhc 7" \
            "stages_hc 4" "stages_hc 5" "stages_hc 6"; do
  set -- $cell
  fun=$1; p=$2
  for v in rver cver; do
    if timeout "$CAP" Rscript "$ROOT/benchmarks/compiled-kernels.R" \
         "$WT/lib-$v" "$v" "$fun" "$p" "$STAGE"; then
      :
    else
      st=$?
      # 124 is timeout(1)'s own exit code for "the command timed out"
      if [ "$st" -eq 124 ]; then record_na "$v" "$fun" "$p"; else
        echo "ERROR: $v $fun p=$p exited $st" >&2; exit 1
      fi
    fi
  done
done

# publish only once every cell is accounted for
printf 'cap\n%s\n' "$CAP" > "$OUT/compiled-cap.txt"
mv "$STAGE" "$OUT/compiled.csv"
echo
echo "Results in $OUT/compiled.csv"
