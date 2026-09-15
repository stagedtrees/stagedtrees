# Performance investigation: where the time goes, and whether Rcpp helps

All numbers below are **measured** on this machine (R 4.3.3, Linux x86_64), not
estimated. Scripts to reproduce are in this directory.

**Headline: the two biggest wins are pure R, not Rcpp.** Rcpp is worth it in
exactly one shape — moving a *whole loop* into C — and is close to useless for
porting individual helper functions, because `.Call` overhead (~3 µs measured)
is larger than the work those helpers do.

---

## 1. Where the time actually goes

`Rprof` on `stages_bhc(full(2000 obs x 6 ternary vars))`, 330 s total
(`01-profile-stages_bhc.R`):

| function | self % | total % |
|---|---|---|
| `join_stages_unsafe` | 55.98 | **87.21** |
| `$` | 11.00 | 11.00 |
| `ifelse` | 7.15 | 13.42 |
| `which` | 3.34 | 5.45 |
| `$<-` | 3.12 | 3.12 |

User-facing paths (20 000 obs, 8 vars, 4 levels):

| call | time |
|---|---|
| `sample_from(n = 20000)` | 16.05 s |
| `prob(5000 rows)` | 4.23 s |
| `full()` | 3.99 s |
| `sevt_fit()` | 0.01 s |

`Rprof` on those two (`03-profile-sample_from-prob.R`): `find_stage` is **59 %**
of `sample_from` and **22 %** of `prob`; `tree_idx` is 47 % of `sample_from`.

So there are two independent hot spots: the **BHC search family** and
**`tree_idx`/`find_stage`**.

---

## 2. Finding A — BHC copies a whole model to extract two numbers

**7.4× end-to-end, bit-identical output. Pure R.**

`stages_bhc` evaluates every candidate stage pair like this:

```r
try <- join_stages_unsafe(object, v, s1, s2)   # builds a FULL modified sevt
try_score <- score(try)                        # reads two scalars out of it
```

`join_stages_unsafe` returns a complete `sevt` — it copies `$stages`,
rebuilds the nested `$prob` list, and carries `$ctables` along (340 KB total for
the profiled model, of which `$prob` is 244 KB). The object is then discarded
for every pair except the winner.

But `score()` only ever needs **two scalars**: the change in log-likelihood and
the change in degrees of freedom. And `ddf` is not even data-dependent — it is
always `-(k - 1)` where `k` is the number of levels.

Measured cost per candidate pair:

| | µs/call |
|---|---|
| `join_stages_unsafe` + `BIC` (current) | 117.55 |
| delta only, lookups hoisted out of the loop | 16.70 |
| — irreducible: two list lookups | 1.20 |
| — irreducible: the actual arithmetic | 1.65 |

The components of the current call (`$stages` copy 9.5 µs, `$prob` assign
10.5 µs, `$prob` delete 15 µs, `ifelse` 8 µs) sum to ~50 µs, well short of
117 µs. The remainder is allocation and GC pressure from churning a 340 KB
object per pair.

`02-bhc-delta-prototype.R` implements a `bhc_fast` that scores candidates from
scalars and calls `join_stages_unsafe` **once per accepted join** instead of
once per candidate:

```
stages identical: TRUE    logLik equal: TRUE    df equal: TRUE
stages_bhc (current):  10.39s
bhc_fast  (delta)   :   1.41s      speedup: 7.4x
```

The delta formula was verified against `join_stages_unsafe` over **all 29 403
stage pairs** of a 6-variable model: **0 mismatches** in both `ll` and `df`.

This is **not** an Rcpp candidate. The arithmetic operates on vectors of length
`k` (2–5 elements). The cost is R-level allocation, not computation.

Applies to `stages_bhc`, `stages_fbhc`, `stages_bj`, `stages_bhcr` — all four
use the same `join_stages_unsafe`-per-candidate pattern.

Two cheap sub-fixes inside `join_stages_unsafe` regardless:
- `ifelse(is.na(p1), 0, p1)` → `c1 <- p1; c1[is.na(c1)] <- 0` — **8.0 → 2.0 µs**
  (`ifelse` is a heavyweight vectorised call; these vectors have 3 elements).
- Hoist `object$prob[[v]]`, `length(object$tree[[v]])`, `object$lambda` out of
  the pair loop — they are invariant per variable, and `$` is 11 % of profile.

---

## 3. Finding B — `tree_idx` recomputes model-invariant values on every call

**5.4× measured. Pure R.** (`04-tree_idx-prototype.R`)

```r
tree_idx <- function(path, tree, complete = FALSE) {
  ls <- sapply(tree, length)                       # invariant, recomputed every call
  is <- vapply(seq_len(k), function(i) (1:ls[i])[tree[[i]] %in% path[i]], 1)
  sum(vapply(1:(k-1), function(i) prod(ls[(i+1):k]), 1) * (is[1:(k-1)]-1)) + is[k]
}
```

Three separate problems, all measured:

| | µs |
|---|---|
| `sapply(tree, length)` — recomputed every call | **17.05** |
| `lengths(tree)` — same result | 1.10 |
| `tree[[i]] %in% path[i]` | 2.05 |
| `match(path[i], tree[[i]])` | 1.10 |

`sapply(tree, length)` alone is **26 % of the entire `tree_idx` call**, for a
quantity that never changes for a given model. The stride vector
`prod(ls[(i+1):k])` is likewise a cumulative product recomputed in an O(k²)
pattern inside a `vapply`.

Precomputing strides and a per-level lookup table once per model:

```
tree_idx (current)                 65.05 us/call
precomputed strides + LUT          12.05 us/call     5.4x
```

Verified to return identical indices. Because `find_stage` drives 59 % of
`sample_from` and 22 % of `prob`, this propagates directly to both.

`find_stage` additionally calls `sevt_varnames(object)` twice per invocation
(lines 45 and 47) — hoist to one local.

---

## 4. Finding C — what Rcpp is actually worth

Measured with a real compiled prototype (`05-rcpp-overhead.R`):

```
Rcpp no-op function                 3.05 us/call
tree_idx_cpp (per-call)             2.70 us/call
```

**`.Call` overhead is ~3 µs.** The index arithmetic itself is unmeasurable
against it. So:

> **Porting individual helpers to Rcpp is not worth it.** A per-call C++
> `tree_idx` costs ~3 µs, against 12 µs for the optimised pure-R version — a 4×
> gain that buys a compiler toolchain, a `src/` directory, `LinkingTo: Rcpp`,
> and a permanent CRAN build-and-maintenance burden across platforms.

The picture changes completely when the **whole loop** moves into C. Generating
20 000 samples × 8 variables in a single `.Call`:

```
sample_paths_cpp    0.007 s total   (0.35 us/sample)
```

versus `sample_from`'s **16.045 s** — roughly **2300×**, because the ~3 µs
overhead is paid once instead of 160 000 times.

**So the rule is: port loops, never leaves.** The only two places in this package
with that shape:

1. **`sample_from`** — the per-sample × per-variable descent. One `.Call` taking
   the stride vector, a stage-index matrix and a probability matrix, returning
   an `n × p` integer matrix. Largest single win available anywhere in the
   package.
2. **`prob`** — the per-row path-probability loop. `Rprof` also shows
   `[.data.frame` at 24.65 % and `expand.grid` at 18.14 % of `prob`; converting
   the input to a plain integer matrix once, up front, may capture much of that
   in pure R before any C is written.

---

## 5. Recommended order

| # | change | measured gain | risk | Rcpp? |
|---|---|---|---|---|
| 1 | BHC scores candidates from scalars (Finding A) | **7.4×** on `stages_bhc` | low — verified identical on 29 403 pairs | no |
| 2 | `tree_idx` precomputed strides + `lengths`/`match` | **5.4×**, feeds `sample_from`/`prob` | low — pure refactor | no |
| 3 | `ifelse` → indexed assign; hoist `$` lookups | 4× on that line | trivial | no |
| 4 | `sample_from` whole loop in C | ~2300× on that call | high — new toolchain | **yes** |
| 5 | `prob` row loop: matrix input first, then maybe C | TBD | medium | maybe |

Items 1–3 are pure R, need no new dependency, and should be done first — they
are most of the realistically available win. Item 4 is the only one that
genuinely justifies Rcpp, and it should be weighed against the ongoing cost of
shipping compiled code on CRAN.

## Status

**Item 1 is implemented** on this branch for `stages_bhc` only. `score` now
accepts `"BIC"` (default) or `"AIC"` and takes the scalar path; a function
still selects the original per-candidate path. Measured **6.0×** on a
121-situation model, with identical stages, log-likelihood and score value.
`R/scores.R` holds the registry so further scores are a single entry, and
`tests/testthat/test-scores.R` asserts the two paths agree across 25 random
models (guarding the `>=` tie-break) and that each score's `full` and `delta`
views agree.

Items 2–5 are **not** applied — still investigation plus prototypes.

---

## Reproducing

```r
Rscript benchmarks/01-profile-stages_bhc.R        # Rprof on stages_bhc
Rscript benchmarks/02-bhc-delta-prototype.R       # Finding A + correctness proof
Rscript benchmarks/03-profile-sample_from-prob.R  # Rprof on sample_from / prob
Rscript benchmarks/04-tree_idx-prototype.R        # Finding B
Rscript benchmarks/05-rcpp-overhead.R             # Finding C (needs Rcpp)
```

Timings vary by machine; the *ratios* are the meaningful part.
