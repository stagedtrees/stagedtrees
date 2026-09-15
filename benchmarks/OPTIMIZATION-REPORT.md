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

How far this generalises, having since read all of them:

| function | score-based | `join_stages_unsafe` per candidate | delta applies |
|---|---|---|---|
| `stages_bhc` | yes | yes | yes — implemented |
| `stages_fbhc` | yes | yes, until the first improvement | yes |
| `stages_bhcr` | yes | only on *rejected* moves | marginal |
| `stages_bj` | **no** | **no — accepted joins only** | no |
| `stages_simplebhc` | yes | uses `join_positions` | no |
| `stages_hclust` | yes | real `sevt_fit(scope = v)` refit | no |

`stages_bj` takes no `score` at all — it picks pairs from a distance matrix
against a threshold and joins only what it accepts, so it never pays the
per-candidate cost. `stages_bhcr` evaluates a single random pair per iteration
and assigns it on acceptance, so only rejected moves waste a copy, bounded by
`max_iter` (default 100).

`stages_fbhc` is the only other real candidate. It uses first-improvement
(`break` out of both loops on the first improving pair) rather than
best-improvement, so a shared helper would have to parameterise the search
policy. Judged not worth it for now: it would fold the verified `stages_bhc`
path into a shared abstraction to serve one lightly-used second caller.

`stages_simplebhc` joins *positions*, which cascades to descendant variables,
so the change is not a local function of `(p1, p2)` and would need its own
derivation.

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

**Item 1 is implemented** on this branch, and the interface it originally
shipped with was withdrawn. The first version let `score` be either a string
naming a predefined score (`"BIC"` or `"AIC"`, taking the fast scalar path)
or a function (taking the original per-candidate path), with an `R/scores.R`
registry behind it. Measured 6.0× on a 121-situation model, identical stages,
log-likelihood and score value — but it bought speed only for users willing
to give up the generality of the argument, and it added a registry to the
public interface.

The design that replaced it keeps `score` an ordinary function and is faster:
**select** the candidate merge by log-likelihood alone, which is
score-independent, then **accept or reject** that one candidate with the
user's score. Valid because every pairwise merge changes the degrees of
freedom by exactly `k - 1`, so the score cannot reorder the candidates.
`R/scores.R` and the registry are gone; `join_ll_delta()` in `R/join_stages.R`
is the pure-R oracle, and `tests/testthat/test-scores.R` asserts the compiled
selection agrees with it — including on perfectly balanced data, where every
candidate delta ties and the `>=` tie-break decides the model.

**Items 2 and 3 are implemented.** `tree_idx` now uses `lengths()`, `match()`
and a walked stride instead of `sapply(tree, length)`, `%in%` and a per-position
`prod()`; `join_stages_unsafe` uses indexed assignment instead of `ifelse`.
Both keep their signatures, so no caller changed. Measured against the
baselines in section 1:

| call | before | after | |
|---|---|---|---|
| `sample_from(20000)`, 8 vars | 16.045 s | 9.495 s | 1.7× |
| `sample_from(5000)`, 6 vars | 2.063 s | 0.940 s | 2.2× |
| `predict(2000 rows)` | 1.951 s | 0.704 s | 2.8× |
| `prob(5000 rows)` | 4.229 s | 3.705 s | 1.14× |

`prob` moves least, as expected: `find_stage` was only 22 % of it, and the
`[.data.frame` (24.65 %) and `expand.grid` (18.14 %) costs are untouched —
that is item 5, still open.

One regression was caught by the existing suite during this work: the rewritten
`tree_idx` indexed `path[[k]]` with `k = 0` for an empty path, where the old
code returned `NA` via `is[1]` on an empty vector. Exhaustive path testing had
missed it because `expand.grid` never yields a zero-length path. Fixed, and
`stages(m)[[character(0)]]` is now covered directly.

**A later pass found the largest win of all, in `full()`/`sevt_fit`** — which
matters more than anything above because `full()` is the entry point for
essentially every workflow.

`sevt_fit` grouped situations by scanning the whole stages vector once per
stage (`ix <- object$stages[[v]] == s` inside `lapply(stages, ...)`). That is
O(stages x situations), and a full model has one stage per situation: 16 384 x
16 384 = 268 M comparisons for the deepest variable of an 8-variable,
4-level model. `split()` partitions in one pass.

Note that swapping `apply(., 2, sum)` for `colSums` gained **nothing** (1.0x)
— the scan was the whole cost — and `colSums` returns double where `apply`
returns integer, silently changing the type of `attr(, "n")`. `apply` was
therefore kept.

`expand_prob` filled its ftable one row at a time (16 384 iterations for the
same variable); stacking the per-stage probabilities once and selecting a row
per situation is 65x on that function alone.

| call | before | after | |
|---|---|---|---|
| `sevt_fit` (given ctables) | 6.486 s | 0.440 s | **14.7×** |
| `full(join_unobserved = FALSE)` | 9.616 s | 1.190 s | **8.1×** |
| `full()` default | 4.094 s | 1.134 s | **3.6×** |

Equivalence checked with `identical()` over 192 models — lambda 0 and 1,
`join_unobserved` both ways, with and without NAs, full and hclust-staged
models, four shapes. Two differences were caught this way and fixed: the
`colSums` type change above, and `expand_prob` losing the (incidental) names
on the `dim` attribute that `array(dim = c(prod(...), dims[i]))` produced.

Items 4 and 5 are **not** applied — still investigation plus prototypes.
Section 3's `stages_bj` findings were also left alone: `probdist.*` and
`distance_mat_stages` are reached only from `stages_bj`, which is not a
commonly used entry point.

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
