# dev

* BREAKING: `na0` now defaults to `FALSE` in `prob`. A situation with no
   observations carries no probability, and the old default supplied one it
   does not have, silently removing probability mass from any event that
   could be reached through it. Asking for that reading is now explicit.
   Since paths that are unreachable because an earlier situation has
   probability zero are reported as zero anyway, this changes the result
   only where a probability is genuinely undefined. `predict` keeps the
   previous reading, since a class value whose probability is undefined
   cannot be ranked against the others.
* BREAKING: a value which is not a level of its variable is an error in
   `prob` wherever it appears. At the last variable of the query it was
   silently reported as a probability of zero, while every other position
   already raised an error.
* `prob` now honours `na0 = FALSE`. The `NA` probabilities of situations
   with no observations were treated as zero whatever `na0` asked for, so
   an event that could only be reached through such a situation silently
   lost probability mass instead of being reported as unknown. This was
   visible in `potential_outcomes`, whose rows could sum to less than one
   when randomizing the treatment gave a never-observed history positive
   probability. A zero factor still dominates an unknown one, so paths that
   are unreachable because an earlier situation has probability zero are
   still reported as zero, not `NA`.
* new function `ps_stratify` to rebuild the staging of an outcome variable
   from the propensity-score stratification already induced by a treatment
   variable, for propensity-score stratification estimation of treatment
   effects. `treatment` and `outcome` default to the last two variables in
   the order of the model. As in `randomize_sevt`, the stages listed in
   `ignore` (by default the unobserved ones) are not re-staged, which
   avoids creating strata with no observations.
* BREAKING: `potential_outcomes` argument order is now
   `(object, treatment, outcome)`, matching `ps_stratify` and
   `randomize_sevt`. Calls using named arguments are unaffected; calls
   relying on positional matching of `outcome`/`treatment` need to be
   updated. `treatment` and `outcome` also gain the same default as
   `ps_stratify`: the last two variables in the order of the model.
* possible to skip checks in development.
* `hamming_stages` earns a new argument `FUN` which specify how to aggregate 
   across variables. The default `FUN = sum` produces the standard hamming 
   distance between stages structures.
* new function `copy_sevt` to copy objects of class sevt.
* `predict` for `sevt` objects can now predict also with
   incomplete inputs (either NA or not provided).
* some fixes in `prob`.
* `sevt_fit`, `full` and `indep` add a new component to the sevt obejct,
   the field `data_raw` which stores the data used to fit the model.
   This is useful when missing data are present.
* New stages learning algorithm for missing data, `stages_em` implements
  the structural EM algorithm for stages structure.
* `sevt_fit_em` implements the hard EM algorithm for parameter
  learning with missing data.
* some fixes in `plot` related utilities.
* functions to plot and handle ALDAGs:
  `as_igraph.parentslist` and `plot.parentslist`
  allowing easy plotting of ALDAGs.
* function to find one or all topological order of a `parentslist` object.
* some changes of internal functions of the `parentslist` class.
* `make_stages_col` accept now as `col` a function of 0, 1, or 2 arguments 
   (it is checked with `length(formals(col))``). 
* function `potential_outcomes` to compute the probability of an outcome 
  variable given intervention on a treatment.
* new method for coloring in `make_stages_col` or `plot.sevt`. If `col = classic` 
  the "classical" coloring of stages is generated, where singleton stages are not 
  assign a color and where stages across different levels are assigned different 
  colors.
* `stages_hclust` will now use the `hclust` implementation in the
   **fastcluster** package, if available. 
* `stages_hclust` accepts now a custom function as distance.
* `stages_hclust` earns the arguments `max_k`, to limit the number of
   stages considered when `k = NA`, and `first_max`, to stop the search
   at the first local maximum of the score instead of searching globally.
   The scores computed during the search are stored in the `scores`
   component of the returned object.
* removed `as_parentslist.string`. The method was non-functional: it could
   never be dispatched to (character vectors have implicit class
   `character`, not `string`), it did not parse the `[A][B|A]` encoding
   produced by `as.character.parentslist`, and it returned an unnamed,
   unclassed list rather than a `parentslist` object.
* `stages_bhc` is substantially faster and its `score` argument is unchanged:
   any function is accepted, as before. The candidate merge is now chosen by
   log-likelihood alone in compiled code, and `score` is evaluated once on
   that candidate to accept or reject it. This is valid because every
   pairwise merge changes the degrees of freedom by the same amount, so the
   score cannot reorder the candidates. On 5000 observations over 3-level
   variables the search takes 291.6s at six variables before the change and
   0.41s after; seven variables did not complete within ten minutes before
   and takes 8.2s now. The package now contains compiled code and requires
   **Rcpp**.
* `stages_hc` is dramatically faster and its `score` argument is unchanged.
   It previously refit the model and recomputed the full log-likelihood for
   every candidate move; the change each move causes is now computed in
   closed form in compiled code. Because a move can add a stage, remove one,
   or neither, candidates are grouped by their change in degrees of freedom
   and `score` is evaluated on the best of each group -- at most three
   evaluations per sweep instead of one per candidate. On 5000 observations
   over 3-level variables the search takes 16.2s at four variables before the
   change and 0.13s after; five variables did not complete within ten minutes
   before and takes 1.0s now. The selected model is the same: over 36 models
   spanning
   seeds, smoothing and model shapes, the stage partition, log-likelihood and
   degrees of freedom are identical. Stage *labels* can differ, since moves
   are taken in a different order.
* `sample_from` draws every observation sitting in the same stage with one
   call instead of one call per observation, and carries the situation index
   down the tree rather than recomputing it from each observation's path.
   Drawing 20000 observations from an 8-variable model with 3 levels takes
   3.84s before the change and 0.069s after. **This changes which values a
   given seed produces**: the distribution is unchanged, but code relying on
   the exact sample from a fixed seed will see different values.
* `prob` no longer reads its query one cell at a time, and no longer builds a
   grid of completions for observations that have nothing to complete.
   Observations that do have missing values now have their completions
   enumerated and evaluated in compiled code, rather than one at a time in R.
* `predict` groups the observations that are missing a predictor by which
   variables those are, and computes each group in one call rather than one
   call per observation per class value.
* the combined effect of all the changes in this release, measured end to end
   against the previous one. Models are fitted to 2000 observations; queries
   are 1000 rows of a 6-variable model with 3 levels. The per-change figures
   elsewhere in these notes each compare against the state just before that
   change, so they do not add up to these.

   |                                       | before  | after  |
   |---------------------------------------|---------|--------|
   | `full()`, 10 variables, 4 levels      | 21.509s | 0.340s |
   | `full()`, 11 variables, 4 levels       | 89.131s | 1.553s |
   | `stages_hclust`, 7 variables           | 27.655s | 4.526s |
   | `prob`, complete observations          | 1.148s  | 0.007s |
   | `prob`, one variable missing           | 1.651s  | 0.009s |
   | `prob`, two variables missing          | 3.371s  | 0.012s |
   | `predict(prob = TRUE)`, complete       | 0.881s  | 0.002s |
   | `predict()`, complete                  | 0.908s  | 0.016s |
   | `predict()`, a fifth missing one       | 2.731s  | 0.025s |
   | `predict()`, all missing one           | 10.035s | 0.048s |
   | `sample_from`, 20000 draws, 8 variables| 11.833s | 0.076s |
* `path_probability` carries the situation index down the path instead of
   rebuilding it at every depth, which was quadratic in the number of
   variables. `predict` on 1000 observations of a 6-variable model takes 0.29s
   before and 0.14s after; the compiled path below then takes it further.
* `make_ctables` computes the counts for each prefix of the variable order by
   summing the next prefix over its last variable, rather than sweeping the
   whole joint table once per prefix. `full()` on 2000 observations of 4-level
   variables takes 13.2s at ten variables before the change and 0.57s after,
   56.6s at eleven variables and 2.3s after; twelve variables, previously not
   feasible, takes 9.1s.
* `has_prob` compares the stored probability vectors' lengths with `lengths()`
   rather than a nested `sapply` over every stage of every variable. It runs on
   every `logLik` call, so a search evaluating a score per candidate paid it per
   candidate: it was 36% of the runtime of `stages_hclust`.
* `stages_hclust` assigns stages by looking each situation's stage up in the
   clustering once, instead of scanning the whole situation vector once per
   cluster, which was quadratic in the number of stages. With the `has_prob`
   change, on 2000 observations of 3-level variables the default search takes
   0.33s at five variables before and 0.16s after, 2.3s at six variables and
   0.85s after, 18.4s at seven variables and 4.5s after.
* `predict` is substantially faster for observations with no missing
   predictor. The walk down the tree for every candidate class value is now
   done in compiled code for all such rows in one call, while rows with a
   missing predictor keep the previous path, since those require summing over
   the missing variable's levels. On 1000 observations of a 6-variable model
   with 3 levels, `predict(prob = TRUE)` takes 0.134s before the change and
   0.0021s after. The default `predict()` call is slower than that figure
   suggests, because turning the probabilities into class labels then
   dominates it.
* `predict` returned a transposed result when the class variable had a single
   level: `apply` yields a vector rather than a matrix in that case, so
   `prob = TRUE` gave a 1 by n matrix instead of n by 1, and `prob = FALSE`
   collapsed the whole of `newdata` to a single value -- an integer index
   rather than a class label. It now returns one prediction per observation.
* `stages_hclust` no longer pays a one-off 0.125s cost on the first call in a
   session. The check for whether **fastcluster** is available used
   `rlang::is_installed`, which is that expensive the first time it runs;
   `requireNamespace` costs 0.003s and loads the namespace that is used
   immediately afterwards in any case. At four variables, where the whole
   search takes 0.07s, this had tripled the cost of a first call.
* internal speedups in `tree_idx` and `join_stages_unsafe`, which no longer
   recompute values that are fixed for a given model. `sample_from` is about
   twice as fast and `predict` about three times; `tree_idx` now reports an
   informative error, instead of a cryptic one, when a path contains a value
   which is not a level of the corresponding variable.
* `find_stage` earns an optional `var` argument naming the variable the path
   leads to, so callers looping over paths can hoist the variable names out
   of the loop instead of having them recomputed on every call. `sample_from`
   and `prob` do so, and additionally reuse the per-variable probability list
   across samples: `sample_from` is about 13% faster and `predict` about 7%.
* `sevt_fit` groups the situations of each variable in a single pass instead
   of scanning the stages vector once per stage, which was quadratic when
   stages are many, as in a full model. `expand_prob` builds its tables in one
   vectorised step rather than a row at a time. Fitting is several times
   faster: on 20000 observations over 8 variables with 4 levels, `full` goes
   from about 4s to about 1s.
* `write_tikz` has now `xlim` and `ylim` parameters, also an 
   `edge_options` argument.
* Bug fixes (code review):
  * `$stages` is now consistently indexed by variable name throughout
    the codebase, preventing silent mis-indexing when the root entry
    is absent or stages are reordered (C1).
  * `prob()` now returns `NA` (with a warning) when conditioning on a
    zero-probability event instead of propagating `NaN` (C2).
  * `rename_stage()` now aborts with an informative error when the new
    name already exists as a stage, preventing silent overwrite of its
    probability vector; a no-op rename (new == old) is handled cheaply (C3).
  * `indep.data.frame()` and `full.data.frame()` now use complete cases
    consistent with `make_ctables`, and store probabilities as plain
    named numeric vectors rather than `table`/`array` objects (C6).
  * `make_ctables()` now passes `drop = FALSE` when subsetting
    contingency tables, preventing silent dimension collapse for variables
    with a single observed level (A-B1).
  * `sample_from()`, `stndnaming()`, `sevt_fit_em()`, `stages_em()`,
    `tree_indexing()`, `probdist`, and `lr_test` now use `seq_len()` /
    `seq_along()` instead of `1:n`, preventing erroneous double-iteration
    on zero- or one-element inputs (B-B1, A-B2, A-B3, A-B4, B-B5, B-B6, B-B7).
  * `stages_em()` no longer leaves `data_c` uninitialised when
    `max_iter_em = 0` (B-B5).
  * `stages_bhcr()` now returns early on single-variable models (B-B2).
  * `stages_hclust()` now skips clustering when only one non-ignored
    stage remains for a variable (B-B3).
  * `split_stage_random()` now uses `scope` when re-fitting after a
    split, avoiding a full refit (A-B6).
  * `join_unobserved()` now deduplicates `name_unobserved` (A-B5).
  * Build artifacts (`*.tar.gz`, `*.Rcheck/`) added to `.gitignore`.
  * `diff_stages()` inner loops replaced `ifelse()` side-effect pattern
    with `if/else` for clarity and correctness.
  * `compare_stages()` now errors with an informative message when an
    unknown `method` is supplied, instead of silently falling back to naive.
  * `cid()` no longer includes the root variable in `$wrong` output,
    making it consistent with `hamming_stages()` and `diff_stages()`.
  * `hamming_stages()` and `cid()` documentation updated to state that
    `FUN = mean` yields a result in [0, p−1] (sum of per-variable means),
    not a value in [0, 1].

# 2.3.0

* Fix new package doc format.
* Carli et al. 2022 JSS citation added in DESCRIPTION.
* Added functions for random generation of staged trees: `random_sevt` and
`random_parentslist`.
* Added new function `depsubtree` which build the dependency subtree.
* New functions to translate `sevt` and `ceg`
  objects to graphs (edge lists and igraph).
* Fix #98; fixed handling of NAs.
* fix bug on `plot.sevt` and on graph conversion methods
  when ignoring stage "1" which is assigned to root
* Fix #80; `sevt_fit` earns the `scope` argument,
  it is now possible to do partial re-fit of staged tree models.
  Moreover, with the `compute_logLik` argument, the
  user can decide if the log-likelihood should be computed when fitting the
  model.
* `stages_hclust` can now search the best cut of the hierarchical cluster with
   respect to some `score` function to be maximized.
* various minor fixes in colors specification for visualization functions.
* `stages_simplebhc` implements a new stage structure search by iterative
  joining of positions, thus searching in the space of simple staged trees.
* `sevt_simplify` return the simplified staged tree, that is a stage tree
  where positions and stages are equivalent.
* fix problem with positions computation in `ceg` when model has less
  than two variables.
* Fix #113; use `cli` package for errors and warnings.
* improve arguments checks in various functions.
* improve specific checks for `sevt` objects.
* fix minor coding style problems (some argument names have changed).
* improve test coverage.
* new return value of `stages` function and new indexing of stages via
  a new dedicated class `sevt.stgs`.
* use faster and unsafe functions internally, speedup in
  various functions (up to 10x).
* implemented `stages_csbhc`, a new stages learning algorithm which
  iteratively add context specific independencies.
  Additionally functions `ci_matrices` and `join_all` have been added.

# 2.2.1

* `inst/CITATION` file added as requested by JSS.
* The DOI in the CITATION is for a new JSS publication that will be registered after publication on CRAN.
* broken badge url fixed in README.
* update documentation.


# 2.2.0

* `plot.ceg` new plotting functions using `igraph` plotting.
* new util function `make_stages_col` which help computing
  stages colors for `sevt` and `ceg` plotting.
* `sample_from` now returns a data.frame of factors.
* `prob` earns a new argument `conditional_on`, that
   makes easier to compute conditional probabilities.
* `confint.sevt`, implement a method for confidence intervals
   for the parameters of a model of class `sevt`.
* `lr_test` new function, implementing likelihood-ratio
   test.
* functions to search optimal staged trees among different orders:
  `search_best` and `search_greedy`.
* `cid` function that implements context intervention discrepancy.
* more and better testing and documentation.

# 2.1.0

* implemented various conversion algorithms between `sevt`
  and DAG representations.
* improve implementation of `as_sevt.bn.fit`.
* use \pkg instead of \code for package names in documentation.
* fix `find_stage`.
* fix problems with single-variable staged trees.
* fix `summary.sevt` for objects without `ctables`.
* fix bug in `path_probability` and `prob` for sevt objects with not
  ordered `$prob`.
* more tests.

# 2.0.1

fix color assignment consistency between `plot.sevt` and `barplot.sevt`

# 2.0.0

This version introduces major changes, in functions capabilities
and in functions naming.
These changes are almost surely breaking any previous code
using older versions of the package.
In particular, all functions named `*.sevt` but class methods are now
called differently.
Moreover, various improvements and functionalities are added
to better deal with unobserved situations and to improve
computations.
Additional model selection methods based on clustering are
now available.

COMPLETE CHANGELOG:

* DESCRIPTION updated.
* documentation updated.
* improve code comments.
* reduced exported functions.
* removed the `fit` parameter from `full`, `indep`.
   Now `full` and `indep` always fit the model while
  `sevt` is just the basic constructor of the `sevt` class.
* in `full` and `indep` by default unobserved situations are joined
  using `join_unobserved`, and probabilities are fitted only after
  the unobserved situations are joined, improving speed. Moreover, the
  name of the unobserved stages are stored as `name_unobserved` in the
  staged tree object.
* update internal function `new_label` to improve speed.
* `plot.sevt` allows now to set edges color with
   `col_edges`.
* In `plot.sevt` and `barplot.sevt` it
  is possible to specify stages that should be ignored
  and not plotted via the `ignore` argument, by default the
  `name_unobserved` stages are ignored.
* `plot.sevt` adds variables names by default (`var_names` argument).
* fix in `compare_stages`: because of changes in `plot.sevt`
  we need to specify that the root is always considered identical.
* internal function `stndnaming` accepts now `uniq`, `prefix` and
  `ignore` arguments, which control how stage names are generated and
  if some stage names should be left untouched (default: the `name_unobserved`
  stages).
* in `stages_bj` (previously `bj.sevt`) distance is now passed with a
  character and no longer as a function.
* two new model selection function: `stages_hclust` and
  `stages_kmeans`, to learn stage structure using hierarchical or
  k-means clustering.
*  all model selection functions accept `scope` and `ignore`
   parameters that allow to specify among which variables
   run the algorithm and which stages should be left untouched
   (default: the `name_unobserved` stages).
*  replace `1:length(x)` with the suggested `seq_along` in all code.
* distance names in `stages_bj` and `stages_hclust` are compatibles.
* fixed bug in some probability distance functions when 0 probabilities.
* Conversion generic function `as.sevt`, only implemented one method
  for `bn.fit` class from bnlearn package
* fix `inclusions_stages` and provide better output.

# 1.0.2

* fix bug in `summary`, stages were wrongly matched to probabilities
* new function `barplot_stages` to draw barplots of the
  floret probabilities. Implemented relative tests.
* new example in `plot.sevt`.
* now `order` can be passed to `staged_ev_tree.bn.fit`
* `join_zero` alias for `join_zero_counts`
* more tests

# 1.0.1

* fix `indep`, probabilities should not be of class table
  (it was triggering a bug in summary)
* fix `subtree`, now removing unused probabilities
  (it was triggering a bug in summary)
* fixing testing without long double
* fixing some errors in testing
* update some tests with unquoted expression
* new functions `get_stage` and `get_path` and relative tests

# 1.0.0

* Minor doc fix and references added
* `xor` is renamed as `noisy_xor`
* test added for `noisy_xor`
* more util functions marked as internal
* fixed return problem with `NaN` log-probabilities in `predict.sevt`
* `summary.sevt` function added
* `subtree.sevt` works now for fitted objects

# 0.9.5

* Add `inclusion.stages` function
* Fix #28; `predict.sevt` now returns conditional probabilities P(C|X)
* Fix #62; `predict.sevt` works also if `newdata` does not include the class

# 0.9.4

* Fix #40; `compare.sevt` works with three different methods.
* Fix #37; `join_stages` throws error with bad stages.
* Fix #51; tests for model selection functions.

# 0.9.3

* Fix #39; `lambda = 0` does not induce errors in `logLik`, `join_stages` and thus in the model selection algorithms.
* Fix #41;  `nvar.sevt` bug
* Fix #34; order of elements in positions list of `ceg.sevt`
* Fix #38  subtree and colors in plot bugs
* Fix #46; removed Trump dataset

# 0.9.1

* New functions: `varnames.sevt` and `nvar.sevt`
* Improved codecov
* `fit.sevt` is replaced by `sevt.fit` (following the `lm.fit` naming).
  `fit.sevt` is still available but marked as (DEPRECATED) in doc.
* minor bug fixing
* Doc for `compare.sevt` is updated with info on the capabilities.


# 0.9.0

First released version of the package
