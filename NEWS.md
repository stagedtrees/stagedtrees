# dev

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
* `sevt_fit_hem` implements the hard EM algorithm for parameter
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
