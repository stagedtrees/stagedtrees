# devel

* improve code comments
* fix correct passing arguments to distance function in `naive.sevt`
* improve `naive.sevt` now working also from non-full model.
* `staged_ev_tree.strt_ev_tree` accepts now an additional parameter `fit` which control if probabilities should be computed. 
* in `full` is now possible to specify if situations should be joined
  using `join_zero`, in that case probabilities will be fitted only     after the empty situations are joined, improving speed.  
* update internal function `new_label` to improve speed.
* `hc.sevt` and `naive.sevt` accept now, as `ignore` 
  argument, a vector of stage names. Such user specified stages will
  be left untouched. 
* `plot.sevt` allows now to specify the color of the edges with 
   `col.edges`
* In `plot.sevt` is possible to specify stages that should be ignored
  and not plotted via the `ignore` argument.
* `naive.sevt` can use now different hierarchical clustering methods
   (using `hclust` from `stats` package), moreover the number of 
   stages per variable can be specified. 
* fix in `compare.sevt`: because of changes in `plot.sevt` we need to
  specify that the root is always considered identical.
* `stndnaming.sevt` accepts now `uniq`, `prefix` and `ignore`      
   arguments, which control how stage names are generated and if 
   some stage names should be left untouched.

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
* Fix #38  subtree and colours in plot bugs 
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
