# development version

* fix bug in `summary`, stages were wrongly matched to probabilities
* new function `barplot_stages` to draw barplots of the 
  floret probabilities. Implemented relative tests.
* new example in `plot.sevt`.

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
