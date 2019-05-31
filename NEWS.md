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