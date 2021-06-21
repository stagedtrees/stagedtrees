## stagedtrees 2.1.0

MINOR version.  

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
* `cid` function that implements context intervention discrepancy
* more and better testing and documentation.

## Test environments

* local ubuntu 20.04.2  (64-bit)  R 4.1.0
* travis-ci ubuntu 16.04.6  (64-bit)  R 4.0.2
* win-builder (r-release)
* win-builder (r-oldrelease)
* win-builder (r-devel)
* github actions windows-latest (r-release)
* github actions macOS-latest (r-release)
* github actions ubuntu-20.04 (r-release)
* github actions ubuntu-20.04 (r-devel)
* R-hub fedora-clang-devel (r-devel)
* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-release (r-release)

## R CMD Check results

### local ubuntu 20.04.2 R 4.1.0

#### devtools::check()

Duration: 36.5s

0 errors | 0 warnings | 0 note

#### R CMD check --as-cran

Status: OK

### travis-ci 

Status: OK

### win-builder

Status: OK

### github actions R-CMD-check 

Status: OK

### R-hub

Status: OK