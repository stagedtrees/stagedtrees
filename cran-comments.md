## Resubmission
This is a resubmission with changes as requested by CRAN and to eliminate 
errors, warnings and notes:

* function `stageinfo.sevt` has been removed, instead `summary.sevt` has been
  added with an R-like structure (`summary.sevt` object and 
  `print.summary.sevt` method)
* removed examples for unexported functions
* fixed error for 
  `unexpected end of input: uni_idx(list(A = c(,,), B = c(,,))` 
  in man/uni_idx.Rd 
* removed `\dontrun{}` statements
* Changed some examples to keep examples time low
* Change `Url` fields to `URL` in DESCRIPTION
* Remove `Authors` and `Mantainer`  fields in DESCRIPTION 
  since `Authors@R` is present
* Fix issue when suggested package `clue` is not available
* Function `subtree.sevt` now works as expected 
* add `@keywords internal` to unexported functions
* change in options for `README.Rmd` to generate a compatible `README.md`
* empty line at the end of file LICENSE to avoid `devtools::check()` 
  end of line warning.


## Test environments
* Ubuntu 18.04.2 R 3.6.1 (local) 
* Ubuntu 14.04.5 R 3.5.0 (on travis-ci) 
* Windows        R 3.4.4 (local)
* OS X           R 3.5.2 (local)
* win-builder    R 3.6.1

## R CMD check results

### Ubuntu 18.04.2 R 3.6.1 (local) 

#### devtools::check()
Duration: 24.3s

0 errors | 0 warnings | 0 note

#### R CMD check --as-cran

Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Gherardo Varando <gherardo.varando@gmail.com>’

### win-builder (R 3.6.1)


