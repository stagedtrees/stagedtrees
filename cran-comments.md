This is a resubmission with changes as requested by CRAN:
* function `stageinfo.sevt` has been removed, instead `summary.sevt` has been
  added with an R-like structure (`summary.sevt` object and 
  `print.summary.sevt` method)
* removed examples for unexported functions
* fixed error for 
  `unexpected end of input: uni_idx(list(A = c(,,), B = c(,,))` 
  in man/uni_idx.Rd 
* removed `\dontrun{}` statements
* Remove some examples to keep examples time low
* Change `Url` fields to `URL` in DESCRIPTION
* Remove `Authors` field in DESCRIPTION since `Authors@R` is present
* Fix issue when suggested package `clue` is not available

## Test environments
* Ubuntu 18.04.2 R 3.6.0 (local) 
* Ubuntu 14.04.5 R 3.5.0 (on travis-ci) 
* Windows        R 3.4.4 (local)
* OS X           R 3.5.2 (local)

## R CMD check results
Duration: 25.5s

0 errors | 0 warnings | 0 note