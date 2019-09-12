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
* eliminate last line of example in `compare.sevt` to avoid error in
  examples for arch i386 (win-builder).
* Fixed spelling.


## Test environments
* Ubuntu 18.04.2      (64-bit)  R 3.6.1 (local) 
* Ubuntu 16.04.6      (64-bit)  R 3.6.1 (on travis-ci) 
* Windows                       R 3.4.4 (local)
* macOS darwin15.6.0  (64-bit)  R 3.6.1 (local)
* Windows Server 2008 (64-bit)  R 3.6.1 (win-builder)

## R CMD check results

### Ubuntu 18.04.2 R 3.6.1 (local) 

#### devtools::check()
Duration: 24.3s

0 errors | 0 warnings | 0 note

#### R CMD check --as-cran

Status: 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Gherardo Varando <gherardo.varando@gmail.com>’

### Ubuntu 16.04.6 R 3.6.1 (on travis-ci)

Status: OK

### OS X R 3.6.1 (local)

Status: Ok

### win-builder (R 3.6.1)

Status: 1 NOTE

Possibly mis-spelled words in DESCRIPTION:
  Bigatti (22:25)
  Collazo (21:14)
  Görgen (21:29, 22:14)
  Riccomagno (22:37)
  Thwaites (23:14)

All the detected possible miss-spelled word are false positive. 
