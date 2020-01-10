This release fix some errors in tests under no long double, some additional 
bugs in some tests. Moreover two functions are added.

## Test environments

* Ubuntu 18.04.2      (64-bit)  R 3.6.2 (local) 
* macOS darwin15.6.0  (64-bit)  R 3.6.1 (local)
* Ubuntu 16.04.6      (64-bit)  R 3.6.1 (on travis-ci) 
* Windows Server 2008 (64-bit)  R 3.6.2 (win-builder, R-release)
* Windows Server 2008 (64-bit)  R 3.5.3 (win-builder, R-oldrelease)
* Windows Server 2008 (64-bit)  R devel (win-builder, R-devel)
* R-hub windows-x86_64-devel (r-devel)

## R CMD Check results

### Ubuntu 18.04.2 R 3.6.2 (local) 

#### devtools::check()

Duration: 29.7s

0 errors | 0 warnings | 0 note

#### R CMD check --as-cran

Status: 1 NOTE

* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: ‘Gherardo Varando <gherardo.varando@gmail.com>’

### Ubuntu 16.04.6 R 3.6.1 (on travis-ci)

Status: OK

### OS X R 3.6.1 (local)

Duration: 26.1s

0 errors | 0 warnings | 0 notes

### win-builder

#### R-release 

Status: OK

#### R-devel 

Status: OK

#### R-oldrelease 

Status: OK

### r-hub

0 errors | 0 warnings | 0 notes