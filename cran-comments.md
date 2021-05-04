## stagedtrees 2.1.0

MINOR version.  

* new conversion methods: `as_sevt`, `as_parentslist`, `as_bn`. 
* improved implementation of `as_sevt.bn.fit`.
* various fixes and improved documentation. 

## Test environments

* Ubuntu 20.04.2          (64-bit)  R 4.0.5 (local) 
* macOS Catalina 10.15.7  (64-bit)  R 3.6.1 (local)
* Ubuntu 16.04.6      (64-bit)  R 4.0.2 (on travis-ci) 
* Windows Server 2008 (64-bit)  R 4.0.5 (win-builder, r-release)
* Windows Server 2008 (64-bit)  R 3.6.3 (win-builder, r-oldrelease)
* Windows Server 2008 (64-bit)  devel (unstable) (win-builder, r-devel)

## R CMD Check results

### Ubuntu 20.04.2 R 4.0.5 (local) 

#### devtools::check()

Duration: 24.6s

0 errors | 0 warnings | 0 note

#### R CMD check --as-cran

Status: OK


### OS X R 3.6.1 (local)

Duration: 26.1s

0 errors | 0 warnings | 0 notes

### Ubuntu 16.04.6 R 3.6.1 (on travis-ci)

Status: OK

### win-builder

Status: OK

