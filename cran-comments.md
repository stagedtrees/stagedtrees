## stagedtrees 2.0.1

fix color assignment consistency between `plot.sevt` and `barplot.sevt`.

## Test environments

* Ubuntu 18.04.2      (64-bit)  R 4.0.2 (local) 
* macOS darwin15.6.0  (64-bit)  R 3.6.1 (local)
* Ubuntu 16.04.6      (64-bit)  R 4.0.0 (on travis-ci) 
* Windows Server 2008 (64-bit)  R 4.0.3 (win-builder, r-release)
* Windows Server 2008 (64-bit)  R 3.6.3 (win-builder, r-oldrelease)
* Windows Server 2008 (64-bit)  R 4.1.0 (win-builder, r-devel)
* R-hub debian-gcc-devel-nold (r-devel)
* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-release (r-release)
* R-hub fedora-clang-devel (r-devel)

## R CMD Check results

### Ubuntu 18.04.2 R 3.6.2 (local) 

#### devtools::check()

Duration: 24.6s

0 errors | 0 warnings | 0 note

#### R CMD check --as-cran

Status: OK

### Ubuntu 16.04.6 R 3.6.1 (on travis-ci)

Status: OK

### OS X R 3.6.1 (local)

Duration: 26.1s

0 errors | 0 warnings | 0 notes

### win-builder

Status: 1 NOTE 

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Gherardo Varando <gherardo.varando@gmail.com>'

### r-hub (check_for_cran)

0 errors | 0 warnings | 0 notes
