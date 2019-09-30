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

Installation time in seconds: 12
Check time in seconds: 159
Status: 1 NOTE

Possibly mis-spelled words in DESCRIPTION:
  Bigatti (22:25)
  Collazo (21:14)
  Görgen (21:29, 22:14)
  Riccomagno (22:37)
  Thwaites (23:14)

All the detected possible miss-spelled word are false positive. 
