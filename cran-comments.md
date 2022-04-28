## stagedtrees 2.2.1 

* `inst/CITATION` file added as requested by JSS.
* The DOI in the CITATION is for a new JSS publication that will be registered after publication on CRAN.
* broken badge url fixed in README.

## Test environments

* local ubuntu 20.04.2  (64-bit)  R 4.1.3
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

Duration: 40.7s

0 errors | 0 warnings | 0 note

#### R CMD check --as-cran

Status: 1 NOTE

```
Found the following (possibly) invalid DOIs:
  DOI: 10.18637/jss.v102.i06
    From: inst/CITATION
    Status: Not Found
    Message: 404
```
The DOI in the CITATION is for a new JSS publication that will be registered after publication on CRAN.

### travis-ci 

Status: Status: OK

### win-builder

Status: 1 NOTE (as above)

### github actions R-CMD-check 

Status: Status: OK

### R-hub

Status: 1 NOTE (as above)
