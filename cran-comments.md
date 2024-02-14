# stagedtrees 2.3.0

* Fix new package doc format.
* Carli et al. 2022 JSS citation added in DESCRIPTION.
* Added functions for random generation of staged trees: `random_sevt` and
`random_parentslist`.
* Added new function `depsubtree` which build the dependency subtree.
* New functions to translate `sevt` and `ceg`
  objects to graphs (edge lists and igraph).
* Fix #98; fixed handling of NAs.
* fix bug on `plot.sevt` and on graph conversion methods
  when ignoring stage "1" which is assigned to root
* Fix #80; `sevt_fit` earns the `scope` argument,
  it is now possible to do partial re-fit of staged tree models.
  Moreover, with the `compute_logLik` argument, the
  user can decide if the log-likelihood should be computed when fitting the
  model.
* `stages_hclust` can now search the best cut of the hierarchical cluster with
   respect to some `score` function to be maximized.
* various minor fixes in colors specification for visualization functions.
* `stages_simplebhc` implements a new stage structure search by iterative
  joining of positions, thus searching in the space of simple staged trees.
* `sevt_simplify` return the simplified staged tree, that is a stage tree
  where positions and stages are equivalent.
* fix problem with positions computation in `ceg` when model has less
  than two variables.
* Fix #113; use `cli` package for errors and warnings.
* improve arguments checks in various functions.
* improve specific checks for `sevt` objects.
* fix minor coding style problems (some argument names have changed).
* improve test coverage.
* new return value of `stages` function and new indexing of stages via
  a new dedicated class `sevt.stgs`.
* use faster and unsafe functions internally, speedup in
  various functions (up to 10x).
* implemented `stages_csbhc`, a new stages learning algorithm which
  iteratively add context specific independencies.
  Additionally functions `ci_matrices` and `join_all` have been added.


## Test environments

* local ubuntu 22.04.3  (64-bit)  R 4.3.2
* github actions windows-latest (R release)
* github actions windows-latest (R 4.1)
* github actions windows-latest (R 3.6)
* github actions macOS-latest (R release)
* github actions ubuntu-latest (R release)
* github actions ubuntu-latest (R devel)
* github actions ubuntu-latest (R oldrel 1-4)
* win-builder (r-release)
* win-builder (r-oldrelease)
* win-builder (r-devel)
* mac-builder (r-release)
* R-hub windows-x86_64-devel (r-devel)
* R-hub ubuntu-gcc-release (r-release)
* R-hub fedora-clang-devel (r-devel)

## R CMD Check results

### local ubuntu 22.04.3 R 4.3.2

#### devtools::check()

── R CMD check results ─ stagedtrees 2.3.0 ────
Duration: 43s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded

#### R CMD check --as-cran

Status: OK

### github actions R-CMD-check

Status: OK

except ubuntu-latest (oldrel 1-4)
Error:
  ! error in pak subprocess
  Caused by error:
  ! Could not solve package dependencies:
  * deps::.: Can't install dependency bnlearn
  * bnlearn: Needs R >= 4.3.0
  * any::sessioninfo: dependency conflict
  * any::rcmdcheck: dependency conflict

The above is unrelated with the stagedtrees package.
Package bnlearn is suggested for stagedtrees thus it should not be required.

### win-builder

Status: 1 NOTE

Possibly misspelled words in DESCRIPTION:
  Carli (22:14)
  Leonelli (22:23)
  Varando (22:49)

The above words are the family names of the package authors and are correct.

### macos-builder

Status: OK

* elapsed time (check, wall clock): 0:33

### Rhub

❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... [14s] NOTE
  Maintainer: 'Gherardo Varando <gherardo.varando@gmail.com>'

  Possibly misspelled words in DESCRIPTION:
    Carli (22:14)
    Leonelli (22:23)
    Varando (22:49)

❯ On windows-x86_64-devel (r-devel)
  checking HTML version of manual ... [12s] NOTE
  Skipping checking math rendering: package 'V8' unavailable

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [8s/32s] NOTE
  Maintainer: ‘Gherardo Varando <gherardo.varando@gmail.com>’

  Possibly misspelled words in DESCRIPTION:
    Carli (22:14)
    Leonelli (22:23)
    Varando (22:49)

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
  Skipping checking math rendering: package 'V8' unavailable

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [9s/38s] NOTE
  Maintainer: ‘Gherardo Varando <gherardo.varando@gmail.com>’

  Possibly misspelled words in DESCRIPTION:
    Carli (22:14)
    Leonelli (22:23)
    Varando (22:49)

0 errors ✔ | 0 warnings ✔ | 7 notes ✖
