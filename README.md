## stagedtrees 

### Use

- install with
  `devtools::install_github("https://github.com/gherardovarando/stagedtrees")`
- 

### Dev

- Creating a project with Rstudio on the cloned folder is the easier way to
develop. 
- `testthat` is used to test the package, test can be run directly from
Rstudio.
- `roxygen2` is used to generate automatically the documentation from the
  block of comments before each function.

In general check [Wickham's R packages](http://r-pkgs.had.co.nz/) to learn
how to develop R packages. 


####  Commit message

We try to keep commit messages cleaned, but sometimes big commit with lot of
changes can happen. 

- [+] file add 
- [-] file remov
- [upd] update (most used)
- [f] fix of a bug or error
- [doc] documentation

#### Guidelines

- Trying not to use too much other external packages.
- Previous point implies that for example plotting function try not to use
  classical packages as graph and Rgraphviz. 
- We should implement also more advanced plotting abilities using Rgraphviz
  from bioconductor. 
- Other way is to use ggplot2. 
- Coherent function names. 
- Well documented code, especially for the main functions.

#### Roadmap 

- [x] stratified event tree model 
- [x] fitting stratified event tree
- [ ] staged event tree
- [ ] fitting staged event tree 
- [ ] plotting: 
     * [x] stratified event tree
     * [ ] staged event tree (colors)
- [ ] structure search:
     * [ ] exhaustive search 
     * [ ] heuristics 

