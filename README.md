## stagedtrees 

### Use

- install with
  `devtools::install_github("https://github.com/gherardovarando/stagedtrees")`
- 


#### examples

For all the examples we use stupid dataset created as:

```
N <- 100 ## number of observations
n <- 4 ## number of variables
max_l <- 5 ## max number of levels for each variable
DD <- as.data.frame(sapply(1:n, function(i){
                                     nl <- sample(2:max_l,size=1)
                                     return(as.factor(sample(c(1:nl), size=N,
				     replace = TRUE)))
				     }  ) )
```

##### Full stratified trees 

This objects just represent the full chain rule with a fixed order.

We can create a stratified event tree object from a list containing the
levels of the variables:
``` 
lvls <- lapply(DD, levels)

evt <- strt_ev_tree(lvls)

plot(evt) ## plotting is still a bit rough 
```

Alternatively we can create the stratified event tree directly from the
dataset of observation. And we can choose to fit the conditional
probabilities (lambda is the laplace smoothing parameter).

```
evt <- strt_ev_tree(DD, fit=TRUE, lambda = 2)

plot(evt)

evt$prob$V2 ### here probabilities table are stored

```


#### Staged event tree

As stratified event trees, staged event trees can be created from a list or
a data.frame, and fitted. By default the staged event tree created will be
the full independent model (that is only one stage per variable). 

```

sevt <- staged_ev_tree(DD, fit =TRUE , laplace = 1)

sevt$paths ## here for every path we indicate the stage (last column)



plot(sevt)
```

#### Conversions





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
- [x] fitting stratified event tree (mle)
- [x] staged event tree
- [x] fitting staged event tree (mle)
- [x] plotting: 
    * [x] stratified event tree
    * [x] staged event tree (colors)
    * [ ] probabilities on plot
    * [ ] ggplot2 (maybe) 
    * [ ] Rgraphviz (....) 
- [ ] inference:
    * [ ] joint prob of a path from root 
    * [x] logLik for full tree (thus AIC and BIC work automatically)
    * [x] logLik staged tree 
    * [ ] classification 
    * [ ] 
- [ ] structure search:
    * [ ] implement ``join_, split_, set_stage`` functions
    * [ ] exhaustive search 
    * [ ] heuristics
    * [ ] Penalized logLik (AIC, BIC)
- [ ] classifiers with staged trees 
    * [ ] define class 
    * [ ] define predict method
    * [ ] eval 
    * [ ] model selection (struct search ...)

