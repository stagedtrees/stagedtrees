## stagedtrees 

[![Build Status](https://travis-ci.com/gherardovarando/stagedtrees.svg?branch=master)](https://travis-ci.com/gherardovarando/stagedtrees)

### Use

- install with
  `devtools::install_github("gherardovarando/stagedtrees")`


#### examples

For the example we use a simulated dataset with 6 binary variables:

```
DD <- generate_random_dataset(n = 5, 1000)
```

##### Full stratified trees 

This object just represent the full chain rule with a fixed order.

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

evt$prob$X1 ### here probabilities table are stored
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

We can move from the staged event tree and the full stratified event tree:
```
evt <- strt_ev_tree(DD, fit =T)
sevt <- staged_ev_tree(evt) ## this will be a full staged event tree,
                            ## that is one different stage per path
evt_back <- strt_ev_tree(sevt)
```
#### Model selection

We are still implementing model selection algorithm, now available:

- ##### Independent model (default) 
  ```
  sevt <- staged_ev_tree(DD, fit = TRUE, method = "indep")
  ```
- ##### Full model 
  ```
  ## if fit=FALSE (default) model will be returned without fitted
  ##  probabilities
  sevt <- staged_ev_tree(DD, fit = TRUE, method = "full")
  ```
- ##### Backward Hill-Climbing

  The algorithm moves to the **best** model that increase the score. 
  We need to avoid 0 probabilitites (`lambda = 1`).
  ```
  sevt_full <- staged_ev_tree(DD, method = "full", fit = TRUE, lambda = 1)
  sevt <- backward_hill_climb(sevt_full, verbose = FALSE)
  sevt$score$value
  plot(sevt)
  ```
  The default score function is `function(object) return(-BIC(object))`. 
  But it can be changed with the `score` parameter as follow:

  ```
  ## using logLik will merge only equal probabilities stages 
  sevt1 <- backward_hill_climb(sevt_full, score = logLik)

  ## instead penalizing a lot complexity
  score <- function(object) return(-AIC(object, k=100))
  sevt2 <- backward_hill_climb(sevt_full, score = score)
  sevt$score$value
  sevt$stages
  plot(sevt)
  ```


- ##### Fast Backward Hill-Climbing
  
  The algorithm moves to the **first** model that increase the score.
  ```
  ## we use verbose = TRUE  to obtain messages 
  ## We can use score as in the back_HC method
  sevt3 <- fast_backward_hill_climb(sevt_full)
  ```

- ##### Backward joining based on KL
  For every variable the algorithm iterates and at every step try to join the
  two stages with the smallest KL (symmetrized) if it's lower than a threshold. 
  ```
  sevt <- backward_joining_KL(sevt_full)
  plot(sevt) 
  ``` 

- ##### Using staged trees as classifiers

  ```
  pr <- predict(sevt, class = "C", newdata = DD[1:10,])

  table(pr, DD$C[1:10])
  ```

### Dev

- `testthat` is used to test the package..
- `roxygen2` is used to generate automatically the documentation.
- `Travis CI` is used to check automatically at every push. 

####  Commit messages

We try to keep commit messages cleaned, but sometimes big commit with lot of
changes can happen. 

- [+] file add 
- [-] file remov
- [upd] update (most used)
- [f] fix of a bug or error
- [doc] documentation


#### Roadmap 

- [x] stratified event tree model 
- [x] fitting stratified event tree (mle)
- [x] staged event tree
- [x] fitting staged event tree (mle)
- [ ] print method for staged and stratified event tree
- [ ] conversion BN to staged event tree
- [ ] extracting sub tree
- [x] plotting: 
    * [x] stratified event tree
    * [x] staged event tree (colors)
    * [ ] probabilities on plot
    * [ ] ggplot2 (maybe) 
    * [ ] Rgraphviz (....) 
- [ ] inference:
    * [x] joint prob of a path from root 
    * [x] logLik for full tree (thus AIC and BIC work automatically)
    * [x] logLik staged tree 
    * [x] lazy logLik
- [ ] structure search:
    * [ ] implement ``join_, split_, set_stage`` functions
    * [ ] exhaustive search 
    * [ ] heuristics:
        - [x] backward hill-climbing (3 variants)
        - [x] backward joining of stage based on KL distance
        - [ ] forward hill-climbing
	  * [ ] other distances (CD, total variation ..) (general function)
    * [x] Penalized logLik (AIC, BIC)
- [ ] classifiers with staged trees 
    * [x] define class 
    * [x] define predict method
    * [ ] eval 
    * [ ] model selection (struct search ...)

