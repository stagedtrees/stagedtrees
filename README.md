## stagedtrees 

[![Build Status](https://travis-ci.com/gherardovarando/stagedtrees.svg?branch=master)](https://travis-ci.com/gherardovarando/stagedtrees)

### Use

- install with
  `devtools::install_github("gherardovarando/stagedtrees")`


#### examples

For all the examples we use stupid dataset created as:

```
N <- 100 ## number of observations
n <- 4 ## number of variables
DD <- as.data.frame(sapply(1:n, function(i){
                                     return(as.factor(sample(c(0,1), size=N,
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
  ```
  ## no need to set fit = TRUE, models will be fitted anyway
  sevt <- staged_ev_tree(DD, method = "back_HC", verbose = T)
  sevt$score$value
  plot(sevt)
  ```
  The default score function is `function(object) return(-BIC(object))`. 
  But it can be changed with the `score` parameter as follow:

  ```
  ## using logLik will not merge any stage as expected since we are not
  ## penalizing complexity 
  sevt <- staged_ev_tree(DD, method="back_HC", score = logLik)

  ## instead penalizing a lot complexity
  score <- function(object) return(-AIC(object, k=100))
  sevt <- staged_ev_tree(DD, method="back_HC", score = score)
  sevt$score$value
  sevt$stages
  plot(sevt)
  ```


- ##### Fast Backward Hill-Climbing
  
  The algorithm moves to the **first** model that increase the score.
  ```
  ## we use verbose = TRUE  to obtain messages 
  ## We can use score as in the back_HC method
  sevt <- staged_ev_tree(DD, method = "fast_back_HC"
                           , eps=0, max_iter = Inf, verbose = TRUE)  
  ```

- ##### Backward joining based on KL
  For every variable the algorithm iterates and at every step try to join the two stages with the smallest KL (symmetrized) if it's lower than a threshold. 
```
 sevt <- staged_ev_tree(DD, method = "back_join_KL", thr = 0.01)
 plot(sevt) 
``` 

- ##### Using staged trees as classifiers

```
D <- generate_xor_dataset(n = 5, N = 100) 

model <- staged_ev_tree(D[1:500,6:1], method = "back_join_KL")

pr <- predict(model, class = "C", newdata = D[501:1000,])

table(pr, D$C[501:1000])

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
- [ ] structure search:
    * [ ] implement ``join_, split_, set_stage`` functions
    * [ ] exhaustive search 
    * [ ] heuristics:
        - [x] backward hill-climbing (3 variants)
        - [x] backward joining of stage based on KL distance
        - [ ] forward hill-climbing
    * [x] Penalized logLik (AIC, BIC)
- [ ] classifiers with staged trees 
    * [x] define class 
    * [x] define predict method
    * [ ] eval 
    * [ ] model selection (struct search ...)

