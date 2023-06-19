### the code for simple stagedtrees was made available initially under 
###  BSD 2-Clause License, we thus add the attribution, 
###  license text and disclaimer here.
### original repo with the code 
### https://github.com/gherardovarando/simple_stagedtrees
# BSD 2-Clause License
# 
# Copyright (c) 2021, Gherardo Varando and Manuele Leonelli,
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#   
#   1. Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#          SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#' Simplify a stagedtree 
#' 
#' Function to simplify a staged tree model. 
#' @param model an object of class \code{sevt}
#' @param fit logical, if \code{TRUE} refit the model after simplification.
#' @return an object of class \code{sevt} 
#'         representing the simplified model. 
#'         The returned model will be fitted if the input \code{model} was. 
#' @details The \code{simplify} function will produce the corresponding simple
#'          staged tree, that is a staged tree where stages and positions are 
#'          equivalent. 
#'          To do so the function \code{ceg} is used to compute positions, and 
#'          then the stages' vectors are replaced with the positions' vectors. 
#'          The model is the re-fitted if the input was a fitted staged tree. 
#'          
#'          Despite the name, the simplified staged tree has always a number 
#'          of stages greater or equal to the initial staged tree, thus it is 
#'          a more complex statistical model. 
#' @examples
#' mod <- stages_kmeans(full(Titanic), k = 2)
#' simpl <- simplify(mod)
#' plot(simpl)
#' @export
simplify <- function(model, fit = TRUE){
  tmp_ceg <- ceg(model)
  oldstg <- model$stages
  model$stages <- sapply(names(model$stages), function(vv){
    pp <- tmp_ceg$positions[[vv]]
    ix <- oldstg[[vv]] %in% model$name_unobserved
    pp[ix] <- oldstg[[vv]][ix]
    pp
  }, USE.NAMES = TRUE)
  ## if model was fitted then refit it
  if (is_fitted_sevt(model)){
    if (fit){
      model <- sevt_fit(model)
    }else{
      model$prob <- NULL
      model$ll <- NULL
    }
  }
  return(model)
}


#' join positions in a staged tree model
#' 
#' @param model an object of class \code{sevt}.
#' @param v the name of a variable in the model.
#' @param s1 stage to join
#' @param s2 stage to join
#' @details this functions works similarly to the \code{join_stages}
#' function in the \code{stagedtrees} package, but it also joins 
#' downstream stages to make nodes with stages \code{s1,s2} in the same 
#' position. This function works properly only when downstream variables 
#' from \code{v} have full stages vectors. 
#' @export
join_positions <- function(model, v, s1, s2){
  i <- which(v == names(model$tree))
  order <- names(model$tree)
  model <- join_stages(model, v, s1, s2)
  if (i == length(model$tree)) return(model)
  for (j in (i+1):length(model$tree)){
    w <- names(model$tree)[j]
    lv <- length(model$tree[[j-1]])
    model$stages[[w]] <- 
      vapply(model$stages[[order[j-1]]], function(s){
        paste0(s, 1:lv)
      }, FUN.VALUE = rep("1", lv))[TRUE]
  }
  return(sevt_fit(model, scope = names(model$tree)[(i+1):length(model$tree)]))
}


#' Total BHC search of simple staged trees
#' 
#' @param object an object of class \code{\link{sevt}} 
#'               with fitted probabilities and data, 
#'               as returned by \code{\link{full}} or 
#'               \code{\link{sevt_fit}}.
#' @param score the score function to be maximized.
#' @param scope names of variables that should be considered for 
#'              the optimization.
#' @param max_iter the maximum number of iterations per variable.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in 
#'               \code{object$name_unobserved}.
#' @return an object of class \code{sevt}, the simple staged tree resulting 
#'         from the search. 
#' @references Leonelli M, Varando G. 
#'             Structural Learning of Simple Staged Trees, 
#'             _arXiv preprint_ 
#'             \href{https://arxiv.org/abs/2203.04390}{arXiv:2203.04390v1}
#' @examples
#' mod <- stages_simplebhc(full(Titanic))
#' plot(mod)
#' @export
stages_simplebhc <- function (object, 
                              score = function(x) {return(-BIC(x))}, 
                              scope = NULL,
                              max_iter = Inf,
                              ignore = object$name_unobserved) {
  check_sevt_fit(object)
  now_score <- score(object)
  if (is.null(scope)){
    scope <- sevt_varnames(object)[-1]
  }
  stopifnot(all(scope %in% sevt_varnames(object)[-1]))
  for (v in scope) {
    r <- 1
    iter <- 0
    done <- FALSE
    while (!done && iter < max_iter) {
      iter <- iter + 1
      temp <- object # clone the object
      temp_score <- now_score
      done <- TRUE
      stages <- unique(object$stages[[v]])
      stages <- stages[!(stages %in% ignore)]
      if (length(stages) > 1) {
        for (i in 2:length(stages)) {
          s1 <- stages[i]
          for (j in 1:(i - 1)) {
            s2 <- stages[j]
            try <- join_positions(object, v, s1, s2)
            try_score <- score(try)
            if (try_score >= temp_score) {
              temp <- try
              temp_score <- try_score
              s1a <- s1
              s2a <- s2
              done <- FALSE
            }
          }
        }
      }
      object <- temp
      now_score <- temp_score
    }
  }
  object$call <- sys.call()
  object$score <- list(value = now_score, f = score)
  return(object)
}