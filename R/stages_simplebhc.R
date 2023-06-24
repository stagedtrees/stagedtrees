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

#' Backward hill-climbing for simple staged trees
#'
#' Greedy search of simple staged event trees
#' with iterative joining of positions.
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
#'
#' @details This function is similar to the classical
#' backward hill-climbing implemented in \code{\link{stages_bhc}}, but
#' instead of joining stages it consider joining of _positions_ via
#' \code{\link{join_positions}}.
#' Thus, the search is in the space of simple staged tree models if the
#' initial stage tree is simple.
#' See the references for additional details.
#'
#' @return an object of class \code{sevt}, the simple staged tree resulting
#'         from the search.
#' @references Leonelli M, Varando G.
#'             Structural Learning of Simple Staged Trees,
#'             _arXiv preprint_
#'             \href{https://arxiv.org/abs/2203.04390}{arXiv:2203.04390v1}
#' @examples
#' mod <- stages_simplebhc(full(Titanic))
#' plot(mod)
#' @seealso [join_positions()]
#'          [sevt_simplify()]
#' @export
stages_simplebhc <- function(object,
                             score = function(x) {
                               return(-BIC(x))
                             },
                             scope = NULL,
                             max_iter = Inf,
                             ignore = object$name_unobserved) {
  check_sevt_fit(object)
  now_score <- score(object)
  if (is.null(scope)) {
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
