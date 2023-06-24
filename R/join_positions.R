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
join_positions <- function(model, v, s1, s2) {
  i <- which(v == names(model$tree))
  order <- names(model$tree)
  model <- join_stages(model, v, s1, s2)
  if (i == length(model$tree)) {
    return(model)
  }
  for (j in (i + 1):length(model$tree)) {
    w <- names(model$tree)[j]
    lv <- length(model$tree[[j - 1]])
    model$stages[[w]] <-
      vapply(model$stages[[order[j - 1]]], function(s) {
        paste0(s, 1:lv)
      }, FUN.VALUE = rep("1", lv))[TRUE]
  }
  return(sevt_fit(model, scope = names(model$tree)[(i + 1):length(model$tree)]))
}
