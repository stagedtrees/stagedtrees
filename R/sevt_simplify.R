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

#' Simplify a staged tree model
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
#'          Despite the name, the simplified staged tree has always a number
#'          of stages greater or equal to the initial staged tree, thus it is
#'          a more complex statistical model.
#' @examples
#' mod <- stages_kmeans(full(Titanic), k = 2)
#' simpl <- sevt_simplify(mod)
#' plot(simpl)
#' @export
sevt_simplify <- function(model, fit = TRUE) {
  tmp_ceg <- ceg(model)
  oldstg <- model$stages
  model$stages <- sapply(names(model$stages), function(vv) {
    pp <- tmp_ceg$positions[[vv]]
    ix <- oldstg[[vv]] %in% model$name_unobserved
    pp[ix] <- oldstg[[vv]][ix]
    pp
  }, USE.NAMES = TRUE)
  ## if model was fitted then refit it
  if (is_fitted_sevt(model)) {
    if (fit) {
      model <- sevt_fit(model)
    } else {
      model$prob <- NULL
      model$ll <- NULL
    }
  }
  return(model)
}
