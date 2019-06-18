#' Staged event trees.
#'
#' Algorithms to crate, learn, fit and explore staged event tree models. 
#' Functions to compute probabilities and make predictions from the fitted 
#' models and to plot, analyze and manipulate staged event trees. 
#' 
#' Model selection algortihms:
#' * full model \code{\link{full}}
#' * independence model \code{\link{indep}}
#' * Hill-Climbing \code{\link{hc.sevt}}
#' * Backward Hill-Climbing \code{\link{bhc.sevt}}
#' * Fast Backward Hill-Climbing \code{\link{fbhc.sevt}}
#' * Backward Hill-Climbing Random \code{\link{bhcr.sevt}}
#' * Backward joining \code{\link{bj.sevt}}
#' * Naive Staged Event Tree \code{\link{naive.sevt}}
#' 
#' Probabilities, log-likelihood and predictions:
#' * Marginal probabilities \code{\link{prob.sevt}}
#' * Log-Likelihood \code{\link{logLik.sevt}}
#' * Predict method \code{\link{predict.sevt}}
#' 
#' Plot, explore and compare:
#' * Plot \code{\link{plot.sevt}}
#' * Compare \code{\link{compare.sevt}}
#' * Stages info \code{\link{stages.sevt}}, \code{\link{stageinfo.sevt}} 
#' @docType package
#' @name stagedtrees
#' @examples 
#' data("PhDArticles")
#' mf <- full(PhDArticles)
#' mod <- fbhc.sevt(mf)
#' plot(mod)
#' 
NULL
