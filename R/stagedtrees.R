#' Staged event trees.
#'
#' Algorithms to create, learn, fit and explore staged event tree models. 
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
#' @references 
#' Collazo R. A., Görgen C. and Smith J. Q. 
#' Chain event graphs. CRC Press, 2018. 
#'              
#' Görgen C., Bigatti A., Riccomagno E. and Smith J. Q.
#' Discovery of statistical equivalence classes using computer algebra.
#' _International Journal of Approximate Reasoning_, vol. 95, pp. 167-184, 2018.
#'       
#' Barclay L. M., Hutton J. L. and Smith J. Q. 
#' Refining a Bayesian network using a chain event graph.
#' _International Journal of Approximate Reasoning_, vol. 54, pp. 1300-1309, 2013.
#'      
#' Smith J. Q. and Anderson P. E.
#' Conditional independence and chain event graphs.
#' _Artificial Intelligence_, vol. 172, pp. 42-68, 2008.
#' 
#' @examples 
#' data("PhDArticles")
#' mf <- full(PhDArticles)
#' mod <- fbhc.sevt(mf)
#' plot(mod)
#' 
NULL
