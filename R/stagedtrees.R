#' Staged event trees.
#'
#' Algorithms to create, learn, fit and explore staged event tree models.
#' Functions to compute probabilities and make predictions from the fitted
#' models and to plot, analyze and manipulate staged event trees.
#'
#' Model selection algorithms:
#' * full model \code{\link{full}}
#' * independence model \code{\link{indep}}
#' * Hill-Climbing \code{\link{hc_sevt}}
#' * Backward Hill-Climbing \code{\link{bhc_sevt}}
#' * Fast Backward Hill-Climbing \code{\link{fbhc_sevt}}
#' * Backward Hill-Climbing Random \code{\link{bhcr_sevt}}
#' * Backward joining \code{\link{bj_sevt}}
#' * Naive Staged Event Tree \code{\link{naive_sevt}}
#' * Hierarchical Clustering \code{\link{hclust_sevt}}
#' * K-Means Clustering \code{\link{kmeans_sevt}}
#'
#' Probabilities, log-likelihood and predictions:
#' * Marginal probabilities \code{\link{prob_sevt}}
#' * Log-Likelihood \code{\link{logLik.sevt}}
#' * Predict method \code{\link{predict.sevt}}
#'
#' Plot, explore and compare:
#' * Plot \code{\link{plot.sevt}}
#' * Compare \code{\link{compare_stages}}
#' * Stages inclusion \code{\link{inclusion_stages}}
#' * Stages info \code{\link{stages}}, \code{\link{summary.sevt}}
#' 
#' Modify models:
#'  * Join and isolate unobserved situations \code{\link{join_zero}}
#'  * Join two stages \code{\link{join_stages}}
#'  * Automatic renaming of stages \code{\link{stndnaming}}
#'  * Rename a stage \code{\link{rename_stage}}
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
#' Thwaites P. A., Smith, J. Q.
#' A new method for tackling asymmetric decision problems.
#' _International Journal of Approximate Reasoning_, vol. 88, pp. 624–639, 2017.
#' @examples
#' data("PhDArticles")
#' mf <- full(PhDArticles)
#' mod <- fbhc_sevt(mf)
#' plot(mod)
NULL
