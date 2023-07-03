#' Confidence intervals for staged event tree parameters
#'
#' Confint method for class \code{sevt}.
#'
#' @param object an object of class \code{sevt}.
#' @param parm 	a specification of which parameters are to be given
#'              confidence intervals, either a vector of numbers
#'              or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required.
#' @param method a character string specifing which method to use:
#'              wald", "waldcc", "goodman", "quesenberry-hurst" or "wilson".
#' @param ignore vector of stages which will be ignored,
#'               by default the name of the unobserved stages stored in
#'               \code{object$name_unobserved}.
#' @param ... additional argument(s) for compatibility
#'            with \code{confint} methods.
#' @details Compute confidence intervals for staged event trees.
#' Currently five methods are available:
#' * \code{wald}, \code{waldcc}: Wald method and with continuity correction.
#' * \code{wilson}, \code{quesenberry-hurst} and \code{goodman}.
#' @return A matrix with columns giving lower and upper confidence
#'         limits for each parameter. These will be labelled as
#'         \code{(1-level)/2} and \code{1 - (1-level)/2} in %
#'         (by default 2.5% and 97.5%).
#' @references Goodman, L. A. (1965) On Simultaneous Confidence Intervals for
#'             Multinomial Proportions Technometrics, 7, 247-254.
#' @references Wald, A. Tests of statistical hypotheses concerning several
#'             parameters when the number of observations is large, Trans.
#'             Am. Math. Soc. 54 (1943) 426-482.
#' @references Wilson, E. B. Probable inference, the law of succession and
#'             statistical inference, J.Am. Stat. Assoc. 22 (1927) 209-212.
#' @references Quesenberry, C., & Hurst, D. (1964). Large Sample Simultaneous
#'             Confidence Intervals for Multinomial Proportions.
#'             Technometrics, 6(2), 191-195
#' @author The function is partially inspired by code in the
#'         \code{MultinomCI} function from the \pkg{DescTools} package,
#'         implemented by Andri Signorelli and Pablo J. Villacorta Iglesias.
#' @examples
#' m1 <- stages_bj(full(PhDArticles), distance = "kullback", thr = 0.01)
#' confint(m1, "Prestige", level = 0.90)
#' confint(m1, "Married", method = "goodman")
#' confint(m1, c("Married", "Kids"))
#' @importFrom stats confint
#' @importFrom stats qchisq
#' @export
confint.sevt <- function (object, parm, level = 0.95,
                          method = c( "wald", "waldcc", "wilson", "goodman",
                                      "quesenberry-hurst"),
                          ignore = object$name_unobserved,
                          ...) {

  check_sevt_fit(object)
  vv <- sevt_varnames(object)
  if (missing(parm)){
    parm <- vv
  } else if (is.numeric(parm)){
    parm <- vv[parm]
  }
  ## order parm and remove NA
  parm <- vv[vv %in% parm]

  ## expand parm
  exparm <- unlist(sapply(parm, function(v){
    s <- unique(object$stages[[v]])
    s <- s[!(s %in% ignore)]
    if (is.null(s)) s <- "1"
    apply(expand.grid(object$tree[[v]],s),1, function(x){
      paste0(v,"=", x[1],"|", x[2])
    })
  }, simplify = FALSE), use.names = FALSE)

  if (missing(method)) {
    method <- "wald"
  }

  lambda <- object$lambda
  method <- match.arg(arg = method, choices = c("wald", "waldcc", "goodman",
                                                "wilson", "quesenberry-hurst"))

  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- paste0(format(100*a,digits = 3, ), " %")
  ci <- array(NA_real_, dim = c(length(exparm), 2L),
              dimnames = list(exparm, pct))

  for(v in parm) {
    stages <- unique(object$stages[[v]])
    stages <- stages[!(stages %in% ignore)]
    if (is.null(stages)) stages <- "1"
    k <- length(object$tree[[v]])
    for(s in stages) {
      p <- object$prob[[v]][[s]]
      n <- attr(p, "n")
      n_stage <- p * (n + k * lambda) - lambda
      if (lambda > 0) p <- n_stage / n  #unbiased prob estimate
      if (method == "wald") {
        q.chi <- qchisq(level, 1)
        lci <- p - sqrt(q.chi * p * (1 - p)/n)
        uci <- p + sqrt(q.chi * p * (1 - p)/n)
      }else if (method == "waldcc") {
        ## wald test with continuity correction
        q.chi <- qchisq(level, 1)
        lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2 * n)
        uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2 * n)
      }else if (method == "wilson") {
        ## Wilson (1927), page 210 (or 76) (JSTOR version)
        q.chi <- qchisq(level, 1)
        tmp <- sqrt(q.chi^2 + 4 * n_stage * q.chi * (1 - p))
        lci <- (q.chi + 2 * n_stage - tmp)/(2 * (n + q.chi))
        uci <- (q.chi + 2 * n_stage + tmp)/(2 * (n + q.chi))
      }else if (method == "quesenberry-hurst") {
        ## Goodman (1965), page 247 (JSTOR version)
        ## Quesenberry, C., & Hurst, D (1964)
        q.chi <- qchisq(1 - level, k - 1, lower.tail = FALSE)
        tmp <- sqrt(q.chi^2 + 4 * n_stage * q.chi * (1 - p))
        lci <- (q.chi + 2 * n_stage - tmp)/(2 * (n + q.chi))
        uci <- (q.chi + 2 * n_stage + tmp)/(2 * (n + q.chi))
      }else if (method == "goodman") {
        ## Goodman (1965), page 248 (JSTOR version)
        ## upper quantile
        q.chi <- qchisq( (1 - level) / k, 1, lower.tail = FALSE)
        tmp <- sqrt(q.chi^2 + 4 * n_stage * q.chi * (1 - p))
        lci <- (q.chi + 2 * n_stage - tmp)/(2 * (n + q.chi))
        uci <- (q.chi + 2 * n_stage + tmp)/(2 * (n + q.chi))
      }

      if (lambda > 0){
        ## correct ci for biased estimator, as E. Riccomagno notes
        lci <- (n * lci + lambda) / (n + k * lambda)
        uci <- (n * uci + lambda) / (n + k * lambda)
      }

      ci[paste0(v,"=", object$tree[[v]],"|", s),1] <- pmax(0,lci)
      ci[paste0(v,"=", object$tree[[v]],"|", s),2] <- pmin(1,uci)

    }
  }
  return(ci)
}
