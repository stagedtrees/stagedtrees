#' Likelihood Ratio Test for staged trees models
#'
#' Function to perform likelihood ratio test between
#' two or multiple staged event tree models.
#' @param object an object of class \code{\link{sevt}}.
#' @param ... further objects of class \code{\link{sevt}}.
#'            Must specify super-models of \code{object}.
#'            See below for details.
#' @details If a single object of class \code{sevt} is passed as
#'          argument, it computes
#'          the likelihood-ratio test with respect to the
#'          independence model.
#'          If multiple objects are passed,
#'          likelihood-ratio tests between the first
#'          object and the followings are computed.
#'          In the latter casem the function checks automatically if
#'          the first model is nested in the additional ones,
#'          via \code{\link{inclusions_stages}}, and throws
#'          an error if not.
#' @return An object of class \code{anova}
#'         which contains the log-likelihood,
#'         degrees of freedom,
#'         difference in degrees of freedom, likelihood ratio
#'         statistics and corresponding p values.
#' @examples
#' data(PhDArticles)
#' order <- c("Gender", "Kids", "Married", "Articles")
#' phd.mod1 <- stages_hc(indep(PhDArticles, order))
#' phd.mod2 <- stages_hc(full(PhDArticles, order))
#'
#' ## compare two nested models
#' lr_test(phd.mod1, phd.mod2)
#'
#' ## compare a single model vs the independence model
#' lr_test(phd.mod1)
#' @importFrom stats pchisq
#' @export
lr_test <- function(object, ...) {
  check_sevt_fit(object)
  others <- list(...)
  nmodels <- length(others) + 1
  variables <- lapply(match.call()[-1L], deparse)[1L:nmodels]
  if (nmodels == 1) {
    base <- sevt(object$tree)
    base$ctables <- object$ctables
    base$lambda <- object$lambda
    base <- sevt_fit(base)
    others <- list(object)
    object <- base
    variables <- c("indep", variables)
    nmodels <- 2
  }
  if (object$lambda != 0) {
    warning("parameters are not fitted with maximum-likelihood")
  }
  others_sts <- lapply(others, function(object2) {
    check_sevt_fit(object2)
    stopifnot(sevt_nvar(object) == sevt_nvar(object2))
    stopifnot(all(sevt_varnames(object) == sevt_varnames(object2)))
    if (object2$lambda != 0) {
      warning("parameters are not fitted with maximum-likelihood")
    }
    # check nested models
    incl_st <- inclusions_stages(object, object2)
    for (i in 1:length(incl_st)) {
      if (any(incl_st[[i]][, 2] %in% c("!=", "<"))) {
        stop(paste(c(
          "Not nested models specified. Check stages structures for ",
          names(incl_st)[i]
        )), call. = FALSE)
      }
    }
    logLik(object2)
  })
  ls <- vapply(object$tree, length, 1L)
  moddesc <- paste(paste0(names(object$tree), "[", ls, "] "), collapse = "-> ")
  L1 <- logLik(object)
  L2 <- vapply(others_sts, as.numeric, 1.0)
  df1 <- attr(L1, "df")
  df2 <- vapply(others_sts, attr, which = "df", FUN.VALUE = 1.0)
  rval <- matrix(NA, ncol = 5, nrow = nmodels)
  rownames(rval) <- 1:nmodels
  colnames(rval) <- c("#Df", "LogLik", "Df", "Chisq", "Pr(>Chisq)")
  rval[, 1] <- c(df1, df2)
  rval[, 2] <- c(L1, L2)
  rval[-1, 3] <- df2 - df1
  rval[-1, 4] <- 2 * (L2 - L1)
  rval[-1, 5] <- pchisq(2 * (L2 - L1), df = abs(df2 - df1), lower.tail = FALSE)
  topnote <- paste("Model ", format(1:nmodels), ": ", variables,
    sep = "", collapse = "\n"
  )
  structure(as.data.frame(rval),
    heading = c("Likelihood-ratio test \n", moddesc, topnote),
    class = c("anova", "data.frame")
  )
}
