#' Compute probability of a path from root
#'
#' Internal function to compute probability of a path. It does not
#' check the validity of the path.
#' @param object An object of class \code{sevt}.
#' @param x the path, expressed 
#'          as a character vector containing the sequence of the value of the variables.
#' @param log logical, if \code{TRUE} log-probability is returned.
#' @return The probability of the given path or its logarithm if \code{log=TRUE}.
#' @details Computes the probability of following a given path (\code{x}) starting from the root.
#' Can be a full path from the root to a leaf or a shorter path.
#' @keywords internal
path_probability <-
  function(object, x, log = FALSE) {
    check_sevt_prob(object)
    vs <- sevt_varnames(object)
    if (!is.null(names(x))) {
      # if it's a named vector just order it
      x <- x[vs]
    }
    # start computing the log probability with first variable
    l <- log(object$prob[[vs[1]]][[1]][x[1]])
    if (length(x) > 1) {
      for (i in 2:length(x)) {
        # get corresponding stage
        s <- find_stage(object, x[1:(i - 1)])
        # and add log-prob
        l <- l + log(object$prob[[vs[i]]][[s]][x[i]])
      }
    }
    # return log prob or prob as requested
    if (log) {
      return(l)
    } else {
      return(exp(l))
    }
  }


#' Probabilities for a staged event tree
#' 
#' Compute (marginal) probabilities of elementary events with respect 
#' to the probability encoded in a staged event tree.
#' @param object an object of class \code{sevt} with probabilities.
#' @param x the vector or data.frame of observations.
#' @param log logical, if \code{TRUE} log-probabilities are returned.
#' @param na0 logical, if \code{NA} should be converted to 0.
#' @return the probabilities to observe each observation given in \code{x}.
#'
#' @details Computes probabilities related to a vector or a 
#' data.frame of observations.
#' @examples
#' DD <- generate_random_dataset(5, 100)
#' model <- full(DD, lambda = 1)
#' pr <- prob(model, expand.grid(model$tree[c(2, 3, 4)]))
#' sum(pr)
#' prob(model, DD[1:10, ])
#' @export
prob <- function(object, x, log = FALSE, na0 = TRUE) {
  check_sevt_prob(object)
  if (is.null(dim(x))) {
    x <- as.data.frame(t(x))
  }
  # get dimensions and variables
  n <- nrow(x)
  i <- ncol(x)
  # get variables in the model
  var <- names(object$tree)
  # variables of the model that are in x
  var1 <- var[var %in% colnames(x)]
  # index of last variable that appears in x
  k <- which(var %in% var1[length(var1)])
  res <- vapply(
    1:n,
    FUN.VALUE = 1,
    FUN = function(i) {
      ll <- object$tree[1:k]
      ll[var1] <- vapply(x[i, var1], as.character, FUN.VALUE = "aaa")
      sum(apply(
        expand.grid(ll),
        MARGIN = 1,
        FUN = function(xx) {
          path_probability(object, as.character(xx), log = FALSE)
        }
      ), na.rm = TRUE)
    }
  )
  if (na0) res[is.na(res)] <- 0
  if (log) {
    return(log(res))
  } else {
    return(res)
  }
}


#' Log-Likelihood of a staged event tree
#'
#' Compute, or extract the log-likelihood of a staged event tree.
#' @param object an fitted object of class \code{sevt}.
#' @param ... additional parameters (compatibility).
#' @return An object of class \code{\link{logLik}}.
#' @importFrom stats logLik
#' @export
#' @examples
#' data("PhDArticles")
#' mod <- indep(PhDArticles)
#' logLik(mod)
logLik.sevt <- function(object, ...) {
  if (!is.null(object$ll)) {
    return(object$ll)
  }
  check_sevt_fit(object)
  vars <- names(object$tree)
  prob <- expand_prob(object)
  ll <- sum(vapply(
    seq_along(object$tree),
    FUN = function(i) {
      if (any(is.nan(prob[[vars[i]]]) &
              object$ctables[[vars[i]]] > 0)) {
        return(-Inf)
      }
      ix <-
        prob[[vars[i]]] > 0 & !is.na(prob[[vars[i]]]) &
        !is.nan(prob[[vars[i]]])
      sum(log(prob[[vars[i]]][ix]) *
            object$ctables[[vars[i]]][ix])
    },
    FUN.VALUE = 1
  ))
  attr(ll, "df") <-
    sum(c(1, vapply(
      object$stages[ vars[-1] ],
      FUN = function(x) {
        length(unique(x))
      },
      FUN.VALUE = 1
    )) *
      (vapply(
        object$tree,
        FUN = length, FUN.VALUE = 1
      ) - 1)) ## compute the degree of freedom
  attr(ll, "nobs") <- sum(object$ctables[[vars[1]]])
  class(ll) <- "logLik"
  return(ll)
}



#' Confidence intervals for staged event tree parameters
#'
#' Confint method for class \code{sevt}.
#'
#' @param object an object of class \code{sevt}.
#' @param parm 	a specification of which parameters are to be given 
#'              confidence intervals, either a vector of numbers 
#'              or a vector of names. If missing, all parameters are considered.
#' @param level the confidence level required.
#' @param sides a character string specifying the side of the confidence interval, must be one of "two.sided" 
#' (default), "left" or "right".
#' @param method a character string specifing which method to use: "goodman", "wald", "waldcc" or "wilson".
#' @param ignore vector of stages which will be ignored, 
#'               by default the name of the unobserved stages stored in 
#'               \code{object$name_unobserved}.
#' @param ... additional argument(s) for methods.
#' @details Compute confidence intervals for staged event trees. By default, 
#'          it computes two-sided confidence intervals at 95% level for the 
#'          whole tree given in \code{object} with the wald method. 
#' @return An object of class \code{confint.sevt} containing the desired confidence intervals.
#' @references Goodman, L. A. (1965) On Simultaneous Confidence Intervals for Multinomial Proportions Technometrics, 7, 247-254.
#' @references Wald, A. Tests of statistical hypotheses concerning several parameters when the number of observations is large, Trans. Am. Math. Soc. 54 (1943) 426-482.
#' @references Wilson, E. B. Probable inference, the law of succession and statistical inference, J.Am. Stat. Assoc. 22 (1927) 209-212.
#' @author The function is partially inspired by code in the 
#'         \code{MultinomCI} function from the \pkg{DescTools} package, 
#'         implemented by Andri Signorelli and Pablo J. Villacorta Iglesias.  
#' @examples
#' m1 <- stages_bj(full(PhDArticles), distance = "kullback", thr = 0.01)
#' confint(m1, "Prestige", level = 0.90, sides = "left")
#' confint(m1, "Married")
#' confint(m1, c("Married", "Kids"))
#' @importFrom stats confint
#' @importFrom stats qchisq
#' @export
confint.sevt <- function (object, parm, level = 0.95,
                          sides = c("two.sided", "left", "right"), 
                          method = c("goodman", "wald", "waldcc", "wilson"),
                          ignore = object$name_unobserved,
                          ...) {
  check_sevt(object)
  check_sevt_fit(object)
  vv <- sevt_varnames(object)
  if (missing(parm)){
    parm <- vv
  }else if (is.numeric(parm)){
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
  if (missing(sides)) {
    sides <- "two.sided"
  }
  
  sides <- match.arg(sides, choices = c("two.sided", "left", "right"), several.ok = FALSE)
  if (sides != "two.sided") {
    level <- 1 - 2 * (1 - level)
  }
  
  lambda <- object$lambda
  method <- match.arg(arg = method, choices = c("goodman", "wald", "waldcc", "wilson"))
  
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
      if (method == "goodman") {
        q.chi <- qchisq(level, k - 1)
        lci <- (q.chi + 2 * n_stage  - sqrt(q.chi * (q.chi + 4 * n_stage * 
                                               (n - n_stage)/n)))/(2 * (n + q.chi))
        uci <- (q.chi + 2 * n_stage + sqrt(q.chi * (q.chi + 4 * n_stage * 
                                                      (n - n_stage)/n)))/(2 * (n + q.chi))
      }
      else if (method == "wald") {
        q.chi <- qchisq(level, 1)
        lci <- p - sqrt(q.chi * p * (1 - p)/n)
        uci <- p + sqrt(q.chi * p * (1 - p)/n)
      }
      else if (method == "waldcc") {
        q.chi <- qchisq(level, 1)
        lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2 * n)
        uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2 * n)
      }
      else if (method == "wilson") {
        q.chi <- qchisq(level, 1)
        lci <- (q.chi + 2 * n_stage - sqrt(q.chi^2 + 4 * n_stage * q.chi * 
                                             (1 - p)))/(2 * (q.chi + n))
        uci <- (q.chi + 2 * n_stage + sqrt(q.chi^2 + 4 * n_stage * q.chi * 
                                             (1 - p)))/(2 * (q.chi + n))
      }
      
      if (sides == "left") {
        uci[] <- 1
      }
      else if (sides == "right") {
        lci[] <- 0
      }
      ci[paste0(v,"=", object$tree[[v]],"|", s),1] <- pmax(0,lci)
      ci[paste0(v,"=", object$tree[[v]],"|", s),2] <- pmin(1,uci)
      
    }
  }
  return(ci)
}

