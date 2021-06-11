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
#' Compute (marginal and/or conditional) probabilities of elementary 
#' events with respect 
#' to the probability encoded in a staged event tree.
#' @param object an object of class \code{sevt} with probabilities.
#' @param x the vector or data.frame of observations.
#' @param conditional_on named vector, the conditioning event. 
#' @param log logical, if \code{TRUE} log-probabilities are returned.
#' @param na0 logical, if \code{NA} should be converted to 0.
#' @return the probabilities to observe each observation in \code{x}, possibly
#' conditional on the event(s) in \code{conditional_on}.
#'
#' @details Computes probabilities related to a vector or a 
#' data.frame of observations.
#' 
#' Optionally, conditional probabilities can be obtained by specifying 
#' the conditioning event in \code{conditional_on}. This can be done either
#' with a single named vector or with a data.frame object with the 
#' same number of rows of \code{x}. In the former, the same conditioning 
#' is used for all the computed probabilities (if \code{x} has multiple rows); 
#' while with the latter different conditioning events (but on the same variables)
#' can be specified for each row of \code{x}. 
#' 
#' @examples
#' data(Titanic)
#' model <- full(Titanic, lambda = 1)
#' samples <- expand.grid(model$tree[c(1, 4)])
#' pr <- prob(model, samples)
#' ## probabilities sum up to one 
#' sum(pr)
#' ## print observations with probabilities
#' print(cbind(samples, probability = pr))
#' 
#' ## compute one probability
#' prob(model, c(Class = "1st", Survived = "Yes"))
#' 
#' ## compute conditional probability 
#' prob(model, c(Survived = "Yes"), conditional_on = c(Class = "1st"))
#' 
#' ## compute conditional probabilities with different conditioning set
#' prob(model, data.frame(Age = rep("Adult", 8)), 
#'      conditional_on = expand.grid(model$tree[2:1])) 
#' ## the above should be the same as 
#' summary(model)$stages.info$Age
#' @export
prob <- function(object, x, conditional_on = NULL, log = FALSE, na0 = TRUE) {
  check_sevt_prob(object)
  if (is.null(dim(x))) {
    x <- as.data.frame(t(x))
  }
  p1 <- 1
  if (!is.null(conditional_on)){
    if (is.vector(conditional_on) && !is.null(names(conditional_on))){
      ## check if same names
      if (any(names(x) %in% names(conditional_on))){
        stop("invalid argument, x and conditional_on names should be disjoint")
      }
      x <- cbind(x, as.data.frame(t(conditional_on)))
      p1 <- prob(object, x = conditional_on, log = FALSE, na0 = na0)
    }else if (is.data.frame(conditional_on)){
      ## check if same names
      if (any(names(x) %in% names(conditional_on))){
        stop("invalid argument, x and conditional_on names should be disjoint")
      }
      x <- cbind(x, conditional_on)
      p1 <- prob(object, x = conditional_on, log = FALSE, na0 = na0)
      }else{
      stop("invalida argument, conditional_on must be NULL,
           a named vector or a data.frame")
    }
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
  res <- res / p1
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
  
#' Likelihood Ratio Test for staged trees models
#' 
#' @param mod1 simplest staged tree model.
#' @param mod2 model that has to be nested in mod1. It must have a number of degrees of freedom
#' greater or equal to the one of \code{mod1}.
#' @details It computes the likelihood ratio test for two nested staged trees models. 
#' The function checks automatically if the models are nested, returning an error if they are not.
#' @return It returns the corresponding p-value.
#' @examples 
#' phd.mod1 <- stndnaming(stages_hc(indep(PhDArticles, order = order)))
#' phd.mod2 <- stndnaming(stages_hc(full(PhDArticles, order = order)))
#' LR_test(phd.mod1, phd.mod2)
#' @export
  LR_test <- function(mod1, mod2) {
    check_sevt(mod1)
    check_sevt(mod2)
    stopifnot(sevt_nvar(mod1) == sevt_nvar(mod2))
    stopifnot(all(sevt_varnames(mod1) == sevt_varnames(mod2)))
    
    # check nested models
    incl_st <- inclusions_stages(mod1, mod2)
    for(i in 1:length(incl_st)) {
      if(any(incl_st[[i]][, 2] %in% c("!=", "<"))) stop(paste(c("mod1 and mod2 are not nested models. Check stages structures for ", names(incl_st)[i])))
    }
    
    L1 <- logLik(mod1)
    L2 <- logLik(mod2) 
    df <- attr(L2, "df") - attr(L1, "df")
    cat("p-value =", pchisq(2 * (L2 - L1), df = df, lower.tail = FALSE))
}


