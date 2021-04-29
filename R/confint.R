#' Confidence intervals for staged event tree parameters
#'
#' Confint method for class \code{sevt}.
#'
#' @param object an object of class \code{sevt}.
#' @param level the confidence level desired.
#' @param sides a character string specifying the side of the confidence interval, must be one of "two.sided" 
#' (default), "left" or "right".
#' @param method a character string specifing which method to use: "goodman", "wald", "waldcc" or "wilson".
#' @param lambda smoothing coefficient (default object$lambda).
#' @param digits number of digits for the confidence intervals; default is 4.
#' @param var name of the variables in \code{object} for which the confidence intervals has to be calculated.
#'  Default is "all", which computes confidence intervals for all the variables in \code{object}.
#' @param stage character vector, the name of the stages for \code{var} for which the confidence intervals 
#' should be calculated. Default is "all", which computes confidence intervals 
#' for all the stages for variable \code{var} in \code{object}.
#' @param ... arguments for compatibility.
#' @details Compute confidence intervals for staged event trees. By default, it computes two-sided confidence intervals
#' at 95% level for the whole tree given in \code{object} with the wald method. If a single variable is given
#' in \code{var}, \code{stage} can be specified as a character vector of stages, while if multiple variables are 
#' given in \code{var}, \code{stage} should be only set to \code{"all"} (this is done automatically in the function).
#' @return An object of class \code{confint.sevt} containing the desired confidence intervals.
#' @references Goodman, L. A. (1965) On Simultaneous Confidence Intervals for Multinomial Proportions Technometrics, 7, 247-254.
#' @references Wald, A. Tests of statistical hypotheses concerning several parameters when the number of observations is large, Trans. Am. Math. Soc. 54 (1943) 426-482.
#' @references Wilson, E. B. Probable inference, the law of succession and statistical inference, J.Am. Stat. Assoc. 22 (1927) 209-212.
#' @examples
#' m1 <- stages_bj(full(PhDArticles), distance = "kullback", thr = 0.01)
#' confint1(m1)
#' confint1(m1, level = 0.90, sides = "left", var = "Prestige")
#' confint1(m1, var = "Married", stage = c(1,2,6))
#' confint1(m1, var = c("Married", "Kids"), stage = c(9:12))
#' @export
confint1 <- function (object, level = 0.95, sides = c("two.sided", "left", "right"), 
                          method = c("goodman", "wald", "waldcc", "wilson"),
                          lambda = object$lambda, digits = 4, var = "all", stage = "all", ...) 
{
  check_sevt(object)
  
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
  
  method <- match.arg(arg = method, choices = c("goodman", "wald", "waldcc", "wilson"))
  
  stage <- as.character(stage)
  var <- as.character(var)
  
  if(var[1] != "all" & length(var) == 1) {
    
    k <- length(object$tree[[var]])
    st <- object$stages[[var]]
    
    if(stage[1] == "all") {
      stage <- unique(st)
    }
    for(i in 1:length(stage)) {
      if (!(stage[i] %in% stages(object, var = var))) {
        stop("At least one Stage is not present")
      }
    }
    
    ci <- list(c())
    attr(ci, "names") <- var
    ci[[var]] <- matrix(NA, nrow = length(stage), ncol = length(object$tree[[var]]) * 3)
    b <- c(paste("CI_Low_", object$tree[[var]], sep = ""), paste("CI_Upp_", object$tree[[var]], sep = ""))
    d <- c()
    for(q in 1:length(object$tree[[var]])) d <- c(d, b[c(q, length(object$tree[[var]]) + q)])
    colnames(ci[[var]]) <- c(object$tree[[var]], d)
    rownames(ci[[var]]) <- stage[stage %in% st]
    count <- 0
    
    for(i in 1:length(object$prob[[var]])) {
      if(attr(object$prob[[var]], "names")[i] %in% stage) {
        count <- count + 1
        n_stage <- as.numeric(object$prob[[var]][[i]]) * (attr(object$prob[[var]][[i]], "n") + k * lambda) - lambda
        n <- sum(n_stage)
        p <- as.numeric(object$prob[[var]][[i]])
        
        if (method == "goodman") {
          q.chi <- qchisq(level, k - 1)
          lci <- (q.chi + 2 *  - sqrt(q.chi * (q.chi + 4 * n_stage * 
                                                 (n - n_stage)/n)))/(2 * (n + q.chi))
          uci <- (q.chi + 2 * n_stage + sqrt(q.chi * (q.chi + 4 * n_stage * 
                                                        (n - n_stage)/n)))/(2 * (n + q.chi))
          res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                     uci))
        }
        else if (method == "wald") {
          q.chi <- qchisq(level, 1)
          lci <- p - sqrt(q.chi * p * (1 - p)/n)
          uci <- p + sqrt(q.chi * p * (1 - p)/n)
          res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                     uci))
        }
        else if (method == "waldcc") {
          q.chi <- qchisq(level, 1)
          lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2 * n)
          uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2 * n)
          res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                     uci))
        }
        else if (method == "wilson") {
          q.chi <- qchisq(level, 1)
          lci <- (q.chi + 2 * n_stage - sqrt(q.chi^2 + 4 * n_stage * q.chi * 
                                               (1 - p)))/(2 * (q.chi + n))
          uci <- (q.chi + 2 * n_stage + sqrt(q.chi^2 + 4 * n_stage * q.chi * 
                                               (1 - p)))/(2 * (q.chi + n))
          res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                     uci))
        }
        
        if (sides == "left") {
          res[, 3] <- 1
        }
        else if (sides == "right") {
          res[, 2] <- 0
        }
        
        ci[[var]][count, ] <- c(res[, 1], t(res[1:k, -1]))
        
      }
    } 
    ci[[var]] <- round(ci[[var]], digits = digits)
    if(which(names(object$tree) == var) >= 2) {
      un_st <- unique(object$stages[[var]])
      un_st <- un_st[un_st %in% stage]
      ci[[var]] <- ci[[var]][match(un_st, rownames(ci[[var]])), ]
    }
  }
  
  if(length(var) > 1) { # stage == "all" for construction.
    
    if(stage[1] != "all") {
      warning("stage is set to 'all'")
    }
    
    
    # re-ordering variables according to variables ordering on the tree.
    var <- names(object$tree)[sort(match(var, names(object$tree)))] 
    ci <- rep(list(c()), length(var))
    attr(ci, "names") <- var
    
    for(v in var) {
      stage <- unique(object$stages[[v]])
      for(i in 1:length(object$prob[[v]])) {
        ci[[v]] <- matrix(NA, nrow = length(object$prob[[v]]), ncol = length(object$tree[[v]]) * 3)
        b <- c(paste("CI_Low_", object$tree[[v]], sep = ""), paste("CI_Upp_", object$tree[[v]], sep = ""))
        d <- c()
        for(q in 1:length(object$tree[[v]])) d <- c(d, b[c(q, length(object$tree[[v]]) + q)])
        colnames(ci[[v]]) <- c(object$tree[[v]], d)
        rownames(ci[[v]]) <- names(object$prob[[v]])
        
        k <- length(object$tree[[v]])
        
        for(j in 1:length(object$prob[[v]])) {
          n_stage <- as.numeric(object$prob[[v]][[j]]) * (attr(object$prob[[v]][[j]], "n") + k * lambda) - lambda
          n <- sum(n_stage)
          p <- as.numeric(object$prob[[v]][[j]])
          
          if (method == "goodman") {
            q.chi <- qchisq(level, k - 1)
            lci <- (q.chi + 2 *  - sqrt(q.chi * (q.chi + 4 * n_stage * 
                                                   (n - n_stage)/n)))/(2 * (n + q.chi))
            uci <- (q.chi + 2 * n_stage + sqrt(q.chi * (q.chi + 4 * n_stage * 
                                                          (n - n_stage)/n)))/(2 * (n + q.chi))
            res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                       uci))
          }
          else if (method == "wald") {
            q.chi <- qchisq(level, 1)
            lci <- p - sqrt(q.chi * p * (1 - p)/n)
            uci <- p + sqrt(q.chi * p * (1 - p)/n)
            res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                       uci))
          }
          else if (method == "waldcc") {
            q.chi <- qchisq(level, 1)
            lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2 * n)
            uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2 * n)
            res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                       uci))
          }
          else if (method == "wilson") {
            q.chi <- qchisq(level, 1)
            lci <- (q.chi + 2 * n_stage - sqrt(q.chi^2 + 4 * n_stage * q.chi * 
                                                 (1 - p)))/(2 * (q.chi + n))
            uci <- (q.chi + 2 * n_stage + sqrt(q.chi^2 + 4 * n_stage * q.chi * 
                                                 (1 - p)))/(2 * (q.chi + n))
            res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                       uci))
          }
          
          if (sides == "left") {
            res[, 3] <- 1
          }
          else if (sides == "right") {
            res[, 2] <- 0
          }
          
          ci[[v]][j, ] <- c(res[, 1], t(res[1:k, -1]))
          
        }
        ci[[v]] <- round(ci[[v]], digits = digits)
        if(which(names(object$tree) == v) >= 2) {
          un_st <- unique(object$stages[[v]])
          un_st <- un_st[un_st %in% stage]
          ci[[v]] <- ci[[v]][match(un_st, rownames(ci[[v]])), ]
        }
      }
    }
  }  
  
  if(var[1] == "all") { # stage == "all" for construction.
    
    if(stage[1] != "all") {
      warning("stage is set to 'all'")
    }
    
    ci <- rep(list(c()), length(object$prob))
    attr(ci, "names") <- names(object$prob)
    
    for(i in 1:length(object$prob)) {
      ci[[i]] <- matrix(NA, nrow = length(object$prob[[i]]), ncol = length(object$tree[[i]]) * 3)
      b <- c(paste("CI_Low_", object$tree[[i]], sep = ""), paste("CI_Upp_", object$tree[[i]], sep = ""))
      d <- c()
      for(q in 1:length(object$tree[[i]])) d <- c(d, b[c(q, length(object$tree[[i]]) + q)])
      colnames(ci[[i]]) <- c(object$tree[[i]], d)
      rownames(ci[[i]]) <- names(object$prob[[i]])
      
      k <- length(object$tree[[i]])
      
      for(j in 1:length(object$prob[[i]])) {
        n_stage <- as.numeric(object$prob[[i]][[j]]) * (attr(object$prob[[i]][[j]], "n") + k * lambda) - lambda
        n <- sum(n_stage)
        p <- as.numeric(object$prob[[i]][[j]])
        
        if (method == "goodman") {
          q.chi <- qchisq(level, k - 1)
          lci <- (q.chi + 2 *  - sqrt(q.chi * (q.chi + 4 * n_stage * 
                                                 (n - n_stage)/n)))/(2 * (n + q.chi))
          uci <- (q.chi + 2 * n_stage + sqrt(q.chi * (q.chi + 4 * n_stage * 
                                                        (n - n_stage)/n)))/(2 * (n + q.chi))
          res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                     uci))
        }
        else if (method == "wald") {
          q.chi <- qchisq(level, 1)
          lci <- p - sqrt(q.chi * p * (1 - p)/n)
          uci <- p + sqrt(q.chi * p * (1 - p)/n)
          res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                     uci))
        }
        else if (method == "waldcc") {
          q.chi <- qchisq(level, 1)
          lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2 * n)
          uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2 * n)
          res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                     uci))
        }
        else if (method == "wilson") {
          q.chi <- qchisq(level, 1)
          lci <- (q.chi + 2 * n_stage - sqrt(q.chi^2 + 4 * n_stage * q.chi * 
                                               (1 - p)))/(2 * (q.chi + n))
          uci <- (q.chi + 2 * n_stage + sqrt(q.chi^2 + 4 * n_stage * q.chi * 
                                               (1 - p)))/(2 * (q.chi + n))
          res <- cbind(est = p, lwr.ci = pmax(0, lci), upr.ci = pmin(1, 
                                                                     uci))
        }
        
        if (sides == "left") {
          res[, 3] <- 1
        }
        else if (sides == "right") {
          res[, 2] <- 0
        }
        
        ci[[i]][j, ] <- c(res[, 1], t(res[1:k, -1]))
        
      }
      ci[[i]] <- round(ci[[i]], digits = digits)
      if(i >= 2) {
        un_st <- unique(object$stages[[i-1]])
        ci[[i]] <- ci[[i]][match(un_st, rownames(ci[[i]])), ]
      }
    }
  }
  class(ci) <- "confint.sevt"
  return(ci)
}

