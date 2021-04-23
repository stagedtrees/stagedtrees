install.packages("DescTools")
library(DescTools)

# function to compute confidence intervals, using one of the 7 methods implemented. "binomial"
# corresponds to BinomCI(), while other 6 methods are from MultinomCI() (both from DescTools package).
# the rownames of the output, for each variable, are the stage names/numbers.
confidence_intervals <- function(object, lambda = object$lambda, conf_method = c("binomial", 
                                                                                 "sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson"), 
                                 conf_lev = 0.95, conf_digits = 6)
{
  stagedtrees:::check_sevt(object)
  ci <- rep(list(c()), length(object$prob))
  attr(ci, "names") <- names(object$prob)
  for(i in 1:length(object$prob)) {
    ci[[i]] <- matrix(NA, nrow = length(object$prob[[i]]), ncol = length(object$tree[[i]]) * 3)
    b <- c(paste("CI_Low_", object$tree[[i]], sep = ""), paste("CI_Upp_", object$tree[[i]], sep = ""))
    d <- c()
    for(k in 1:length(object$tree[[i]])) d <- c(d, b[c(k, length(object$tree[[i]]) + k)])
    colnames(ci[[i]]) <- c(object$tree[[i]], d)
    rownames(ci[[i]]) <- names(object$prob[[i]])
    for(j in 1:length(object$prob[[i]])) {
      n_stage <- attr(object$prob[[i]][[j]], "n") * as.numeric(object$prob[[i]][[j]]) + lambda
      
      if(conf_method == "binomial") {
        ci_stage <- BinomCI(n_stage, n = sum(n_stage), conf.level = conf_lev)
      }
      else {
        ci_stage <- MultinomCI(n_stage, conf.level = conf_lev, method = conf_method)
      }
      
      ci[[i]][j, ] <- c(ci_stage[, 1], t(ci_stage[1:length(object$tree[[i]]), -1]))
    }
    ci[[i]] <- round(ci[[i]], digits = conf_digits)
    if(i >= 2) {
      un_st <- unique(object$stages[[i-1]])
      ci[[i]] <- ci[[i]][match(un_st, rownames(ci[[i]])), ]
    }
  }
  return(ci)
}




# Examples
bin <- confidence_intervals(stages_bj(full(PhDArticles), distance = "kullback", thr = 0.1), 
                            conf_method = "binomial", conf_digits = 3)
wal <- confidence_intervals(stages_bj(full(PhDArticles), distance = "kullback", thr = 0.1), 
                            conf_method = "wald", conf_digits = 5)




################################################################################################################



# It is the summary.sevt print, with the addition of confidence intervals for each stage distribution.
summary_confint.sevt <- function(object, ...) {
  stagedtrees:::check_sevt(object)
  vns <- stagedtrees:::sevt_varnames(object)
  nv <- stagedtrees:::sevt_nvar(object)
  ci <- confidence_intervals(object, conf_method = "wald")
  out <- list()
  out[[vns[1]]] <- data.frame(
    stage = "1",
    npaths = 0, stringsAsFactors = FALSE
  )
  if (stagedtrees:::is_fitted_sevt(object)) {
    out[[vns[1]]][["sample.size"]] <- attr(object$prob[[vns[1]]][[1]], "n")
    out[[vns[1]]] <- cbind(
      out[[vns[1]]],
      t(as.data.frame(object$prob[[vns[1]]]))
    )
  }
  out[[vns[[1]]]] <- cbind(out[[vns[[1]]]], t(ci[[vns[[1]]]][, -c(1:length(object$tree[[vns[[1]]]]))]))
  for (i in 2:nv) {
    v <- vns[i]
    D <- data.frame(
      stage = unique(object$stages[[v]]),
      stringsAsFactors = FALSE,
      row.names = make.names(unique(object$stages[[v]]))
    )
    D$npaths <- vapply(D$stage, function(s) {
      sum(object$stages[[v]] == s)
    }, FUN.VALUE = 1)
    if (stagedtrees:::has_prob(object)) {
      D[["sample.size"]] <- vapply(D$stage, function(s) {
        ifelse(is.null(attr(object$prob[[v]][[s]], "n")), 
               NA, attr(object$prob[[v]][[s]], "n")) 
      }, FUN.VALUE = 1)
      if (nrow(D) <= 1){
        D <- cbind(D, t(as.data.frame(object$prob[[v]])))
        out[[vns[i]]] <- cbind(D, t(ci[[vns[[i]]]][-c(1:length(object$tree[[vns[[i]]]]))]))
      }else{
        D <- cbind(D, t(as.data.frame(object$prob[[v]]))[rownames(D),])
        out[[vns[i]]] <- D
        un_st <- unique(object$stages[[vns[[i]]]])
        out[[vns[i]]] <- cbind(out[[vns[i]]], ci[[vns[[i]]]][match(un_st, rownames(ci[[vns[[i]]]])), -c(1:length(object$tree[[vns[[i]]]]))])
      }
    }
  }
  out <- list(stages.info = out)
  out$call <- object$call
  out$ll <- object$ll
  out$lambda <- object$lambda
  class(out) <- "summary.sevt"
  return(out)
}


# Examples
object <- stages_fbhc(full(Pokemon))
summary_confint.sevt(object)


object1 <- stages_bhc(full(Pokemon))
summary_confint.sevt(object1)















