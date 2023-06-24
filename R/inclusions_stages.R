#' Inclusions of stages
#'  
#' @description Display the relationship between two staged tree models over the 
#' same variables.
#' @param object1 an object of class \code{sevt}.
#' @param object2 an object of class \code{sevt}.
#' @return a list with inclusion relations between stage
#' structures for each variable in the models.
#' @details Computes the 
#'  relations between 
#'  the stages structures of the two models.
#'  
#'  The relations between stages of the same variable 
#'  are stored in a data frame with three columns 
#'  where each row represent  
#'  a relation between a stage of the first model (\code{s1}) and 
#'  a stage of the second model (\code{s2}). 
#'  The relation can be one of the following: inclusion (\code{s1 < s2} 
#'  or \code{s1 > s2}; equal (\code{s1 = s2}); not-equal (\code{s1 != s2}).  
#' @examples
#' mod1 <- stages_bhc(full(PhDArticles[, 1:5], lambda = 1))
#' mod2 <- stages_fbhc(full(PhDArticles[, 1:5], lambda = 1))
#' inclusions_stages(mod1, mod2)
#' @export
inclusions_stages <- function(object1, object2) {
  check_sevt(object1)
  check_sevt(object2)
  stopifnot(sevt_nvar(object1) == sevt_nvar(object2))
  stopifnot(all(sevt_varnames(object1) == sevt_varnames(object2)))
  out <- rep(list(c()), length(object1$stages))
  attr(out, "names") <- attr(object1$stages, "names")
  out2 <- out
  
  for (k in seq_along(object1$stages)) {
    a <- object1$stages[[k]]
    b <- object2$stages[[k]]
    unique_a <- unique(a)
    unique_b <- unique(b)
    out_a <- out_b <- rep(0, length(a))
    for (i in seq_along(unique_a)) {
      ifelse((length(unique(b[which(a == unique_a[i])])) == 1),
             out_a[which(a == unique_a[i])] <- 1,
             out_a[which(a == unique_a[i])] <- 0
      )
    }
    for (i in seq_along(unique_b)) {
      ifelse((length(unique(a[which(b == unique_b[i])])) == 1),
             out_b[which(b == unique_b[i])] <- 1,
             out_b[which(b == unique_b[i])] <- 0
      )
    }
    
    out[[k]] <- ifelse((out_a + out_b) == 2, 0, 1)
    out2[[k]] <- data.frame("A" = rep(NA, length(out[[k]])),
                            "B" =  rep(NA, length(out[[k]])),
                            "C" = rep(NA, length(out[[k]])))
    
    ord1 <- ord2 <- c()
    
    for (i in seq_along(out[[k]])) {
      out2[[k]][i, 1] <- object1$stages[[k]][i]
      out2[[k]][i, 3] <- object2$stages[[k]][i]
      if (out[[k]][i] == 0) {
        out2[[k]][i, 2] <- "="
        ord1 <- c(ord1, object1$stages[[k]][i])
        ord2 <- c(ord2, object2$stages[[k]][i])
      }
      else if (out[[k]][i] == 1) {
        if (out_a[i] == 1 & out_b[i] == 0) {
          out2[[k]][i, 2] <- "<"
          ord1 <- c(ord1, object1$stages[[k]][i])
          ord2 <- c(ord2, object2$stages[[k]][i])
        }
        if (out_a[i] == 0 & out_b[i] == 1) {
          out2[[k]][i, 2] <- ">"
          ord1 <- c(ord1, object1$stages[[k]][i])
          ord2 <- c(ord2, object2$stages[[k]][i])
        }
        if (out_a[i] == 0 & out_b[i] == 0) {
          out2[[k]][i, 2] <- "!="
          ord1 <- c(ord1, object1$stages[[k]][i])
          ord2 <- c(ord2, object2$stages[[k]][i])
        }
      }
    }
    
    # nice print
    out2[[k]] <- unique(noquote(out2[[k]]))
    colnames(out2[[k]]) <- c(deparse(substitute(object1)), "  ", deparse(substitute(object2)))
  }
  return(out2)
}