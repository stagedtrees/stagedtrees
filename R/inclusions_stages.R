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
  check_same_tree(object1, object2)


  vars <- sevt_varnames(object1)
  if (length(vars) <= 1){
    return(NULL)
  }
  out <- rep(list(c()), length(vars) - 1)
  names(out) <- vars[-1]
  out2 <- out
  for (k in seq_along(vars)[-1]) {
    vark <- vars[k]
    a <- object1$stages[[vark]]
    b <- object2$stages[[vark]]
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

    out[[vark]] <- ifelse((out_a + out_b) == 2, 0, 1)
    out2[[vark]] <- data.frame("A" = rep(NA, length(out[[vark]])),
                            "B" =  rep(NA, length(out[[vark]])),
                            "C" = rep(NA, length(out[[vark]])))

    ord1 <- ord2 <- c()

    for (i in seq_along(out[[vark]])) {
      out2[[vark]][i, 1] <- object1$stages[[vark]][i]
      out2[[vark]][i, 3] <- object2$stages[[vark]][i]
      if (out[[vark]][i] == 0) {
        out2[[vark]][i, 2] <- "="
        ord1 <- c(ord1, object1$stages[[vark]][i])
        ord2 <- c(ord2, object2$stages[[vark]][i])
      }
      else if (out[[vark]][i] == 1) {
        if (out_a[i] == 1 & out_b[i] == 0) {
          out2[[vark]][i, 2] <- "<"
          ord1 <- c(ord1, object1$stages[[vark]][i])
          ord2 <- c(ord2, object2$stages[[vark]][i])
        }
        if (out_a[i] == 0 & out_b[i] == 1) {
          out2[[vark]][i, 2] <- ">"
          ord1 <- c(ord1, object1$stages[[vark]][i])
          ord2 <- c(ord2, object2$stages[[vark]][i])
        }
        if (out_a[i] == 0 & out_b[i] == 0) {
          out2[[vark]][i, 2] <- "!="
          ord1 <- c(ord1, object1$stages[[vark]][i])
          ord2 <- c(ord2, object2$stages[[vark]][i])
        }
      }
    }

    # nice print
    out2[[vark]] <- unique(noquote(out2[[vark]]))
    colnames(out2[[vark]]) <- c(deparse(substitute(object1)), "  ", deparse(substitute(object2)))
  }
  return(out2)
}
