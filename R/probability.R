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
    vs <- sevt_varnames(object)
    if (!is.null(names(x))) {
      # if it's a named vector just order it
      x <- x[vs]
    }
    # start computing the log probability with first variable
    l <- log(object$prob[[vs[1]]][[1]][x[1]])
    n <- length(x)
    if (n > 1) {
      tree <- object$tree
      prob <- object$prob
      stages <- object$stages
      ## The situation index is a mixed-radix number with the last variable
      ## varying fastest, so extending a path by one variable is one
      ## multiply-add: idx_j = (idx_{j-1} - 1) * ls_j + m_j. Carrying it along
      ## the walk replaces a find_stage() per depth, each of which had
      ## tree_idx() rebuild the index from the start of the path and recompute
      ## lengths(tree). That was quadratic in the path length: seven variables
      ## cost 21 match() calls and six lengths() calls per path, where six and
      ## one suffice.
      v <- vs[[1]]
      idx <- match(x[[1]], tree[[v]])
      if (is.na(idx)) stop_unknown_level(x[[1]], v)
      for (i in 2:n) {
        vi <- vs[[i]]
        st <- stages[[vi]]
        s <- st[(idx - 1) %% length(st) + 1]
        l <- l + log(prob[[vi]][[s]][x[i]])
        if (i < n) {
          m <- match(x[[i]], tree[[vi]])
          ## tree_idx names the offending value and variable; match() alone
          ## would return NA and let it travel silently into log()
          if (is.na(m)) stop_unknown_level(x[[i]], vi)
          idx <- (idx - 1) * length(tree[[vi]]) + m
        }
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
#'   conditional_on = expand.grid(model$tree[2:1])
#' )
#' ## the above should be the same as
#' summary(model)$stages.info$Age
#' @export
prob <- function(object, x, conditional_on = NULL, log = FALSE, na0 = TRUE) {
  check_sevt_prob(object)
  if (is.null(dim(x))) {
    x <- as.data.frame(t(x))
  }
  p1 <- 0
  if (!is.null(conditional_on)) {
    if (is.vector(conditional_on) && !is.null(names(conditional_on))) {
      if (length(conditional_on) > 0){
        ## check if same names
        if (any(names(x) %in% names(conditional_on))) {
          cli::cli_abort(c(
            "Variable names in {.arg x} and {.arg conditional_on}
          must be disjoint.",
            "x" = "You've supplied {.arg x} and {.arg conditional_on} and both
                 have values for
                 {.field {intersect(names(x), names(conditional_on))}}."
          ))
        }
        x <- cbind(x, as.data.frame(t(conditional_on)), row.names = NULL)
        p1 <- prob(object, x = conditional_on, log = TRUE, na0 = na0)
      }
    } else if (is.data.frame(conditional_on)) {
      ## check if not empty
      if (nrow(conditional_on) > 0 & ncol(conditional_on) > 0){
        ## check if same names
        if (any(names(x) %in% names(conditional_on))) {
          cli::cli_abort(c(
            "Variable names in {.arg x} and {.arg conditional_on}
          must be disjoint.",
            "x" = "You've supplied {.arg x} and {.arg conditional_on} and both
                 have values for
                 {.field {intersect(names(x), names(conditional_on))}}."
          ))
        }
        x <- cbind(x, conditional_on, row.names = NULL)
        p1 <- prob(object, x = conditional_on, log = TRUE, na0 = na0)
      }
    } else {
      cli::cli_abort(c(
        "{.arg conditional_on} must be {.value NULL},
           a named vector or a {.cls data.frame}.",
        "x" = "You've supplied {.arg conditional_on}
        which is {.type {conditional_on}}."
      ))
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
  vk <- var[1:k]
  ## Pull the query into a character matrix once. The loop below otherwise
  ## reads x cell by cell, and `[.data.frame` dispatches, builds a one-row
  ## frame and throws it away for every variable of every row: a third of this
  ## function's time went there. A variable of the model that is absent from x
  ## is left as NA, which is how the cell-by-cell version treated it -- x[i, vv]
  ## returns NULL for a missing column, and both mean "unobserved".
  xm <- matrix(NA_character_, nrow = n, ncol = length(vk),
               dimnames = list(NULL, vk))
  for (vv in intersect(vk, colnames(x))) xm[, vv] <- as.character(x[, vv])
  lvls <- object$tree[vk]
  res <- vapply(
    seq_len(n),
    FUN.VALUE = 1.0,
    FUN = function(i) {
      row <- xm[i, ]
      miss <- is.na(row)
      if (!any(miss)) {
        ## Nothing to sum over, so the grid of completions is a single path.
        ## expand.grid() built a data.frame per row to hold it, and apply()
        ## walked it; both are skipped here. logSumExp is kept even for the
        ## one term: it maps a NA to -Inf under na.rm, which is what the
        ## callers below distinguish from NA when na0 is FALSE.
        return(matrixStats::logSumExp(
          path_probability(object, as.character(row), log = TRUE),
          na.rm = TRUE
        ))
      }
      ll <- as.list(row)
      ll[miss] <- lvls[miss]
      matrixStats::logSumExp(apply(
        expand.grid(ll),
        MARGIN = 1,
        FUN = function(xx) {
          path_probability(object, as.character(xx), log = TRUE)
        }
      ), na.rm = TRUE)
    }
  )
  res <- res - p1
  # NaN arises from log(0) - log(0), i.e. conditioning on a zero-probability
  # event. This is undefined; return NA per entry with a warning. na0 converts
  # other NAs (unknown levels, numerical issues) to 0, but not these.
  zero_cond <- is.nan(res)
  if (any(zero_cond)) {
    cli::cli_warn(
      "Conditioning on a zero-probability event; \\
       returning {.val NA} for the affected \\
       {sum(zero_cond)} entr{?y/ies}."
    )
    res[zero_cond] <- NA_real_
  }
  if (na0) res[is.na(res) & !zero_cond] <- 0
  # always NA for undefined entries, even when na0 = TRUE
  res[zero_cond] <- NA_real_
  if (log) {
    return(res)
  } else {
    return(exp(res))
  }
}
