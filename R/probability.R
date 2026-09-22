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
    p1 <- object$prob[[vs[1]]][[1]][x[1]]
    ## a zero factor dominates an unknown one, see path_lp_cpp
    zero <- isTRUE(p1 == 0)
    l <- log(p1)
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
        pi <- prob[[vi]][[s]][x[i]]
        zero <- zero || isTRUE(pi == 0)
        l <- l + log(pi)
        if (i < n) {
          m <- match(x[[i]], tree[[vi]])
          ## tree_idx names the offending value and variable; match() alone
          ## would return NA and let it travel silently into log()
          if (is.na(m)) stop_unknown_level(x[[i]], vi)
          idx <- (idx - 1) * length(tree[[vi]]) + m
        }
      }
    }
    if (zero) l <- -Inf
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
#' @param na0 logical, if \code{NA} should be converted to 0. This
#'            includes the \code{NA} probabilities of situations with no
#'            observations: with \code{na0 = FALSE}, the default, they
#'            propagate, and the probability of any event they contribute
#'            to is \code{NA}; with \code{na0 = TRUE} they are summed over
#'            as zeros instead.
#'
#'            A situation with no observations carries no probability, and
#'            \code{na0 = TRUE} supplies one it does not have. That is only
#'            sound where the situation cannot be reached anyway, which the
#'            zero-dominance rule below already handles; elsewhere it
#'            silently removes probability mass from the result, so it is
#'            left to the caller to ask for.
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
prob <- function(object, x, conditional_on = NULL, log = FALSE, na0 = FALSE) {
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
  nk <- length(vk)

  ## Level codes. NA in `codes` means either a value to marginalise over or a
  ## value that is not a level of its variable at all; the two are different
  ## and only the first can be handed to the kernel.
  codes <- matrix(NA_integer_, nrow = n, ncol = nk)
  for (j in seq_len(nk)) codes[, j] <- match(xm[, j], lvls[[j]])
  missing_val <- is.na(xm)
  unknown_val <- is.na(codes) & !missing_val

  res <- numeric(n)
  ## A value that is not a level of its variable is an error wherever it sits,
  ## the last variable included. Reporting it here names the variable and the
  ## value, and spares the kernel a case it cannot represent.
  if (any(unknown_val)) {
    j <- which(colSums(unknown_val) > 0)[1]
    i1 <- which(unknown_val[, j])[1]
    stop_unknown_level(xm[i1, j], vk[j], arg = "x")
  }

  rest <- seq_len(n)
  if (length(rest) > 0) {
    flat <- sevt_flat(object, vk)
    ## Rows are grouped by which variables they are missing, so that one group
    ## shares the same set of completions. Every completion of every row in the
    ## group goes to the kernel in a single call, and the sum over a row's
    ## completions is taken afterwards. The completions are laid out in
    ## expand.grid's order -- first variable varying fastest -- because
    ## logSumExp over the same values in a different order need not give the
    ## same last bits.
    pat <- apply(missing_val[rest, , drop = FALSE], 1, function(z)
      paste0(as.integer(z), collapse = ""))
    for (g in split(rest, pat)) {
      mpos <- which(missing_val[g[1], ])
      if (length(mpos) == 0) {
        lp <- path_lp_cpp(codes[g, , drop = FALSE], flat$ls,
                          flat$stagemap, flat$probs)
        ## logSumExp of one term is that term, and of a lone NA is -Inf,
        ## which is the `na0 = TRUE` reading of it; with `na0 = FALSE` the
        ## NA is kept so that it propagates to the caller.
        res[g] <- if (na0) ifelse(is.na(lp), -Inf, lp) else lp
      } else {
        comb <- as.matrix(expand.grid(lapply(flat$ls[mpos], seq_len)))
        ncomb <- nrow(comb)
        blk <- codes[g, , drop = FALSE][rep(seq_along(g), each = ncomb), ,
                                        drop = FALSE]
        blk[, mpos] <- comb[rep(seq_len(ncomb), times = length(g)), ,
                            drop = FALSE]
        lp <- path_lp_cpp(blk, flat$ls, flat$stagemap, flat$probs)
        for (t in seq_along(g)) {
          res[g[t]] <- matrixStats::logSumExp(
            lp[((t - 1) * ncomb + 1):(t * ncomb)], na.rm = na0)
        }
      }
    }
  }
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
