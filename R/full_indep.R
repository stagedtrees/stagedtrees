#' Full and independent staged event tree
#'
#' Build fitted staged event tree from data.
#' @name full_indep
#' @details  Functions to create full or independent staged tree models from
#'           data.
#'           The full (or saturated) staged tree is the model where every
#'           situation is in a different stage, and thus the model has the
#'           maximum number of parameters.
#'           Conversely, the independent staged tree (`indep`) assigns
#'           all the situations related to the same variable to the same
#'           stage, thus it is equivalent to the independence factorization.
NULL

#' @rdname full_indep
#' @param data data to create the model, data.frame or table.
#' @param order character vector, order of variables.
#' @param join_unobserved logical, if situations with zero observations should
#'                           be joined (default TRUE).
#' @param name_unobserved name to pass to \code{\link{join_unobserved}}.
#' @param lambda smoothing coefficient (default 0).
#' @examples
#'
#' ## full model
#' DD <- generate_xor_dataset(4, 100)
#' model_full <- full(DD, lambda = 1)
#' @export
full <- function(data, order = NULL,
                 join_unobserved = TRUE,
                 lambda = 0,
                 name_unobserved = "UNOBSERVED") {
  UseMethod("full", data)
}

#' @rdname full_indep
#' @export
full.table <- function(data, order = names(dimnames(data)),
                       join_unobserved = TRUE,
                       lambda = 0,
                       name_unobserved = "UNOBSERVED") {
  object <- sevt(data, full = TRUE, order = order)
  object$ctables <- make_ctables(object, data)
  if (join_unobserved) {
    join_unobserved(object,
      fit = TRUE, name = name_unobserved, lambda = lambda
    )
  } else {
    sevt_fit(object, lambda = lambda)
  }
}

#' @rdname full_indep
#' @export
full.data.frame <- function(data, order = colnames(data),
                            join_unobserved = TRUE,
                            lambda = 0,
                            name_unobserved = "UNOBSERVED") {
  object <- sevt(data, full = TRUE, order = order)
  object$ctables <- make_ctables(object, data)
  if (join_unobserved) {
    join_unobserved(object,
      fit = TRUE, name = name_unobserved, lambda = lambda
    )
  } else {
    sevt_fit(object, lambda = lambda)
  }
}

#' @rdname full_indep
#' @export
indep <- function(data, order = NULL,
                  join_unobserved = TRUE,
                  lambda = 0,
                  name_unobserved = "UNOBSERVED") {
  UseMethod("indep", data)
}

#' @rdname full_indep
#' @export
indep.table <- function(data, order = names(dimnames(data)),
                        join_unobserved = TRUE, lambda = 0,
                        name_unobserved = "UNOBSERVED") {
  object <- sevt(data, full = FALSE, order = order)
  object$ctables <- make_ctables(object, data)
  object$lambda <- lambda
  if (join_unobserved) {
    join_unobserved(object,
      fit = TRUE, name = name_unobserved, lambda = lambda
    )
  } else {
    sevt_fit(object, lambda = lambda)
  }
}

#' @rdname full_indep
#' @examples
#'
#' ## independence model (data.frame)
#' DD <- generate_xor_dataset(4, 100)
#' model <- indep(DD, lambda = 1)
#' model
#' @export
indep.data.frame <- function(data, order = colnames(data),
                             join_unobserved = TRUE, lambda = 0,
                             name_unobserved = "UNOBSERVED") {
  # create the staged tree object
  model <- sevt(data, full = FALSE, order = order)
  # store lambda value
  model$lambda <- lambda
  # store contingency tables
  model$ctables <- make_ctables(model, data)
  if (join_unobserved) {
    return(join_unobserved(model, fit = TRUE, trace = 0, name = name_unobserved))
  }
  # create empty probability list
  model$prob <- list()
  # extract names of variables
  var <- names(model$tree)
  # initialize loglik to 0
  model$ll <- 0
  # iterate for each variable
  for (v in var) {
    # extract the table of the given variable
    ctab <- table(data[[v]])
    # obtain sums of cases
    n <- sum(ctab)
    # compute probability table prob = (ctab + lambda)/sum(ctab + lambda)
    model$prob[[v]] <- list("1" = ctab + lambda)
    model$prob[[v]][["1"]] <-
      model$prob[[v]][["1"]] / sum(model$prob[[v]][["1"]])
    # store sample size
    attr(model$prob[[v]][["1"]], "n") <- n
    # compute where prob > 0
    ix <- ctab > 0
    # set appropriate class (get rid of table formatting)
    class(model$prob[[v]][["1"]]) <- "numeric"
    # update loglik
    model$ll <-
      model$ll + sum(ctab[ix] * log(model$prob[[v]][["1"]][ix]))
  }
  # finish setting up loglik
  # store degrees of freedom
  attr(model$ll, "df") <-
    sum(vapply(model$tree, length, FUN.VALUE = 1) - 1)
  # store number of obs
  attr(model$ll, "nobs") <- nrow(data)
  # set logLik class
  class(model$ll) <- "logLik"
  return(model)
}
