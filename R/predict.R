#' Class-conditional log-probabilities for fully observed rows
#'
#' Internal helper for \code{\link{predict.sevt}}.
#' @param object an object of class \code{sevt} with fitted probabilities.
#' @param newdata rows with no missing value among the predictors.
#' @param class character, name of the variable being predicted.
#' @param vars the model's variable names, in tree order.
#' @return a matrix with one row per observation and one column per level of
#'         \code{class}, holding normalised log-probabilities.
#' @details Marshals the model into the flat arrays the compiled kernel walks:
#'          level codes in tree order, the level count per variable, the
#'          situation-to-stage map per variable, and a stage-by-level
#'          probability matrix per variable. The kernel carries the situation
#'          index down each path exactly as \code{\link{path_probability}}
#'          does, for every row and every candidate class level in one call.
#' @keywords internal
predict_lp_fast <- function(object, newdata, class, vars) {
  p <- length(vars)
  cpos <- match(class, vars)
  stagemap <- vector("list", p)
  probs <- vector("list", p)
  for (j in seq_len(p)) {
    v <- vars[j]
    pv <- object$prob[[v]]
    pm <- do.call(rbind, lapply(pv, as.numeric))
    dimnames(pm) <- NULL
    probs[[j]] <- pm
    stagemap[[j]] <- if (j == 1) {
      1L
    } else {
      match(as.character(object$stages[[v]]), names(pv))
    }
  }
  codes <- matrix(1L, nrow = nrow(newdata), ncol = p)
  for (j in seq_len(p)) {
    if (j == cpos) next            # overwritten by each candidate class level
    v <- vars[j]
    codes[, j] <- match(as.character(newdata[[v]]), object$tree[[v]])
  }
  predict_lp_cpp(codes, vapply(object$tree[vars], length, 1L),
                 stagemap, probs, cpos)
}

#' Predict method for staged event tree
#'
#' Predict class values from a staged event tree model.
#' @param object an object of class \code{sevt} with fitted probabilities.
#' @param newdata the newdata to perform predictions
#' @param class character, the name of the variable to use as
#' the class variable, if NULL  the first element \code{names(object$tree)}
#' will be used.
#' @param prob logical, if \code{TRUE} the probabilities of class are
#'                      returned
#' @param log logical, if \code{TRUE} log-probabilities are returned
#' @param ... additional parameters, see details
#' @details Predict the most probable a posterior value for the class variable
#'  given all the other variables in the model. Ties are broken at random and
#'  if, for a given vector of predictor variables, all conditional probabilities
#'  are 0, NA is returned.
#' @return A vector of predictions or the corresponding matrix of probabilities.
#' @examples
#' DD <- generate_xor_dataset(p = 4, n = 600)
#' order <- c("C", "X1", "X2", "X3", "X4")
#' train <- DD[1:500, order]
#' test <- DD[501:600, order]
#' model <- full(train)
#' model <- stages_bhc(model)
#' pr <- predict(model, newdata = test, class = "C")
#' table(pr, test$C)
#' # class values:
#' predict(model, newdata = test, class = "C")
#' # probabilities:
#' predict(model, newdata = test, class = "C", prob = TRUE)
#' # log-probabilities:
#' predict(model, newdata = test, class = "C", prob = TRUE, log = TRUE)
#' @details if \code{prob = TRUE}, a matrix with number of rows equals to the number of
#' rows in the \code{newdata} and number of columns as the number of levels of the
#' \code{class} variable is returned. if \code{log = TRUE}, log-probabilities are returned.
#'
#' if \code{prob = FALSE}, a vector of length as the number of rows in the \code{newdata}
#' with the level with higher estimated probability for each new observations is returned.
#' @export
#' @importFrom stats predict
predict.sevt <-
  function(object,
           newdata = NULL,
           class = NULL,
           prob = FALSE,
           log = FALSE,
           ...) {
    check_sevt_prob(object)
    vars <- sevt_varnames(object)
    if (is.null(newdata)) {
      if (!has_ctables(object)) {
        cli::cli_abort(c(
          "Observations are needed to obtain predictions.",
          "x" = "You've not supplied {.arg newdata} and
          the provided {.arg object} does not have attached {.field ctables}."
        ))
      }
      newdata <- object$ctables[[vars[length(vars)]]]
      newdata <- as.data.frame(newdata)
    } ## we are now sure we have newdata as a data.frame
    # we search now for wich variable we need to make predicitons
    if (is.null(class)) {
      if (!is.null(object$class)) {
        class <- object$class
      } else {
        ## take the virst variable
        class <- vars[1]
      }
    }
    check_var_in(class, object)
    ## we create a dummy variable
    newdata[[class]] <- NA
    class_idx <-
      (1:length(vars))[vars %in% class] # find class index in the order
    preds <- vars[!(vars %in% class)] # define the predictors
    preds <- intersect(preds, colnames(newdata))
    all_preds <- FALSE
    if (setequal(setdiff(vars, preds), class)){ ## check if we have all predictors
      newdata <- newdata[, vars]
      all_preds <- TRUE
    }
    ## A row whose predictors are all present needs only a walk down the tree
    ## per class level, which is what the compiled kernel does for every such
    ## row in one call. Rows with a missing predictor still need the sum over
    ## its completions that prob() performs, so they keep the original path.
    cls_lvl <- object$tree[[class]]
    if (all_preds) {
      others <- setdiff(vars, class)
      fast <- rowSums(is.na(newdata[, others, drop = FALSE])) == 0
    } else {
      fast <- rep(FALSE, nrow(newdata))
    }
    pred <- matrix(NA_real_, nrow = nrow(newdata), ncol = length(cls_lvl),
                   dimnames = list(rownames(newdata), cls_lvl))
    if (any(fast)) {
      pred[fast, ] <- predict_lp_fast(object, newdata[fast, , drop = FALSE],
                                      class, vars)
    }
    if (any(!fast)) {
      pred[!fast, ] <- t(apply(newdata[!fast, , drop = FALSE], MARGIN = 1, function(x) {
        res <- array(
          dim = c(length(object$tree[[class]])),
          dimnames = list(object$tree[[class]])
        )
        for (cv in object$tree[[class]]) {
          x[class] <- cv
          if (!any(is.na(x)) && all_preds){
            res[cv] <-
              path_probability(object, x, log = TRUE)
          } else {
            res[cv] <- prob(object, x[!is.na(x), drop = FALSE], log = TRUE)
          }
        }
        res[is.nan(res)] <- -Inf
        return(res - log(sum(exp(res)))) ## normalize, that is conditional prob
      }))
    }
    if (prob) {
      if (log) {
        return(pred)
      } else {
        return(exp(pred))
      }
    } else {
      class_values <- colnames(pred)
      return(apply(pred, MARGIN = 1, which_class, levels = class_values))
    }
  }
