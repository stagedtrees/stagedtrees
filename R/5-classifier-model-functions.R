#' Predict method for staged event tree
#'
#' Predict class values from a staged event tree model.
#' @param object A staged event tree
#' @param newdata The newdata to perform predictions 
#' @param class A string indicating the name of the variable to use as
#' the class variable, otherwise the first name in \code{names(object$tree)}
#' will be used.
#' @param prob logical, if \code{TRUE} the probabilities of class are
#'                      returned
#' @param log logical, if \code{TRUE} log-probabilities are returned
#' @param ... additional parameters, see details
#' @details Predict the most probable a posterior value for the class variable
#'  given all the other variables in the model. Ties are broken at random and 
#'  if, for a given vector of predictor variables, all conditional probabilities
#'  are 0, NA is returned.
#' @return A vector of predictions or the corresponding probabilities
#' @examples
#' DD <- generate_xor_dataset(n = 4, 600)
#' order <- c("C", "X1", "X2", "X3", "X4")
#' train <- DD[1:500, order]
#' test <- DD[501:600, order]
#' model <- full(train)
#' model <- bhc.sevt(model)
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
    stopifnot(is(object, "sevt"))
    if (!is_fitted.sevt(object)) {
      stop("Provide a fitted object")
    }
    if (is.null(newdata)) {
      newdata <- object$data
      if (is.null(newdata)) {
        stop("Nothing to predict, newdata argument is missing and data is not
             attached to object")
      }
      newdata <- as.data.frame(newdata)
    } ## we are now sure we have newdata as a data.frame
    vars <- names(object$tree)
    #we search now for wich variable we need to make predicitons
    if (is.null(class)) {
      if (!is.null(object$class)) {
        class <- object$class
      } else {
        ##take the virst variable
        class <- vars[1]
      }
    }
    if (!(class %in% vars)) {
      stop("class is not one of the variable of the model")
    }
    if (is.null(newdata[[class]])){## we create a dummy variable
      newdata[[class]] <- NA
    }
    class_idx <-
      (1:length(vars))[vars %in% class] #find class index in the order
    preds <- vars[!(vars %in% class)] #define the predictors
    newdata <- newdata[, vars]
    pred <- t(apply(newdata, MARGIN = 1, function(x) {
      res <- array(dim = c(length(object$tree[[class]])),
                   dimnames = list(object$tree[[class]]))
      for (cv in object$tree[[class]]) {
        x[class_idx] <- cv
        res[cv] <-
          path_probability.sevt(object, x, log = TRUE)
      }
      res[is.nan(res)] <- -Inf
      return(res - log(sum(exp(res)))) ##normalize, that is conditional prob
    }))
    if (prob) {
      if (log) {
        return(pred)
      } else{
        return(exp(pred))
      }
    } else{
      class_values <- colnames(pred)
      return(apply(pred, MARGIN = 1, which_class, levels = class_values))
    }
  }
