#' Predcict method for staged event tree
#'
#' @param object A staged event tree
#' @param newdata The newdata to perform predictions 
#' @param class A string indicating the name of the variable to use as
#' the class variable, otherwise the first name in \code{names(object$tree)}
#' will be used.
#' @param prob logical, if \code{TRUE} the probabilities of class are
#'                      returned
#' @param log logical, if \code{TRUE} log-probabilities are returned
#' @param ... additional parameters, see details
#' @return A vector of predictions or the corresponding probabilities
#' @examples
#' DD <- generate_xor_dataset(n = 5, 1000)
#' order <- c("C", "X1", "X2", "X3", "X4", "X5")
#' train <- DD[1:500, order]
#' test <- DD[501:1000, order]
#' model <- full(train)
#' model <- bhc.sevt(model)
#' pr <- predict(model, class = "C", newdata = test)
#' table(pr, test$C)
#' predict(model, class = "C", newdata = test, prob = TRUE)
#' @return A vector of predicitons
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
      return(apply(pred, MARGIN = 1, function(x) {
        factor(class_values[which.max(x)], levels = class_values)
      }))
    }
  }
