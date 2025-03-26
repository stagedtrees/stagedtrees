randomize <- function(object, var, p = NULL, ignore = object$name_unobserved){
  check_sevt_fit(object)
  check_scope(var, object)
  kk <- length(object$tree[[var]])
  if (is.null(p)){
    p <- rep.int(1/kk, kk)
  }
  names(p) <- object$tree[[var]]
  tmp <- object$stages[[var]]
  object$stages[[var]][!(tmp %in% ignore)] <- "randomized"
  object$prob[[var]] <- c(list(randomized = p), object$prob[[var]][ignore])
  object$prob[[var]] <- object$prob[[var]][!is.na(names(object$prob[[var]]))]
  return(object)
}
