#' Backward joining of stages
#'
#' Join stages from more complex to simpler models
#' using a distance and a threshold value.
#'
#' @param object an object of class \code{sevt} with fitted probabilities and
#' data, as returned by \code{full} or \code{sevt_fit}.
#' @param distance character, see details.
#' @param thr the threshold for joining stages
#' @param scope names of variables that should be considered
#'              for the optimization.
#' @param ignore vector of stages which will be ignored and left untouched,
#'               by default the name of the unobserved stages stored in
#'               `object$name_unobserved`.
#' @param trace if >0 increasingly amount of info
#' is printed (via \code{message}).
#'
#' @details For each variable in the model stages are joined iteratively.
#' At each iteration the two stages with minimum distance are selected and
#' joined if their distance is less than \code{thr}.
#'
#' Available distances are: manhattan (`manhattan`), euclidean (`euclidean`),
#' Renyi divergence (`reny`), Kullback-Liebler (`kullback`),
#' total-variation (`totvar`), squared Hellinger (`hellinger`),
#' Bhattacharyya (`bhatt`), Chan-Darwiche (`chandarw`).
#' See also \link{probdist}.
#'
#' @return The final staged event tree obtained.
#' @examples
#' DD <- generate_xor_dataset(p = 5, n = 1000)
#' model <- stages_bj(full(DD, lambda = 1), trace = 2)
#' summary(model)
#' @export
stages_bj <-
  function(object,
           distance = "kullback",
           thr = 0.1,
           scope = NULL,
           ignore = object$name_unobserved,
           trace = 0) {
    distances <- c(
      "manhattan", "euclidean", "reny", "kullback",
      "totvar", "hellinger",
      "bhatt", "chandarw"
    )
    check_sevt_fit(object)
    if (!is.character(distance)) {
      cli::cli_abort(c(
        "{.arg distance} should be a character string.",
        "x" = "You've supplied {.type {distance}}."
      ))
    }
    if (!(distance %in% distances)) {
      cli::cli_abort(c(
        "{.arg distance} should be on of {.value {distances}}.",
        "x" = "You've supplied {.value {distance}}."
      ))
    }
    dist_fun <- switch(distance,
      manhattan = probdist.l1,
      euclidean = probdist.l2,
      reny = probdist.ry,
      kullback = probdist.kl,
      totvar = probdist.tv,
      hellinger = probdist.hl,
      bhatt = probdist.bh,
      chandarw = probdist.cd
    )
    if (is.null(scope)) {
      scope <- sevt_varnames(object)[-1]
    }
    check_scope(scope, object)
    for (v in scope) {
      finish <- FALSE
      while (!finish) {
        finish <- TRUE
        stages <- unique(object$stages[[v]])
        stages <- stages[!(stages %in% ignore)]
        if (length(stages) > 1) {
          M <- as.matrix(distance_mat_stages(object$prob[[v]][stages], dist_fun))
          diag(M) <- Inf
          idx <- which.min(M)
          i <- ceiling(idx / dim(M)[1])
          j <- idx - (i - 1) * dim(M)[1]
          s1 <- stages[i]
          s2 <- stages[j]
          if (abs(M[i, j]) < thr) {
            object <-
              join_stages_unsafe(object, v, s1, s2) ## join the 2 stages
            finish <- FALSE # if joined the stages we are not finish
            if (trace > 1) {
              cli::cli_text("{v}: joined stages: {c(s1,s2)}.")
            }
          }
        } ## end if there are more than 1 stage
      } ## end while
      if (trace > 0) {
        cli::cli_text("backward join over {v} done.")
      }
    } ## end for over variables
    if (trace > 0) {
      cli::cli_text("backward join done.")
    }
    object$call <- match.call()
    return(object)
  }
