#' New label
#' 
#' give a safe-to-add label that is not in \code{labels}
#' @param labels vector of labels
#' @return a string label that is different from each \code{labels}
#' @keywords internal
new_label <- function(labels){
  k <- 1
  labels <- as.character(labels)
  while (TRUE){
    if (!(as.character(k) %in% labels)){
      return(as.character(k))
    }
    k <- k + 1
  }
}

#' Unique id from named list 
#' 
#' @param x a named list
#' @return A named list with unique ids
#' @keywords internal
uni_idx <- function(x){
  nn <- names(x)
  x <- lapply(1:length(x), function(i){
    paste0(nn[i], ":", x[[i]])
  })
  names(x) <- nn
  return(x)
}


#' return path index
#' 
#' @param path a path from root in the tree
#' @param tree a symmetric tree given as a list of levels 
#' @param complete logical, if \code{TRUE} the complete indexing is returned
#' 
#' @details Compute the integer index of the node associated with the 
#' given path in a symmetric tree defined by \code{tree}.
#' 
#' @return an integer, the index of the node corresponding to \code{path}
#' @keywords internal
tree_idx <- function(path, tree, complete = FALSE){
 k <- length(path)
 ls <- sapply(tree, length)
 is <- vapply(1:k, FUN = function(i){
   (1:ls[i])[ tree[[i]] %in% path[i]  ]
 }, FUN.VALUE = 1) 
 if (k <= 1){
   return(is[1])
 }
 if (complete){
   sum(vapply(1:(k-1), FUN = function(i){
     prod(ls[(i+1):(k)])
   }, FUN.VALUE = 1) * is[1:(k - 1)])  + is[k]
 }else{
   sum(vapply(1:(k-1), FUN = function(i){
     prod(ls[(i+1):(k)])
   }, FUN.VALUE = 1) * (is[1:(k - 1)] - 1))  + is[k]  
 }
}



#' find the stage in the path
#' 
#' no checking is done 
#' @param object a staged event tree object
#' @param path vector of the path
#' @return the stage name corresponding of the path
#' @keywords internal 
find_stage <- function(object, path) {
  k <- length(path)
  ix <- tree_idx(path = path, tree = object$tree)
  l <- length(object$stages[[k]])
  ### stages can be defined in a reduced vector
  return(object$stages[[k]][(ix - 1) %% l + 1]) 
}


# find the paths for a given stage index
# paths is a data.frame as the ones obtained with expand.grid
# stage is an integer, the stage index
find_paths <- function(obj, stage, var) {
  ##to do
}




#' Compute the distance matrix
#' 
#' @param x list of conditional probabilities for each stage
#' @param distance the distance function e.g. \code{\link{kl}}
#' @param ... additional parameters to be passed to the distance function
#' @return The matrix with the distances between stages
distance_mat_stages <- function(x, distance = kl, ...) {
  d <- length(x)
  M <- matrix(nrow = d, ncol = d, 0)
  for (i in 1:d) {
    for (j in 1:i) {
      M[i, j] <- distance(x[[i]], x[[j]], ...)
    }
  }
  return(M + t(M))
}

#' perform a simple clustering in 2 groups
#'
#' clustering in 2 groups is performed starting
#' from a distance (positive, symmetric) matrix
#' @param M matrix of distance
#' @return a list with component \code{I} and \code{J} (the two group of clusters)
simple_clustering <- function(M) {
  ##first we find the two indxs that return the maximum distance.
  idx <- which.max(M)
  i <- ceiling(idx / dim(M)[1])
  j <- idx - (i - 1) * dim(M)[1]
  todo <- (1:(dim(M)[1]))[-c(i, j)] ## index to assign to cluster
  I <- c(i) #initialize clusters
  J <- c(j) #initialize clusters
  for (k in todo) {
    ## assign other indxs to the closest cluster
    if (M[k, i] < M[k, j]) {
      I <- c(I, k)
    } else{
      J <- c(J, k)
    }
  }
  return(list(
    "1" = I,
    "2" = J 
  ))
}



#' Distances between probabilities
#' 
#' @param x vector of probabilities
#' @param y vector of probabilities
#' @param p exponent in the \eqn{L^p} norm
#' @param alpha the order of the Renyi divergence
#' @param ... additional parameters for compatibility
#' @details Functions to compute distances between probabilities:
#' * \code{lp}: the \eqn{L^p} distance, \eqn{||x - y||_p^p}
#' * \code{ry}: the symmetric Renyi divergence of order \eqn{\alpha}
#' * \code{kl}: the symmetrized Kullback-Leibler divergence
#' * \code{tv}: the total variation or \eqn{L^1} norm
#' * \code{hl}: the (squared) Hellinger distance 
#' * \code{bh}: the Bhattacharyya distance
#' * \code{cd}: the Chan-Darwiche distance
#' @return The distance between \code{p} and \code{q}
#' @name probdist
NULL



#' @rdname probdist
#' @export
lp <- function(x, y, p = 2, ...){
  (sum( abs(x - y) ^ p )) ^ (1 / p)
}

#' @rdname probdist
#' @export
ry <- function(x, y, alpha = 2, ...){
  if (alpha == 1){
    return(kl(x,y))
  }
  if (alpha == Inf){
    return( log(max( x / y)) + log(max(y / x)) )
  }
  (log(sum(x^(alpha) / y^(alpha - 1) )) + 
    log(sum(y^(alpha) / x^(alpha - 1) ))) / (alpha - 1)
}

#' @rdname probdist
#' @export
kl <- function(x, y, ...) {
  return(sum(x * (log(x) - log(y))) + 
           sum(y * (log(y) - log(x))))
}

#' @rdname probdist
#' @export
tv <- function(x, y, ...){
  sum(abs(x - y))
}

#' @rdname probdist
#' @export
hl <- function(x,y,...){
  sum( (sqrt(x) - sqrt(y))^2)
}

#' @rdname probdist
#' @export
bh <- function(x, y, ...){
  -log(sum(sqrt(x * y)))
}

#' @rdname probdist
#' @export
cd <- function(x, y, ...){
  log(max(x / y)) - log(min(x / y))
}


#' noisy xor function
#'
#' @param x a vector of +1 nad -1
#' @param eps the uniform noise amount
#' @return the computed noisy xor
#' @keywords internal
#' @importFrom stats runif
noisy_xor <- function(x, eps = 0) {
  return(sign(prod(x) + runif(
    n = 1, min = -eps, max = eps
  )))
}

#' generate a xor dataset
#'
#' @param n number of variables
#' @param N number of observations
#' @param eps error
#' @return The xor dataset with \code{n} + 1 variables, where the last one is 
#' the class variable \code{C} computed as xor logical operator.
#' 
#' @export
#' @importFrom stats runif
#' @examples 
#' DD <- generate_xor_dataset(n = 5, N = 1000, eps = 1.2)
generate_xor_dataset <- function(n = 2,
                                 N = 100,
                                 eps = 1.2) {
  DD <- data.frame(observation = 1:N)
  for (i in 1:n) {
    DD[[paste("X", i, sep = "")]] <- sample(
      c(-1, +1),
      #prob=rep(1,2),
      prob = runif(2, min = 0, max = 1),
      size = N,
      replace = TRUE
    )
  }
  DD$C <- apply(
    DD,
    MARGIN = 1,
    FUN = function(x) {
      return(noisy_xor(x = x[-1], eps = eps))
    })
  DD <- DD[,-1]
  for (i in 1:(n+1)){
    DD[[i]] <- factor(DD[[i]], levels = c(-1, 1))
  }
  return(DD)
}


#' generate a random binary dataset for classification
#'
#' @param n number of variables
#' @param N number of observations
#' @param eps noise
#' @param gamma numeric
#' @param alpha numeric vector of length \code{n}
#' 
#' @return A data.frame with \code{n} independent random variables and
#'  one class variable \code{C} computed as
#'  \code{sign(sum(x * alpha) + runif(1, -eps, eps) + gamma)}
#' @export
#' @importFrom stats runif
#' @examples 
#' DD <- generate_linear_dataset(n = 5, 1000)
generate_linear_dataset <-
  function(n = 2,
           N = 10000,
           eps = 1.2,
           gamma = runif(1, min = -n, max = n),
           alpha = runif(n, min = -n, max = n)) {
    DD <- data.frame(observation = 1:N)
    for (i in 1:n) {
      DD[[paste("X", i, sep = "")]] <- sample(c(-1, +1),
                                              prob = runif(2),
                                              size = N,
                                              replace = TRUE)
    }
    DD$C <- apply(
      DD,
      MARGIN = 1,
      FUN = function(x) {
        return(sign(
          sum(x[-1] * alpha) + runif(
            n = 1,
            min = -eps,
            max = eps
          ) + gamma
        ))
      }
    )
    DD$C[DD$C == 0] <- 1
    DD <- DD[,-1]
    for (i in 1:(n+1)){
      DD[[i]] <- factor(DD[[i]], levels = c(-1, 1))
    }
    return(DD)
  }

#' generate a random binary dataset
#'
#' @param n number of variables
#' @param N number of observations
#' @return A data.frame with \code{n} independent random variables
#' @export
#' @importFrom stats runif
#' @examples 
#' DD <- generate_random_dataset(n = 5, 1000)
generate_random_dataset <-
  function(n = 2,
           N = 10000) {
    DD <- data.frame(observation = 1:N)
    for (i in 1:n) {
      DD[[paste("X", i, sep = "")]] <- sample(c(-1, +1),
                                              prob = runif(2),
                                              size = N,
                                              replace = TRUE)
    }
    DD <- DD[,-1]
    for (i in 1:(n)){
      DD[[i]] <- factor(DD[[i]], levels = c(-1, 1))
    }
    return(DD)
  }

#' Find maximum value
#' 
#' @param x numerical, the log-probabilities 
#' @param character the levels to be returned same length as x
#' 
#' @return factor 
#' @keywords internal
which_class <- function(x, levels){
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L){
    ix <- sample(y, 1L) 
  }else ix <- y
  factor(levels[ix], levels = levels)
}