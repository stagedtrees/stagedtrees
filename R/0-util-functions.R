#' New label
#' 
#' give a safe-to-add label that is not in \code{labels}
#' INTERNAL USE
#' 
#' @param labels vector of labels (strings)
#' 
#' @return a string label that is different from each \code{labels}
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


#' return path index
#' 
#' @param path a path from root in the tree
#' @param tree a symmetric tree given as a list of levels 
#' 
#' This function return the integer index of the node associated with the 
#' given path in a symmetric tree defined by \code{tree}.
#' 
#' @return an integer, the index of the node corresponding to \code{path}
#' @export
tree_idx <- function(path, tree){
 k <- length(path)
 ls <- sapply(tree, length)
 is <- vapply(1:k, FUN = function(i){
   (1:ls[i])[ tree[[i]] %in% path[i]  ]
 }, FUN.VALUE = 1) 
 if (k <= 1){
   return(is[1])
 }
 sum(vapply(1:(k-1), FUN = function(i){
   prod(ls[(i+1):(k)])
 }, FUN.VALUE = 1) * (is[1:(k - 1)] - 1))  + is[k]  
 ##index in the strata now otherwise remove the -1, the following
 ## function is the complete indexing
 ## sum(vapply(1:(k-1), FUN = function(i){
 ## prod(ls[(i+1):(k)])
 ## }, FUN.VALUE = 1) * is[1:(k - 1)])  + is[k]
 ##
 ##
}



# find the stage in the path
# no checking, be careful how to use it
# to do implement TEST
# paths is a data.frame as the ones obtained with expand.grid
# plus one last column with the stage index.
# path is a vector of length = dim(paths)[2] - 1 or longer 
find_stage <- function(object, path) {
  k <- length(path)
  ix <- tree_idx(path = path, tree = object$tree)
  return(object$stages[[k]][ix])
}


# find the paths for a given stage index
# paths is a data.frame as the ones obtained with expand.grid
# stage is an integer, the stage index
find_paths <- function(obj, stage, var) {
  ##to do
  ###return(paths[paths[, dim(paths)[2]] == as.character(stage),])
}

#' Compute the KL matrix
#'
#' @param x conditional probability table
#' @return The matrix witht the symmetric KL (KL(i,j) + KL(j,i))
KL_mat_prob <- function(x) {
  d <- dim(x)[1]
  M <- matrix(nrow = d, ncol = d)
  for (i in 1:d) {
    for (j in 1:d) {
      M[i, j] <- sum( (x[i, ] / sum(x[i, ]) ) * (log(x[i, ] / sum(x[i, ])) - log(x[j, ] /
                                                                 sum(x[j, ]))))
    }
  }
  return(M + t(M))
}



#' Compute the KL matrix
#' @param x list of conditional probabilities for each stage
#' @return The matrix witht the symmetric KL (KL(i,j) + KL(j,i))
KL_mat_stages <- function(x) {
  d <- length(x)
  M <- matrix(nrow = d, ncol = d)
  for (i in 1:d) {
    for (j in 1:d) {
      M[i, j] <- sum(x[[i]] * (log(x[[i]]) - log(x[[j]])))
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

#' compute the entropy
#' @param p the vector of normalized probability
#' @return the entropy as \code{-sum(p*log(p))}
entr <- function(p) {
  return(-sum(p * log(p)))
}

kl <- function(p, q) {
  return(sum(p * (log(p) - log(q))))
}

#' noisy xor function
#'
#' @param x a vector of +1 nad -1
#' @param eps the uniform noise amount
#' @return the computed noisy xor
#' @importFrom stats runif
xor <- function(x, eps = 0) {
  #set.seed(seed = 0)
  return(sign(prod(x) + runif(
    n = 1, min = -eps, max = eps
  )))
}

#' generate a xor dataset
#'
#' @param n number of variables
#' @param N number of observations
#' @param eps error
#'
#' @return The xor dataset
#' @export
#' @importFrom stats runif
#' @examples DD <- generate_xor_dataset(n = 5, N = 1000, eps = 1.2)
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
      return(xor(x = x[-1], eps = eps))
    })
  DD <- as.data.frame(sapply(
    DD,
    FUN = function(x) {
      factor(x, levels = c(-1, 1))
    }
  ))
  return(DD[, -1])
}


#' generate a random binary dataset for classification
#'
#' @param n number of variables
#' @param N number of observations
#' @param eps noise
#' @param gamma numeric
#' @param alpha numeric vector of length \code{n}
#' @return A data.frame with \code{n} independent random variables and
#'  one class variable \code{C} computed as
#'  \code{sign(sum(x * alpha) + runif(1, -eps, eps) + gamma)}
#' @export
#' @importFrom stats runif
#' @examples DD <- generate_random_dataset(n = 5, 1000)
generate_random_dataset <-
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
    DD <- as.data.frame(sapply(
      DD,
      FUN = function(x) {
        factor(x, levels = c(-1, 1))
      }
    ))
    return(DD[, -1])
  }
