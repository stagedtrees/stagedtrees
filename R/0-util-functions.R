
# find the stage in the path
# no checking, be careful how to use it
# to do implement TEST
# paths is a data.frame as the ones obtained with expand.grid
# plus one last column with the stage index.
# path is a vector of length = dim(paths)[2] - 1
find_stage <- function(paths, path){
  k <- dim(paths)[2]
  for (i in 1:(dim(paths)[1])){
    if (all(paths[i,-k] == path)) return(paths[i,k])
  }
}

# find the paths for a given stage index
# paths is a data.frame as the ones obtained with expand.grid
# stage is an integer, the stage index
find_paths <- function(paths, stage){
  return(paths[paths[,dim(paths)[2]] == stage, ])
}

#' Compute the KL matrix
#'
#' @param x conditional probability table
#' @return The matrix witht the symmetric KL (KL(i,j) + KL(j,i))
KL_mat_prob <- function(x){
  d <- dim(x)[1]
  M <- matrix(nrow = d, ncol=d)
  for (i in 1 : d){
    for (j in 1 : d){
      M[i, j] <- sum( x[i,] * (log(x[i,])-log(x[j,])) )
    }
  }
  return(M + t(M))
}

#' Compute the KL matrix
#' @param x list of conditional probabilities for each stage
#' @return The matrix witht the symmetric KL (KL(i,j) + KL(j,i))
KL_mat_stages <- function(x){
  d <- dim(x)[1]
  M <- matrix(nrow = d, ncol=d)
  for (i in 1 : d){
    for (j in 1 : d){
      M[i, j] <- sum( x[[i]] * (log(x[[i]])-log(x[[j]])) )
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
simple_clustering <- function(M){
  ##first we find the two indxs that return the maximum distance.
  idx <- which.max(M)
  i <- ceiling(idx / dim(M)[1])
  j <- idx - (i-1)*dim(M)[1]
  todo <- (1 : (dim(M)[1]) )[-c(i,j)] ## index to assign to cluster
  I <- c(i) #initialize clusters
  J <- c(j) #initialize clusters
  for (k in todo){ ## assign other indxs to the closest cluster
     if (M[k,i] < M[k,j]){
       I <- c(I, k)
     }else{
       J <- c(J, k)
     }
  }
  return(list(I = I, J =J , i =i , j =j ))
}


