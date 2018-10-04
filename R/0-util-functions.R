
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
