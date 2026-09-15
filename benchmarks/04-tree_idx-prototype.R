pkgload::load_all(".", quiet=TRUE); set.seed(1)
tree <- lapply(1:8, function(i) letters[1:4]); names(tree) <- paste0("V",1:8)
path <- vapply(tree[1:7], function(l) sample(l,1), "a")

# optimized: strides + level lookups precomputed ONCE per model
make_idx <- function(tree) {
  ls <- lengths(tree)
  p <- length(ls)
  lut <- lapply(tree, function(l) setNames(seq_along(l), l))
  strides <- rev(cumprod(c(1, rev(ls[-1]))))   # stride[i] = prod(ls[(i+1):p])
  function(path) {
    k <- length(path)
    idx <- 0L
    for (i in seq_len(k-1)) idx <- idx + (lut[[i]][[path[i]]] - 1L) * prod(ls[(i+1):k])
    idx + lut[[k]][[path[k]]]
  }
}
fidx <- make_idx(tree)
stopifnot(identical(as.numeric(fidx(path)), as.numeric(tree_idx(path, tree))))
cat("correctness: optimized == tree_idx  TRUE\n\n")

N <- 20000
b <- function(l,f){f();t<-system.time(for(i in seq_len(N))f())[["elapsed"]]
  cat(sprintf("%-42s %8.2f us/call\n",l,t/N*1e6)); t/N*1e6}
a <- b("tree_idx (current)", function() tree_idx(path, tree))
bb<- b("precomputed strides+LUT (pure R)", function() fidx(path))
cat(sprintf("\npure-R speedup: %.1fx\n\n", a/bb))
cat("--- component costs in current tree_idx ---\n")
b("  sapply(tree,length) [per call!]", function() sapply(tree, length))
b("  lengths(tree) equivalent",        function() lengths(tree))
b("  tree[[1]] %in% path[1]",          function() tree[[1]] %in% path[1])
b("  match(path[1], tree[[1]])",       function() match(path[1], tree[[1]]))
