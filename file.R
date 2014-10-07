
tree.grow <- function (x, y, nmin, minleaf) 
{
  if (length(y) == 1 || impurity(y) == 0) {
    y[1]  
  } else {
    
    
    split <- dataSplit (x, y)
    
    #cat("Best data split: ", split, "\n")
    
    splitcol <- split[1]
    splitval <- split[3]
    
    splitdat <- x[[splitcol]]  
    smaller <- splitdat <= splitval
    larger <- splitdat > splitval
    
    #    cat(split)
    left <- x[smaller,]
    right <- x[larger,]
    
    yleft <- y[smaller]
    yright <- y[larger]
    
    #rownames(left) <- NULL
    #rownames(right) <- NULL    
    
    #cat("left: ", data.frame(left), "\n")
    #cat("right: ", data.frame(right), "\n")
    
    #cat("yleft: ", yleft, "\n")
    #cat("yright: ", yright, "\n")

    
    cat ("Stepping into recursion")
    #cat (left)
    #cat (yleft)
    
    
    
    l <- tree.grow(left, yleft, nmin, minleaf)
    r <- tree.grow(right, yright, nmin, minleaf)
    
    list(split=c(splitcol, splitval), left=l, right=r)
    
    #((1,1), (1,1))
    #list(left = left, right = right, yleft = yleft, yright = yright)
    
  }
  
  #c(left,yleft,right,yright)
  
  #treeleft <- tree.grow(left, )
  
  #Filter(x, x[,split[1]] < split[3])
}

tree.classify <- function (tree, cases) {
  as.vector(apply(cases, 1, function(x) { classifyCase(tree, x)}))
}

classifyCase <- function (tree, x) {
  if (typeof(tree) == "integer") {
    tree
  } else {
    col <- tree$split[1]
    val <- tree$split[2]
    if (x[col] <= val) {
      classifyCase(tree$left, x)
    } else {
      classifyCase(tree$right, x)
    }
  }  
}

#tree.classify <- function (tree, x) {
#  
#  if (typeof(tree) == "integer") {
#    rep(tree, nrow(x))
#  } else {
#  
#    col <- tree$split[1]
#    val <- tree$split[2]
#  
#    tol <- x[col] <= val
#    tor <- x[col] > val
#    
#    l <- x[tol,]
#    r <- x[tor,]
#  
#    c(tree.classify (tree$left, l), tree.classify (tree$right, r))
#  }
  
  
  #resl <- 
  #print(l)
  #print(r)
#}



dataSplit <- function (data, y) 
{
 
  #print(typeof(data))
  # calculate best split for every column
  # in order to see which attribute split
  # leads to highest impurity reduction.
  reducs <- apply(data, 2, function(x) {
    if (length(unique(x)) == 1) {
      c(0,0)
    } else {
      split <- bestSplit(x, y)  ### x incorrect here
      #cat("bestSplit: ", split, "\n")
      red <- impurityReduction(x, y, split)
      c(red, split)
      #c(0,0)
    }
  })
  
  #print(typeof(data))
  #print(typeof(y))
  #print(typeof(reducs))
  
  
  # find best attribute split
  red <- max(reducs[1,])
  attr <- which(reducs[1,] == red)
  split <- reducs[2,attr]
  
  # return best split
  c(attr, red, split)
}

bestSplit <- function (x, y) 
{
  #cat('xa: ', x, "\n")
  #cat('ya: ', y, "\n")
  
  # sort input
  y <- sortBy(y, x)
  x <- sort(x) 
  
  #cat('x1: ', x, "\n")
  #cat('y1: ', y, "\n")
  
  # splits to consider
  splits <- splits(x)
  #cat("splits: ", splits, "\n")
  
  #cat('x2: ', x, "\n")
  #cat('y2: ', y, "\n")
  
  # calculate impurity reduction for each split
  rds <- sapply(splits, function (split) {
    ## XXX  conflict: x and y have changed here
    #cat('x3: ', x, "\n")
    #cat('y3: ', y, "\n")
    impurityReduction(x, y, split)
  })
  
  #cat("rds: ", rds, "\n")
  
  # return the best split value
  splits[rds == max(rds)][1]
}

impurity <- function (v) 
{   
  # impurity
  # i(t): p(0|t)(1 - p(0|t))
  
  p0 = frac(v==0)
  p0 * (1 - p0) 
}

impurityReduction <- function (x, y, split) 
{
  pi_l <- frac(x <= split)
  pi_r <- frac(x > split)
  
  impur_l    <- impurity(y[x <= split])
  impur_r    <- impurity(y[x > split])
  
  impurity(y) - pi_l*impur_l - pi_r*impur_r
}

splits <- function(v) {
  v <- sort(unique(v))
  #cat("array: ", v, "\n")
  xs <- v[-length(v)] # init
  ys <- v[-1]         # tail
  zs <- ys + xs       # zipWith (+) (tail y) (init y)
  zs / 2              # map (/2) zs
}

frac <- function (v) 
{
  # count the fraction TRUE elements
  length(v[v]) / length(v)
}

sortBy <- function (x, y) 
{
  #cat('x0: ', x, "\n")
  #cat('y0: ', y, "\n")
  x[sort.int(y, index.return = TRUE)$ix]
}



