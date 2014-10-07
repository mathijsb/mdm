
tree.grow <- function (x, y, nmin, minleaf) 
{
  if (length(y) == 1 || impurity(y) == 0) {
    cat("Impurity == 0")        
  } else {
    
    
    split <- dataSplit (x, y)
    
    cat("Best data split: ", split, "\n")
    
    #    cat(split)
    left <- x[x[,split[1]] <= split[3],]
    right <- x[x[,split[1]] > split[3],]
    
    yleft <- y[x[,split[1]] <= split[3]]
    yright <- y[x[,split[1]] > split[3]]
    
    cat("left: ", data.frame(left), "\n")
    cat("right: ", data.frame(right), "\n")
    
    cat("yleft: ", yleft, "\n")
    cat("yright: ", yright, "\n")

    
    cat ("Stepping into recursion")
    #cat (left)
    #cat (yleft)
    
    
    
    tree.grow(data.frame(left), yleft, nmin, minleaf)
    tree.grow(data.frame(right), yright, nmin, minleaf)
    
    #list(left = left, right = right, yleft = yleft, yright = yright)
    
  }
  
  #c(left,yleft,right,yright)
  
  #treeleft <- tree.grow(left, )
  
  #Filter(x, x[,split[1]] < split[3])
}




dataSplit <- function (data, y) 
{
  # calculate best split for every column
  # in order to see which attribute split
  # leads to highest impurity reduction.
  reducs <- apply(data, 2, function(x) {
    split <- bestSplit(x, y)  ### x incorrect here
    cat("bestSplit: ", split, "\n")
    red <- impurityReduction(x, y, split)
    c(red, split)
  })
  
  #print(reducs)
  
  
  # find best attribute split
  red <- max(reducs[1,])
  attr <- which(reducs[1,] == red)
  split <- reducs[2,attr]
  
  # return best split
  c(attr, red, split)
}

bestSplit <- function (x, y) 
{
  cat('xa: ', x, "\n")
  cat('ya: ', y, "\n")
  
  # sort input
  y <- sortBy(y, x)
  x <- sort(x) 
  
  cat('x1: ', x, "\n")
  cat('y1: ', y, "\n")
  
  # splits to consider
  splits <- splits(x)
  cat("splits: ", splits, "\n")
  
  cat('x2: ', x, "\n")
  cat('y2: ', y, "\n")
  
  # calculate impurity reduction for each split
  rds <- sapply(splits, function (split) {
    ## XXX  conflict: x and y have changed here
    cat('x3: ', x, "\n")
    cat('y3: ', y, "\n")
    impurityReduction(x, y, split)
  })
  
  cat("rds: ", rds, "\n")
  
  # return the best split value
  splits[rds == max(rds)]
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
  cat("array: ", v, "\n")
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
  cat('x0: ', x, "\n")
  cat('y0: ', y, "\n")
  x[sort.int(y, index.return = TRUE)$ix]
}



