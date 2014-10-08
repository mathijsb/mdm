#
# Data Mining 2014
# Assignment 1: Classification Trees
#
# Jarno Le Conte (?)
# Mathijs Baaijens (3542068)
#

tree.grow <- function (x, y, nmin, minleaf) 
{
  # Grows a classification tree based on the input training data.
  #
  # Args:
  #   x: Training data matrix.
  #   y: Training data classs labels.
  #   nmin : Minimum number of observations that a node must contain at least,
  #          for it to be allowed to be split.
  #   minleaf : s the minimum number of observations required for a leaf node.
  #
  # Returns:
  #   Classification tree based on input training data.
  
  if (length(y) < nmin || impurity(y) == 0) {
    #TODO : improve this
    y[1]  
  } else {
      
    # Find the column and attribute value for the optimal split.
    split <- optimal.split (x, y)
    
    # Select the rows classified in the left and right sides of the tree.
    smaller <- x[[split$attr]] <= split$val
    larger <- x[[split$attr]] > split$val
    
    # Build the left and right branches of the classification tree.
    left <- tree.grow(x[smaller,], y[smaller], nmin, minleaf)
    right <- tree.grow(x[larger,], y[larger], nmin, minleaf)
    
    # Return the classification tree.
    list(attr = split$attr, val = split$val, left = left, right = right)    
  }  
}

tree.classify <- function (x, tr) {
  # Predicts class labels for the input data based on the input classification
  # tree.
  #
  # Args:
  #   x: Data matrix containing the attribute values for the cases to classify.  
  #   tr: Classification tree to use to predict class labels.
  #
  # Returns:
  #   Vector of the predicted class labels for the input data.
  
  as.vector(apply(x, 1, function(row) {
    tree.classify.case(tr, row)
  }))
}

tree.classify.case <- function(tr, x) { 
  if (typeof(tr) == "integer") {
    tr
  } else {
    if (x[tr$attr] <= tr$val) {
      tree.classify.case(tr$left, x)
    } else {
      tree.classify.case(tr$right, x)
    }
  }  
}

optimal.split <- function (data, y) 
{
 
  # calculate best split for every column
  # in order to see which attribute split
  # leads to highest impurity reduction.
  reducs <- apply(data, 2, function(x) {
    if (length(unique(x)) == 1) {
      c(0,0)
    } else {
      split <- bestSplit(x, y)
      red <- impurityReduction(x, y, split)
      c(red, split)
    }
  })
    
  # find best attribute split
  red <- max(reducs[1,])
  attr <- which(reducs[1,] == red)
  split <- reducs[2,attr]
  
  # return best split
  list(attr = attr[1], red = red[1], val = split[1])
}

bestSplit <- function (x, y) 
{  
  # sort input
  y <- sortBy(y, x)
  x <- sort(x) 
    
  # splits to consider
  splits <- splits(x)

  # calculate impurity reduction for each split
  rds <- sapply(splits, function (split) {
    impurityReduction(x, y, split)
  })
  
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
  x[sort.int(y, index.return = TRUE)$ix]
}



