#
# Data Mining 2014
# Assignment 1: Classification Trees
#
# Jarno Le Conte (3725154)
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
    
    # Leaf node.
    # Determine most probable class label from all observations in this leaf.
    determine.classLabel(y)
    
  } else {
      
    # Find the column and attribute value for the optimal split.
    split <- optimal.split (x, y, minleaf)
    
    if (is.null(split)) {
      determine.classLabel(y)
    } else {
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

optimal.split <- function (data, y, minleaf) 
{
 
  # calculate best split for every column
  # in order to see which attribute split
  # leads to highest impurity reduction.
  reducs <- apply(data, 2, function(x) {
    #if (length(unique(x)) == 1) {
    #  c(0,0)
    #} else {
      #NULL
      split <- bestSplit(x, y, minleaf)
      if (!is.null(split)) {
        split
      } else {
        c(0,0)
      }
      #cat(split)
      #  red <- impurityReduction(x, y, split)
      #  c(red, split)
      #} else {
      #  c(0,0)
      #}
    #}
  })
    
  # find best attribute split
  red <- max(reducs[1,])
  if (red > 0.0) {
    attr <- which(reducs[1,] == red)
    split <- reducs[2,attr]
  
    # return best split
    list(attr = attr[1], red = red[1], val = split[1])
  }
}

bestSplit <- function (x, y, minleaf) 
{  
  # sort input
  y <- sortBy(y, x)
  x <- sort(x) 
    
  # splits to consider
  splits <- splits(x, minleaf)

  if (length(splits) == 0) {
    #return()
    #NA
  } else {
  
  # calculate impurity reduction for each split
  rds <- sapply(splits, function (split) {
    impurityReduction(x, y, split)
  })
  
    # return the best split value
    maxrds <- max(rds)
    c(maxrds, splits[rds == maxrds][1])
  }
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

splits <- function(v, minleaf) {
  if (minleaf <= length(v) / 2) {
    v <- v[(minleaf):(length(v) - minleaf + 1)]
    v <- sort(unique(v))
    zs <- (v[-1] + v[-length(v)])
    zs / 2
  } else {
    return(NULL)
  }
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


# Return classification label with highest probability
determine.classLabel = function(classLabels) {
  freqTable <- table(classLabels)
  as.integer(names(which.max(freqTable)))
}

