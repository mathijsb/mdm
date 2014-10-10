#
# Data Mining 2014
# Assignment 1: Classification Trees
#
# Jarno Le Conte (3725154)
# Mathijs Baaijens (3542068)
#

tree.grow <- function (x, y, nmin, minleaf) {
  # Grows a classification tree based on the input training data.
  #
  # Args:
  #   x: Training data matrix.
  #   y: Training data classs labels.
  #   nmin : Minimum number of observations that a node must contain at least,
  #          for it to be allowed to be split.
  #   minleaf : The minimum number of observations required for a leaf node.
  #
  # Returns:
  #   Classification tree based on input training data.
  
  if (length(y) < nmin) {
    
    determine.optimal.label(y)
    
  } else {
    
    split <- find.optimal.split (x, y, minleaf)
    
    if (is.null(split)) {
      
      # If no split was found (e.g. due to impurity = 0 or the minleaf constraint)
      # this becomes a leaf node.
      determine.optimal.label(y)
      
    } else {
      
      # Select the rows classified in the left and right sides of the tree.
      smaller <- x[split$attr] <= split$val
      larger <- x[split$attr] > split$val
    
      # Build the left and right branches of the classification tree.
      left <- tree.grow(x[smaller,], y[smaller], nmin, minleaf)
      right <- tree.grow(x[larger,], y[larger], nmin, minleaf)
      
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
  
  # Predicts class labels for the input row based on the input classification
  # tree.
  #
  # Args:
  #   x: Vector containing the attribute values for the cases to classify.  
  #   tr: Classification tree to use to predict class labels.
  #
  # Returns:
  #   Predicted class labels for the input data.
  
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

find.optimal.split <- function (data, y, minleaf) {
  # Finds the column and value for splitting the input dataset that results
  # in the largest impurity reduction.
  #
  # Args:
  #   x: Dataset.
  #   y: Class labels.
  #   minleaf : The minimum number of observations required for a leaf node.
  #
  # Returns:
  #   List containing the column number, impurity reduction and value for the
  #   optimal split.
  
  # Determine the optimal split for every column in the dataset.
  reducs <- lapply(1:ncol(data), function(col) {
    
    splits <- find.split.values(data[,col], minleaf)
    
    if (length(splits) == 0) {
      
      # No splits for this column (due to no impurity or minleaf constraint).
      list(attr = col, red = NA, val = NA)
      
    } else {
      
      # Calculate impurity reduction for all possible split values.
      rds <- sapply(splits, function (split) {
        determine.impurity.reduction(data[,col], y, split)
      })
      
      # Return the split with the largest impurity reduction.
      list(attr = col, red = max(rds), val = splits[which.max(rds)])
            
    }
  })

  # Select the optimal column to split on (if any) and return it.
  bestsplit <- which.max(lapply(reducs, function(x) x$red))
  if (length(bestsplit) != 0) {
    reducs[[bestsplit]]
  }

}
impurity <- function (v) {   
  # Determines the impurity (Gini coefficient) for a vector of class labels
  #
  # Args:
  #   v: Vector of class labels
  #
  # Returns:
  #   Impurity (Gini coefficient) for the input vector.
  
  p0 = frac(v==0)
  p0 * (1 - p0) 
}

determine.impurity.reduction <- function (x, y, split) {
  # Determines the impurity reduction for a given split.
  #
  # Args:
  #   v: Vector of attributes to split
  #   y: Vector of class labels for the input data.
  #   split: Split value.
  #
  # Returns:
  #   Impurity (Gini coefficient) for the given split.
  
  pi_l <- frac(x <= split)
  pi_r <- frac(x > split)
  
  impur_l    <- impurity(y[x <= split])
  impur_r    <- impurity(y[x > split])
  
  impurity(y) - pi_l*impur_l - pi_r*impur_r
}

find.split.values <- function(v, minleaf) {
  # Determines the possible split values for the input vector.
  #
  # Args:
  #   v: Vector of attributes to split
  #   minleaf : The minimum number of observations required for a leaf node.
  #
  # Returns:
  #   Vector of possible split values.
  
  if (minleaf <= length(v) / 2) {
    
    v <- sort(v)
    
    # Drop the first and last 'minleaf' elements so that only splits that
    # respect 'minleaf' will be returned.
    v <- v[(minleaf):(length(v) - minleaf + 1)]
    
    v <- unique(v)
    
    # Calculate the mid-value for every possible split.
    zs <- (v[-1] + v[-length(v)])
    zs / 2
    
  } else {
    
    # No splits possible.
    return(NULL)
  }
}

frac <- function (v){
  # Returns the fraction of true elements in an input vector
  #
  # Args:
  #   v: Input vector.
  #
  # Returns:
  #   Fraction of true elements in the input.
  length(v[v]) / length(v)
}

determine.optimal.label = function(labels) {
  # Determines the most common class label in a vector of class labels
  #
  # Args:
  #   labels: Vector of class label.
  #
  # Returns:
  #   Most common label.
  
  freqtable <- table(labels)
  as.integer(names(which.max(freqtable)))
}

