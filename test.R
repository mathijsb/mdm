######################
### TEST FUNCTIONS ###
######################

path <- "~/dev/school/mdm/introduction/"

# credit data

credit <- function() {
  credit.data <- read.csv(sprintf("%s/credit.txt", path))
  credit.x <- credit.data[-6]  # without class labels
  credit.y <- credit.data[,6]  # vector of class labels only

  credit.tree <- tree.grow(credit.x, credit.y, 1, 1)    # build tree
  credit.class <- tree.classify(credit.x, credit.tree)  # predict / classification
  credit.confusion <- table(credit.class, credit.y)     # confusion table (correct)
  credit.error <- errorRate(credit.confusion)
  
  cat("confusion table:\n", credit.confusion, "\n")
  cat("error:", credit.error, "\n")
}

# pima indians numbers

pima <- function() {
  pima.data <- read.csv(sprintf("%s/pima.txt", path))
  pima.x <- pima.data[-9]  # without class labels
  pima.y <- pima.data[,9]  # vector of class labels only

  pima.tree <- tree.grow(pima.x, pima.y, 20, 5)   # build tree
  pima.class <- tree.classify(pima.x, pima.tree)  # predict / classification
  pima.confusion <- table(pima.class, pima.y)     # confusion table (correct)
  pima.error <- errorRate(pima.confusion)
  
  cat("confusion table:\n", pima.confusion, "\n")
  cat("error:", pima.error, "\n")
}



###
# IonoSphere dataset
# algorithm to find best parameters for ionosphere
# First you have to generate data sets with iono.generate, once!
###

# generate 10 ionosphere datsets for training and testing, writing datasets to files
iono.generate <- function() {
  data <- read.csv(sprintf("%s/ionosphere.data", path), header = FALSE)
  sapply(c(1:10), function(i) {
    data <- getTrainingAndTestSet(iono.data)
    write.table(data$training, file = sprintf("%s/iono-training-%i.dat", path, i), row.names=FALSE, col.names=FALSE, sep=",")
    write.table(data$test, file = sprintf("%s/iono-test-%i.dat", path, i), row.names=FALSE, col.names=FALSE, sep=",")
  }) 
}

# find optimal paramaters
iono.run <- function() {
  nmin.range <- c(1:200)
  minleaf.range <- c(1:100)
  allCombinations <- expand.grid(nmin.range, minleaf.range)
  sampleCombinations = c[sample(nrow(c),40),]
  
  # calculate errors for a subset of combination of parameters
  r <- data.frame(t(apply(sampleCombinations, 1, function(x){ 
    err <- iono.paramTests(x[1], x[2])
    c(nmin = x[1], minleaf = x[2], error = err)
  })))
  
  # return parameteor configuration with lowest error rate
  r[r$error == min(r$error),]
}

# determine average error rate using given parameters on different samples
iono.paramTests <- function(nmin, minleaf) {
  errors <- sapply(c(1:5), function(i) {
    training <- read.csv(sprintf("%s/iono-training-%i.dat", path, i))    
    test <- data <- read.csv(sprintf("%s/iono-test-%i.dat", path, i))
    iono.paramTest(nmin, minleaf, training, test)
  });
  mean(errors) # avarage error 
}

# calculate error for given parameters using training to build a tree and test set for classifying
iono.paramTest <- function(nmin, minleaf, training, test) {
  training.x <- training[-35]  # without class labels
  training.y <- training[,35]  # vector of class labels only
  test.x <- test[-35]  # without class labels
  test.y <- test[,35]  # vector of class labels only
  
  tr <- tree.grow(training.x, training.y, nmin, minleaf)   # build tree
  test.classified <- tree.classify(test.x, tr)  # predict / classification
  confusionTable <- table(test.classified, test.y)  # confusion table
  
  errorRate(confusionTable)
}


###
# helpers
###

# split data set in 70% training 30% test
getTrainingAndTestSet <- function(frame) {
  frame <- shuffleRows(frame)
  split <- nrow(frame) * 0.7
  training <- frame[c(1:split),]
  test <- frame[c(split+1):nrow(frame),]
  list(training = training, test = test)
}

# shuffle rows of a data.frame
shuffleRows = function(frame) {
  frame[sample(nrow(frame)),]
}

# determine the error of a confusion table
errorRate <- function(confusionTable) {
  (confusionTable[2] + confusionTable[3]) / sum(confusionTable)
}

