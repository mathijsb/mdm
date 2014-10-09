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
  credit.confusion <- confusionTable(credit.class, credit.y) # confusion table (correct)
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
  pima.confusion <- confusionTable(pima.class, pima.y)  # confusion table (correct)
  pima.error <- errorRate(pima.confusion)
  
  cat("confusion table:\n", pima.confusion, "\n")
  cat("error:", pima.error, "\n")
}



###
# blood dataset
# algorithm to find best parameters for blood dataset
# First you have to generate data sets with dataset.generate, once!
###

# generate 10 blood sample datasets, split into training and testing sets, and writes to files on disk
dataset.generate <- function() {
  data <- read.csv(sprintf("%s/blood.txt", path), header = TRUE)
  sapply(c(1:10), function(i) {
    data <- getTrainingAndTestSet(data)
    write.table(data$training, file = sprintf("%s/samples/blood-training-%i.dat", path, i), row.names=FALSE, col.names=TRUE, sep=",")
    write.table(data$test, file = sprintf("%s/samples/blood-test-%i.dat", path, i), row.names=FALSE, col.names=TRUE, sep=",")
  }) 
}

# find optimal paramaters
dataset.run <- function() {
  nmin.range <- c(1:200)
  minleaf.range <- c(1:200)
  allCombinations <- expand.grid(nmin.range, minleaf.range)
  sampleCombinations = allCombinations[sample(nrow(allCombinations),100),]
  
  # calculate errors for a subset of parameter combinations
  r <- data.frame(t(apply(sampleCombinations, 1, function(x){ 
    err <- dataset.paramTests(x[1], x[2])
    c(nmin = x[1], minleaf = x[2], error = err)
  })))
  
  # return parameteor configuration with lowest error rate
  r[r$error == min(r$error),]
}

# determine average error rate using given parameters on different samples
dataset.paramTests <- function(nmin, minleaf) {
  errors <- sapply(c(1:10), function(i) {
    training <- read.csv(sprintf("%s/samples/blood-training-%i.dat", path, i), header = TRUE)    
    test <- data <- read.csv(sprintf("%s/samples/blood-test-%i.dat", path, i), header = TRUE)
    dataset.paramTest(nmin, minleaf, training, test)
  });
  mean(errors) # avarage error 
}

# calculate error for given parameters using training to build a tree and test set for classifying
dataset.paramTest <- function(nmin, minleaf, training, test) {
  training.x <- training[-5]  # without class labels
  training.y <- training[,5]  # vector of class labels only
  test.x <- test[-5]  # without class labels
  test.y <- test[,5]  # vector of class labels only
  
  tr <- tree.grow(training.x, training.y, nmin, minleaf)   # build tree
  test.classified <- tree.classify(test.x, tr)  # predict / classification
  confusion <- confusionTable(test.classified, test.y)  # confusion table
  
  errorRate(confusion)
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

#create confusion table
confusionTable <- function(classified, training) {
  table(c(0,0,1,1,classified), c(0,1,0,1,training)) - 1 # force 2x2 matrix
}

# determine the error of a confusion table
errorRate <- function(confusionTable) {
  (confusionTable[2] + confusionTable[3]) / sum(confusionTable)
}

