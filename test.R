######################
### TEST FUNCTIONS ###
######################

# credit data

credit.data <- read.csv("~/dev/school/mdm/introduction/credit.txt")
credit.x <- credit.data[-6]  # without class labels
credit.y <- credit.data[,6]  # vector of class labels only

credit.tree <- tree.grow(credit.x, credit.y, 1, 1)    # build tree
credit.class <- tree.classify(credit.x, credit.tree)  # predict / classification
credit.confusion <- table(credit.class, credit.y)     # confusion table (correct)

# pima indians numbers

pima.data <- read.csv("~/dev/school/mdm/introduction/pima.txt")
pima.x <- pima.data[-9]  # without class labels
pima.y <- pima.data[,9]  # vector of class labels only

pima.tree <- tree.grow(pima.x, pima.y, 20, 5)   # build tree
pima.class <- tree.classify(pima.x, pima.tree)  # predict / classification
pima.confusion <- table(pima.class, pima.y)     # confusion table (XXX I think this differs to much!!!!)