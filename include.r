require(caret)
#install.packages("caret", dep = TRUE)

readData <- function(s=1000) {
  dataset <- read.csv("spambase/spambase.data",header=FALSE,sep=",")
  names <- read.csv("spambase/names",header=FALSE,sep=",")
  names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
  
  #TODO: attribute discretization
  #http://en.wikipedia.org/wiki/Discretization_of_continuous_features
  
  dataset$spam <- as.factor(dataset$spam) # encode spam vector as factor (category)
  sample <- dataset[sample(nrow(dataset), s),]
  
  #split 0.7/0.3 keeping $spam distribution
  trainIndex <- createDataPartition(sample$spam, p = .7, list = FALSE, times = 1)
  dataTrain <- sample[ trainIndex,]
  dataTest  <- sample[-trainIndex,]
  
  list(dataTrain = dataTrain, dataTest = dataTest)
}