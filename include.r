
readData <- function(s=1000) {
  dataset <- read.csv("spambase/spambase.data",header=FALSE,sep=",")
  names <- read.csv("spambase/names",header=FALSE,sep=",")
  names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
  
  #TODO: attribute discretization
  #http://en.wikipedia.org/wiki/Discretization_of_continuous_features
  
  dataset$spam <- as.factor(dataset$spam) # encode spam vector as factor (category)
  dataset_sample <- dataset[sample(nrow(dataset), s),]
  
  #split 0.7/0.3 keeping $spam distribution
  split = sample(2, size = s, replace=TRUE, prob = c(0.7,0.3))
  dataTrain <- dataset_sample[ split==1,]
  dataTest  <- dataset_sample[ split==2,]
  list(dataTrain = dataTrain, dataTest = dataTest)
}