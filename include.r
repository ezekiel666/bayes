#reads spambase data and makes preprocessing
readData <- function(s=1000) {
  dataset <- read.csv("spambase/spambase.data",header=FALSE,sep=",")
  names <- read.csv("spambase/names",header=FALSE,sep=",")
  names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
  
  #TODO: attribute discretization
  #http://en.wikipedia.org/wiki/Discretization_of_continuous_features
  
  dataset$spam <- as.factor(dataset$spam) # encode spam vector as factor (category)
  datasetSample <- dataset[sample(nrow(dataset), s),]
  
  #split 0.7/0.3 keeping $spam distribution
  split = sample(2, size = s, replace=TRUE, prob = c(0.7,0.3))
  dataTrain <- datasetSample[split==1,]
  dataTest  <- datasetSample[split==2,]
  list(dataTrain = dataTrain, dataTest = dataTest)
}

#computes given function for each pair of matrix column 
#(twice: we could optimize it, but it's easier not to for indexing)
#and returns outputs as list
lapply2 <- function(x, fun) {
  list <- list()  
  
  for(i in colnames(x)) {
    for(j in colnames(x)) {
      list[[i]][[j]] <- fun(x[,i],x[,j])      
    }
  }  
  return(list)
}

#check
check <-function(var)
  if(is.numeric(var)) {
    stop("variable cannot be numeric - only discrete attributes are allowed")
  }

#Mode function for factor datatype
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}