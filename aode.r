source("include.r")

#based on e1071::naiveBayes

aode <- function(x, ...)
  UseMethod("aode") #generic function @see methods(aode)

#training
#x - attributes matrix
#y - class vector
#laplace - laplace smoothing coefficient (default-1)
#returns model(apriori - class frequencies, tables - attributes statistics, levels - class levels, call - method call)
aode.default <- function(x, y, laplace = 1, ...) {
  call <- match.call() #text of method call
  Yname <- deparse(substitute(y)) #getting y name
  x <- as.data.frame(x)
  
  #estimation-function
  est <- function(var)
    if(is.numeric(var)) {
      stop("variable cannot be numeric - only discrete attributes are allowed")
    } else {
      tab <- table(y, var)
      (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
      #TODO: aode calculation instead of nb
    }
  
  #create tables
  apriori <- table(y) #count class frequencies
  tables <- lapply(x, est)
  
  #fix dimname names
  for(i in 1:length(tables)) {
    names(dimnames(tables[[i]])) <- c(Yname, colnames(x)[i])
  }
  names(dimnames(apriori)) <- Yname
  
  structure(
    list(apriori = apriori,
         tables = tables,
         levels = levels(y),
         call   = call
    ),
    class = "aode"
  )
}

#predicting
#model - model
#vector - vector
#returns predicted class for given vector
aode.predict <- function(model, vector, ...) {
  
}


#invocation

mydata <- readData(10)

#print(mydata)
#listing output in console: mydata
#clearing console: CTRL + E + L

dataTrain <- mydata$dataTrain
x <- dataTrain[,-which(names(dataTrain) %in% c("spam"))] #omit $spam column
y <- dataTrain$spam

#model<-aode(x,y) 

a <- matrix(as.factor(seq(1:6)), nrow = 2, ncol = 3)
b <- as.factor(c(0,1))
  
model<-aode(a,b) 
