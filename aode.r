source("include.r")

#based on e1071::naiveBayes

aode <- function(x, ...)
  UseMethod("aode") #generic function @see methods(aode)

apply2 <- function(x, fun) {
  list <- list()  
  
  for(i in colnames(x)) {
    for(j in colnames(x)) {
      list[[i]][[j]] <- fun(x[,i],x[,j])      
    }
  }  
  return(list)
}


#training
#x - attributes matrix
#y - class vector
#laplace - laplace smoothing coefficient (default=1) @see http://en.wikipedia.org/wiki/Additive_smoothing
#returns model(apriori - class frequencies, tables - attributes statistics, levels - class levels, call - method call)
aode.default <- function(x, y, laplace = 1, ...) {
  call <- match.call() #text of method call
  Yname <- deparse(substitute(y)) #getting y name
  x <- as.data.frame(x)
  
  check <-function(var)
    if(is.numeric(var)) {
      stop("variable cannot be numeric - only discrete attributes are allowed")
    }
  
  lapply(x, check)
  
  #estimation
  est1 <- function(var) {
    tab1 <- table(y, var) #count class frequencies for a given variable
    (tab1 + laplace) / (rowSums(tab1) + laplace * nlevels(var))  
    #print("tab1")
    #print(tab1) #[y, var]
    return(tab1)
  }
  
  est2 <- function(var1, var2) {  
    vc <- table(var1, y) #count class frequencies for a given variable
    #print("vc")
    #print(vc) #[var1, y]
    
    tab2 <- table(var1, var2, y) #count variable pair frequency for a given class
    #print("tab2 before")
    #print(tab2) #[var1, var2, y]
    
    # for each tuple(var1,y) ...
    for(f in dimnames(tab2)$var1){
      for(s in dimnames(tab2)$y){
        #print(paste("(",f,",",s,")", sep=""))
        tab2[f,,s] <- (tab2[f,,s] + laplace) / (vc[f,s] + laplace * nlevels(var1))
      }    
    }
    
    #print("tab2 after")
    #print(tab2)
    return(tab2)
  }
  
  cfreq <- table(y) #count class frequencies
  names(dimnames(cfreq)) <- Yname 
  
  #create tables
  tables1 <- lapply(x, est1)
  #for(i in 1:length(tables1)) { # y -> yname
  #  names(dimnames(tables1[[i]])) <- c(Yname, colnames(x)[i])
  #}
  tables2 <- apply2(x, est2)
  
  structure(
    list(cfreq = cfreq,
         tables1 = tables1,
         tables2 = tables2,
         levels = levels(y),
         call   = call
    ),
    class = "aode"
  )
}

#predicting
#model - model
#vector - vector
#m - frequency lower limit (default=1)
#returns predicted class for a given vector
aode.predict <- function(model, vector, m=1, ...) {
  
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

a <- matrix(as.factor(1:6), nrow = 2, ncol = 3)
colnames(a) <- c("first", "second", "thrid")
b <- as.factor(c(0,1))

model<-aode(a,b,1) 
