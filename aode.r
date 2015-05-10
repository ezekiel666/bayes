source("include.r")

#based on e1071::naiveBayes

aode <- function(x, ...)
  UseMethod("aode") #generic function @see methods(aode)

#training
#x - attributes matrix
#y - class vector
#laplace - laplace smoothing coefficient (default=1) @see http://en.wikipedia.org/wiki/Additive_smoothing
#returns model(apriori - class frequencies, tables - attributes statistics, levels - class levels, call - method call)
aode.default <- function(x, y, laplace = 1, ...) {
  call <- match.call() #text of method call
  Yname <- deparse(substitute(y)) #getting y name
  x <- as.data.frame(x)
  
  #type check
  check <-function(var)
    if(is.numeric(var)) {
      stop("variable cannot be numeric - only discrete attributes are allowed")
    }
  
  lapply(x, check)
  
  #estimation functions
  est1 <- function(var) {
    tab1 <- table(y, var) #count class frequencies for a given variable
    tab1 / length(y) #there is no need to smoothe here!     
  }
  
  est2 <- function(var1, var2) {  
    vc <- table(var1, y) #count class frequencies for a given variable   
    tab2 <- table(var1, var2, y) #count variable pair frequency for a given class
        
    for(f in dimnames(tab2)$var1){
      for(s in dimnames(tab2)$y){
        # for each tuple(var1,y) ...
        tab2[f,,s] <- (tab2[f,,s] + laplace) / (vc[f,s] + laplace * nlevels(var2)) #we smoothe here!
      }    
    }  
    
    tab2
  }
  
  #create tables  
  vfreq <- lapply(x, table)
  tables1 <- lapply(x, est1)
  tables2 <- lapply2(x, est2)  
  
  #fixing names
  for(i in 1:length(tables1)) {
    names(dimnames(tables1[[i]])) <- c(Yname, colnames(x)[i])
  }
  for(i in 1:length(tables2)) {
    for(j in 1:length(tables2[[i]])) {
      names(dimnames(tables2[[i]][[j]])) <- c(colnames(x)[i], colnames(x)[j], Yname)
    }
  }
  
  #returning model
  structure(
    list(vfreq = vfreq,
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
#newdata - attributes matrix with new data (column names are matched against the training data ones)
#c - {class, raw} indicates if return only class with maximum propability (default) or set with all classes/probabilities
#m - frequency lower limit (including, default=1)
#returns predicted classes
predict.aode <- function(model, newdata, type = c("class", "raw"), m=1, ...) {
  type <- match.arg(type) #matches against specified arguments  
  newdata <- as.data.frame(newdata)
  attribs <- match(names(model$tables1), names(newdata)) #vector of the positions of (first) matches of its first argument in its second
  newdata <- data.matrix(newdata) #numeric matrix
  
  #TODO: if no such i, that F(vi) >= m AODE defaults to NB (assign NA)
  
  L <- sapply(1:nrow(newdata), function(i) {
    ndata <- newdata[i, ]    
    #logarithm cannot be applied, because of sum (it makes computations more numerically unstable)  
    #we compute in rows for each class
    L <- apply(sapply(seq_along(attribs),
      function(v) {
        nd <- ndata[attribs[v]]
        if(is.na(nd)) {
          #it's missing value in test set
          rep(0, length(model$levels))
        } else {
          freq <- model$vfreq[[v]][nd]
          if(is.na(freq) || freq < m) {
            #it's missing value in training set or freq is under the limit
            rep(0, length(model$levels))
          } else {        
            model$tables1[[v]][, nd]
            #TODO: multiply by attr pair probabilities + smoothing
          }
        }
      }),1,sum)
    
      if (type == "class")
        L
      else {
        #probabilities normalization
        sapply(L, function(lp) {
          lp/sum(lp)
          lp[is.na(lp)] <- 0
        })
      }
  })
  
  if (type == "class")
    factor(model$levels[apply(L, 2, which.max)], levels = model$levels)
  else t(L)
}

#printing
print.aode <- function(x, ...) {
  cat("\nAODE\n\n")
  cat("Call:\n")
  print(x$call)
  
  cat("\nLevels:\n")
  print(x$levels)
  
  cat("\nVariable frequencies:\n")
  print(x$vfreq)

  cat("\nConditional probabilities:\n")
  print(x$tables1)
  
  cat("\nConditional probabilities of pairs of attributes:\n")
  print(x$tables2)
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

a <- matrix(as.factor(1:6), nrow = 2, ncol = 3) #discrete values
colnames(a) <- c("a1", "a2", "a3")
b <- as.factor(c(0,1))

model <- aode(a,b)
predicted <- predict(model, a)
predictedraw <- predict(model, a, type = "raw")
