source("include.r")

#based on e1071::naiveBayes

#it treats numeric values as discrete
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
        tab2[f,,s] <- (tab2[f,,s] + laplace) / (vc[f,s] + laplace * nlevels(as.factor(var2))) #we smoothe here!        
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
         call   = call,
         laplace = laplace
    ),
    class = "aode"
  )
}

#printing
print.aode <- function(x, ...) {
  cat("\nAODE\n\n")
  cat("Call:\n")
  print(x$call)
  
  cat("\nLevels:\n")
  print(x$levels)
  
  cat("\nLaplace coefficient:\n")
  print(x$laplace)
  
  cat("\nVariable frequencies:\n")
  print(x$vfreq)
  
  cat("\nConditional probabilities:\n")
  print(x$tables1)
  
  cat("\nConditional probabilities of pairs of attributes:\n")
  print(x$tables2)
}

#predicting
#model - model
#newdata - attributes matrix with new data (column names are matched against the training data ones)
#c - {class, raw} indicates if return only class with maximum propability (default) or set with all classes/probabilities
#m - frequency lower limit (including, default=1)
#returns predicted classes
#in case frequency condition is to strict or whole vector consists of new/unknown values, NA is returned
predict.aode <- function(model, newdata, type = c("class", "raw"), m=1, ...) {
  type <- match.arg(type) #matches against specified arguments  
  df <- as.data.frame(newdata) #to match names
  attribs <- match(names(model$tables1), names(df)) #vector of the positions of (first) matches of its first argument in its second    
  attribs <- attribs[!is.na(attribs)]
  
  L <- sapply(1:nrow(newdata), function(i) {    
    ndata <- newdata[i, ]    
    #logarithm cannot be applied, because of sum (it makes computations more numerically unstable)  
    #we compute in rows for each class
    L <- apply(sapply(seq_along(attribs),
      function(v) {
        a_v <- attribs[v]
        nd_v <- as.character(ndata[[a_v]])        
        if(is.na(nd_v)) {
          #it's missing value in test set
          rep(0, length(model$levels))
        } else {                    
          freq_v <- model$vfreq[[a_v]][nd_v]          
          if(is.na(freq_v) || freq_v < m) {            
            #it's missing value in training set or freq_v is under the limit
            rep(0, length(model$levels))
          } else {        
            model$tables1[[a_v]][,nd_v] * apply(sapply(seq_along(attribs),
              function(u) {
                a_u <- attribs[u]
                nd_u <- as.character(ndata[[a_u]])
                if(is.na(nd_u)) {
                  #it's missing value in test set
                  rep(0, length(model$levels))
                } else {
                  freq_u <- model$vfreq[[a_u]][nd_u]
                  if(is.na(freq_u)) {
                    #it's missing value in training set
                    lc <- model$laplace / model$laplace * length(model$vfreq[[a_u]])
                    if(is.na(lc)) { lc = 0 }
                    rep(lc, length(model$levels)) #we smoothe here!
                  } else {
                    model$tables2[[a_v]][[a_u]][nd_v,nd_u,]                    
                  }
                }
              }),1,prod)              
          }
        }
      }),1,sum)
    
      #we assign NA if there is zero for all classes
      s <- sum(L)      
      if(s == 0) {
        rep(NA, length(model$levels))
      } else {
        if(type == "class") {
          L
        } else {
          #probabilities normalization
          L/s      
        }
      }        
  })
  
  if(type == "class") { 
    factor(model$levels[apply(L, 2, function(v) { ifelse(is.na(v[1]), NA, which.max(v)) })], levels = model$levels) 
  } else { 
    t(L) 
  }
}

#invocation

#example usage
# a <- data.frame(1:2, 3:4, 5:6)
# names(a) <- c("a1", "a2", "a3")
# b <- as.factor(c(0,1))
# object <- aode(a, b) 
# newdata <- data.frame(c(1,7), c(3,8), c(5,9))
# names(newdata) <- c("a2", "a1", "a3")
# predicted_class <- predict(object, newdata, type = "class")
# predicted_raw <- predict(object, newdata, type = "raw")

#iris usage
set.seed(1235)
sam <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3) )
dataTrain <- iris[sam==1,]
dataTest <- iris[sam==2,]
x <- dataTrain[,-which(names(dataTrain) %in% c("Species"))]
y <- dataTrain$Species
model = aode(x,y)
z <- dataTest[,-which(names(dataTest) %in% c("Species"))]
predicted_class = predict(model, z, type = "class")
predicted_raw = predict(model, z, type = "raw")
comparsion <- table(predicted_class, dataTest$Species)

#spambase usage
# mydata <- readData()
# dataTrain <- mydata$dataTrain
# dataTest <- mydata$dataTest
# x <- dataTrain[,-which(names(dataTrain) %in% c("spam"))]
# y <- dataTrain$spam
# model <- aode(x,y) 
# z <- dataTest[,-which(names(dataTest) %in% c("spam"))]
# predicted_class = predict(model, z, type = "class")
# predicted_raw = predict(model, z, type = "raw")
# comparsion <- table(predicted_class, dataTest$spam)
