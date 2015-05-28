naiveBayes <- function(x, ...)
    UseMethod("naiveBayes")

#based on e1071::naiveBayes

naiveBayes.default <- function(x, y, laplace = 1, ...) {
    call <- match.call()
    Yname <- deparse(substitute(y))
    x <- as.data.frame(x)

    ## estimation-function
    est <- function(var) {
      tab <- table(y, var)
      (tab + laplace) / (rowSums(tab) + laplace * nlevels(as.factor(var)))
    }

    ## create tables
    vfreq <- lapply(x, table)
    apriori <- table(y)
    tables <- lapply(x, est)

    ## fix dimname names
    for (i in 1:length(tables)) {
        names(dimnames(tables[[i]])) <- c(Yname, colnames(x)[i])
    }
    names(dimnames(apriori)) <- Yname

    structure(
      list(vfreq = vfreq,
          apriori = apriori,
          tables = tables,
          levels = levels(y),
          call = call
      ),
      class = "naiveBayes"
    )
}

print.naiveBayes <- function(x, ...) {
    cat("\nNaive Bayes Classifier for Discrete Predictors\n\n")
    cat("Call:\n")
    print(x$call)
    
    cat("\nLevels:\n")
    print(x$levels)
    
    cat("\nVariable frequencies:\n")
    print(x$vfreq)
    
    cat("\nClass frequencies:\n")
    print(x$apriori)

    cat("\nConditional probabilities:\n")
    print(x$tables)
}

predict.naiveBayes <- function(object, newdata, type = c("class", "raw"), ...) {
    type <- match.arg(type)
    df <- as.data.frame(newdata)
    attribs <- match(names(object$tables), names(df))

    L <- sapply(1:nrow(newdata), function(i) {
        ndata <- newdata[i, ]
        L <- log(object$apriori) + apply(log(sapply(seq_along(attribs),
            function(v) {
                a <- attribs[v]
                if(is.na(a)) {
                  #attribute is missing in test set
                  rep(1, length(object$apriori))
                } else {
                  nd <- as.character(ndata[[a]])                  
                  if (is.na(nd)) {
                    #it's missing value in test set
                    rep(1, length(object$apriori))
                  } else {
                    freq <- object$vfreq[[v]][nd]
                    if(is.na(freq)) {
                      #it's missing value in training set
                      rep(1, length(object$apriori))
                    } else {                                  
                      object$tables[[v]][, nd]
                    }
                  }
                }
            })), 1, sum)
        
        #we assign NA if there is zero for all classes
        s <- sum(L)
        if(s == 0) {
          rep(NA, length(object$levels))
        } else {
          if (type == "class") {
              L
          } else {
              sapply(L, function(lp) {
                1/sum(exp(L - lp))
              })
          }
        }
    })
    
    if(type == "class") {
      factor(object$levels[apply(L, 2, function(v) { ifelse(is.na(v[1]), NA, which.max(v)) })], levels = object$levels)
    } else {
      t(L)
    }
}
