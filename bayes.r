source("include.r")

require(e1071)
#install.packages("e1071", dep = TRUE)

#http://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R/Classification/Na%C3%AFve_Bayes

a <- matrix(as.factor(1:6), nrow = 2, ncol = 3) #discrete values
colnames(a) <- c("a1", "a2", "a3")
b <- as.factor(c(0,1))

model <- naiveBayes(a, b) 
predicted <- predict(model, a)
predictedraw <- predict(model, a, type = "raw")