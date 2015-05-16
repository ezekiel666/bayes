source("include.r")

require(e1071)
#install.packages("e1071", dep = TRUE)
#bug for character variables

#example usage
set.seed(1235)
sam <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3) )
firis <- iris
firis[] <- lapply(firis, as.factor)
dataTrain <- firis[sam==1,]
dataTest <- firis[sam==2,]

x <- dataTrain[,-which(names(dataTrain) %in% c("Species"))]
y <- dataTrain$Species
model = naiveBayes(x,y)
z <- dataTest[,-which(names(dataTest) %in% c("Species"))]
predicted_class = predict(model, z, type = "class")
predicted_raw = predict(model, z, type = "raw")
comparsion <- table(predicted_class, dataTest$Species)

#spambase usage