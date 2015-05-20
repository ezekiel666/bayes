source("include.r")
source("aode.r")
source("tan.r")

require(e1071)
#install.packages("e1071", dep = TRUE)

# Predicted table:
#           \ real 0     1
# predicted
#       0         22     2
#       1          1     8

data = readData(10);
dataTrain <- data$dataTrain
dataTest <- data$dataTest

x <- dataTrain[,-which(names(dataTrain) %in% c("spam"))]
y <- dataTrain$spam
z <- dataTest[,-which(names(dataTest) %in% c("spam"))]

modelNaive = naiveBayes(x,y)
predictedClassNaive = predict(modelNaive, z, type = "class")
# For probabilities
#predictedRawNaive = predict(modelNaive, z, type = "raw")
comparsionNaive <- table(predictedClassNaive, dataTest$spam)

modelAODE = aode(x,y)
predictedClassAODE = predict(modelAODE, z, type = "class")
# For probabilities
# predictedRawAODE = predict(modelAODE, z, type = "raw")
comparsionAODE <- table(predictedClassAODE, dataTest$spam)

modelTAN = tan(dataTrain)
predictedClassTAN = predict(modelTAN, dataTest, type = "class")
# For probabilities
# predictedRawTAN = predict(modelTAN, dataTest, type = "raw")
comparsionTAN <- table(predictedClassTAN, dataTest$spam)
