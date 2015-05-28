source("include.r")
source("aode.r")
source("tan.r")

source("naiveBayes.r")
#require(e1071)
#install.packages("e1071", dep = TRUE)

# Predicted table:
#           \ real 0     1
# predicted
#       0         22     2
#       1          1     8

data = readData(s=200, attributes=5);
dataTrain <- data$dataTrain
dataTest <- data$dataTest

x <- dataTrain[,-which(names(dataTrain) %in% c("spam"))]
y <- dataTrain$spam
z <- dataTest[,-which(names(dataTest) %in% c("spam"))]

modelNaive = naiveBayes(x,y);
predictedClassNaive = predict(modelNaive, z, type = "class");
# For probabilities
#predictedRawNaive = predict(modelNaive, z, type = "raw");
comparsionNaive <- table(predictedClassNaive, dataTest$spam);
resultsNaive <- calculateResults(comparsionNaive);

modelAODE = aode(x,y);
predictedClassAODE = predict(modelAODE, z, type = "class");
# For probabilities
# predictedRawAODE = predict(modelAODE, z, type = "raw");
comparsionAODE <- table(predictedClassAODE, dataTest$spam);
resultsAODE <- calculateResults(comparsionAODE);

modelTAN = tan(dataTrain);
predictedClassTAN = predict(modelTAN, dataTest, type = "class");
# For probabilities
# predictedRawTAN = predict(modelTAN, dataTest, type = "raw");
comparsionTAN <- table(predictedClassTAN, dataTest$spam);
resultsTAN <- calculateResults(comparsionTAN);

plotsNr = length(names(resultsNaive));
if (plotsNr %% 2 != 0) {
  plotsNr = plotsNr + 1;
}
layout(matrix(1:plotsNr, plotsNr/2, 2, byrow = TRUE))
barplotDataFram(resultsAODE, resultsNaive, resultsTAN, names.arg=c("AODE", "Naive", "TAN"))
