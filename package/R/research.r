# Predicted table:
#           \ real 0     1
# predicted
#       0         22     2
#       1          1     8

performResearch <- function() {
    resultsNaive <- NULL;
    resultsAODE <- NULL;
    resultsTAN <- NULL;
    dataList = readData(attributes=30);
    for (data in dataList) {
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
      resultsNaive <- addCalculatedResults(comparsionNaive, resultsNaive);
    
      modelAODE = aode(x,y);
      predictedClassAODE = predict(modelAODE, z, type = "class");
      # For probabilities
      # predictedRawAODE = predict(modelAODE, z, type = "raw");
      comparsionAODE <- table(predictedClassAODE, dataTest$spam);
      resultsAODE <- addCalculatedResults(comparsionAODE, resultsAODE);
    
      modelTAN = tan(dataTrain);
      predictedClassTAN = predict(modelTAN, dataTest, type = "class");
      # For probabilities
      # predictedRawTAN = predict(modelTAN, dataTest, type = "raw");
      comparsionTAN <- table(predictedClassTAN, dataTest$spam);
      resultsTAN <- addCalculatedResults(comparsionTAN, resultsTAN);
    }
    
    for (att in names(resultsNaive)) {
      resultsNaive[att] <- resultsNaive[att] / length(dataList);
      resultsAODE[att] <- resultsAODE[att] / length(dataList);
      resultsTAN[att] <- resultsTAN[att] / length(dataList);
    }
    
    plotsNr = length(names(resultsNaive));
    if (plotsNr %% 2 != 0) {
      plotsNr = plotsNr + 1;
    }
    layout(matrix(1:plotsNr, plotsNr/2, 2, byrow = TRUE))
    barplotDataFrame(resultsAODE, resultsNaive, resultsTAN, names.arg=c("AODE", "Naive", "TAN"))
}
