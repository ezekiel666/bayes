source("include.r")
source("aode.r")

require(e1071)
#install.packages("e1071", dep = TRUE)

data = readData();
dataTrain <- data$dataTrain
dataTest <- data$dataTest

x <- dataTrain[,-which(names(dataTrain) %in% c("spam"))]
y <- dataTrain$spam
model = naiveBayes(x,y)
z <- dataTest[,-which(names(dataTest) %in% c("spam"))]
predicted_class = predict(model, z, type = "class")
predicted_raw = predict(model, z, type = "raw")
comparsionNaive <- table(predicted_class, dataTest$spam)

model = aode(x,y)
z <- dataTest[,-which(names(dataTest) %in% c("spam"))]
predicted_class = predict(model, z, type = "class")
predicted_raw = predict(model, z, type = "raw")
comparsionAODE <- table(predicted_class, dataTest$spam)
