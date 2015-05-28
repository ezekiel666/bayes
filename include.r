discretizate <- function(data, maxValues = 10) {
  last <- function(v) {
    return (v[length(v)]);
  }

  if (length(unique(data)) > maxValues) {
    valuesCount = length(data);
    levelStep = valuesCount / maxValues;
    sorted = sort(data);

    thresholdsArray = vector(mode="numeric", length=0);

    for (thresholdLevel in lapply(seq(from = levelStep, to = valuesCount - levelStep, by = levelStep), round)) {
      threshold = sorted[thresholdLevel];
      if (!length(thresholdsArray) || threshold > last(thresholdsArray)) {
        thresholdsArray = append(thresholdsArray, threshold);
      }
    }

    getThreshold <- function(val) {
      for (t in 1:length(thresholdsArray)) {
        if (val <= thresholdsArray[t]) {
          return (t);
        }
      }
      return (length(thresholdsArray) + 1);
    }

    data = sapply(data, getThreshold);
  }
  return (data);
}

attributeReduction <- function(data, attributes=0) {
  pc <- prcomp(data[1:(ncol(data)-1)], center = TRUE, scale = FALSE)

  if (attributes < 1) {
    # Find attribute which cummulative variance
    # except with it takes 3 attributes for spam and that's bad
    cum = cumsum(pr$sdev^2/sum(pr$sdev^2));
    attributes = 1;
    for (i in 2:length(cum)) {
      # More then 99,9% of variance
      if (cum[i] > 0.999) {
        attributes = i;
        break;
      }
    }
  }

  reduced = data.frame(pc$x[,1:attributes]);
  reduced$spam <- data$spam;

  return (reduced);
}

#reads spambase data and makes preprocessing
readData <- function(s=0, supervised=FALSE, maxValues = 10, reduce=TRUE, attributes=30) {
  dataset <- read.csv("spambase/spambase.data",header=FALSE,sep=",")
  names <- read.csv("spambase/names",header=FALSE,sep=",")
  names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))

  if (reduce) {
    dataset <- attributeReduction(dataset, attributes);
  }

  if (!supervised) {
    for (i in 1:(length(dataset)-1)) {
      dataset[[i]] = discretizate(dataset[[i]], maxValues);
    }
  } else {
    # Takes much longer but better
    require(discretization)
    datset <- disc.Topdown(dataset,method=2)
  }

  dataset$spam <- as.factor(dataset$spam) # encode spam vector as factor (category)

  if (s > 0) {
    dataset <- dataset[sample(nrow(dataset), s),];
  } else {
    s = nrow(dataset);
  }

  #split 0.7/0.3
  split = sample(2, size = s, replace=TRUE, prob = c(0.7,0.3))
  dataTrain <- dataset[split==1,]
  dataTest  <- dataset[split==2,]
  list(dataTrain = dataTrain, dataTest = dataTest)
}

#computes given function for each pair of matrix column
#(twice: we could optimize it, but it's easier not to for indexing)
#and returns outputs as list
lapply2 <- function(x, fun) {
  list <- list()

  for(i in colnames(x)) {
    for(j in colnames(x)) {
      list[[i]][[j]] <- fun(x[,i],x[,j])
    }
  }
  return(list)
}

#Mode function for factor datatype
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

calculateResults <- function(predictionTable) {
  ff <- predictionTable[1,1];
  tf <- predictionTable[2,1];
  ft <- predictionTable[1,2];
  tt <- predictionTable[2,2];

  result <- data.frame(
  "accuracy" = (tt + ff) / (ff + tf + ft +tt),
  "error" = (tf + ft) / (ff + tf + ft +tt),
  "precision" = (tt) / (ft + tt),
  "sensitivity" = (tt) / (tf + tt),
  "falseNegativeRate" = (tf) / (tf + tt));
  return (result);
}

barplotDataFram <- function(..., names.arg = c()) {
  input_list = list(...);
  if (length(input_list) < 1) {
    return ();
  }

  argsNames = sapply(as.list(substitute(list(...)))[-1L], deparse);
  if (length(names.arg) > 0) {
    for (i in 1:length(names.arg)) {
      argsNames[i] = names.arg[i];
    }
  }

  namesList = names(input_list[[1]]);
  for (i in 2:length(input_list)) {
    if (!identical(namesList, names(input_list[[i]]))) {
      stop("Objects need to have the same attributes");
    }
  }

  sapply(namesList, FUN=function(attribute) {
    values = sapply(input_list, FUN=function(frame) { frame[attribute][[1]]; });
    maxVal = max(values);
    labels = argsNames;
    colors = rep("gray", length(labels));
    for (i in 1:length(labels)) {
      if (values[i] >= maxVal) {
        labels[i] = eval(substitute(expression(bold(A)), list(A=labels[i])));
        colors[i] = "red";
      }
    }

    plot = barplot(height = values,
          names.arg = labels,
          main = attribute,
          col=colors,
          ylim=c(0,1));
    text(plot, 0, round(values, 3),cex=1,pos=3)
  });
  return ();
}
