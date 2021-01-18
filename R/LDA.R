library(MASS)


typeToLDAClass <- function(type) {
  as.factor(ifelse(type == MTP_MODEL, 'ant', 'non-ant'))
}

trjListToLDAClass <- function(trjList) typeToLDAClass(trjList$metaInfo$mimicType)


# Calculates an LDA or QDA on trajectories, whereby the DA is used to
# discriminate ants from non-ants.
#
# @param inTrainingSet If trainOnAll is FALSE, specifies the indices of rows to
#   be used as the training set. Defaults to all models and non-mimics.
# @param priorWeights Prior weighting for encountering an ant or a non-ant. Can
#   be used to allow for the relative costs of mis-identifying an ant (by
#   increasing the first value), or a non-ant (increase the 2nd value), or
#   alternatively it could represent the likelihood of encounter of each class
#   of animals.
# @param transformParams If TRUE, Values are de-skewed according to PARAMS_INFO$transform
# @param crossValidate If TRUE, leave-one-out cross validation is performed,
#   which can be used to check for overfitting.
CalcMotionDA <- function(trjList, 
                         analysisType = c("quadratic", "linear"), 
                         trainOnAll = TRUE, 
                         inTrainingSet = trjList$metaInfo$mimicType %in% c(MTP_MODEL, MTP_NON_MIMIC),
                         priorWeights = NULL, 
                         classFn = trjListToLDAClass, 
                         transformParams = TRUE,
                         crossValidate = FALSE) {
  
  analysisType <- match.arg(analysisType)
  if (crossValidate && !trainOnAll)
    stop("If crossValidate == TRUE, trainOnAll must be TRUE")
  
  # Extract the stats of interest, removing NA values
  stats <- GetAnalysableStats(trjList, TRUE, applyTransform = transformParams)

  # Get classes to be discriminated
  stats$class <- classFn(trjList)

  if (trainOnAll) {
    # Train on ants and non-ants. Convert prior weights to probabilities
    trainingSet <- stats
  } else {
    # Train on ants and non-mimics
    trainingSet <- stats[inTrainingSet, ]
  }
  
  # If not specified, make prior weights equal for each class, i.e. uninformative
  if (is.null(priorWeights)) {
    priorWeights <- rep(1, length(unique(stats$class)))
  }
  
  if (analysisType == "quadratic") {
    # Unfortunately, can only use stats with variation in all groups!
    for (stat in names(trainingSet)) {
      nAntVals <- length(unique(trainingSet[trainingSet$class == "ant", stat]))
      nNonAntVals <- length(unique(trainingSet[trainingSet$class == "non-ant", stat]))
      if ((nAntVals == 1 || nNonAntVals == 1) && stat != "class") {
        warning(sprintf("Discarding %s, only a single value for %s\n", stat, if(nAntVals == 1) "ants" else "non-ants"))
        trainingSet[, stat] <- NULL
      }
    }
    r <- qda(class ~ ., data = trainingSet, prior = priorWeights / sum(priorWeights), CV = crossValidate)
  } else {
    r <- lda(class ~ ., data = trainingSet, prior = priorWeights / sum(priorWeights), CV = crossValidate)
  }
  
  if (crossValidate) {
    p <- r
  } else {
    p <- predict(r, newdata = stats)
  }
    
  # Table of actual vs predicted
  tab <- table(factor(trjList$metaInfo$mimicType), p$class)
  names(dimnames(tab)) <- c('Actual', 'Predicted')
  # Proportion correct. Taking means works because TRUE is 1 and FALSE is 0.
  # Note that sometimes qda returns NA values in class & posterior - I don't know why! 
  # This calculation returns the score, ignoring NA values
  score <- mean(stats$class == p$class, na.rm = TRUE)
  
  if (analysisType == "quadratic" || crossValidate) {
    accuracy <- p$posterior[,1]
    list(performance = tab, accuracy = accuracy, da = r, predicted = p, score = score)
  } else {
    # Return accuracy scores for videos. Negate value since we want ants to score higher than non-ants
    accuracy <- p$posterior[,1]
    # Return accuracy scores for videos. Negate value since we want ants to score higher than non-ants
    # I was using ithis but it can't be compared to accuracy as returned by a cross-validated lda
    #accuracy <- -p$x[,1]
    
    # Calculate sd for ants and non-ants
    classCol <- which(names(stats) == "class")
    antsSd <- apply(stats[stats$class == 'ant', -classCol], 2, sd, na.rm = TRUE)
    nonAntsSd <- apply(stats[stats$class == 'non-ant', -classCol], 2, sd, na.rm = TRUE)
    
    # Calculate means and sd for mimics and non-mimics
    mimicsMean <- apply(stats[trjList$metainfo$mimicType == 'mimetic spider', -classCol], 2, mean, na.rm = TRUE)
    mimicsSd <- apply(stats[trjList$metainfo$mimicType == 'mimetic spider', -classCol], 2, sd, na.rm = TRUE)
    nonMimicsMean <- apply(stats[trjList$metainfo$mimicType == 'non-mimic', -classCol], 2, mean, na.rm = TRUE)
    nonMimicsSd <- apply(stats[trjList$metainfo$mimicType == 'non-mimic', -classCol], 2, sd, na.rm = TRUE)
    
    list(performance = tab, accuracy = accuracy, da = r, predicted = p,
         score = score,
         antsSd = antsSd, nonAntsSd = nonAntsSd, 
         mimicsMean = mimicsMean, mimicsSd = mimicsSd, 
         nonMimicsMean = nonMimicsMean, nonMimicsSd = nonMimicsSd)
  }
}

MorphoPCA <- function(coe, retain = .99) {
  # Start with PCA
  p <- PCA(coe)
  # Discard constant dimensions and convert to a list of columns
  compToRetain <- scree_min(p, prop = retain)
  as.data.frame(p$x[, 1:compToRetain])
}

# Calculates an LDA or QDA on morphometric analysis of body outlines, whereby
# the DA is used to discriminate ants from non-ants. Starts by performing a PCA
# and retaining as many components as specified by the \code{retain} argument,
# thereby eliminating constant dimensions.
#
# @param coe Momocs Coe object to analyse.
# @param retain Proportion of variation to retain after PCA.
# @param trainOnAll If TRUE, the DA is trained on all specimens. If false, it is
#   only trained on ants and non-mimics.
# @param priorWeights Prior weighting for encountering an ant or a non-ant. Can
#   be used to allow for the relative costs of mis-identifying an ant (by
#   increasing the first value), or a non-ant (increase the 2nd value), or
#   alternatively it could represent the likelihood of encounter of each class
#   of animals.
CalcMorphoDA <- function(coe, retain = .99, analysisType = c("quadratic", "linear"), trainOnAll = TRUE, crossValidate = trainOnAll, priorWeights = NULL, randomiseData = FALSE) {
  
  analysisType <- match.arg(analysisType)
  
  # Start with PCA
  data <- MorphoPCA(coe, retain)
  
  # # Bartlett test
  # bt <- bartlett.test(as.list(data))
  # print(bt)
  
  # DBG create random data to test for overfitting
  if (randomiseData) {
    .rndV <- function(v) rnorm(length(v), mean(v), sd(v))
    data <- as.data.frame(apply(data, 2, FUN = .rndV))
  }
  
  # Get class for discriminant analysis, i.e. ant or not ant
  data$class <- typeToLDAClass(coe$fac$mimicType)
  
  if (trainOnAll) {
    # Train on ants and non-ants
    trainingSet <- data
  } else {
    # Train on ants and non-mimics
    inTrainingSet <- coe$fac$mimicType %in% c(MTP_MODEL, MTP_NON_MIMIC)
    trainingSet <- data[inTrainingSet, ]
  }
  
  # If not specified, make prior weights equal for each class
  if (is.null(priorWeights)) {
    priorWeights <- rep(1, length(unique(data$class)))
  }
  
  if (analysisType == "quadratic") {
    r <- qda(class ~ ., data = trainingSet, prior = priorWeights / sum(priorWeights), crossValidate = crossValidate)
  } else {
    r <- lda(class ~ ., data = trainingSet, prior = priorWeights / sum(priorWeights), crossValidate = crossValidate)
  }
  
  # "Predict" the entire dataset
  p <- predict(r, newdata = data, type = "response")
  
  # Table of actual vs predicted
  tab <- table(factor(coe$fac$mimicType), p$class)
  names(dimnames(tab)) <- c('Actual', 'Predicted')
  
  if (analysisType == "quadratic") {
    accuracy <- p$posterior[,1]
    list(performance = tab, accuracy = accuracy, da = r, predicted = p)
  } else {
    # Return accuracy scores for shapes. Negate value since we want ants to score higher than non-ants
    # accuracy <- p$posterior[,1]
    accuracy <- -p$x[,1]
    
    list(performance = tab, accuracy = accuracy, da = r, predicted = p)
  }
}

