# An alternative to LDA for calculating accuracy, logistic regression doesn't
# require the independent variables to be normally distributed

# A function to ignore specific warnings. Adapted from https://stackoverflow.com/a/55182432/1287461
suppressNamedWarnings <- function(warningToIgnore, .expr) {
  eval.parent(substitute(
    withCallingHandlers(.expr, warning = function(w) {
      cm <- conditionMessage(w)
      if (grepl(warningToIgnore, cm)) {
        invokeRestart("muffleWarning")
      }
    })
  ))
}

# # Performs a GLM then predicts the result. Cross validates by running the glm
# # for all rows except 1, then running predict on the 1 missing row, then
# # repeating for all other rows.
# #
# # The predicted type defaults to "link" which means the value is on the scale of
# # the linear predictors, so log odds rather than predicted probabilities.
# # 
# myGlm <- function(formula, df, crossValidate = TRUE,  type = c("link", "response", "terms"), ...) {
#   if (crossValidate) {
#     # For each row in the data set...
#     sapply(seq_len(nrow(df)), function(i) {
#       # Train on all but row i
#       m <- glm(formula, data = df[-i, ], ...)
#       
#       # "Predict" the single row
#       p <- predict(m, newdata = df[i, ], type = type)
#     })
#   } else {
#     m <- glm(formula, data = df, ...)
#     
#     # "Predict" the result
#     p <- predict(m, newdata = df[i, ], type = type)
#   }
#   list(m = m, p = p)
# }

# General function to calculate mimetic accuracy using logistic regression based
# on an arbitrary data set.
# @param data data frame with columns to be used as predictor variables.
# @param mimicType Type of each row in data.
# @param responseType Passed as parameter type to predict. Defaults to "link"
#   which means the value is on the scale of the linear predictors, so log odds
#   rather than predicted probabilities.
# @param warningsToSuppress As I understand it, the specified warnings don't
#   indicate a problem, so ignore them
# 
CalcLogitAccuracy <- function(data, mimicType, trainOnAll, crossValidate,
                              responseType = c("link", "response", "terms"),
                              warningsToSuppress = "algorithm did not converge|fitted probabilities numerically 0 or 1 occurred") {
  
  
  # Which ones are models?  
  data$model <- typeToLDAClass(mimicType)
  
  # There are 3 possibilities: 
  # 1. train on all and cross-validate, 
  # 2. train on all and don't cross-validate, 
  # 3. train on subset and don't cross-validate.
  result <- suppressNamedWarnings(warningsToSuppress, {
    if (trainOnAll) {
      
      # Train and predict on all
      if (crossValidate) {
        # Option 1
        # For each row in the data set...
        m <- NULL
        p <- sapply(seq_len(nrow(data)), function(i) {
          # Train on all but row i
          m <- glm(model ~ ., data = data[-i, ], family = binomial(link = "logit"))
          
          # "Predict" the single row
          predict(m, newdata = data[i, ], type = responseType)
        })
      } else {
        # Option 2
        m <- glm(model ~ ., data = data, family = binomial(link = "logit"))
        
        # "Predict" the result
        p <- predict(m, newdata = data, type = responseType)
      }
    } else {
      if (crossValidate)
        stop("Cannot cross validate logistic regression unless training on all data")
      
      # Option 3
      # Train on ants and non-mimics
      trainingSet <- data[mimicType %in% c(MTP_MODEL, MTP_NON_MIMIC), ]
      
      # Fit model to ants and non-mimics
      m <- glm(model ~ ., data = trainingSet, family = binomial(link = "logit"))
      # Predict all rows
      p <- predict(m, newdata = data, type = responseType)
    }
    
    list(m = m, p = p)
  })  
  
  # Since ants have numeric value 0 and non-ants are 1, and we want accuracy to
  # mean how ant-like animals are, reverse the sense of p
  p <- -result$p
  
  # Construct results similar to CalcMotionDA
  # Table of actual vs predicted
  predictedClass <- ifelse(p >= 0, 'ant', 'non-ant')
  tab <- table(factor(mimicType), predictedClass)
  names(dimnames(tab)) <- c('Actual', 'Predicted')
  # Proportion correct. Taking means works because TRUE is 1 and FALSE is 0.
  score <- mean(data$model == predictedClass, na.rm = TRUE)
  
  list(performance = tab, accuracy = p, predicted = data.frame(class = predictedClass), model = result$m, score = score)
}

# -------------------------------------------------------- #
#### Trajectory functions ####

CalcMotionLogit <- function(trjList, unskew = TRUE, trainOnAll = TRUE, crossValidate = trainOnAll) {
  # Extract the stats of interest, removing NA values
  stats <- GetAnalysableStats(trjList, TRUE, applyTransform = unskew)

  # Run the regression
  CalcLogitAccuracy(stats, trjList$metaInfo$mimicType, trainOnAll = trainOnAll, crossValidate = crossValidate)
}

# Calls the appropriate function based on analysisType; CalcMotionLogit if
# analysisType == "logistic, or CalcMotionDA if analysisType = "linear" or
# "quadratic".
# All additional arguments are passed on to the appropriate function
CalcMotionAccuracy <- function(trjList, 
                               analysisType = c("quadratic", "linear", "logistic"),
                               trainOnAll = TRUE, 
                               inTrainingSet = trjList$metaInfo$mimicType %in% c(MTP_MODEL, MTP_NON_MIMIC),
                               priorWeights = NULL, 
                               classFn = trjListToLDAClass, 
                               transformParams = TRUE,
                               crossValidate = FALSE) {
  analysisType <- match.arg(analysisType)
  if (analysisType == "logistic") {
    CalcMotionLogit(trjList, transformParams, trainOnAll = trainOnAll, crossValidate = crossValidate)
  } else {
    CalcMotionDA(trjList, analysisType = analysisType, trainOnAll = trainOnAll, 
                 inTrainingSet = inTrainingSet, priorWeights = priorWeights, 
                 classFn = classFn, transformParams = transformParams, 
                 crossValidate = crossValidate)
  }
}

# -------------------------------------------------------- #
#### Morpho functions ####


# Calculates a logistic regression on body outlines, discriminating ants from
# non-ants. Starts by performing a PCA and retaining as many components as
# specified by the \code{retain} argument, thereby eliminating constant
# dimensions.
#
# @param coe Momocs Coe object to analyse.
# @param retain Proportion of variation to retain after PCA.
# @param trainOnAll If TRUE, the DA is trained on all specimens. If false, it is
#   only trained on ants and non-mimics.
CalcMorphoLogit <- function(coe, retain = .99, trainOnAll = TRUE, crossValidate = trainOnAll) {
  
  # Start with PCA
  data <- MorphoPCA(coe, retain)
  
  CalcLogitAccuracy(data, coe$fac$mimicType, trainOnAll = trainOnAll, crossValidate = crossValidate)
}

