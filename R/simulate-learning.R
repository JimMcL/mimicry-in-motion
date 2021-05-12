# Simulates a simplistic learning process that a naive predator could undergo.
# The predator is assumed to try to avoid models, but is initially unable to
# identify them.
#
# It samples encountered prey sequentially until it encounters a model, at which
# point it updates its criteria for identifying models, then proceeds to sample
# prey, now avoiding items which it considers to be models. This process
# continues indefinitely.
#
# This code simulates that process, recording the number of mimics, models and
# non-mimics that would be correctly identified after each model encounter. Note
# that due to the use of discriminant analysis to identify models, this
# simulation assumes all prey are attacked until sufficient variation in models
# has been encountered to enable the analysis to work.

library(trajr)
library(foreach, quietly = TRUE)
suppressWarnings(library(doParallel, quietly = TRUE, verbose = FALSE))
library(JUtils)
source("constants.R")
source("load-trajectories.R")
source("mimic-types.R")
source("LDA.R")
source("characterise.R")
source("plotting.R")
source("morpho_fns.R")
source("logit.R")


# Calculates bootstrapped 95% confidence interval of the mean.
# @return vector containing mean, 95% CI lower limit and upper limit
CI95ofMean <- function(v) {
  if (length(v) == 1) {
    return(as.numeric(v, NA, NA))
  }
  
  if (length(v) > 0) {
    b <- boot::boot(as.numeric(v), function(data, idx) mean(data[idx]), R = 1000)
    if (!is.na(b$t0)) {
      ci <- boot::boot.ci(b, type = "norm")
      return(c(mean = ci$t0, ci$normal[2:3]))
    }
  }    
  return(as.numeric(c(NA, NA, NA)))
}

CalcMeanAndVar <- function(v) {
  if (length(v) == 0) {
    return(as.numeric(NA, NA, NA))
  }
  if (length(v) == 1) {
    return(as.numeric(v, NA, NA))
  }
  
  m <- mean(v)
  va <- var(v)
  return(c(mean = m, m - va, m + va))
}

# Given a matrix where rows are mean, lower CI, upper CI, plots the mean with the CI shown as a polygon
PlotMeanAndRegion <- function(m, col, ...) {
  x <- c(1:ncol(m), ncol(m):1)
  y <- c(m[2,], rev(m[3,]))
  polygon(x, y, col = JTransparentColour(col, 64), border = JTransparentColour(col, 90))
}

# Runs a single repetition of the learning simulation
# 
# Returns matrix, rows are results after each encounter, columns are number of mimics, models and non-mimics attacked
.runRep <- function(trjList, mimics, numMimics, models, numModels, nonMimics, numNonMimics, replace, reportNumAttacked, analysisType = "quadratic") {
  
  # Create a matrix to hold the result. Each row is the result after
  # encountering another model, values are counts of correctly identified
  # mimics, models and non-mimics
  result <- matrix(NA, nrow = numMimics + numModels + numNonMimics, ncol = 3)
  
  # Decide on encounter order for this repetition. The specified number of each
  # type of organism will be encountered in a random order
  numEncounters <- numMimics + numModels + numNonMimics
  encounters <- data.frame(
    trjIdx = sample(c(sample(mimics, numMimics, replace = replace),
                      sample(models, numModels, replace = replace),
                      sample(nonMimics, numNonMimics, replace = replace))),
    
    # Initially we assume everything encountered is a suitable prey item, i.e a
    # non-ant. This is updated each time we encounter a new prey
    predictedClasses = rep("non-ant", times = numEncounters),
    
    # Keep a list of what we thought each item was when it was encountered
    historical = rep("non-ant", times = numEncounters),
    
    stringsAsFactors = FALSE
  )
  
  # For discriminant analysis, the the size of the smallest group must be larger
  # than the number of predictor variables, hence we just assume all prey are
  # attacked until we get enough encounters to analyse
  minEncounters <- sum(PARAMS_INFO$in_discr)
  
  # For each encounter...
  for (mi in minEncounters:nrow(encounters)) {
    # Our predator is now encountering the prey with sampleID encounters$trjIdx[mi]

    # Save our decision for later analysis
    encounters$historical[mi] <- encounters$predictedClasses[mi]

    # Do we think this is suitable prey?
    if (encounters$predictedClasses[mi] == "ant") {
      # Just skip this animal since we currently think it's an ant.
      # Copy results from previous encounter since nothing has changed
      result[mi, ] <- result[mi - 1, ]
      next
    }
      
    # Get a list of all encounters up to and including this specimen, but only
    # if we thought they were suitable prey at the time of encounter
    encounterIds <- encounters$trjIdx[which(head(encounters$historical, mi) == "non-ant")]
    
    tryCatch( {
      # Reclassify everything using this set of encounters
      acc <- CalcMotionAccuracy(trjList, analysisType = analysisType, trainOnAll = FALSE, inTrainingSet = encounterIds)
      #acc <- CalcMotionDA(trjList, analysisType = "quadratic", trainOnAll = FALSE, inTrainingSet = encounterIds)
      
      # Update predicted classes based on our current knowledge. Entries in
      # acc$predicted$class corresponds to trjList, so we need to map back to
      # order of encounters
      encounters$predictedClasses <- as.character(acc$predicted$class)[encounters$trjIdx]

      # Save results for plotting. Either record number correct or number that would be attacked
      if (reportNumAttacked) {
        # No. attacked mimics. Non-ants (column 2) are always attacked
        result[mi, 1] <- acc$performance[1, 2] + acc$performance[2, 2]
        # No. attacked models
        result[mi, 2] <- acc$performance[3, 2]
        # No. attacked non-mimics
        result[mi, 3] <- acc$performance[4, 2]
      } else {
        # No. correct mimics. Correct is non-ant (column 2)
        result[mi, 1] <- acc$performance[1, 2] + acc$performance[2, 2]
        # No. correct models. Correct is ant (column 1)
        result[mi, 2] <- acc$performance[3, 1]
        # No. correct non-mimics. Correct is non-ant (column 2)
        result[mi, 3] <- acc$performance[4, 2]
      }
    },
    error = function(e) {
      # After a small number of encounters, we are likely to get errors from the
      # QDA in CalcMotionDA because there is not enough variation within one of
      # the groups. We just ignore the error and it gets treated as an attack,
      # although the results remain NA
      e
    }
    )
  }
  
  result
}

##############################################################################
### Main simulation functions

# Simulates a predator learning, calculating the proportion of correctly
# identified (or else attacked/avoided) mimics, models and non-mimics at each
# step, and plots the means (and 95% CIs) of multiple repetitions.
# 
# This is very slow, so repetitions are run in parallel.
SimulatePredatorLearning <- function(trjList, numMimics = 2, numModels = 49, numNonMimics = 49, 
                                     repetitions = 100, replace = TRUE,
                                     report = c("numCorrect", "numAttacked"),
                                     drawMimicCI = TRUE, drawModelCI = TRUE, drawNonMimicCI = TRUE,
                                     numCores = 8,
                                     analysisType = "quadratic") {
  
  report <- match.arg(report)
  reportNumAttacked <- report == "numAttacked"
  
  mimics <- which(trjList$metaInfo$mimicType %in% MTP_MIMICS)
  models <- which(trjList$metaInfo$mimicType == MTP_MODEL)
  nonMimics <- which(trjList$metaInfo$mimicType == MTP_NON_MIMIC)
  
  # Run the loop in parallel
  cl <- makeCluster(8)
  registerDoParallel(cl)
  
  # For each repetition...
  fns <- c(".runRep", "CalcMotionAccuracy", "CalcMotionDA", "GetAnalysableStats", "PARAMS_INFO", "RemoveNAsFromStats", "TransformParams", "trjListToLDAClass", "typeToLDAClass", "MTP_MODEL")
  results <- foreach(rep = seq_len(repetitions), 
                      .export = fns,
                      .packages = c("trajr", "MASS"),
                      .inorder = FALSE) %dopar% {
      .runRep(trjList, mimics, numMimics, models, numModels, nonMimics, numNonMimics, replace, reportNumAttacked, analysisType = analysisType)
  }
  
  stopCluster(cl)
  print("Repetitions complete")
  
  # Extract results
  mimicResults <- do.call(cbind, lapply(results, function(m) m[, 1]))
  modelResults <- do.call(cbind, lapply(results, function(m) m[, 2]))
  nonMimicResults <- do.call(cbind, lapply(results, function(m) m[, 3]))
  
  mimicsCorrect <- apply(mimicResults, 1, mean, na.rm = TRUE) / length(mimics)
  modelsCorrect <- apply(modelResults, 1, mean, na.rm = TRUE) / length(models)
  nonMimicsCorrect <- apply(nonMimicResults, 1, mean, na.rm = TRUE) / length(nonMimics)
  
  .maybeDrawCI <- function(doIt, results, total, mimicType) {
    # To draw 95% CI
    if (doIt) {
      #ci <- apply(results, 1, function(v) CI95ofMean(na.omit(v) / total))
      ci <- apply(results, 1, function(v) CalcMeanAndVar(na.omit(v) / total))
      PlotMeanAndRegion(as.data.frame(ci), col = typeToCol(mimicType), lty = typeToLty(mimicType), lwd = 2)
    }
  }
  
  plot(NULL, xlim = c(0, numMimics + numModels + numNonMimics), ylim = c(0, 1),
       xlab = "", ylab = "")
       #, xlab = "Encounter", ylab = ifelse(reportNumAttacked, "Proportion attacked", "Proportion correct"))
  .maybeDrawCI(drawMimicCI, mimicResults, length(mimics), MTP_SPIDER)
  .maybeDrawCI(drawModelCI, modelResults, length(models), MTP_MODEL)
  .maybeDrawCI(drawNonMimicCI, nonMimicResults, length(nonMimics), MTP_NON_MIMIC)
  lines(mimicsCorrect, col = typeToCol(MTP_SPIDER), lty = typeToLty(MTP_SPIDER), lwd = 2)
  lines(modelsCorrect, col = typeToCol(MTP_MODEL), lty = typeToLty(MTP_MODEL), lwd = 2)
  lines(nonMimicsCorrect, col = typeToCol(MTP_NON_MIMIC), lty = typeToLty(MTP_NON_MIMIC), lwd = 2)
  
  sprintf("final attack proportions: mimics %.2g%% +-%.2g, ants %.2g%% +-%.2g, non-mimics %.2g%% +-%.2g",
          tail(mimicsCorrect, 1) * 100, sd(mimicResults[, ncol(mimicResults)] / length(mimics), na.rm = TRUE) * 100,
          tail(modelsCorrect, 1) * 100, sd(modelResults[, ncol(modelResults)] / length(models), na.rm = TRUE) * 100,
          tail(nonMimicsCorrect, 1) * 100, sd(nonMimicResults[, ncol(nonMimicResults)] / length(nonMimics), na.rm = TRUE) * 100
          )
}

LearningLegend <- function(...) {
  legend(...,
         c("Mimics", "Ants", "Non-mimics"), 
         lwd = 2,
         col = c(typeToCol(MTP_SPIDER), typeToCol(MTP_MODEL), typeToCol(MTP_NON_MIMIC)),
         lty = c(typeToLty(MTP_SPIDER), typeToLty(MTP_MODEL), typeToLty(MTP_NON_MIMIC)),
         inset = c(.01, .015))
}

PlotLearningSimulation <- function(trjList, totalPrey, repetitions, analysisType) {
  # The 2 scenarios. Non-mimics and models are equally abundant
  percentMimicsAbundant <- 33.3
  percentMimicsRare <- 5
  
  .simulateProportions <- function(percentMimics, ...) {
    numMimics <- round(percentMimics / 100 * totalPrey)
    numModels <- numNonMimics <- round((totalPrey - numMimics) / 2)
    SimulatePredatorLearning(trjList, numMimics, numModels, numNonMimics, report = "numAttacked", repetitions = repetitions, analysisType = analysisType, ...)
  }
  
  par(mfrow = c(1, 2), oma = c(2, 2, 0, 0))
  # Common mimics scenario
  s <- .simulateProportions(percentMimicsAbundant)
  cat(sprintf("Mimics common, %s\n", s))
  mtext("A)", line = -1.5, adj = .05)
  LearningLegend("bottomleft")
  # Rare mimics scenario
  s <- .simulateProportions(percentMimicsRare)
  cat(sprintf("Mimics rare, %s\n", s))
  mtext("B)", line = -1.5, adj = .05)
  
  mtext("Encounter", 1, outer = TRUE)
  mtext("Proportion attacked", 2, outer = TRUE)
}

#################################################################################

# Creates a figure showing 2 simulation scenarios, rare mimics and abundant mimics.
# The simulation is plotted and textual results are written to a file.
CreateLearningSimulationFigure <- function(trjList, quickDbg = FALSE, 
                                   pngFile = ifelse(quickDbg, "../output/learning-quick-and-dirty.png", "../output/learning.png"),
                                   pdfFile = ifelse(quickDbg, FALSE, "../output/learning.pdf"),
                                   txtFile = ifelse(quickDbg, "../output/learning-quick-and-dirty.txt", "../output/learning.txt"),
                                   analysisType = "quadratic") {
  # Define simulation parameters
  startTime <- proc.time()
  
  if (quickDbg) {
    # Small parameters for faster debugging
    totalPrey <- 60
    reps <- 60
  } else {
    # Intended final parameters
    totalPrey <- 300
    reps <- 1000
  }
  
  # PNG file
  JPlotToPNG(pngFile, {
    par(mar = c(2.5, 2.5, 0, 0) + .1)
    JReportToFile(txtFile, PlotLearningSimulation(trjList, totalPrey, reps, analysisType))
  },
  units = "px", width = 1200, height = 500, res = 120)
  
  # Same plot as a PDF file  
  if (!isFALSE(pdfFile)) {
    JPlotToPDF(pdfFile, {
      par(mar = c(2.5, 2.5, 0.5, 0) + .1)
      PlotLearningSimulation(trjList, totalPrey, reps, analysisType)
    },
    pointsize = 10, aspectRatio = 1200 / 500, embedFonts = TRUE)
  }
  
  ShowTime("Learning simulation:", startTime)
}


#########################################################################

# To run from the command line, and this file was run explicitly...
runFile <- basename(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))
if (Sys.getenv("RSTUDIO") != "1" && runFile == "simulate-learning.R") {
  trjInfo <- LoadCachedTrajectories()
  set.seed(1)
  CreateLearningSimulationFigure(trjInfo, quickDbg = FALSE)
}