# Script to calculate and save morphometrics analysis of outlines.
# Reads in a set of outline images described in CSV files, and performs geometric morphometric analysis.
# 
# Usage: Rscript cache_morphometrics.R {Dorsal|Lateral|Both}

source("morpho_fns.R", local = TRUE)
source("LDA.R", local = TRUE)
source("logit.R")

RunMorphoAnalysis <- function(angle, analysisType = "quadratic", subsample = NULL) {
  # Get list of outlines
  # 1. from sampleIt
  si <- getSampleItOutlinesList()
  # 2. From other project
  oo <- getOtherOutlinesList()
  
  # Merge datasets
  commonCols <- intersect(names(si), names(oo))
  photos <- rbind(si[, commonCols], oo[, commonCols])
  
  # Report missing outlines
  missing <- !file.exists(photos$file)
  if (sum(missing) > 0) {
    message(sprintf("There are %d missing outline files.\n%s\n", sum(missing), paste(photos$file[missing], collapse = "\n")))
  }
  
  exist <- !missing
  photos <- photos[exist, ]

  ExportAccuracy <- function(coe, level, angle, trainOnAll) {
    l <- CalcMorphoDA(coe, analysisType = analysisType, trainOnAll = trainOnAll)
    x <- cbind(coe$fac, accuracy = l$accuracy)
    trainedOn <- ifelse(trainOnAll, "trainedOnAll", "trainedOnlimited")
    filename <- file.path(OUTLINE_DIR, sprintf("accuracy-%s-%s-%s.csv", level, tolower(angle), trainedOn))
    write.csv(x, filename, row.names = FALSE)
  }
  
  DoAngle <- function(angle) {
    # Do morphometric analysis
    photos <- photos[photos$angle == angle, ]
    if (!is.null(subsample))
      photos <- photos[sample(nrow(photos), subsample), ]
    
    # Perform the actual morphometric analysis, but not accuracy calculation
    # (because accuracy is quick to calculate, so no need to cache)
    m <- GetMorphoForPhotos(photos)
    
    ExportAccuracy(m$individual$Coe, "individuals", angle, trainOnAll = TRUE)
    ExportAccuracy(m$individual$Coe, "individuals", angle, trainOnAll = FALSE)
    ExportAccuracy(m$species$Coe, "species", angle, trainOnAll = TRUE)
    ExportAccuracy(m$species$Coe, "species", angle, trainOnAll = FALSE)
    #ExportAccuracy(m$type$Coe, "types", angle)
  }
  DoAngle(angle)
}


# This script is runnable from the command line.
runFile <- basename(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))
if (Sys.getenv("RSTUDIO") != "1" && runFile == "cache_morphometrics.R") {
  angle <- commandArgs(TRUE)[1]
  if (!angle %in% c("Dorsal", "Lateral", "Both"))
    stop("Usage: {Dorsal|Lateral|Both}")
  if (angle == "Both") {
    RunMorphoAnalysis("Lateral")
    RunMorphoAnalysis("Dorsal")
  } else {
    RunMorphoAnalysis(angle)
  }
  cat("Morpho analysis complete\n")
}

