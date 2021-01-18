# Functions for testing the motion limited discrimination hypothesis

TestMotionLimDiscrHypo <- function(trjList, analysisType = "quadratic", speedMeasure = "speed_mean", trainOnAll = TRUE, retain = 0.99, debuggingPlots = FALSE) {
  # Get morpho accuracy (only have values per species for now)
  trjList <- FillInTrjMorphoAccuracy(trjList, analysisType = analysisType, trainOnAll = trainOnAll, retain = retain)
  # Get human accuracy scores. Since this doesn't use DA, there's no equivalent to analysisType and trainOnAll
  # I'm not reporting human results as it adds complexity to the paper without any different results
  #trjList <- FillInTrjHumanAccuracy(trjList)

  # withAcc <- SubsetTrjInfo(trjList, !is.na(trjList$stats$accuracyHuman) | !is.na(trjList$stats$accuracyMorphoDorsal) | !is.na(trjList$stats$accuracyMorphoLateral))
  withAcc <- SubsetTrjInfo(trjList, !is.na(trjList$stats$accuracyMorphoDorsal) | !is.na(trjList$stats$accuracyMorphoLateral))
  
  # Aggregate accuracy to species, taking means
  speciesStats <- aggregate(withAcc$stats, list(species = withAcc$metaInfo$species), mean)
  # Fill in mimic types - assumes constant mimic type per species which may not be strictly accurate
  speciesStats$mimicType <- withAcc$metaInfo$mimicType[match(speciesStats$species, withAcc$metaInfo$species)]
  
  mimics <- speciesStats$mimicType %in% MTP_MIMICS
  mimicStats <- speciesStats[mimics, ]
  
  cat(sprintf("Motion-limited discrimination hypothesis tests:\n"))
  oldPar <- par(mfrow = c(2, 1))
  ReportCorrelation(speedMeasure, "accuracyMorphoDorsal", mimicStats, report95CI = TRUE, plotScatter = debuggingPlots, labels = mimicStats$species)
  ReportCorrelation(speedMeasure, "accuracyMorphoLateral", mimicStats, report95CI = TRUE, plotScatter = debuggingPlots, labels = mimicStats$species)
  #ReportCorrelation(speedMeasure, "accuracyHuman", mimicStats, report95CI = TRUE, plotScatter = debuggingPlots)
  par(oldPar)
  
}