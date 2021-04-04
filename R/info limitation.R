source("morpho_fns.R", local = TRUE)
source("LDA.R", local = TRUE)
source("logit.R")


# Converts analysis performance to a single rowed table, with the same structure
# as the table in the paper.
# 
# Either proportion correctly identified or proportion identified as prey to be attacked can be reported.
SummariseDAPerformance <- function(perf, reportCorrect = FALSE) {
  # Combine insect mimics and spider mimics into 1 row, mimics
  perf <- rbind(perf, mimic = colSums(perf[MTP_MIMICS, ]))[c("mimic", "model", "non-mimic"), ]
  # Calculate row totals
  perf <- cbind(perf, total = rowSums(perf))

  .fmtCell <- function(r, corCol) sprintf("%d / %d (%d%%)", perf[r, corCol], perf[r, 3], round(perf[r,corCol] / perf[r,3] * 100))
  if (reportCorrect)
    data.frame(Mimics = .fmtCell(1, 2), Ants = .fmtCell(2, 1), "Non-mimics" = .fmtCell(3, 2), check.names = FALSE)
  else
    data.frame(Mimics = .fmtCell(1, 2), Ants = .fmtCell(2, 2), "Non-mimics" = .fmtCell(3, 2), check.names = FALSE)
}

# Reports chi-sq test results for ant-discrimination based on limited or full information 
LIChiSqTests <- function(mimicType, predictedTypeFull, predictedTypeLimited) {
  # Tabulate performance for testing chi-sq
  fullTab <- table(factor(ThreeWayTypeFromMimicType(mimicType)), predictedTypeFull)
  limTab <- table(factor(ThreeWayTypeFromMimicType(mimicType)), predictedTypeLimited)
  
  ReportChiSq <- function(reportType, errorCol, alpha = 0.05) {
    # Build "observed" grid: 
    # columns are no. of misclassification, no. of correct classifications
    # Rows are full information, limited information
    correctCol <- 3 - errorCol
    observed <- matrix(c(fullTab[reportType, errorCol], fullTab[reportType, correctCol], limTab[reportType, errorCol], limTab[reportType, correctCol]), byrow = TRUE, ncol = 2, dimnames = list(c("Full", "Limited"), c("Error", "Correct")))
    cat("----------------------------------------------\n")
    if (observed[1, 1] == observed[2, 1]) {
      cat(sprintf("Are %s mis-classifications affected by limited information?\n    No, as error rates are equal for full and limited information\n", reportType))
    } else {
      chi <- chisq.test(observed)
      significantlyDifferent <- chi$p.value < alpha
      cat(sprintf("Are %s mis-classifications affected by limited information? %s\n", reportType, ifelse(significantlyDifferent, "Yes", "No")))
      ReportStatTest(chi)
      if (significantlyDifferent) {
        StdResidToStr <- function(resid) {
          # For interpreting standardised residuals, see https://www.statisticshowto.datasciencecentral.com/what-is-a-standardized-residuals/
          ifelse(resid < -4, "very much lower",
                 ifelse(resid < -3, "much lower",
                        ifelse(resid < -2, "lower",
                               ifelse(resid > 4, "very much higher",
                                      ifelse(resid > 3, "much higher",
                                             ifelse(resid > 2, "higher",
                                                    "not significantly different"
                                             ))))))
        }
        cat(sprintf("With limited information, mis-classifications are %s than expected (std residual %g),\n\tcorrect classifications are %s\n",
                    StdResidToStr(chi$stdres[2, 1]), chi$stdres[2, 1], StdResidToStr(chi$stdres[2, 2])))
      }
    }
  }
  
  # Chi-squared test for mimics: grid is correct, incorrect; full info, limited info
  ReportChiSq(TWT_MIMIC, 1)
  ReportChiSq(TWT_ANT, 2)
  ReportChiSq(TWT_NON_ANT, 1)
}

LIPanelPlot <- function(panel, mimicType, accuracy, groups = list(MTP_MODEL, MTP_NON_MIMIC, MTP_MIMICS), jitterAmount = 0.05) {
  cols <- c("black", "black", "black")
  alpha <- 160
  modelCol <- JTransparentColour(typeToCol(MTP_NAMES[1]), 255)
  nonMimicCol <- JTransparentColour(typeToCol(MTP_NAMES[4]), 255)
  # mimicCol <- JTransparentColour(typeToCol(MTP_NAMES[2]), alpha)
  #mimicCol <- JTransparentColour("red", alpha)
  mimicCol <- "red"
  fillCols <- c(modelCol, nonMimicCol, mimicCol)
  fillAngles <- c(-45, 45, 0)
  fillDensities <- 20
  
  if (jitterAmount > 0)
    accuracy <- jitter(accuracy, amount = jitterAmount)
  densities <- lapply(groups, function(tp) density(accuracy[mimicType %in% tp], na.rm = TRUE))
  JPlotDensities(densities, 
                 lineColours = cols,
                 fillColours = fillCols,
                 fillDensities = fillDensities,
                 fillAngles = fillAngles,
                 xlab = "", 
                 ylab = "",
                 axes = FALSE)
  axis(1, at = c(0, 1), labels = c("Least ant-like", "Most ant-like"))
  mtext("Probability density", 2)
  # Panel label
  mtext(panel, 3, -1.1, adj = 0.01)
  legend("topright", c("Ants", "Non-mimics", "Mimics"), 
         pt.cex = 3, fill = fillCols, angle = fillAngles, density = fillDensities,
         cex = 1.3, inset = c(0.01, 0.01), xpd = TRUE)
}


analTypeName <- function(analysisType) {
  if(analysisType == "logistic") {
    "Logistic regression"
  } else if (analysisType == "linear") {
    "Linear discriminant analysis"
  } else {
    "Quadratic discriminant analysis"
  }
}
##############################################################################
# Trajectory functions

# Builds a density graph for the specified groups
ILTrjBuildDensity <- function(trjList, trainOnAll, groups = list(MTP_MODEL, MTP_NON_MIMIC, MTP_MIMICS), analysisType = "quadratic", unskew = TRUE, jitterAmount = 0.05) {
  trjList$stats$accuracyQDAAll <- CalcMotionDA(trjList, analysisType, trainOnAll = trainOnAll, unskew = unskew)$accuracy
  if (jitterAmount > 0)
    trjList$stats$accuracyQDAAll <- jitter(trjList$stats$accuracyQDAAll, amount = jitterAmount)
  lapply(groups, function(tp) density(trjList$stats$accuracyQDAAll[trjList$metaInfo$mimicType %in% tp]))
}

# Builds an animation showing the change in distributions of accuracy based on going from full information to limited information
BuildInfoLimAnimationTrj <- function(dir, trjList, analysisType = "quadratic", unskew = TRUE) {
  
  groups <- list(MTP_MODEL, MTP_NON_MIMIC, MTP_MIMICS)
  # Add noise to spread the values out a bit, otherwise density plot is too tall
  set.seed(5)
  # Full information
  trainOnAll <- TRUE
  densities1 <- ILTrjBuildDensity(trjList, trainOnAll = trainOnAll, groups = groups, analysisType = analysisType, unskew = unskew)

  # Simulate limited information by training on just models and non-mimics
  trainOnAll <- FALSE
  densities2 <- ILTrjBuildDensity(trjList, trainOnAll = trainOnAll, groups = groups, analysisType = analysisType, unskew = unskew)
    
  xlim <- range(sapply(c(densities1, densities2), function(d) range(d$x)))
  ylim <- range(sapply(c(densities1, densities2), function(d) range(d$y)))
  
  ant <- readJPEG("../data/149.jpg")
  spider <- readJPEG("../data/3451.jpg")
  
  lwd <- c(4, 4, 4)
  lty <- c(1, 1, 1)
  #cols <- typeToCol(c(MTP_MODEL, MTP_SPIDER, MTP_NON_MIMIC))
  cols <- c("black", "black", "black")
  alpha <- 160
  modelCol <- JTransparentColour(typeToCol(MTP_NAMES[1]), 255)
  nonMimicCol <- JTransparentColour(typeToCol(MTP_NAMES[4]), 255)
  mimicCol <- JTransparentColour(typeToCol(MTP_NAMES[2]), alpha)
  mimicCol <- JTransparentColour("red", alpha)
  fillCols <- c(modelCol, nonMimicCol, mimicCol)
  typeCounts <- sapply(groups, function(tp) sum(trjList$metaInfo$mimicType %in% tp))
  AnimateDensities(file.path(dir, "densities.gif"), densities1, densities2, 
                   100, loop = 1,
                   cols = cols, 
                   lty = lty, lwd = lwd, 
                   xlim = xlim, ylim = ylim,
                   fillCols = fillCols,
                   #title1 = "Lab trajectories, training set: all",
                   #title2 = "Lab trajectories, training set: ants and non-mimics",
                   typeLabels = c("Ants", "Non-mimics", "Mimics"),
                   typeCounts = typeCounts,
                   title.cex = 2,
                   axes = FALSE,
                   mar = c(2.5, 2.5, 2.5, 0) + .1,
                   legInset = c(0, -0.1),
                   extrasFn = function () {
                     abline(v = 0.5, col = "lightgrey", lwd = 2, lty = 2)
                     JPlotRaster(spider, xlim[1] - .05, 3, 0.4, "left")
                     JPlotRaster(ant, xlim[2] + .05, 3, 0.4, "right")
                   })
}


#BuildInfoLimAnimationTrj("../output/junk", SubsetTrjInfo(trjInfo, lab))


# Tests the prediction of the information limitation hypothesis on trajectories:
# that more mimics should be considered accurate when the model is trained on only 
# ants and non-mimics than when it is trained on all trajectories
# 
# Output is a combination of console text describing the statistical tests and results,
# and a plot to the file called info-limitation-trj.png.
TestInformationLimitationForTrj <- function(trjList, crossValidateLI = TRUE, analysisType = "linear") {

  mimics <- trjList$metaInfo$mimicType %in% MTP_MIMICS
  nonMimics <- trjList$metaInfo$mimicType == MTP_NON_MIMIC
  allButMimics <- SubsetTrjInfo(trjList, trjList$metaInfo$mimicType %in% c(MTP_MODEL, MTP_NON_MIMIC))
  
  # Statistical test. Cross validate to avoid overfitting
  allFull <- CalcMotionAccuracy(analysisType = analysisType, trjList, trainOnAll = TRUE, crossValidate = TRUE)
  
  # No need to cross validate here because mimics aren't part of the training set
  allLimited <- CalcMotionAccuracy(analysisType = analysisType, trjList, trainOnAll = FALSE)

  # Plot the two cases together
  # JPlotToPNG("../output/info-limitation-trj.png", {
  #   par(mfrow = c(1, 2))
  #   par(mar = c(2.2, 2, 2, 0))
  #   LIPanelPlot("(a)", trjList$metaInfo$mimicType, allFull$accuracy, jitterAmount = 0)
  #   LIPanelPlot("(b)", trjList$metaInfo$mimicType, allLimited$accuracy, jitterAmount = 0)
  # }, units = "px", width = 1200, height = 600, res = 120)
  
  # Report error performance
  cat("Trajectory analysis\n___________________\n")
  cat(sprintf("%s proportion classified as prey\n", analTypeName(analysisType)))
  print(cbind(Trait = c("Trajectory", ""), 
              "Trained on" = c("Full", "Limited"),
              rbind(SummariseDAPerformance(allFull$performance),
              SummariseDAPerformance(allLimited$performance))))
  
  # Run chi-square tests
  LIChiSqTests(trjList$metaInfo$mimicType, allFull$predicted$class, allLimited$predicted$class)
}

######################################################################################
# Body shape functions

# Tests the prediction of the information limitation hypothesis on body outlines.
# 
# Output is a combination of console text describing the statistical tests and results,
# and a plot to the file called info-limitation.png.
# 
# @param angle "Dorsal" or "Lateral"
# @param retain Proportion of variation to retain after PCA
TestInformationLimitationForMorpho <- function(angle = c("Dorsal", "Lateral"), retain = .99, analysisType = c("quadratic", "linear"), checkForOverfitting = FALSE) {
  angle <- match.arg(angle)

  # Get precalculated morphometrics
  m <- GetMorphoForPhotos(NULL, angle)
  # We are interested in individuals
  coe <- m$individual$Coe

  # Run discriminant analysis
  allFull <- CalcMorphoAccuracy(coe, trainOnAll = TRUE, crossValidate = TRUE, analysisType = analysisType, randomiseData = checkForOverfitting, retain = retain)
  allLimited <- CalcMorphoAccuracy(coe, trainOnAll = FALSE, crossValidate = FALSE, analysisType = analysisType, randomiseData = checkForOverfitting, retain = retain)
  
  # Plot limited and full information together
  JPlotToPNG(sprintf("../output/info-limitation-morpho-%s.png", tolower(angle)), {
    par(mfrow = c(1, 2))
    par(mar = c(2.2, 2, 2, 0))
    LIPanelPlot("(a)", coe$fac$mimicType, allFull$accuracy)
    LIPanelPlot("(b)", coe$fac$mimicType, allLimited$accuracy)
  }, units = "px", width = 1200, height = 600, res = 120)
  
  
  # Report error performance
  title <- sprintf("Limited info hypothesis: %s body shape analysis", angle)
  cat(sprintf("%s\n%s\n", title, gsub(".", "_", title)))
  cat(sprintf("%s proportion classified as prey:\n", analTypeName(analysisType)))
  print(cbind(Trait = c(paste(angle, "outline"), ""), 
              "Trained on" = c("Full", "Limited"),
              rbind(SummariseDAPerformance(allFull$performance),
                    SummariseDAPerformance(allLimited$performance))))
  
  # Run chi-square tests
  LIChiSqTests(coe$fac$mimicType, allFull$predicted$class, allLimited$predicted$class)
}

