source("resampling methods.R")

###################################################################################
# Increased deception

DrawLMTex <- function(l, side = 1, adj = .99, firstLine = 0, lineInc = 1, ...) {
  sl <- summary(l)
  t <- bquote(F[.(sl$fstatistic[2])][","][.(sl$fstatistic[3])] ~ "=" ~ .(signif(sl$fstatistic, 2)))
  # White background behind text
  #rect(par("usr")[2] - 1.2 * strwidth(t), par("usr")[3] + .01, par("usr")[2] - .01, par("usr")[3] + 4 * strheight(t), col = "white", border = NA)
  line <- firstLine
  mtext(bquote(R^2 ~ "=" ~ .(round(sl$r.squared, 2))), side, line, adj = adj, ...)
  line <- line + lineInc
  mtext(t, side, line, adj = adj, padj = 0.2, ...)
  line <- line + lineInc
  mtext(bquote(p ~ "=" ~ .(signif(sl$coefficients[2,4], 2))), side, line, adj = adj, ...)
}

# For increased deception, need to compare trajectories with morphometrics.
# If lots of specimens have both, we could run a model with species as a random effect,
# otherwise we need to average to species
IncDecEvaluation <- function(trjList) {
  # Only look at mimics
  mimics <- SubsetTrjInfo(trjList, trjList$metaInfo$mimicType %in% MTP_MIMICS)
  # Fill in individual accuracy
  ind <- FillInTrjMorphoAccuracy(mimics, "individuals")$stats
  indEither <- !is.na(ind$accuracyMorphoDorsal) | !is.na(ind$accuracyMorphoLateral)
  indBoth <- !is.na(ind$accuracyMorphoDorsal) & !is.na(ind$accuracyMorphoLateral)
  # Fill in species accuracy
  spec <- FillInTrjMorphoAccuracy(mimics, "species")$stats
  specEither <- !is.na(spec$accuracyMorphoDorsal) | !is.na(spec$accuracyMorphoLateral)
  specBoth <- !is.na(spec$accuracyMorphoDorsal) & !is.na(spec$accuracyMorphoLateral)
  
  tb <- data.frame(Either = c(length(unique(mimics$metaInfo$species[indBoth])), length(unique(mimics$metaInfo$species[specBoth]))),
                   Both = c(length(unique(mimics$metaInfo$species[indEither])), length(unique(mimics$metaInfo$species[specEither]))))
  rownames(tb) <- c("Specimens", "Species")
  cat("Number of mimic species with both trajectories and morphometrics,\n  with either or both dorsal and lateral morphometrics,\n  matched at specimen or species level:\n")
  print(tb)
  cat(sprintf("Number of matched specimens:\n"))
  tb <- data.frame(Either = sum(indEither), Both = sum(indBoth))
  rownames(tb) <- "Specimens"
  print(tb)
  
  # Can we increase statistical power by getting outlines for mimics which already have trajectories?
  cat("Mimic species with trajectories but without outlines:\n")
  missingOutlines <- setdiff(unique(mimics$metaInfo$species), unique(mimics$metaInfo$species[specEither]))
  cat(paste0(paste(missingOutlines, collapse = ", ")), "\n")
}

# Preliminary plot of trajectory vs morpho accuracy
PlotIncreasedDeceptionForTalk <- function(trjList, analysisType = "quadratic", trainOnAll = TRUE, ...) {
  # Get trajectory accuracy
  trjList$stats$accuracyQDA <- CalcMotionDA(trjList, analysisType, trainOnAll = trainOnAll)$accuracy
  # Get morpho accuracy (only have values per species for now)
  trjList <- FillInTrjMorphoAccuracy(trjList)
  withAcc <- SubsetTrjInfo(trjList, !is.na(trjList$stats$accuracyMorphoDorsal) | !is.na(trjList$stats$accuracyMorphoLateral))
  
  # Aggregate accuracy to species, taking means
  speciesStats <- aggregate(withAcc$stats, list(species = withAcc$metaInfo$species), mean)
  # Fill in mimic types - assumes constant mimic type per species which is not strictly accurate
  speciesStats$mimicType <- withAcc$metaInfo$mimicType[match(speciesStats$species, withAcc$metaInfo$species)]
  
  .scatterPlotStats <- function(stats, xStat, yStat, xlab, ylab, ...) {
    data <- stats[, c(xStat, yStat)]
    names(data) <- c("x", "y")
    mt <- stats$mimicType
    plot(y ~ x, data = data, col = typeToCol(mt), pch = typeToPch(mt),
         axes = FALSE, cex = 2, xlab = "", ylab = "", ...)
    title(xlab = xlab, ylab = ylab, cex.lab = 2, line = 1.5)
    legTypes <- unique(mt)
    llwd <- 4
    llty <- 2
    lcol <- "#663344"
    legend("topleft", c(JCapitalise(legTypes), "Linear regression"),
           col = c(typeToCol(legTypes), lcol), 
           pch = c(typeToPch(legTypes), NA),
           lwd = c(0, 0, llwd),
           lty = c(0, 0, llty),
           cex = 1.5, inset = c(.07, -.11), xpd = TRUE)
    l <- lm(y ~ x, data = data)
    abline(l, col = lcol, lwd = llwd, lty = llty)
    #DrawLMTex(l)
    invisible(l)
  }
  
  mimics <- speciesStats$mimicType %in% MTP_MIMICS
  antsNonMim <- speciesStats$mimicType %in% c(MTP_MODEL, MTP_NON_MIMIC)
  par(mar = c(3, 3, 2.8, 0) + .1)
  .scatterPlotStats(speciesStats[mimics, ], "accuracyMorphoDorsal", "accuracyQDA", 
                    xlab = "Ant-like body shape", ylab = "Ant-like trajectory")
}

# Performs tests of multi-component hypotheses
TestMultiComponentHypos <- function(trjList, analysisType = "logistic", retain = 0.99, crossValidate = TRUE, trainOnAll = TRUE, alpha = .05, debuggingPlots = FALSE, summaryFileName = NULL) {
  # Get trajectory accuracy
  trjList$stats$accuracyMotion <- CalcMotionAccuracy(trjList, analysisType, trainOnAll = trainOnAll, crossValidate = crossValidate)$accuracy

  # Get morpho accuracy (only have values per species for now). 
  trjList <- FillInTrjMorphoAccuracy(trjList, analysisType = analysisType, retain = retain, trainOnAll = trainOnAll, crossValidate = crossValidate)
  
  doHumanComparisons <- FALSE
  if (doHumanComparisons) {
    # Get human accuracy scores. Since this doesn't use DA, there's no equivalent to analysisType and trainOnAll
    trjList <- FillInTrjHumanAccuracy(trjList)
    withAcc <- SubsetTrjInfo(trjList, !is.na(trjList$stats$accuracyHuman) | !is.na(trjList$stats$accuracyMorphoDorsal) | !is.na(trjList$stats$accuracyMorphoLateral))
  } else {
    withAcc <- SubsetTrjInfo(trjList, !is.na(trjList$stats$accuracyMorphoDorsal) | !is.na(trjList$stats$accuracyMorphoLateral))
  }
  
  # Aggregate accuracy to species, taking means
  speciesStats <- aggregate(withAcc$stats, list(species = withAcc$metaInfo$species), mean, na.rm = TRUE)
  # Fill in mimic types - assumes constant mimic type per species which is not strictly accurate (i.e. ignores transformational mimicry)
  speciesStats$mimicType <- withAcc$metaInfo$mimicType[match(speciesStats$species, withAcc$metaInfo$species)]
  # Fill in sample sizes
  speciesStats$motionSampleSize <- aggregate(withAcc$stats$speed_mean, list(species = withAcc$metaInfo$species), length)$x
  
  mimics <- speciesStats$mimicType %in% MTP_MIMICS
  mimicStats <- speciesStats[mimics, ]

  .reportCorrelation <- function(xcol, ycol) {
    l <- lm(reformulate(xcol, ycol), data = mimicStats)
    ls <- summary(l)
    p <- ls$coefficients[2,4]
    slope <- coef(l)[2]
    cat(sprintf("Correlation between %s and %s (n = %d):\n\t%s correlated, slope %g, adj r2 %g, F-statistic %g[%d, %d], p %g\n",
                xcol, ycol, sum(!is.na(residuals(l))),
                ifelse(p >= alpha, "not", ifelse(slope > 0, "positively", "negatively")),
                slope, ls$adj.r.squared, 
                ls$fstatistic[1], ls$fstatistic[2], ls$fstatistic[3],
                p))
    corCI <- BootstrapCorCI(mimicStats[, xcol], mimicStats[, ycol])
    cat(sprintf("Correlation coefficient 95%% CI: %s, %g\n", round(corCI[1], 2), round(corCI[2], 2)))
    pu <- RandomisationCorTest(mimicStats[, xcol], mimicStats[, ycol])
    cat(sprintf("P-value upper bound %g\n", pu))
    # Optionally plot residuals to check fit
    if (debuggingPlots) {
      residuals <- FALSE
      if (residuals)
        plot(density(l$residuals))
      else {
        plot(summary(l)$terms, data = mimicStats, pch = 16, col = as.numeric(factor(mimicStats$mimicType)))
        abline(l, lwd =  ifelse(p < alpha, 2, 1), lty =  ifelse(p < alpha, 1, 2), col =  ifelse(p < alpha, "black", "grey"))
      }
    }
  }  
  cat(sprintf("Multi-component hypothesis tests:\n"))
  
  oldPar <- par(mfrow = c(ifelse(doHumanComparisons, 2, 1), 2))
  .reportCorrelation("accuracyMotion", "accuracyMorphoDorsal")
  .reportCorrelation("accuracyMotion", "accuracyMorphoLateral")
  if (doHumanComparisons) {
    .reportCorrelation("accuracyMotion", "accuracyHuman")
    .reportCorrelation("accuracyMorphoDorsal", "accuracyHuman")
  }
  #.reportCorrelation("accuracyMorphoDorsal", "accuracyMorphoLateral")
  par(oldPar)
  
  # Output CSV file reporting data used for this analysis
  if (!is.null(summaryFileName)) {
    cols <- c("species", "accuracyMotion", "motionSampleSize",
              "accuracyMorphoDorsal", "morphoDorsalSampleSize", 
              "accuracyMorphoLateral", "morphoLateralSampleSize", 
              "mimicType")
    write.csv(mimicStats[, cols], summaryFileName, row.names = FALSE)
  }
  invisible(mimicStats)
}
