library("JUtils")
source("LDA.R")
source("logit.R")
source("PCA.R")
source(file.path(SAMPLEIT_DIR, "sampleItMaps.R"))

ReportStatTest <- function(htest, qualifier = "") {
  cat(sprintf("%s%s\n", htest$method, qualifier))
  cat(sprintf("%s = %g, %s = %g, p-value = %f\n", names(htest$statistic), round(htest$statistic, 2), names(htest$parameter), htest$parameter, htest$p.value))
}

PValToSigStr <- function(p) {
  ifelse(p < .001, '***',
         ifelse(p < .01, '**',
                ifelse(p < .05, '*', 
                       ifelse(p < .1, '.', ' ')
                )
         )
  )
}

# Produces estimation plots for differences between means of groups for all of the trajectory statistics 
DabestAllStats <- function(trjList) {
  stats <- GetAnalysableStats(trjList, removeNA = TRUE)
  stats$mimicTypes <- ThreeWayType(trjList)
  ncols <- round(sqrt(ncol(stats)))
  nrows <- ceiling(ncol(stats) / ncols)
  
  # Crap - ggplot ignores mfrow!
  JPlotToPNG("../output/stats/speed_mean.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, speed_mean, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/speed_CV.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, speed_CV, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/speed_min.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, speed_min, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/speed_max.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, speed_max, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/stopped_duration_mean.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, stopped_duration_mean, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/stopped_duration_CV.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, stopped_duration_CV, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/moving_duration_mean.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, moving_duration_mean, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/moving_duration_CV.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, moving_duration_CV, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/sinuosity.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, sinuosity, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/straightness.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, straightness, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/first_min_deltaS.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, first_min_deltaS, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/first_min_C.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, first_min_C, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/Emax.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, Emax, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/directional_change_mean.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, directional_change_mean, c("ant", "mimic", "non-mimic"))))))
  JPlotToPNG("../output/stats/directional_change_sd.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, directional_change_sd, c("ant", "mimic", "non-mimic"))))))
  tryCatch(JPlotToPNG("../output/stats/missing_min.png", print(plot(mean_diff(dabestr::dabest(stats, mimicTypes, missing_min, c("ant", "mimic", "non-mimic")))))))
}

ReportSignificantDifferences <- function(trjList, alpha = 0.05) {
  
  # Plots 95% CI
  .plotTukey <- function(an, stat) {
    t <- TukeyHSD(an, conf.level = 1 - alpha)[[1]]
    par(mar = c(5, 9, 4, 2) + .1)
    title <- sprintf("Tukey multiple comparisons of means\n%d%% family-wise confidence level", round(100 * (1 - alpha)))
    plot(NULL, xlim = range(c(0, t[, 2:3])), ylim = c(0.8, 3.2), xlab = sprintf("%s difference", stat), yaxt = "n", ylab = "", main = title)
    labels <- c("Mimics - Ants", "Non-mimics - Ants", "Non-mimics - Mimics")
    axis(2, 1:3, labels, las = 1)
    abline(v = 0, col = "#994010", lty = 2)
    arrows(t[, 2], 1:3, t[, 3], 1:3, length = .08, angle = 90, code = 3, col = "#2277cc", lwd = ifelse(t[, 4] < alpha, 3, 1))
    points(t[, 1], 1:3, pch = 16, col = "#2277cc", cex = ifelse(t[, 4] < alpha, 1.3, .7))
  }
  stats <- GetAnalysableStats(trjList)
  
  mimicTypes <- ThreeWayType(trjList)

  names <- colnames(stats)
  aovs <- lapply(names, function(col) {
    # Check whether types differ
    aov(stats[,col] ~ factor(mimicTypes))
  })
  pVals <- sapply(aovs, function(a) summary(a)[[1]]$`Pr(>F)`[1])
  # Apply Holms-Bonferroni adjustment to correct for multiple comparisons
  pVals <- p.adjust(pVals)
  if (!any(pVals < alpha, na.rm = TRUE)) {
    print("No significant differences")
  } else {
    # Plot Tukey HSD comparions of means
    for (i in which(pVals < alpha)) {
      n <- names[i]
      JPlotToPNG(sprintf("../output/tukey-%s.png", n),
                 .plotTukey(aovs[[i]], n), res = 100)
    }
  }
}

# Plots all candidate motion statistics in 1 big plot
PlotAllStats <- function(trjList, fileName) {
  stats <- GetAnalysableStats(trjList)
  
  .doIt <- function() {
    oldPars <- par(no.readonly = T)
    on.exit(par(oldPars))
    ncols <- round(sqrt(ncol(stats)))
    nrows <- ceiling(ncol(stats) / ncols)
    par(mfrow = c(nrows, ncols))
    
    .tidyText <- function(t) { JCapitalise(gsub('\\.', ' ', t)) }
    
    mimicTypes <- trjList$metaInfo$mimicType
    
    sapply(colnames(stats), function(col) {
      # Weird use of AsPlottableMimicTypeFactor is to remove unused "insect mimic" level and sort appropriately
      plot(stats[,col] ~ AsPlottableMimicTypeFactor(mimicTypes), main = .tidyText(col), xlab = '', ylab = col)
      
      # For info, check whether all types differ
      an <- anova(lm(stats[,col] ~ factor(mimicTypes)))
      p <- an$`Pr(>F)`[1]
      title(sub = sprintf("All %d differ? p = %g %s\n",
                          length(unique(mimicTypes)),
                          p,
                          PValToSigStr(an$`Pr(>F)`[1])))
      
      #cat(sprintf("%s,%g,%s\n", col, tt$p.value, PValToSigStr(tt$p.value)))
      p
    })
  }
  JPlotToPNG(fileName, .doIt, units = "px", width = 1600, height = 1600, res = 100)
}

PlotLocations <- function(trjList, trajType) {
  JPlotToPNG(sprintf("../output/collection-sites-%s.png", trajType), {
    SIGMapSpecimenSites(trjList$metaInfo, title = sprintf("%s collection sites", trajType), ylimFrac = .2)
  }, units = "px", width = 600, height = 900)
}

PlotAllTracks <- function(trjList, trajType) {
  .plotEverythingForTrack <- function(trj, metaInfo, prefix = c('a', 'b', 'c'), specimenFmt = '%s) Specimen %d, %s') {
    # Plot the track itself
    plot(trj, main = sprintf(specimenFmt, prefix[1], metaInfo$imageableid, metaInfo$scientificName), xlab = 'x (mm)', ylab = 'y (mm)')
    # CalculatePlot autocorrelation
    corr <- GetDirnAutocorrelation(trj, REDISCRETIZE_STEP_LENGTH)
    
    plot(corr, main = paste0(prefix[2], ") Direction autocorrelation"), col = typeToCol(metaInfo$mimicType))
    # Plot speed over time
    ints <- TrajSpeedIntervals(trj, slowerThan = STOPPED_SPEED)
    plot(ints, main = paste0(prefix[3], ') Speed / time'), xlim = c(0, 20))
  }
  
  .plotEverythingForType <- function(type) {
    cat(sprintf("%s\n", type))
    # Subset tracks to those for type
    idx <- which(trjList$metaInfo$mimicType == type)
    idx <- idx[order(trjList$metaInfo$scientificName[idx])]
    trjList <- SubsetTrjInfo(trjList, idx)
    n <- length(idx)
    
    par(mfrow = c(n, 3))
    
    for (i in 1:n) {
      cat(sprintf("%g%%\n", round(100 * i / n)))
      .plotEverythingForTrack(trjList$trjs[[i]], trjList$metaInfo[i, ])
    }
  }
  
  # Break into types
  for (type in unique(trjList$metaInfo$mimicType)) {
    JPlotToPNG(file.path("../output", sprintf("tracks-%s-%s.png", trajType, type)),
               .plotEverythingForType(type),
               units = "px", width = 800, height = sum(trjList$metaInfo$mimicType == type) * 300)
  }
}

GetErrorRatesForContigencyTable <- function(tab, errorCol = c(`mimetic spider` = 1, model = 2, `non-mimic` = 1)) {
  
  errors <- sapply(seq_len(nrow(tab)), function(i) tab[i, errorCol[rownames(tab)[i]]])
  errorRates <- c(errors / rowSums(tab), total = sum(errors) / sum(tab))
  er <- paste0(round(100 * errorRates), "%")
  names(er) <- names(errorRates)
  er
}

.getErrorRates <- function(trjList, type, trainOnAll = TRUE, unskew = TRUE) {
  l <- tryCatch({
    CalcMotionAccuracy(trjList, type, trainOnAll = trainOnAll, transformParams = unskew, crossValidate = TRUE)
  }, error = function(e) { message(e); NULL }
  )
  
  if (is.null(l)) {
    er <- rep("error", length(MTP_NAMES) + 1)
    names(er) <- c(MTP_NAMES, "total")
  } else {
    er <- GetErrorRatesForContigencyTable(l$performance,
                                          errorCol = c(`mimetic insect` = 1, `mimetic spider` = 1, model = 2, `non-mimic` = 1))
  }
  er[c(MTP_NAMES, "total")]
}

SummariseBodyLength <- function(trjList) {
  mi <- trjList$metaInfo
  types <- MTP_NAMES
  df <- data.frame(min = sapply(types, function(type) { min(mi$bodylength[mi$mimicType == type], na.rm = TRUE)}),
                   mean = sapply(types, function(type) { round(mean(mi$bodylength[mi$mimicType == type], na.rm = TRUE), 1) }),
                   max = sapply(types, function(type) { max(mi$bodylength[mi$mimicType == type], na.rm = TRUE)}))
  rownames(df) <- typeToLabel(types)
  df
}

SummariseTrajectories <- function(trjList, typeLabel) {
  tt <- table(trjList$metaInfo$mimicType)
  cat(sprintf("=====\n%s trajectories\nN = %d (%s, = %d mimics)\nfrom %d videos, approximately %d individuals and %d species\n", 
              typeLabel, nrow(trjList$metaInfo), 
              JToSentence(paste(tt, paste0(names(tt), "s"))),
              tt[MTP_INSECT] + tt[MTP_SPIDER],
              length(unique(trjList$metaInfo$id)), 
              length(unique(trjList$metaInfo$imageableid)),
              length(unique(trjList$metaInfo$scientificName))))
  cat("Body lengths:\n")
  print(SummariseBodyLength(trjList))
  dt <- SIRecordedDateTime(trjList$metaInfo)
  cat(sprintf("Collected between %s and %s (%d MRes, %d PhD)\n", min(dt), max(dt), sum(trjList$metaInfo$year == 2017), sum(trjList$metaInfo$year > 2017)))
  types <- unique(trjList$metaInfo$mimicType)
  if (!MTP_MODEL %in% trjList$metaInfo$mimicType)
    cat("Can't run discriminant analysis, as there are no ant trajectories\n")
  else {
    .p <- function(header, vals) {
      cat(sprintf("%18s%9s%15s%15s%10s%11s\n", header, vals[1], vals[2], vals[3], vals[4], vals[5]))
    }

    cat("Discrimination error rates\n")
    .p("", c(MTP_NAMES, "Total"))
    #.p("Linear (skew)", .getErrorRates(trjList, "linear", unskew = FALSE))
    .p("Linear", .getErrorRates(trjList, "linear", unskew = TRUE))
    #.p("Quadratic (skew)", .getErrorRates(trjList, "quadratic", unskew = FALSE))
    .p("Quadratic", .getErrorRates(trjList, "quadratic", unskew = TRUE))
    .p("Logistic", .getErrorRates(trjList, "logistic", unskew = TRUE))
    JPlotToPNG(sprintf("../output/PCA-%s.png", typeLabel), 
               plotMotionPCA(trjList, main = typeLabel, fill = TRUE, cex = 1.4, cex.main = 1.6),
               width = 800, height = 800, units = "px")
    bt <- bartlett.test(as.list(trjList$stats))
    homogenous <- bt$p.value > 0.05
    ReportStatTest(bt)
    cat(sprintf("which indicates variances %s homogenous, so should %suse linear DA\n",ifelse(homogenous, "are", "are not"), ifelse(homogenous, "", "not ")))
  }
  cat("\n")
  
  # Ignore errors
  tryCatch({
    JPlotToPNG(sprintf("../output/acc-%s-all.png", typeLabel), 
               PlotAccuracyDensities(trjList, analysisType = "quadratic", trainOnAll = TRUE, typeLabel),
               units = "px", width = 900, height = 600)
    JPlotToPNG(sprintf("../output/acc-%s-limited.png", typeLabel), 
               PlotAccuracyDensities(trjList, analysisType = "quadratic", trainOnAll = FALSE, typeLabel),
               units = "px", width = 900, height = 600)
  },
    error = function(e) cat(sprintf("Error plotting accuracy for %s: %s\n", typeLabel, e))
  )
  PlotAllStats(trjList, sprintf("../output/chars-%s.png", typeLabel))
  PlotLocations(trjList, typeLabel)
}

