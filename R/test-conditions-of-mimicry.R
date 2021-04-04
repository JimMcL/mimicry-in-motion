# Test whether the some of the conditions of locomotor ant mimicry are met
source("load-trajectories.R")
source("characterise.R")
source("LDA.R")
source("logit.R")

TRANSFORM_PARAMS <- TRUE

# Returns the probability that testStatistic belongs to the distribution of
# randomizedStatistics.
# @param assumeNormal Should we assume that randomizedStatistics come from a
#   normal distribution? If FALSE, then the returned p-value has a lower limit
#   of 1/(length(randomizedStatistics) + 1).
CalcPValue <- function(testStatistic, randomizedStatistics, assumeNormal, alternative = c("greater", "less")) {
  
  alternative <- match.arg(alternative)
  
  if (assumeNormal) {
    sd <- sd(randomizedStatistics)
    m <- mean(randomizedStatistics)
    # Determine probability of obtaining real score by chance by calculating the
    # probability that it belongs to the distribution of random scores
    pnorm(testStatistic, m, sd, lower.tail = (alternative == "less"))
  } else {
    # Probability is just the proportion of randomizedStatistics >= the test
    # statistic. Include the test statistic in the proportion, as described by Manly 1997,p. 7
    randomizedStatistics <- c(randomizedStatistics, testStatistic)
    if (alternative == "greater")
      sum(randomizedStatistics >= testStatistic) / length(randomizedStatistics)
    else
      sum(randomizedStatistics <= testStatistic) / length(randomizedStatistics)
  }
}

RandomiseTrjStats <- function(stats) {
  n <- nrow(stats)
  for (c in names(stats)) {
    stats[[c]] <- rnorm(n) + 10
  }
  stats
}

# Use bootstrapping to test whether ants walk differently from non-ants
TestCondition1 <- function(trjList, analysisType = "linear", repetitions = 9999) {
  # If there are distinct non-mimic/ant trajectory types, then randomly changing
  # the mimic types should affect the results
  
  # Since we expect mimics to confuse the groupings, just test on ants and non-mimics
  testTypes <- c(MTP_NON_MIMIC, MTP_MODEL)
  trjs <- SubsetTrjInfo(trjList, trjList$metaInfo$mimicType %in% testTypes)
  missing <- !testTypes %in% trjs$metaInfo$mimicType
  if (any(missing))
    stop(sprintf("There are no %s trajectories in this set", testTypes[missing]))

  #trjs$stats <- RandomiseTrjStats(trjs$stats)
  
  # Use uninformative priors  
  priors <- NULL
  
  # Cross validate since we are testing the effectiveness of assignment to classes
  crossValidate <- TRUE
  
  # Get score for real classes
  da <- CalcMotionAccuracy(trjs, analysisType, priorWeights = priors, crossValidate = crossValidate, transformParams = TRANSFORM_PARAMS)
  realScore <- da$score * 100

  minGroupSize <- sum(PARAMS_INFO$in_discr)
  
  # Now get randomised scores by repeatedly applying random model/non-mimic
  # categories to real trajectories and running DA
  scores <- replicate(repetitions, {
    changed <- trjs
    # Shuffle mimic types, but don't allow group sizes to be too small
    repeat {
      changed$metaInfo$mimicType <- sample(trjs$metaInfo$mimicType, replace = TRUE)
      
      # There are 2 groups in the training set: ants and non-mimics. For the QDA
      # to work, there must be at least as many items in each group as values
      # being analysed
      if (sum(changed$metaInfo$mimicType == MTP_NON_MIMIC) > minGroupSize && 
          sum(changed$metaInfo$mimicType == MTP_MODEL) > minGroupSize)
        break;
    }
    # Calculate accuracy score on randomised trajectories
    CalcMotionAccuracy(changed, analysisType, priorWeights = priors, crossValidate = crossValidate, transformParams = TRANSFORM_PARAMS)$score * 100
  })
  # Note that scores now form a "randomization distribution" (Manly 1997)
  
  list(h1 = "Do ants walk differently from non-ants?",
       "test statistic" = realScore,
       "p-value" = CalcPValue(realScore, scores, TRUE),
       "p-value not normal" = CalcPValue(realScore, scores, FALSE),
       "test alternative" = "greater",
       "randomized statistics" = scores,
       analysisType = analysisType,
       repetitions = repetitions, 
       trajectories = trjs)
}


# Bootstrap test to determine whether mimics are part-way between ants and non-mimics.
# 
# Idea is to train DA on ants and non-mimics, then use that to predict the class of mimics.
# Then mimic and non-mimic labels are shuffled and the process repeated to get a randomised distribution.
TestCondition3 <- function(trjList, analysisType = "linear", repetitions = 9999) {

  trjs <- trjList
  
  # Randomise trajectory characterisations
  mimics <- which(trjs$metaInfo$mimicType %in% MTP_MIMICS)
  
  # Use uninformative priors  
  priors <- NULL
  
  # Train DA on just ants and non-mimics, then score mimic classification accuracy  
  da <- CalcMotionAccuracy(trjs, analysisType = analysisType,  trainOnAll = FALSE, 
                           priorWeights = priors, crossValidate = FALSE, transformParams = TRANSFORM_PARAMS)
  mimicAccuracy <- sum(da$predicted$class[mimics] == "non-ant") / length(mimics) * 100

  # Now get randomised scores by repeatedly applying mimic/non-mimic
  # categories on non-ant trajectories and running DA
  nonAnts <- which(trjs$metaInfo$mimicType != MTP_MODEL)
  minGroupSize <- sum(PARAMS_INFO$in_discr)
  scores <- replicate(repetitions, {
    changed <- trjs
    # Shuffle mimic types, but don't allow group sizes to be too small
    repeat {
      changed$metaInfo$mimicType[nonAnts] <- sample(trjs$metaInfo$mimicType[nonAnts], size = length(nonAnts), replace = TRUE)

      # There are 2 groups in the training set: ants and non-mimics. Ants are
      # unchanging. For the QDA to work, there must be at least as many items in
      # each group as values being analysed
      if (sum(changed$metaInfo$mimicType == MTP_NON_MIMIC) > minGroupSize)
        break;
    }

    # Calculate accuracy score on randomised trajectories
    da <- CalcMotionAccuracy(changed, analysisType = analysisType,  trainOnAll = FALSE, 
                             priorWeights = priors, crossValidate = FALSE, transformParams = TRANSFORM_PARAMS)
    # Just score the "mimics"
    sum(da$predicted$class[mimics] == "non-ant") / length(mimics) * 100
  })
  
  # Convert from accuracy % to ant-like walking.
  # 0 accuracy for mimics means 100% of trajectories are mis-classified as ants, so 100 - accuracy = ant-like
  invisible(list(h1 = "Do mimics walk in a more ant-like manner than non mimics?",
                 "test statistic" = 100 - mimicAccuracy,
                 "p-value" = CalcPValue(mimicAccuracy, scores, TRUE, "less"),
                 "p-value not normal" = CalcPValue(mimicAccuracy, scores, FALSE, "less"),
                 "test alternative" = "less",
                 "randomized statistics" = 100 - scores,
                 analysisType = analysisType,
                 repetitions = repetitions, 
                 trajectories = trjs))
}

##########################################################################################
# For data exploration and debugging

PrintBootstrapResult <- function(result, alpha = 0.05) {
  cat(sprintf("%s %s, value = %g, p-value = %g, %d repetitions\n\tRandomized distribution mean = %g, sd = %g\n", 
              result$h1, ifelse(result$`p-value` < alpha, "Yes", "No"),
              result$`test statistic`, result$`p-value`, result$repetitions,
              mean(result$`randomized statistics`), sd(result$`randomized statistics`)))
}

PlotBootstrapResult <- function(result) {
  rnd <- result$`randomized statistics`
  real <- result$`test statistic`
  plot(density(rnd), xlim = range(c(rnd, real)), xlab = "Ant-like trajectories (%)", lwd = 2, main = result$h1)
  abline(v = real, col = "blue", lwd = 3)
  sd <- sd(rnd)
  m <- mean(rnd)

  abline(v = m, col = rgb(1, .1, .1, 1))
  for (i in 1:8) {
    abline(v = c(m - i * sd, m + i * sd), col = rgb(1, .1, .1, 1 / (2 * i)))
  }
  
  # Plot the assumed distribution of random scores
  xs <- seq(min(c(rnd, real)), max(c(rnd, real)), length.out = 300)
  lines(xs, dnorm(xs, m, sd), col = "#00000040")
  
  proportions <- table(trjListToLDAClass(result$`trajectories`))
  
  
  labels <- c(sprintf("Actual group accuracy (%g%%)", round(real)),
              sprintf("Randomization distribution (mean = %g%%)", round(m)), 
              "Randomization mean +- i*sd")
  l <- legend("topleft", labels, col = c("blue", "black", rgb(1, .1, .1, .8)), lwd = c(3, 2, 1), bg = "#fcfcfc")
}

##########################################################################################

# Performs bootstrap tests for 2 of the conditions of mimicry: 1) Ants walk
# differently from other arthropods (but excluding mimics), and 2) mimics walk
# more like ants than non-mimics.
# 
# Prints results and also plots 2 bootstrap visualisation graphs.
# 
# Warning: this is slow! About 25 minutes to run
TestConditionsForMimicry <- function(trjList, analysisType, repetitions, plotFile = NULL, ...) {
  set.seed(1)
  c1 <- TestCondition1(trjList, repetitions = repetitions, analysisType = analysisType)
  PrintBootstrapResult(c1)
  c3 <- TestCondition3(trjList, repetitions = repetitions, analysisType = analysisType)
  PrintBootstrapResult(c3)

  .doPlot <- function() {  
    par(mfrow = c(2, 1))
    PlotBootstrapResult(c1)
    PlotBootstrapResult(c3)
  }
  if (is.null(plotFile)) {
    .doPlot()
  } else {
    JPlotToFile(plotFile, .doPlot, ...)
  }
  invisible(NULL)
}

# If not interactive, and command line says "run", run it
runFile <- basename(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)))
if (!interactive() && runFile == "test-conditions-of-mimicry.R") {
  JReportToFile("../output/test-locomotor-ant-mimicry.txt", {
    trjInfo <- LoadCachedTrajectories()
    analysisType <- "quadratic"
    TestConditionsForMimicry(trjInfo, analysisType, 99999, "../output/test-locomotor-ant-mimicry.png", units = "px", aspect = 0.8, width = 800)
  })
}


