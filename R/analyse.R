source("load-trajectories.R")
source("summary.R")
source("characterise.R")
source("plotting.R")
source("info limitation.R")
source("simulate-learning.R")
source("test-conditions-of-mimicry.R")
source("increased deception.R")
source("motion limited discr.R")
source("camera-effects.R")
source("figure.R")


DeleteCache <- function(trajectories = TRUE, morpho = TRUE, autocorrelations = FALSE) {
  if (trajectories && file.exists(CACHED_TRJS))
    file.remove(CACHED_TRJS)
  if (morpho)
    UncacheMorpho()
  autocorrDir <- "../data/.cache"
  if (autocorrelations && dir.exists(autocorrDir))
    unlink(autocorrDir, recursive = TRUE)
}
  
#############################################################################


# By default, everything is cached. Delete ../data/TrajectoryData.rds to redo calculations, 
# but direction autocorrelations are still cached (see GetDirnAutocorrelation() in characterise.R).
# To also recalculate direction autocorrelations, delete the directory ../data/.cache
#DeleteCache(autocorrelations = FALSE, morpho = FALSE)

# Build all trajectories
if (!file.exists(CACHED_TRJS)) {
  # Create trajectories from CSV files, and obtain metadata from database
  trjInfo <- buildTrajectories(file.path(VIDEO_DIR, "video-info.csv"))
  # Numerically characterise them
  trjInfo$stats <- TrajsMergeStats(trjInfo$trjs, characteriseTrajectory)
  # Add body length as another stat
  trjInfo$stats$bodyLength <- trjInfo$metaInfo$bodylength
  # Eliminate trajectories with no ID
  trjInfo <- SubsetTrjInfo(trjInfo, trjInfo$metaInfo$genus != "")
  # Save results in cache file so we don't need to do these steps every time
  saveRDS(trjInfo, CACHED_TRJS)
}

# Read in the previously calculated trajectories and stats from the cache file.
# trjInfo is a list with 3 elements:
# trjs:     a list of trajectories
# metaInfo: information about each trajectory, including specimen info etc
# stats:    calculated statistics for each trajectory
trjInfo <- LoadCachedTrajectories()

# NOTE: I am using laboratory trajectories only in the analysis because:
# 1. It is easier to describe and explain, and
# 2. Results are qualitatively the same as with all trajectories.

labTrjs <- trjInfo
SummariseTrajectories(labTrjs, "Lab")

analysisType <- "quadratic"
retain <- 0.95

# Produce a CSV file describing specimens for supplementary material. Include
# accuracy values as used in multicomponent and motion-limited discrimination
# hypothesis testing, i.e. full information, accuracy calculated using logistic
# regression
specimens <- TrjSpecimenInfo(labTrjs, "logistic")
write.csv(specimens, "../output/trajectory-specimens.csv", row.names = FALSE)
specimens <- MorphoSpecimenInfo("logistic", retain)
write.csv(specimens, "../output/morpho-specimens.csv", row.names = FALSE, na = "")

# SummariseMorphometrics(trjInfo, label = "lab or wild", analysisType = analysisType)
SummariseMorphometrics(labTrjs, label = "lab", analysisType = analysisType)

#### Data figure
JPlotToPNG("../output/data.png", PlotAllData(labTrjs, "individual"), units = "px", width = 900, res = 110)
JPlotToPDF("../output/data.pdf", PlotAllData(labTrjs, "individual"), pointsize = 11)


# Evidence supporting the idea of locomotor ant mimicry?
cat("\n==== Is locomotor ant mimicry a thing? ==================================================\n")
# This can be run non-interactively with more repetitions
TestConditionsForMimicry(labTrjs, analysisType, 999, "../output/test-locomotor-ant-mimicry-quick.png", units = "px", aspect = 0.8, width = 800)
cat("Note that the above tests are just quick versions with 999 repetitions. Run test-conditions-of-mimicry.R (with argument \"run\") from the command line for the full test\n")

# Evidence supporting information limitation?
cat("\n==== Limited information hypothesis ==================================================\n")
TestInformationLimitationForTrj(labTrjs, analysisType = analysisType)
cat("\n")
TestInformationLimitationForMorpho(angle = "Dorsal", analysisType = analysisType, retain = retain)
cat("\n")
TestInformationLimitationForMorpho(angle = "Lateral", analysisType = analysisType, retain = retain)


cat("\n==== Multicomponent hypothesis =======================================================\n")
# Must retain fewer principal components when using quadratic analysis on
# morphology because the no. of components must be < no. of data points, and
# there are fewer ant species than PCs if retain == 0.99
TestMultiComponentHypos(labTrjs, analysisType = "logistic", retain = retain, summaryFileName = "../output/multi-component-data.csv")


cat("\n==== Motion-limited discrimination hypothesis ========================================\n")
TestMotionLimDiscrHypo(labTrjs, analysisType = "logistic", speedMeasure = "speed_mean", retain = retain)
TestMotionLimDiscrHypo(labTrjs, analysisType = "logistic", speedMeasure = "speed_max", retain = retain)
TestMotionLimDiscrHypo(labTrjs, analysisType = "logistic", speedMeasure = "moving_proportion", retain = retain)

cat("\n==== Learning simulation figure ========================================\n")
# WARNING this takes ~20 minutes to run! To test quickly, set quickDbg to TRUE
cat("Skipping because it's toooo slow\n")
set.seed(1)
#CreateLearningSimulationFigure(labTrjs, quickDbg = FALSE)
CreateLearningSimulationFigure(labTrjs, quickDbg = TRUE)


# Generate plots for talk
#BuildTalkPlots(trjInfo)


cat("\n==== Camera effects ==================================================================\n")
CheckForCameraEffects(labTrjs, analysisType = "quadratic", trainOnAll = TRUE)
cat("\n---- Numbers of trajectories by camera and frame rate\n")
ReportCamerasUsed(labTrjs)
