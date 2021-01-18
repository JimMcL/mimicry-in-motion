# Checks for effects of different cameras in results
library(trajr)
source("LDA.R")
source("logit.R")

ReportCamerasUsed <- function(trjList) {
  t <- table(data.frame(camera = trjList$metaInfo$camera, fps = trjList$stats$fps))
  t <- cbind(t, total = rowSums(t))
  t <- rbind(t, total = colSums(t))
  names(dimnames(t)) <- c("Camera", "FPS")
  print(t)
}

# Returns the frequency table of actual mimic types to camera/fps
GetCameraFrequencies <- function(trjList, classFn) {
  t <- data.frame(type = trjList$metaInfo$mimicType, camera = classFn(trjList))
  table(t)  
}

# Tests the question: does the camera and/or frame rate used to record the
# trajectory bias the results? Possibly need to take into account that different
# types of trajectories were recorded on different cameras.
CheckForCameraEffects <- function(trjList, analysisType = "linear", trainOnAll = TRUE, alpha = 0.05) {
  
  .cameraFrameRate <- function(tl) sprintf("%s@%s fps", tl$metaInfo$camera, tl$stats$fps)
  .frameRate <- function(tl) tl$stats$fps
  .camera <- function(tl) tl$metaInfo$camera
  
  .yn <- function(x) ifelse(x, "Yes", "No")

  repBias <- function(classFn, question, reportTable = FALSE, alpha = 0.05, simulate.p.value = TRUE) {
    # Get predictions
    da <- CalcMotionAccuracy(trjList, analysisType, trainOnAll = trainOnAll, crossValidate = trainOnAll)
    
    # Combine predictions with actuals to determine errors, and add camera/frame rate
    tab <- data.frame(error = trjListToLDAClass(trjList) != da$predicted$class, camera = classFn(trjList))
    # Tabulate
    errors <- table(tab$error, tab$camera)
    rownames(errors) <- c("Correct classification", "Misclassifications")

    # Pearson's Chi-squared test to see if errors are correlated to camera/frame rate
    ch <- chisq.test(errors, simulate.p.value = simulate.p.value)

    cat(sprintf("%s %s\n", question, .yn(ch$p.value < alpha)))
    cat(sprintf("%s\n", gsub("\n\t", "", ch$method)))
    cat(sprintf("chi-squared = %g, %s = %g, p-value = %g%s\n", signif(ch$statistic, 2), names(ch$parameter), ch$parameter, signif(ch$p.value, 2),
                ifelse(simulate.p.value, sprintf(" %s is NA because p-value was simulated", names(ch$parameter)), "")))
    if (reportTable)
      print(errors)
  }

  
  repBias(.camera, "Does the camera used to record the trajectory bias the ant/non-ant classification error rate?")
  repBias(.frameRate, "Does the frame rate used to record the trajectory bias the ant/non-ant classification error rate?", reportTable = FALSE)
  repBias(.cameraFrameRate, "Do the camera and frame rate together bias the ant/non-ant classification error rate?", reportTable = TRUE)
}

