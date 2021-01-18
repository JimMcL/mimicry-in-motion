library(memoise)
library(WaveletComp)


# Velocity that is counted as not moving, in m/sec
# Value 4 mm/sec is used by Shamble et al.
STOPPED_SPEED <- .004
# Step length (in m) for rediscretization, 2mm after Shamble et all
REDISCRETIZE_STEP_LENGTH <- .0002

# List of info describing calculated parameters
# Weird initialisation is so that values can be specified by row rather than column.
# TODO what about stopping_freq?
PARAMS_INFO <- rbind(
  speed_mean              = data.frame(in_discr = TRUE,  transform = "log",  label = "ln mean speed (m/s)", title = "Mean speed while moving"),
  speed_CV                = data.frame(in_discr = TRUE,  transform = "log",  label = "ln coeff. of variation of speed (m/s)", title = "Coeff. of variation of speed"),
  speed_min               = data.frame(in_discr = TRUE,  transform = "",     label = "Minimum speed (m/s)", title = "Minimum speed"),
  speed_max               = data.frame(in_discr = TRUE,  transform = "log",  label = "ln maximum speed (m/s)", title = "Maximum speed"),
  stopped_duration_mean   = data.frame(in_discr = TRUE,  transform = "",     label = "ln mean stopping durations (s)", title = "Mean stopped duration"),
  stopped_duration_CV     = data.frame(in_discr = TRUE,  transform = "",     label = "ln coeff. of variation of stopping durations (s)", title = "Coeff. of variation of stopped duration"),
  moving_duration_mean    = data.frame(in_discr = TRUE,  transform = "",     label = "ln mean moving durations (s)", title = "Mean moving duration"),
  moving_duration_CV      = data.frame(in_discr = TRUE,  transform = "",     label = "ln coeff. of variation of moving durations (s)", title = "Coeff. of variation of moving duration"),
  sinuosity               = data.frame(in_discr = TRUE,  transform = "log",  label = "ln sinuosity", title = "Sinuosity"),
  straightness            = data.frame(in_discr = TRUE,  transform = "",     label = "Straightness", title = "Straightness"),
  first_min_deltaS        = data.frame(in_discr = TRUE,  transform = "",     label = "First min deltaS", title = "First min deltaS"),
  first_min_C             = data.frame(in_discr = TRUE,  transform = "",     label = "First min C", title = "First min C"),
  missing_min             = data.frame(in_discr = TRUE,  transform = "",     label = "No first min", title = "No first min"),
  Emax                    = data.frame(in_discr = TRUE,  transform = "log",  label = "ln Emax", title = "Emax"),
  directional_change_mean = data.frame(in_discr = TRUE,  transform = "log",  label = "ln mean directional change (°/s)", title = "Mean directional change"),
  directional_change_sd   = data.frame(in_discr = TRUE,  transform = "log",  label = "ln standard deviation of directional change (°/s)", title = "Standard deviation of directional change"),
  bodyLength              = data.frame(in_discr = FALSE, transform = "",     label = "Body length (m)", title = "Body length")
)

# Don't call this function! Call GetDirnAutocorrelation instead
# Function to rediscretize a trajectory then calculate the direction
# autocorrelation function for it. The intent is that this function is
# "memoised", meaning that the results are saved the first time it is run, and
# subsequent calls with the same arguments simply return the original result.
# 
# See the memoise package for more info.
.GetDirnAutocorrelation <- function(trj, rediscretizeStepLength, deltaSMax) {
  resampled <- TrajRediscretize(trj, rediscretizeStepLength)
  if (missing(deltaSMax) || is.null(deltaSMax))
    deltaSMax <- round(nrow(resampled) / 4)
  TrajDirectionAutocorrelations(resampled, deltaSMax)
}

# # and the memoised version of the above
GetDirnAutocorrelation <- function(trj, rediscretizeStepLength, deltaSMax = NULL) {
  # Try to come up with a file name which is unique for the trajectory and rediscretization parameters
  dsm <- if(missing(deltaSMax) || is.null(deltaSMax)) "NULL" else as.character(deltaSMax)
  cachedFile <- sprintf("../data/.cache/dirnautocorr_trj%s_n%d_sl%g_dsm%s.csv", attr(trj, "trjID"), nrow(trj), rediscretizeStepLength, dsm)
  # Note we don't attempt to memoize trajectories without a trjId attribute, they are probably simulated trajectories
  if (length(cachedFile) > 0 && file.exists(cachedFile)) {
    # cat(sprintf("Reading from %s\n", cachedFile))
    r <- read.csv(cachedFile)
    class(r) <- c("TrajDirectionAutocorrelations", class(r))
    
  } else {
    r <- .GetDirnAutocorrelation(trj, rediscretizeStepLength, deltaSMax)
    if (length(cachedFile) > 0) {
      if (!dir.exists(dirname(cachedFile)))
        dir.create(dirname(cachedFile))
      write.csv(r, cachedFile, row.names = FALSE)
    }
  }
  r
}

# Returns indices of all local maxima.
#
# To obtain local minima, call .localMaxima(-v).
#
# A maximimum is defined here as a point equal to the greatest value within a
# window. Hence, two or more equal contiguous values will count as maxima if
# there are no higher values within each window.
#
# @param v vector of values.
# @param window Number of points on each side which defines what counts as a
#   local maxima.
# @param startIndex Index of first point which can qualify as a maximum.
# @param endIndex Index of last point which can qualify as a maximum.
.localMaxima <- function(v, window = 1, startIndex = 1, endIndex = length(v))
{
  getWindow <- function(i) {
    # Don't try to look past the ends of the data
    si <- max(1, i - window)
    ei <- min(length(v), i + window)
    v[si : ei]
  }
  
  maxima <- numeric(length(v) / 2)
  nm <- 0
  for (i in startIndex:endIndex) {
    # Is this point a maximum?
    if (v[i] == max(getWindow(i))) {
      nm <- nm + 1
      maxima[nm] <- i
    }
  }
  
  utils::head(maxima, nm)
}

# Select the strongest (most powerful) period which is also a local maximum
# 
# @param w Result of calling \code{WaveletComp}[analyze.wavelet]
# @return The index of the period in the wavelet transform \code{w} that is the most powerful local maximum.
.selectPeakPeriod <- function(w, window = 3) {
  # Get all local maxima
  power <- rowSums(w$Power)
  maxes <- .localMaxima(power, window, startIndex = 2, endIndex = length(power) - 1)
  # Pick the strongest of them
  s <- which.max(power[maxes])
  maxes[s]
}

# Returns a vector of frequencies in speed across whole trajectory, ordered from
# most to least powerful, calculated using FFT. Accordingly, the first element
# is the dominant (i.e. most powerful, largest amplitude) frequency.
# 
# @param numFreqs Number of freuqncies to return, starting from the highest
#   powered frequency.
DominantSpeedFreq <- function(trj, minFreq = NA, maxFreq = NA, frameRate = 1 / mean(diff(trj$time)), numFreqs = 1) {
  spd <- Mod(na.omit(TrajVelocity(trj)))
  pgram <- spectrum(spd, taper = 0, plot = FALSE, fast = FALSE, detrend = FALSE, demean = TRUE)
  # Sort frequencies on power ("spectral density"), highest to lowest
  freqs <- pgram$freq[order(pgram$spec, decreasing = TRUE)]
  # Convert frequency units to Hz
  freqs <- freqs * frameRate
  # Limit to frequencies within our range of interest
  if (!is.na(minFreq))
    freqs <- freqs[freqs >= minFreq]
  if (!is.na(maxFreq))
    freqs <- freqs[freqs <= maxFreq]
  head(freqs, numFreqs)
}

# Returns the frequency of starting/stopping in the trajectory, calculated using Wavelet transform.
#
# @param trj Trajectory to analyse
# @param minPeriod Periods smaller than this will be ignored. This is used to
#   disregard high frequency noise in the trajectory.
# @param maxPeriod Periods larger than this will be ignored. This is used to
#   disregard low frequency noise in the trajectory.
GetStopStartFreq <- function(trj, minPeriod = 1 / 10, maxPeriod = 5) {
  # Determine frame rate from time differences between frames
  frameTime <- mean(diff(trj$time))
  # Calculate speed
  speed <- Mod(TrajVelocity(trj))
  # Remove first and last because they are NA
  speed <- head(tail(speed, -1), -1)
  # Perform a wavelet transform
  x <- data.frame(x = speed)
  # Any cycles faster than about 10 per second are not likely to be biologically meaningful
  w <- analyze.wavelet(x, dt = frameTime, lowerPeriod = minPeriod, upperPeriod = maxPeriod, verbose = F, make.pval = F)
  #wt.image(w)
  # Select the most powerful period
  strongest <- .selectPeakPeriod(w)
  # Convert period to frequency
  c(1 / w$Period[strongest], w$Power.avg[strongest])
}

# Returns Nelson & Card stats relating to bouts.
# A bout is defined by Nelson & Card as "movement uninterrupted by a pause of
# over 5 consecutive frames", when recorded at 25 fps.
NnCStats <- function(trj) {
  
  # Merges selected adjacent moving intervals, returning a new TrajSpeedIntervals
  # @param movingInt TrajSpeedIntervals instance containing movement intervals to be merged
  # @param mergeIdxs Index of first of each pair of intervals to be merged.
  .mergeIntervals <- function(movingInt, mergeIdxs) {
    if (length(mergeIdxs) == 0) {
      # Return original intervals unchanged
      result <- movingInt
    } else {
      # Get list of all start/stop indices
      starts <- seq_len(nrow(movingInt))
      stops <- seq_len(nrow(movingInt))
      
      # Remove unwanted stops/starts, i.e. stops at merge index, starts at merge index + 1
      starts <- starts[-(mergeIdxs + 1)]
      stops <- stops[-mergeIdxs]
      
      # Construct a new set of moving intervals, taking values (other than duration) from the original moving intervals
      result <- data.frame(startFrame = movingInt$startFrame[starts],
                           startTime = movingInt$startTime[starts],
                           stopFrame = movingInt$stopFrame[stops],
                           stopTime = movingInt$stopTime[stops])
      # Recalculate durations
      result$durations <- result$stopTime - result$startTime
      
      # Turn it into a TrajSpeedIntervals, copy attributes
      attr(result, "slowerThan") <- attr(movingInt, "slowerThan")
      attr(result, "fasterThan") <- attr(movingInt, "fasterThan")
      attr(result, "speed") <- attr(movingInt, "speed")
      attr(result, "trajectory") <- attr(movingInt, "trajectory")
      class(result) <- c("TrajSpeedIntervals", class(result))
    }
    result
  }
  
  # Convert moving intervals to "bouts"
  .getBouts <- function(trj) {
    movingInt <- TrajSpeedIntervals(trj, fasterThan = STOPPED_SPEED, diff = "central")
    # Determine which breaks are too short to interrupt a bout
    MIN_STOP_TIME <- .2
    i <- seq_len(nrow(movingInt) - 1)
    shortBreaks <- (movingInt$startTime[i+1] - movingInt$stopTime[i]) < MIN_STOP_TIME
    # Now merge adjacent moving intervals if the break between them is too short
    .mergeIntervals(movingInt, which(shortBreaks))
  }
  
  track_length <- TrajLength(trj)
  track_duration <- trj$time[nrow(trj)] - trj$time[1]
  bouts <- .getBouts(trj)
  c(
    bouts_per_10mm = nrow(bouts) / track_length * 0.10,
    bouts_per_30s = nrow(bouts) / track_duration * 30,
    bout_distance_mean = track_length / nrow(bouts),
    bout_duration_mean = mean(bouts$duration)
  )
}


# Transform the various variables so that they are approximately normally distributed.
# The transforms to apply are defined in params_info
TransformParams <- function(params, params_info) {
  norm <- params
  
  for (pn in rownames(params_info)) {
    pi <- params_info[pn, ]
    if (pi$transform != "")
      # Call the function given its name
      norm[, pn] <- do.call(get(pi$transform), list(norm[, pn]))
  }
  norm
}

RemoveNAsFromStats <- function(stats) {
  stats <- TrajsStatsReplaceNAs(stats, column = "first_min_deltaS", flagColumn = "missing_min")
  stats <- TrajsStatsReplaceNAs(stats, column = "first_min_C")
  stats
}

# Returns a data frame containing just the statistics to be analysed.
# Values are optionally de-skewed according to PARAMS_INFO$transform.
# NA values are optionally removed and a flag column added.
# Any remaining NA values are reported as an error.
#
# @param forDiscrimination If TRUE, only stats to be used in discrimination are returned,
# otherwise all stats are returned.
GetAnalysableStats <- function(trjList, removeNA = FALSE, forDiscrimination = TRUE, applyTransform = TRUE) {
  # Get names of params to use in discriminant analysis
  in_discr <- PARAMS_INFO$in_discr
  param_names <- if (forDiscrimination)
    rownames(PARAMS_INFO)[in_discr]
  else
    rownames(PARAMS_INFO)
  stats <- trjList$stats
  
  if (removeNA) {
    stats <- RemoveNAsFromStats(stats)
    # Check and report if any other columns contain NAs
    sapply(param_names, function(col) if (any(is.na(stats[, col]))) stop(sprintf("Unexpected NAs in column %s", col)))
  } else {
    if (PARAMS_INFO["missing_min", "in_discr"]) {
      # Create the mising_min column
      stats[, "missing_min"] <- as.numeric(is.na(stats$first_min_C))
    }
  }

  if (applyTransform)
    stats <- TransformParams(stats, PARAMS_INFO)

  stats[, param_names]
}

# Calculates some statistics of interest for a single trajectory
characteriseTrajectory <- function(rawTrj) {
  #cat(sprintf("Characterising %s track %s\n", attr(trj, "CSV"), attr(trj, "trackID")))
  
  # 'Special' mean function which returns 0 rather than NA for an empty vector
  .mean <- function(v) { if (length(v) == 0) 0 else mean(v) }
  # 'Special' standard deviation which returns 0 for sd of a single value (or no values)
  .sd <- function(v) ifelse(length(v) < 2, 0, sd(v))
  # 'Special' coefficient of variation which returns 0 if mean is 0
  .CV <- function(v) { mv <- .mean(v); ifelse(mv == 0, 0, .sd(v) / mv) }
  
  # Record fps so we can perform sanity checks - we hope it is independent of other values
  fps <- FpsFromPoints(rawTrj)
  
  # Extract start/stop frequency from unsmoothed trajectory
  strobing_freq <- GetStopStartFreq(rawTrj)
  rawFreq <- DominantSpeedFreq(rawTrj) # EXP TODO
  
  # Subsample to 50 FPS (i.e. the lowest frame rate for any videos) to reduce
  # the effects of recording at different frame rates (and also reduce noise)
  trj <- TrajResampleTime(rawTrj, 1 / 50, newFps = 1 / 50)

  # Now smooth trajectory for all other stats
  trj <- TrajSmoothSG(trj, 4, 101)
  
  post_smooth_strobing_freq <- GetStopStartFreq(trj)
  smoothFreq <- DominantSpeedFreq(trj) # EXP TODO
  
  # Frequency of stopping
  stoppedInt <- TrajSpeedIntervals(trj, slowerThan = STOPPED_SPEED)
  stopping_freq <- 1 / mean(diff(stoppedInt$startTime))
  # Assume frequency > 10 Hz is just noise, not biologically meaningful
  if (is.nan(stopping_freq) || stopping_freq > 10)
    stopping_freq <- NA
  stopped_duration_mean <- .mean(stoppedInt$duration)
  stopped_duration_CV <- .CV(stoppedInt$duration)
  movingInt <- TrajSpeedIntervals(trj, fasterThan = STOPPED_SPEED)
  moving_duration_mean <- .mean(movingInt$duration)
  moving_duration_CV <- .CV(movingInt$duration)
  
  # Measures of speed (reuse derivatives calculated by TrajSpeedIntervals, don't calculate them twice)
  derivs <- attr(stoppedInt, "derivs")
  # Segments when moving
  moving <- derivs$speed[derivs$speed > STOPPED_SPEED]
  speed_mean <- mean(moving)
  speed_min <- min(derivs$speed)
  speed_max <- max(derivs$speed)
  
  # Use coefficient of variation in preference to standard deviation so it's not so closely correlated with mean speed
  speed_CV <- sd(moving) / speed_mean
  
  resampled <- TrajRediscretize(trj, REDISCRETIZE_STEP_LENGTH)
  
  # Measures of straightness
  sinuosity <- TrajSinuosity2(resampled)
  #straightness <- TrajStraightness(trj)
  straightness <- Mod(TrajMeanVectorOfTurningAngles(resampled))
  Emax <- TrajEmax(trj)
  dc <- TrajDirectionalChange(trj)
  directional_change_mean <- mean(dc)
  directional_change_sd <- sd(dc)
  
  # Directional periodicity (rediscretized inside GetDirnAutocorrelation)
  corr <- GetDirnAutocorrelation(trj, REDISCRETIZE_STEP_LENGTH)
  first_min <- TrajDAFindFirstMinimum(corr)
  
  # A few summary values for information purposes
  track_length <- TrajLength(trj)
  track_duration <- trj[nrow(trj), "time"] - trj[1, "time"]
  
  # Stats from Nelson & Card (2015)
  # Resample and smooth for stop/start frequency calculations
  subsampled <- TrajResampleTime(trj, 1 / 25, newFps = 1 / 25)
  rst <- TrajSmoothSG(subsampled, 4, 21)
  ncn <- NnCStats(rst)
  
  # Return a list with all of the statistics for this trajectory
  list(speed_mean = speed_mean,
       speed_CV = speed_CV,
       speed_min = speed_min, 
       speed_max = speed_max,
       stopped_duration_mean = stopped_duration_mean,
       stopped_duration_CV = stopped_duration_CV,
       moving_duration_mean = moving_duration_mean,
       moving_duration_CV = moving_duration_CV,
       sinuosity = sinuosity,
       straightness = straightness,
       Emax = Emax,
       directional_change_mean = directional_change_mean, 
       directional_change_sd = directional_change_sd,
       first_min_deltaS = first_min[1],
       first_min_C = first_min[2],
       stopping_freq = stopping_freq,
       track_length = track_length,
       track_duration = track_duration,
       fps = fps,
       bouts_per_10mm = ncn[1],
       bouts_per_30s = ncn[2],
       bout_distance_mean = ncn[3],
       bout_duration_mean = ncn[4],
       strobing_freq = strobing_freq[1],
       strobing_freq_power = strobing_freq[2],
       strobing_freq_smoothed = post_smooth_strobing_freq[1],
       strobing_freq_power_smoothed = post_smooth_strobing_freq[2],
       raw_freq = rawFreq,
       smooth_freq = smoothFreq
  )
}

# Calculate frame rate from points
# This function assumes a constant frame rate, although some frames may be missing
FpsFromPoints <- function(points) {
  timeCol <- if ("Time" %in% names(points)) "Time" else "time"
  frames <- unique(points[, c("Frame", timeCol)])
  # Time interval between frames. Allow for some errors and missing frames
  interval <- median(diff(frames[, timeCol]) / diff(frames$Frame))
  round(1 / interval)
}

