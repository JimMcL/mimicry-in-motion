# Code to check tracks and smoothing etc on a CSV track file
# See the guts of the function doCheckingStuff
library(trajr)
library(JUtils)

source("VarLine.R")
source("velocity-plot.R")

# Compares frame rates in tracks with what we would expect based on camera model and frame size.
# Information about frame sizes and rates is in ../camera configs.csv.
CheckFrameRate <- function(file) {
  
  points <- read.csv(file, comment.char = '#')
  
  # Photo id from file name
  fileName <- tools::file_path_sans_ext(file)
  id <- basename(fileName)
  # Get camera model
  photoInfo <- SIQueryPhotos(sprintf("id=%s", id))
  
  camera <- photoInfo[photoInfo$id == id, "camera"]

  # Calculate frame rate from points
  fps <- FpsFromPoints(points)
  if (is.na(fps))
    cat(sprintf("Unable to calculate FPS for %s\n", file))
  
  # Read in known camera configurations
  cc <- read.csv(file.path("..", "camera configs.csv"))
  cc <- cc[cc$Camera == camera,]
  if (nrow(cc) < 1)
    cat(sprintf("Unknown camera '%s' for video id %s\n", camera, id))
  
  # Get video image size
  files <- paste0(fileName, c(".mp4", ".mov", ".MOV", ".MP4"))
  imageFile <- Filter(function(f) file.exists(f), files)
  if (length(imageFile) == 0 || !file.exists(imageFile))
    cat(sprintf("Can't find image file for id %s\n", id))
  imageFile <- imageFile[1]
  sz <- system2("exiftool.exe", c("-ImageSize", imageFile), stdout = TRUE)
  sz <- sub(".*: ", "", sz)
  sizes <- as.numeric(strsplit(sz, "x")[[1]])
  
  # Get rows for the camera and the frame size (allow rotation)
  idxs <- (cc$Width == sizes[1] & cc$Height == sizes[2]) | (cc$Width == sizes[2] & cc$Height == sizes[1])
  if (sum(idxs) == 0)
    cat(sprintf("Wrong camera model? video id %s, camera model %s, frame size %dx%d\n", id, camera, sizes[1], sizes[2]))
  cc <- cc[idxs, ]
  
  if (sum(cc$FPS == fps) == 0)
    cat(sprintf("Invalid frame rate for video id %s, camera %s, size %dx%d, was %g, expect %s\n",
                 id, camera, sizes[1], sizes[2], fps, JToSentence(cc$FPS, conjunction = " or ")))
}

CheckAllFrameRates <- function(dir) {
  files <- list.files(dir, pattern = "[0-9]+\\.csv", full.names = TRUE, recursive = TRUE)
  info <- file.info(files)
  cat(sprintf("The following files are empty: %s\n", JToSentence(files[which(info$size == 0)])))
  for (file in files[info$size > 0]) {
    CheckFrameRate(file)
  }
}

# Experimental function to plot all trajectories in points, coloured according
# to some criterion
plotTrjs <- function(filename, points, minLength) {
  xlim <- extendrange(points$x)
  ylim <- extendrange(points$y)
  #realTrjs <- .pickRealTracks(points, colFun)
  realTrjs <- .pickLongTracks(points, minLength)
  plot(NULL, xlim = xlim, ylim = ylim, asp = 1, xlab = "x (m)", ylab = "y (m)", main = filename)
  for (tid in unique(points$TrackId)) {
    trj <- TrajFromCoords(points[points$TrackId == tid, ], "x", "y", "Time")
    isReal <- tid %in% realTrjs
    col <- ifelse(isReal, "red", "blue")
    #col <- ifelse(isReal, "#ff000040", "#0000bb40")
    plot(trj, add = TRUE, start.pt.col = col, col = col)
    if (isReal) {
      text(trj[1, "x"], trj[1, "y"], labels = tid)
      cat(sprintf("%d length %g, %d points\n", tid, TrajLength(trj), nrow(trj)))
    }
  }
}


plotFileTrjs <- function(file, minLength) {
  if (!file.exists(file))
    stop(sprintf("File doesn't exist: %s", file))
  points <- read.csv(file, comment.char = '#')
  # Convert left-handed coord-system to right (or is it the other way around?)
  points$y <- max(points$y, na.rm = TRUE) - points$y
  
  plotTrjs(file, points, minLength)  
}

readYATPoints <- function(file, tid) {
  points <- read.csv(file, comment.char = '#')
  # Convert left-handed coord-system to right (or is it the other way around?)
  points$y <- max(points$y, na.rm = TRUE) - points$y
  if (!missing(tid)){
    points <- .addMissingFrames(points[points$TrackId == tid, ])
  }
  points
}

expTrjPlot <- function(file, tid, smooth = TRUE, add = FALSE, p = 3, n = 101, speedCol = "blue", ...) {
  points <- readYATPoints(file, tid)
  trj <- TrajFromCoords(points, 
                        CSV_STRUCT["x"], CSV_STRUCT["y"], CSV_STRUCT["time"])

  # Smooth before calculating derivatives
  smoothed <- trj
  if (smooth)
    smoothed <- TrajSmoothSG(trj, p, n)
  
  # Calculate speed and acceleration
  derivs <- TrajDerivatives(smoothed)
  
  if (!add) {  
    par(mar = c(5, 4.5, 4, 4.5) + .1)
    # Plot acceleration and speed
    plot(derivs$speed ~ derivs$speedTimes, type = 'l', col = speedCol, 
         xlab = 'Time (s)',
         ylab = expression(paste('Speed (m/s)')),
         ...)
    abline(h = 0, col = 'lightGrey')  
  } else {
    lines(derivs$speed ~ derivs$speedTimes, type = 'l', col = speedCol)
  }
}

######################################################################################

#csvs <- list.files(LAB_DIR, pattern = "[0-9]+.*\\.csv", full.names = TRUE)

#CheckAllFrameRates(VIDEO_DIR)

# Produces a plot that demonstrates a possible negative result 
# of smoothing too much - short stops are lost.
# Inspection of the video showed that the animal was regularly stopping.
produceSmoothingIssuesPlot <- function() {
  csv <- structure(list(Dir = "lab", Video = 4728L, Scale.photo = "4728", 
                        Usable = "", Track.ID = "", Minimum.track.length..m. = NA_real_, 
                        Smoothing = NA_integer_, Notes = "TODO", X = "", file = "../data/videos/lab/4728.csv"), row.names = 308L, class = "data.frame")
  
  .doPlot <- function() {
    xlim <- c(.1, 6)
    expTrjPlot(csv$file, tid, smooth = T, main = csv$file, ylim = c(0, .15), xlim = xlim, lwd = 2)
    expTrjPlot(csv$file, tid, smooth = T, n = 21, add = T, speedCol = "#ff4444")
    legend("topright", inset = c(.01, .01), legend = c("n = 101", "n = 21"), lwd = c(2, 1), col = c("blue", "#ff4444"))
  }
  JPlotToPNG(file.path("..", "output", "smoothing-issues.png"), .doPlot, units = "px", width = 900, height = 600)
}

# Manually run through the lines in this function.
# It is in a function so that I can source the file without running anything
doCheckingStuff <- function () {
  .pickCSV <- function() {
    videoList <- read.csv(file.path(VIDEO_DIR, "video-info.csv"), stringsAsFactors = FALSE)
    firstTODO <- which(videoList$Notes == "TODO")[1]
    row <- videoList[firstTODO, ]
    row$file <- file.path(VIDEO_DIR, row$Dir, paste0(row$Video, ".csv"))
    row
  }
  
  minLength <- .2
  # csv <- .pickCSV()
  csv$Video <- "4117"
  #csv$file <- sprintf("../data/videos/wild/%s.csv", csv$Video)
  csv$file <- sprintf("../data/videos/lab/%s.csv", csv$Video)
  plotFileTrjs(csv$file, minLength)
  tid <- 0
  # Compare different smoothing parameters
  xlim = NULL
  xlim <- c(2, 4)
  expTrjPlot(csv$file, tid, smooth = T, n = 81, main = sprintf("%s, track %d", csv$file, tid), ylim = c(0, .2), xlim = xlim)
  expTrjPlot(csv$file, tid, smooth = T, n = 21, add = T, speedCol = "#ff4444")
  expTrjPlot(csv$file, tid, smooth = T, n = 11, add = T, speedCol = "#ff008888", accCol = NULL)
  expTrjPlot(csv$file, tid, smooth = F, add = T, speedCol = "#00ff8888", accCol = NULL, ylim = c(0, .8))
  browseURL(paste0("http://localhost/photos/", csv$Video))
  
  trjRough <- TrajsBuild(csv$file, 
                         csvStruct = CSV_STRUCT,
                         csvReadFn = .readPoints,
                         minLength = minLength, smoothP = NA, 
                         overrideSpecimen = -1)[[1]]
  trjSmooth <- TrajsBuild(csv$file, 
                          csvStruct = CSV_STRUCT,
                          csvReadFn = .readPoints,
                          minLength = minLength, smoothN = 81, 
                          overrideSpecimen = -1)[[1]]

  plot(trjSmooth, col = "green", lwd = 3)
  plot(trjRough, col = "red", add = TRUE)
  
  #expTrjPlot("../data/videos/wild/4115.csv", 14)
  TrajPlotAtTime(TrajResampleTime(trjRough, .1))
  TrajPlotAtTime(TrajResampleTime(trjSmooth, .1), col = "red")
}

# Plots all of the specified trajectories to files for manual checking
PlotAllTrajs <- function(trjList) {
  for (i in seq_len(length(trjList$trjs))) {
    trj <- trjList$trjs[[i]]
    mi <- trjList$metaInfo[i,]
    JPlotToPNG(sprintf("../output/tocheck/trj_%s_%s.png", attr(trj, "video"), mi$species), {
      par(mfrow = c(2, 1), mar = c(5, 4, 2, 1) + .1)
      plot(trj, main = sprintf("Sp %d, %s, video %d", mi$imageableid, mi$species, mi$id))
      plot(TrajSpeedIntervals(trj, slowerThan = STOPPED_SPEED))
      plot(TrajSpeedIntervals(TrajResampleTime(trj, 1 / 20, newFps = 20), slowerThan = STOPPED_SPEED))
    }, units = "px", width = 720, aspectRatio = .9)
  }
}
