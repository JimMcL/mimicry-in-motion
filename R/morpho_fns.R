# Functions to perform outline morphometric analysis on body outline images 
# Input is a set of photos and meta data, output is a fourier characterisation of
# photos, individuals, species and mimic types.
# These calculations are slow, so results are saved and reused 
# if no photos have changed since the last calculation.

suppressMessages(library(Momocs))
source("constants.R", local = TRUE)
source("mimic-types.R", local = TRUE)

# Loads a set of photos, converts them to outlines, subsamples and smooths them.
#
# photos - data frame which specifies the set of photos to be loaded. 
#          Must have a "file" column. Used as the "fac" for the Out object.
# sampleSize - each outline is subsampled to this number of points
# 
# value: momocs Out object
.MALoadPhotos <- function(photos, sampleSize = 1600) {
  # Convert images to outlines
  coords <- import_jpg(as.character(photos$file))
  
  # Subsample or interpolate points to a standard number of points.
  coords <- lapply(coords, function(m) {
    if (nrow(m) < sampleSize) {
      coo_interpolate(m, sampleSize)
    } else {
      coo_sample(m, sampleSize)
    }
  })
  
  o <- Out(coords, fac = photos)
  
  # Close the outline and smooth
  coo_close(o) %>% coo_smooth(5)
}

# Performs a morphometric analysis on a set of photos
#
# @param photos - data frame with a row for each photo to be processed. Columns:
#   file - location of jpg file
#   specimenId - unique ID of specimen
#   species - scientific name of species
#   mimicType - one of MTP_NAMES
#   bodyLength - optional
#
# @returns list with elements "photo", "individual", "species", "type". photo
#   is a Momocs Coe object, remaining elements are lists with members "Coe" and
#   "shp". 
MorphoAnalysisForPhotos <- function(photos, startTime = NULL) {
  
  # specimenId and speciedId must be a factors with no unused levels
  photos$specimenId <- droplevels(as.factor(photos$specimenId))
  photos$species <- droplevels(as.factor(photos$species))
  photos$mimicType <- droplevels(as.factor(photos$mimicType))
  
  .st <- function(msg) {
    ShowTime(msg, startTime)
  }
  # .st <- function(...) {}
  
  # Load photos and convert to subsampled, smoothed outlines
  outlines <- .MALoadPhotos(photos)
  .st(sprintf("Loaded %d outlines in", length(outlines)))
  
  # Align them
  pt <- proc.time()
  aligned <- fgProcrustes(outlines, coo = TRUE)
  .st(sprintf("Procrustes alignment (%g secs/shape):", round((proc.time() - pt)[3] / length(aligned$coo), 1)))
  
  minCoords <- min(sapply(outlines$coo, length)) / 2
  # Run the elliptical fourier analysis
  fr <- efourier(aligned, norm = T, nb.h = minCoords %/% 2)
  .st("Fourier analysis")
  # Average multiple photo outlines to individual outlines.
  # Note that specimen id is called imageableid
  individual <- mshapes(fr, 'specimenId')
  # Remove outlineId since there may be multiple outlines contributing to an individual shape
  individual$Coe$fac$outlineId <- NULL
  # Average individuals to species
  species <- mshapes(individual$Coe, 'species')
  # Remove columns that don't make sense for a species
  species$Coe$fac$specimenId <- NULL
  species$Coe$fac$file <- NULL
  # Average species to mimic types
  types <- mshapes(species$Coe, 'mimicType')
  # Remove columns that don't make sense for a species
  types$Coe$fac$species <- NULL
  .st("Shape averaging")
  
  # Fix up mean values in averaged groups, i.e. take mean instead of first
  # value. mshapes just takes the value from the first row of a group
  .agg <- function(specific, group, idCol, valueCol = 'bodylength', fun = mean, na.rm = TRUE) {
    r <- c()
    for(i in 1:nrow(group)) {
      r[i] <- fun(specific[specific[,idCol] == group[i,idCol], valueCol], na.rm = na.rm)
    }
    r
  }
  if ("bodylength" %in% names(individual$Coe$fac)) {
    species$Coe$fac$bodylength <- .agg(individual$Coe$fac, species$Coe$fac, 'species', 'bodylength')
    types$Coe$fac$bodylength <- .agg(species$Coe$fac, types$Coe$fac, 'mimicType', 'bodylength')
  }
  
  list(photo = fr, individual = individual, species = species, type = types)
}

# Print elapsed time. Silent noop if startTime is null.
#
# startTime <- proc.time()
# ...
# ShowTime("Long process:", startTime)
# 
# value - elapsed seconds
ShowTime <- function(msg, startTime) {
  if (!is.null(startTime)) {
    elapsedSecs <- (proc.time() - startTime)[3]
    elapsed <- elapsedSecs
    if (elapsed >= 3600) {
      elapsed <- elapsed / 3600
      units <- "hours"
    } else if (elapsed >= 60) {
      elapsed <- elapsed / 60
      units <- "mins"
    } else {
      units <- "secs"
    }
    cat(sprintf(paste(msg, "%g %s\n"), round(elapsed, 2), units))
    invisible(elapsedSecs)
  }
}

.getMorphoCacheFileName <- function(angle = c("Dorsal", "Lateral")) {
  angle <- match.arg(angle)
  file.path(OUTLINE_DIR, paste0("morpho-", angle, '.rds'))
}

# Reads in a precalculated morphometric analysis, checks if it matches the
# photos, and if not, recalculates it. All photos are assumed to have been taken
# from the same angle (photos$angle). If photos are not specified, precalculated
# results are unconditionally returned (and angle must be specified).
# 
# To obtain pre-calculated morphometric analysis for Dorsal outlines, call 
# \code{GetMorphoForPhotos(NULL, "Dorsal")}
#
# @para photos Data frame describing the photos to be analysed. Must include the
#   columns required by MorphoAnalysisForPhotos, plus an outlineId column. If NULL,
#   the cached analysis is returned (angle must be specified in this case).
# @param angle Viewing angle of photos to be analysed.
# @see MorphoAnalysisForPhotos for return value
GetMorphoForPhotos <- function(photos, angle = photos[1,]$angle, verbose = TRUE, force = FALSE) {
  
  # Try to read in the result of the last analysis
  analFile <- .getMorphoCacheFileName(angle)
  result <- tryCatch(readRDS(analFile), 
                     error = function(c) NULL,
                     warning = function(c) NULL)
  
  # Did we manage to read in an up-to-date file? Assume photos can't change unless their ids change
  #cat(sprintf("Null result? %s, null photos? %s, idsets equal? %s, verbose? %s\n", is.null(result), is.null(photos), setequal(result$photo$id, photos$id), verbose))
  if (force || is.null(result) || (!is.null(photos) && !setequal(result$photo$outlineId, photos$outlineId))) {
    if (is.null(photos)) {
      stop("Looks like you tried to use cached morphometrics which don't exist! Run the script cache_morphometrics.R")
    }
    if (verbose) {
      reason <- if (force) "recalculation requested" else if (is.null(result)) "no cached data" else "requested outline ids have changed"
      cat(sprintf("Recalculating morphometrics for %s photos, %s\n", angle, reason))
    }
    # Something has changed. Perform morphometric analysis on all the photos
    startTime <- if(verbose) proc.time() else NULL
    
    result <- MorphoAnalysisForPhotos(photos, startTime)
    saveRDS(result, analFile)
    
    ShowTime("Total calculation time", startTime)
  }
  
  result
}

UncacheMorpho <- function() {
  fn <- .getMorphoCacheFileName("Lateral")
  if (file.exists(fn))
      file.remove(fn)
  fn <- .getMorphoCacheFileName("Dorsal")
  if (file.exists(fn))
      file.remove(fn)
}

MpPlotAccuracyDensities <- function(type, accuracy, subset = NULL, showSpeciesCount = TRUE, legendPos = 'topright', legendInsetFn = JInset, extrasFn = NULL, xlabFn = MLabelAccuracyAxis, mar = c(3, 2, 0.2, 0.2), ...) {
  
  if (!is.null(subset)) {
    type <- type[subset]
    accuracy <- accuracy[subset]
  }
  
  types <- unique(type)
  densities <- lapply(types, function(tp) density(accuracy[type == tp], na.rm = TRUE))
  names(densities) <- types
  
  .plotDensityVert <- function(density, x, ...) {
    .yForX <- function(x) { density$y[which.min(abs(density$x - x))] }
    segments(x, 0, x, .yForX(x), ...)
  }
  .plotTypeLines <- function(tp) {
    a <- accuracy[type == tp]
    d <- densities[[tp]]
    col = typeToCol(tp)
    m <- mean(a, na.rm = TRUE)
    sda <- sd(a, na.rm = TRUE)
    .plotDensityVert(d, m, col = col)
    .plotDensityVert(d, m + sda, lty = 3, col = col)
    .plotDensityVert(d, m - sda, lty = 3, col = col)
  }
  
  #JSetParsTemporarily(mar = mar)
  JPlotDensities(densities, col = typeToCol(types), lty = typeToLty(types), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ...)
  title(ylab = 'Probability Density', line = .5)
  xlabFn(axis = 'x')
  
  sapply(names(densities), .plotTypeLines)
  if (!is.null(extrasFn))
    extrasFn(densities)
  
  ltypes <- sort(AsPlottableMimicTypeFactor(types))
  fmt <- ifelse(showSpeciesCount, '%s (n = %d)', '%s')
  labels <- sapply(ltypes, function(tp) sprintf(fmt, typeToLabel(tp), sum(as.character(type) == as.character(tp))))
  legend(legendPos, legend = labels, inset = legendInsetFn(), col = typeToCol(ltypes), lty = typeToLty(ltypes), lwd = 2)
}

##############################################################################

# Reads in the pre-calculated morphometric scores
ReadMorphoAccuracy <- function(level = c("species", "individuals"), angle = c("dorsal", "lateral")) {
  angle <- match.arg(angle)
  level <- match.arg(level)
  filename <- file.path(OUTLINE_DIR, paste0("accuracy-", level, "-", angle, ".csv"))
  if (!file.exists(filename))
    stop(paste0("Pre-calculated morphometric results not found: ", filename))
  read.csv(filename, stringsAsFactors = FALSE)
}

# Function to perform either a DA (LDA or QDA) or else a logistic regression to
# calculate morphometric mimetic accuracy
CalcMorphoAccuracy <- function(coe, retain = .99, analysisType = c("quadratic", "linear", "logistic"), trainOnAll = TRUE, crossValidate = trainOnAll, priorWeights = NULL, randomiseData = FALSE) {
  analysisType <- match.arg(analysisType)
  if (analysisType == "logistic") {
    CalcMorphoLogit(coe = coe, retain = retain, trainOnAll = trainOnAll, crossValidate = crossValidate)
  } else {
    CalcMorphoDA(coe = coe, retain = retain, analysisType = analysisType, trainOnAll = trainOnAll, crossValidate = crossValidate, priorWeights = priorWeights, randomiseData = randomiseData)
  }
}
  

# Fills in morphometric accuracy in a list of trajectories.
#
# @param level Should matching occur at the specimen (individuals) or species level?
# @param retain  Proportion of variation to retain after PCA.
FillInTrjMorphoAccuracy <- function(trjList, level = c("species", "individuals"), analysisType = c("quadratic", "linear", "logistic"),
                                    retain = 0.99, trainOnAll = TRUE, crossValidate = trainOnAll) {
  
  # By species or by individuals?
  level <- match.arg(level)
  if (level == "species") {
    mtype <- "species"
    leftCol <- "species"
    rightCol <- "species"
  } else {
    mtype <- "individual"
    leftCol <- "imageableid"
    rightCol <- "specimenId"
  }
  
  # Get all known (cached) dorsal morphometrics
  dms <- GetMorphoForPhotos(NULL, "Dorsal")[[mtype]]$Coe
  # Calculate accuracy from morphometrics
  l <- CalcMorphoAccuracy(dms, analysisType = analysisType, retain = retain, trainOnAll = trainOnAll, crossValidate = crossValidate)
  # Put accuracy back into trjList
  morphoIdx <- match(trjList$metaInfo[[leftCol]], dms$fac[[rightCol]])
  trjList$stats$accuracyMorphoDorsal <- l$accuracy[morphoIdx]
  
  # Same for lateral
  dms <- GetMorphoForPhotos(NULL, "Lateral")[[mtype]]$Coe
  # Calculate accuracy from morphometrics
  l <- CalcMorphoAccuracy(dms, analysisType = analysisType, retain = retain, trainOnAll = trainOnAll, crossValidate = crossValidate)
  # Put accuracy back into trjList
  morphoIdx <- match(trjList$metaInfo[[leftCol]], dms$fac[[rightCol]])
  trjList$stats$accuracyMorphoLateral <- l$accuracy[morphoIdx]
  
  trjList
}

##############################################################################

# Summarises morphometrics for individuals/species with trajectories
SummariseMorphometrics <- function(trjList, label, analysisType = "quadratic", retain = 0.95) {

  # All outlines - used to train discriminant analysis
  # Get full morpho info
  dm <- GetMorphoForPhotos(NULL, "Dorsal")
  dm$individual$Coe$fac$specimenId <- as.character(dm$individual$Coe$fac$specimenId)
  lm <- GetMorphoForPhotos(NULL, "Lateral")
  lm$individual$Coe$fac$specimenId <- as.character(lm$individual$Coe$fac$specimenId)
  
  .findTypes <- function(ids, col = "specimenId", typeCol = "mimicType", fac1, fac2) {
    # Get any defined values from fac1
    types <- as.character(fac1[[typeCol]])[match(ids, fac1[[col]])]
    # If any are NA, get the value from fac2
    types[is.na(types)] <- as.character(fac2[[typeCol]])[match(ids, fac2[[col]])][is.na(types)]
    types
  }
  # Merge info for specimens
  indIds <- unique(c(dm$individual$Coe$fac$specimenId, lm$individual$Coe$fac$specimenId))
  specimens <- data.frame(id = indIds, 
                          mimicType = .findTypes(indIds, "specimenId", "mimicType", dm$individual$Coe$fac, lm$individual$Coe$fac),
                          species = .findTypes(indIds, "specimenId", "species", dm$individual$Coe$fac, lm$individual$Coe$fac),
                          hasDorsal = indIds %in% dm$individual$Coe$fac$specimenId,
                          hasLateral = indIds %in% lm$individual$Coe$fac$specimenId)
  
  # Mostly only interested in reporting outlines for mimics
  mimicSpecimens <- specimens$mimicType %in% MTP_MIMICS
  mimics <- SubsetTrjInfo(trjList, trjList$metaInfo$mimicType %in% MTP_MIMICS)
  
  # Fill in species accuracy
  ms <- FillInTrjMorphoAccuracy(mimics, "species", analysisType = analysisType, retain = retain)
  msd <- SubsetTrjInfo(ms, !is.na(ms$stats$accuracyMorphoDorsal))
  msl <- SubsetTrjInfo(ms, !is.na(ms$stats$accuracyMorphoLateral))
  
  cat(sprintf("Morphometrics for specimens with both outlines and %s trajectories\n", label))
  cat("___________________________________________________________________\n")
  cat(sprintf("All outlines: %d specimens, %d mimics (%d species), %d ants, %d non-mimics\n",
              length(indIds), 
              sum(mimicSpecimens), length(unique(specimens[mimicSpecimens, "species"])),
              sum(specimens$mimicType == MTP_MODEL), sum(specimens$mimicType == MTP_NON_MIMIC)))
  cat(sprintf("%d specimens have only dorsal outlines, %d have only lateral, and %d have both\n",
              sum(specimens$hasDorsal & !specimens$hasLateral),
              sum(!specimens$hasDorsal & specimens$hasLateral),
              sum(specimens$hasDorsal & specimens$hasLateral)))
  cat(sprintf("Mimic species with both trajectories and outlines: %d dorsal, %d lateral\n", 
              length(unique(msd$metaInfo$species)), length(unique(msl$metaInfo$species))))
  
  cat("\n")
  bt <- bartlett.test(as.list(MorphoPCA(dm$individual$Coe)))
  homogenous <- bt$p.value > 0.05
  ReportStatTest(bt, " for dorsal individuals")
  cat(sprintf("which indicates variances %s homogenous, so should %suse linear DA\n",ifelse(homogenous, "are", "are not"), ifelse(homogenous, "", "not ")))
  bt <- bartlett.test(as.list(MorphoPCA(lm$individual$Coe)))
  homogenous <- bt$p.value > 0.05
  ReportStatTest(bt, " for lateral individuals")
  cat(sprintf("which indicates variances %s homogenous, so should %suse linear DA\n",ifelse(homogenous, "are", "are not"), ifelse(homogenous, "", "not ")))
}
