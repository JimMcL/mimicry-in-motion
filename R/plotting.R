library(jpeg)
library(JUtils)
library(trajr)
library(MASS)

# Returns the specified colour with the same RGB components but the specified alpha
# colour - any values that can be passed to rgb()
# alpha - new transparency value, [0, 255]
#
# Note that some graphics devices may not support partial alpha.
JTransparentColour <- function(colour, alpha) {
  c <- col2rgb(colour)
  rgb(c[1,], c[2,], c[3,], alpha, maxColorValue = 255)
}


# Creates an empty plot with range covering the specified trajectories, all of
# which have been translated to start at (0, 0).
# 
# Returns the recentred trajectories
PrepareRecentredTrjsPlot <- function(trjList, xlim, ylim, translate = TRUE, ...) {
  # Translate all to start at the origin
  if (translate) {
    tt <- lapply(trjList$trjs, function(trj) TrajTranslate(trj, -trj[1,"x"], -trj[1,"y"], -trj[1,"time"]))
  } else {
    tt <- trjList$trjs
  }
  
  if (missing(xlim))
    xlim <- range(sapply(tt, function(t) range(t$x)))
  if (missing(ylim))
    ylim <- range(sapply(tt, function(t) range(t$y)))
  plot(NULL, xlim = xlim, ylim = ylim, asp = 1, xlab = "x (m)", ylab = "y (m)", ...)
  tt
}


# Plots multiple trajectories in a single plot, intended for debugging. By
# default, all trajectories will be translated to start at (0, 0). To prevent
# this behaviour, specify <code>translate = FALSE</code>.
PlotTrjList <- function(trjList, labelTracks = TRUE, ...) {
  tt <- PrepareRecentredTrjsPlot(trjList, ...)
  for (idx in seq_along(tt)) {
    plot(tt[[idx]], add = TRUE, col = typeToCol(trjList$metaInfo$mimicType[idx]))
    #label <- trjList$metaInfo$id[idx] # Video id
    #label <- attr(trjList$trjs[[idx]], "trackID") # Track id
    if (labelTracks) {
      label <- sprintf("%d (track %d)", trjList$metaInfo$id[idx], attr(trjList$trjs[[idx]], "trackID"))
      text(tt[[idx]]$x[1], tt[[idx]]$y[1], label, pos = 4)
    }
  }
}

# Use JUtils version instead
# # Plots a list of densities as lines
# #
# # @param extrasFn Function called with no arguments after plot limits are setup but before anything is plotted.
# JPlotDensities <- function(densities, cols = "black", fillCols = NULL, fillDensities = NULL, fillAngles = 45,
#                            lty = 1, lwd = 2, add = FALSE, 
#                            xlim = NULL, ylim = NULL, includeInX = numeric(0), 
#                            extrasFn = function() NULL, ...) {
#   
#   # Create empty plot
#   if (!add) {
#     if (is.null(xlim))
#       xlim <- range(lapply(densities, function(d) d$x), na.rm = TRUE)
#     xlim <- range(c(xlim, includeInX))
#     if (is.null(ylim))
#       ylim <- range(lapply(densities, function(d) d$y), na.rm = TRUE)
#     plot(NULL, xlim = xlim, ylim = ylim, ...)
#   }
#   
#   # Run extras function
#   extrasFn()
#   
#   # Recycle col, lty, lwd if it's a single number
#   if (length(cols) == 1)
#     cols <- rep(cols, length.out = length(densities))
#   if (length(lty) == 1)
#     lty <- rep(lty, length.out = length(densities))
#   if (length(lwd) == 1)
#     lwd <- rep(lwd, length.out = length(densities))
#   if (length(fillDensities) == 1)
#     fillDensities <- rep(fillDensities, length.out = length(densities))
#   if (length(fillAngles) == 1)
#     fillAngles <- rep(fillAngles, length.out = length(densities))
#   
#   # Optionally fill shapes first
#   if (!is.null(fillCols)) {
#     i <- 1
#     for(d in densities) {
#       if (!is.na(fillCols[i]) && fillCols[i] != "")
#         polygon(d, col = fillCols[i], density = fillDensities[i], angle = fillAngles[i], border = NA)
#       i <- i + 1
#     }
#   }
#   
#   # Plot densities as lines
#   i <- 1
#   for(d in densities) {
#     lines(d, col = cols[i], lty = lty[i], lwd = lwd[i])
#     i <- i + 1
#   }
# }

# Plots a legend for mimic types
MLegend <- function(types = MTP_NAMES, typeCounts = NULL, legendPos = "topright", insetFn = JInset, fill = FALSE, ...) {
  ltypes <- AsPlottableMimicTypeFactor(types)
  idx <- order(ltypes)
  if (!is.null(typeCounts)) {
    labels <- sapply(1:length(ltypes), function(i) sprintf('%s (n = %d)', typeToLabel(ltypes[i]), typeCounts[i]))
  } else {
    labels <- typeToLabel(ltypes)
  }
  bg = NA
  if (fill)
    bg <- typeToCol(ltypes[idx])
  legend(legendPos, legend = labels[idx], inset = insetFn(), col = typeToCol(ltypes[idx]), pt.bg = bg, ...)
}

# Plots a legend for an arbitrary type of grouping 
GroupTypeLegend <- function(type, ltypes = type$toTypeFactor(names), typeCounts = NULL, legendPos = "topright", insetFn = JInset, fill = FALSE, ...) {
  idx <- order(ltypes)
  if (!is.null(typeCounts)) {
    labels <- sapply(1:length(ltypes), function(i) sprintf('%s (n = %d)', type$toLabel(ltypes[i]), typeCounts[i]))
  } else {
    labels <- type$toLabel(ltypes)
  }
  bg = NA
  if (fill)
    bg <- type$toCol(ltypes[idx])
  legend(legendPos, legend = labels[idx], inset = insetFn(), col = type$toCol(ltypes[idx]), pt.bg = bg, ...)
}

# Returns a legend inset, given input values in inches
JInset <- function(x = 0.1, y = 0.1) {
  c(x, y) / par()$pin
}

# Plots probability density for accuracy of discrimination between mimic types based on specified criteria
PlotAccuracyDensities <- function(trjList, analysisType, trainOnAll, subsetName) {
  # Add noise to spread the values out a bit, otherwise density plot is too tall
  noise <- runif(nrow(trjList$stats), min = -.08, max = .08)
  trjList$stats$accuracy <- CalcMotionAccuracy(trjList, analysisType, trainOnAll = trainOnAll, crossValidate = trainOnAll)$accuracy + noise
  densities <- lapply(MTP_NAMES, function(tp) density(trjList$stats$accuracy[trjList$metaInfo$mimicType == tp]))
  title <- sprintf("%s trained on %s, %s", analTypeName(analysisType), ifelse(trainOnAll, "all", "ants & non-mimics"), subsetName)
  lty <- c(1, typeToLty(MTP_NAMES[2]), typeToLty(MTP_NAMES[3]), 1)
  JPlotDensities(densities, lineColours = typeToCol(MTP_NAMES), 
                 lty = lty, lwd = 4,
                 fillColours = c(typeToCol(MTP_NAMES[1]), NA, NA, typeToCol(MTP_NAMES[4])),
                 main = title, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  title(ylab = "Density", xlab = "Ant-like trajectory", line = 1)
  MLegend(lty = lty, lwd = 4, legendPos = "topright")
}



PlotStatDensities <- function(trjList, statName, showSpeciesCount = TRUE, legendPos = 'topright', legendInsetFn = JInset, extrasFn = NULL, mar = c(3, 2, 0.2, 0.2), types = MTP_NAMES, ...) {
  
  stats <- GetAnalysableStats(trjList, forDiscrimination = FALSE)
  densities <- lapply(types, function(tp) density(stats[trjList$metaInfo$mimicType == tp, statName], na.rm = TRUE))
  names(densities) <- types
                      
  .plotDensityVert <- function(density, x, ...) {
    .yForX <- function(x) { density$y[which.min(abs(density$x - x))] }
    segments(x, 0, x, .yForX(x), ...)
  }
  
  .plotTypeLines <- function(tp) {
    a <- stats[trjList$metaInfo$mimicType == tp, statName]
    d <- densities[[tp]]
    col = typeToCol(tp)
    m <- mean(a, na.rm = TRUE)
    sda <- sd(a, na.rm = TRUE)
    .plotDensityVert(d, m, col = col)
    .plotDensityVert(d, m + sda, lty = 3, col = col)
    .plotDensityVert(d, m - sda, lty = 3, col = col)
  }
  
  JPlotDensities(densities, col = typeToCol(types), lty = typeToLty(types), yaxt = 'n', xlab = PARAMS_INFO[[statName]]$label, ylab = '', main = PARAMS_INFO[statName, ]$title, ...)
  title(ylab = 'Probability Density', line = .5)

  sapply(names(densities), .plotTypeLines)
  if (!is.null(extrasFn))
    extrasFn(densities)
  
  ltypes <- sort(AsPlottableMimicTypeFactor(types))
  fmt <- ifelse(showSpeciesCount, '%s (n = %d)', '%s')
  labels <- sapply(ltypes, function(tp) sprintf(fmt, typeToLabel(tp), sum(as.character(trjList$metaInfo$mimicType) == as.character(tp))))
  legend(legendPos, legend = labels, inset = legendInsetFn(), col = typeToCol(ltypes), lty = typeToLty(ltypes), lwd = 2)
}


InterpolateDensities <- function(d1, d2, fraction) {
  data.frame(x = d1$x + (d2$x - d1$x) * fraction,
             y = d1$y + (d2$y - d1$y) * fraction)
  
}

AnimateDensities <- function(gifFileName, densities1, densities2, nFrames = 50, frameRate = 30, loop = 0, lwd = 4, lty = 1,
                             cols = typeToCol(MTP_NAMES), fillCols = NULL,
                             title1 = "", title2 = title1, title.cex = 1, 
                             mar = c(2.5, 2.5, 2, 0) + .1,
                             typeLabels = MTP_NAMES, typeCounts = NULL, 
                             legInset = 0, 
                             ...) {

  legLabels <- sapply(1:length(typeLabels), function(i) sprintf('%s (n = %d)', typeLabels[i], typeCounts[i]))

  # x <- function(lwd = 4, ...) {
  .plotIntermediate <- function(frac, group = NULL) {
    densities <- lapply(seq_along(densities1), function(i) InterpolateDensities(densities1[[i]], densities2[[i]], frac))
    # Optionally plot only a single group
    if (is.null(group)) {
      group <- seq_along(densities)
    }
    densities <- densities[group]
    p.cols <- cols[group]
    p.fillCols <- fillCols[group]
    p.lty <- lty[group]
    p.lwd <- lwd[group]
    par(mar = mar)
    JPlotDensities(densities, p.cols, p.fillCols,
                   main = ifelse(frac < 0.5, title1, title2), 
                   lty = p.lty, lwd = p.lwd,
                   xaxt = "n", yaxt = "n", xlab = "", ylab = "", ...)
    title(ylab = "Density", xlab = "Ant-like walking", line = 1, cex.lab = title.cex)
    leg.pch <- ifelse(fillCols == "", NA, 15)
    leg.col <- ifelse(fillCols == "", col, fillCols)
    leg.lty <- ifelse(fillCols == "", lty, 0)
    legend("topright", legLabels, col = leg.col, lty = leg.lty, lwd = lwd, 
           pch = leg.pch, pt.cex = 3, cex = 1.3, inset = legInset, xpd = TRUE)
  }
  # .plotIntermediate(.5)
  # }

  fracs <- seq(0, 1, length.out = nFrames)
  # Ease in and out
  fracs <- sin(fracs * pi - pi / 2) / 2 + .5

  width <- 900
  height <- 600
  res <- 100
  # Just in case the animation doesn't work
  firstFrame <- sub(".gif", "-0.png", gifFileName)
  JPlotToPNG(firstFrame, .plotIntermediate(0), units = "px", width = width, height = height, res = res)
  JPlotToPNG(sub(".gif", "-ants.png", gifFileName), .plotIntermediate(0, 1), units = "px", width = width, height = height, res = res)
  JPlotToPNG(sub(".gif", "-non-mimics.png", gifFileName), .plotIntermediate(0, 2), units = "px", width = width, height = height, res = res)
  JPlotToPNG(sub(".gif", "-mimics.png", gifFileName), .plotIntermediate(0, 3), units = "px", width = width, height = height, res = res)
  firstFrame <- sub(".gif", "-1.png", gifFileName)
  JPlotToPNG(firstFrame, .plotIntermediate(1), units = "px", width = width, height = height, res = res)
  
  JAnimateGIF(frameKeys = fracs, gifFileName = gifFileName, plotFn = .plotIntermediate, frameRate = frameRate, loop = loop, 
              units = "px", width = width, height = height, res = res)
}

MLabelAccuracyAxis <- function(label = 'Mimetic accuracy', poorLabel = 'Poor mimics', goodLabel = 'Good mimics', axis = 'x', cex = NULL) {
  xlab = ''
  ylab = ''
  if (axis == 'x') {
    xlab = label
    side = 1
    pAdj <- 0.5
  } else {
    ylab = label
    side = 2
    pAdj = -0.5
  }
  title(xlab = xlab, ylab = ylab, line = 2, cex.lab = cex)
  mtext(poorLabel, side, adj = 0.01, padj = pAdj, cex = cex)
  mtext(goodLabel, side, adj = .99, padj = pAdj, cex = cex)
  
}
###

# Draws a confidence ellipse and centroid
ConfidenceEllipse <- function(pts, col = "black", conf = .95, ...) {
  if (nrow(pts) > 2) {
    # From https://www.researchgate.net/post/How_to_draw_a_95_confidence_ellipse_to_an_XY_scatter_plot2
    centroid <- colMeans(pts)
    car::ellipse(center = centroid, shape = cov(pts), radius = sqrt(qchisq(conf, df = 2)), col = col, ...)
    #points(centroid[1], centroid[2], pch = 3, cex = 0.3, col = col)
    invisible(centroid)
  }
}

# Converts values in inches to user plot coordinates
InchesToUsr <- function(x, y, units = "in") {
  usr <- par("usr")
  pin <- par("pin")
  c(x / pin[1] * (usr[2] - usr[1]),
    y / pin[2] * (usr[4] - usr[3]))
}

# Plot a median and cross-arms, as in Shamble (2017) Figure 4
# 
# @param data - Data frame - data[, 1] are x points, data[, 2] are y points
MedianCross <- function(data, bg = "lightGrey", col = "black", extents = .25, width = .015, pt.cex = 1.8) {
  sx <- quantile(data[, 1], probs = c(.25, .5, .75), na.rm = TRUE)
  sy <- quantile(data[, 2], probs = c(.25, .5, .75), na.rm = TRUE)
  # Centre - median
  cx <- sx[2]
  cy <- sy[2]
  extents_2 <- InchesToUsr(width, width) # Half height/width in inches
  # Horizontal arm
  rect(sx[1], cy - extents_2[2], sx[3], cy + extents_2[2], border = col, col = bg)
  # Vertical arm
  rect(cx - extents_2[1], sy[1], cx + extents_2[1], sy[3], border = col, col = bg)
  # Median point
  points(cx, cy, pch = 21, cex = pt.cex, col = col, bg = bg)
}

### 
# Group functions

GroupLegend <- function(groups, x = "topleft", legend = groups$toLabel(groups$names), col = "black", ...) {
  names <- groups$names
  legend(x, legend = legend, inset = JInset(), 
       col = col, 
       pt.bg = groups$toCol(names), pt.cex = 1.8, ...)
}

# Given a set of points and their associated types, draws confidence ellipses for each group
GroupConfidenceEllipses <- function(pts, types, groups, confidence = .95) {
  for (grp in groups$names) {
    ConfidenceEllipse(pts[types == grp, ], col = groups$toCol(grp), conf = confidence)
  }
}

# Given a set of points and their associated types, draws median crosses for each group
GroupMedianCrosses <- function(pts, types, groups, border = "black", extents = .25, width = .015, pt.cex = 1.8) {
  for (grp in groups$names) {
    MedianCross(pts[types == grp, ], bg = groups$toCol(grp), col = border, extents = extents, width = width, pt.cex = pt.cex)
  }
}

###

# General function to report and optionally plot a correlation
ReportCorrelation <- function(xCol, yCol, data, report95CI = FALSE, alpha = .05, plotScatter = FALSE, plotResiduals = FALSE, labels = FALSE) {
  l <- lm(reformulate(xCol, yCol), data = data)
  ls <- summary(l)
  p <- ls$coefficients[2,4]
  slope <- coef(l)[2]
  cat(sprintf("Correlation between %s and %s (n = %d):\n\t%s correlated, slope %g, adj r2 %g, F-statistic %g[%d, %d], p %g\n",
              xCol, yCol, sum(!is.na(residuals(l))),
              ifelse(p >= alpha, "not", ifelse(slope > 0, "positively", "negatively")),
              slope, ls$adj.r.squared, 
              ls$fstatistic[1], ls$fstatistic[2], ls$fstatistic[3],
              p))
  # Optionally report 97% confidence interval of correlation
  if (report95CI) {
    corCI <- BootstrapCorCI(data[, xCol], data[, yCol])
    cat(sprintf("\tCorrelation coefficient 95%% CI: %s, %g\n", round(corCI[1], 2), round(corCI[2], 2)))
  }
  
  # Optionally plot residuals to check fit
  if (plotResiduals)
    plot(density(l$residuals))
  if (plotScatter) {
    plot(summary(l)$terms, data = data, col = "red")
    abline(l)
    if (!isFALSE(labels) && !is.null(labels))
      text(l$model[,2], l$model[,1], labels = labels, xpd = TRUE, pos = 1)
  }
}


