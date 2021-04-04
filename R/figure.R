library(trajr)
source("plotting.R")


PlotTrjsSubset <- function(trjList, subset, title, ...) {
  tt <- SubsetTrjInfo(trjList, subset)
  # Random subsample
  tt <- SubsetTrjInfo(tt, sample(length(tt$trjs), 30))
  
  # Shift so they all start at the same place, and prepare a plot
  trjs <- PrepareRecentredTrjsPlot(tt, main = sprintf(title, length(tt$trjs)), ...)
  
  for (idx in seq_len(length(trjs))) {
    plot(trjs[[idx]], add = TRUE, col = idx)
  }
}


############################################################################################


# Plot a morphospace PCA. This duplicates plotting functionality in Momocs,
# except that it works properly when used with multiple rows/columns or layout.
# The Momocs function draws axes and some text in outside regions
PlotMorphoPCA <- function(morpho, title, ...) {
  
  pca <- PCA(morpho$Coe)
  
  par(mar = c(0, 0, 5, 0) + .1)
  # Plot 1st two principal components
  xy <- pca$x[, c(1, 2)]
  cex = 0.8
  types <- ThreeWayTypeFromMimicType(morpho$Coe$fac$mimicType)
  
  # Zoom in a bit
  .shrinkrange <- function(x, f = 0.1) {
    r <- range(x)
    r + c(f, -f) * diff(r)
  }
  
  plot(xy, asp = 1, xpd = FALSE,
       xlim = .shrinkrange(xy[, 1]), ylim = .shrinkrange(xy[, 2]),
       cex = cex, col = ThreeWayTypes$toCol(types), bg = ThreeWayTypes$toCol(types), pch = ThreeWayTypes$toPCH(types), axes = FALSE, xlab = "", ylab = "")
  
  for (grp in ThreeWayTypes$names) {
    centroid <- ConfidenceEllipse(xy[types == grp, ], col = ThreeWayTypes$toCol(grp), conf = 0.2, lwd = 1, center.pch = 3, center.cex = .5)
    # Hack legend to draw text with white background
    legend(centroid[1], centroid[2], ThreeWayTypes$toLabel(grp), text.col = ThreeWayTypes$toCol(grp), bg = "#ffffff80", box.col = NA, xjust = 0.5, yjust = 0.5, adj = 0.2, cex = 1.2)
  }
  
  box("plot", fg = "#c0c0c0")
  
  # Label axes
  var <- pca$sdev^2
  var <- signif(100 * var/sum(var), 3)
  mtext(sprintf("PC1 (%g%%)", var[1]), side = 1, line = -5.1, adj = 0.95, padj = 5, cex = 0.8)
  mtext(sprintf("PC2 (%g%%)", var[2]), side = 2, line = -4.6, adj = 0.1, padj = -4, cex = 0.8)
  
  # Calculate mean shapes
  #TODO plot mean shapes for types
  morpho$Coe$fac$type3 <- types
  type3 <- MSHAPES(morpho$Coe, 'type3')
  
  DrawShape <- function(shape, x, y, width, height, fill) {
    s <- coo_close(shape)
    # Make x-origin left of the bounding rectangle
    s[, 1] <- (s[, 1] - min(s[, 1])) * width + x
    s[, 2] <- s[, 2] * height + y
    lines(s, type = "l", asp = 1)
    polygon(s, col = JTransparentColour(fill, 180))
    
    # Return width
    c(diff(range(s[, 1])), diff(range(s[, 2])))
  }
  
  LegEntry <- function(x, y, which) {
    points(x, y, pch = ThreeWayTypes$toPCH(which), col = ThreeWayTypes$toCol(which), bg = ThreeWayTypes$toCol(which))
    padx <- strwidth("*")
    x <- x + 2 * padx
    label <- ThreeWayTypes$toLabel(which)
    text(x, y, label, adj = c(0, 0.4))
    x <- x + strwidth(label) + padx
    width <- 3 * strwidth("M")
    r <- DrawShape(type3$shp[[which]], x, y, width, width, ThreeWayTypes$toCol(which))
    x <- x + r[1] + 4 * padx
    x
  }
  
  # Title and legend
  mtext(title, line = 3, adj = 0.01)
  x <- par()$usr[1] + strwidth("M")
  y <- par()$usr[4] + 2.5 * strheight("M")
  par(xpd = NA)
  x <- LegEntry(x, y, "ant")
  x <- LegEntry(x, y, "mimic")
  x <- LegEntry(x, y, "non-mimic")
}

# PlotMorphoPCA(md$species)
# PlotMorphoPCA(ml$species)

############################################################################################

JPlotToPNG("../output/data.png", {
  layout(matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5), byrow = TRUE, nrow = 2))
  PlotTrjsSubset(trjList, trjList$metaInfo$mimicType == MTP_MODEL, title = "Models (n = %d)")
  PlotTrjsSubset(trjList, trjList$metaInfo$mimicType %in% MTP_MIMICS, title = "Mimics (n = %d)")
  PlotTrjsSubset(trjList, trjList$metaInfo$mimicType == MTP_NON_MIMIC, title = "Non-mimics (n = %d)")

    set.seed(1)
  md <- GetMorphoForPhotos(NULL, "Dorsal")
  PlotMorphoPCA(md$species, "Dorsal body shapes")
  ml <- GetMorphoForPhotos(NULL, "Lateral")
  PlotMorphoPCA(ml$species, "Lateral body shapes")
}, units = "px", width = 900, res = 100)
