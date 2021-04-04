library(trajr)
source("plotting.R")


PlotTrjsSubset <- function(trjList, subset, title, ...) {
  tt <- SubsetTrjInfo(trjList, subset)
  # Random subsample
  #tt <- SubsetTrjInfo(tt, sample(length(tt$trjs), 30))

  # Shift so they all start at the same place, and prepare a plot
  trjs <- PrepareRecentredTrjsPlot(tt, main = sprintf(title, length(tt$trjs)), ...)
  
  for (idx in seq_len(length(trjs))) {
    plot(trjs[[idx]], add = TRUE, col = idx, lwd = 1)
  }
}


############################################################################################

# Evil and ugly hack to use Momocs to draw morphospace indicators
AddMorphoSpace <- function(pca, xy, axes = c(1, 2)) {
  
  # Hack together the list needed by layer_morphospace_PCA
  x <- list(xy = xy, method =pca$method, mshape = pca$mshape, rotation = pca$rotation[, axes])

  par(xpd = FALSE)
  layer_morphospace_PCA(x)
}

# Plot a morphospace PCA. This duplicates plotting functionality in Momocs,
# except that it works properly when used with multiple rows/columns or layout.
# The Momocs function draws axes and some text in outside regions
PlotMorphoPCA <- function(morpho, title, legPos = "topleft", ...) {
  
  pca <- PCA(morpho$Coe)
  
  par(mar = c(0, 0, 3, 0) + .1)
  # Plot 1st two principal components
  xy <- pca$x[, c(1, 2)]
  cex = 0.8
  types <- ThreeWayTypeFromMimicType(morpho$Coe$fac$mimicType)
  
  # Zoom in a bit
  .shrinkrange <- function(x, f = 0.1) {
    r <- range(x)
    r + c(f, -f) * diff(r)
  }
  
  # Prepare the plot
  xlim <- .shrinkrange(xy[, 1])
  ylim <- .shrinkrange(xy[, 2])
  plot(NULL, asp = 1, 
       xlim = xlim, ylim = ylim,
       axes = FALSE, xlab = "", ylab = "")
  
  # Draw morphospace indicators. Make them cover the entire plotted range
  pu <- par()$usr
  AddMorphoSpace(pca, expand.grid(.shrinkrange(c(pu[1], pu[2]), 0.05), .shrinkrange(c(pu[3], pu[4]), 0.05)))

  # Add the points
  points(xy, cex = cex, col = ThreeWayTypes$toCol(types), bg = ThreeWayTypes$toCol(types), pch = ThreeWayTypes$toPCH(types))
  
  # Confidence ellipses with labels
  for (grp in ThreeWayTypes$names) {
    centroid <- ConfidenceEllipse(xy[types == grp, ], col = ThreeWayTypes$toCol(grp), conf = 0.2, lwd = 1, center.pch = 3, center.cex = .5)
    # Hack legend to draw text with white background
    legend(centroid[1], centroid[2], ThreeWayTypes$toLabel(grp), text.col = ThreeWayTypes$toCol(grp), bg = "#ffffff80", box.col = NA, xjust = 0.5, yjust = 0.5, adj = 0.2, cex = 1.2)
    #text(centroid[1], centroid[2], ThreeWayTypes$toLabel(grp), col = ThreeWayTypes$toCol(grp), adj = c(0.5, 0.5), cex = 1.2)
  }
  
  # Decorative border
  box("plot", fg = "#c0c0c0")
  
  # Label axes
  var <- pca$sdev^2
  var <- signif(100 * var/sum(var), 3)
  mtext(sprintf("PC1 (%g%%)", var[1]), side = 1, line = -5.1, adj = 0.95, padj = 5, cex = 0.8)
  mtext(sprintf("PC2 (%g%%)", var[2]), side = 2, line = -4.6, adj = 0.1, padj = -4, cex = 0.8)
  
  # Title
  mtext(sprintf(title, nrow(xy)), line = 1, adj = 0.01)
  
  # Legend
  legTypes <- ThreeWayTypes$names
  legend <- sapply(legTypes, function(tp) sprintf("%s (n = %d)", ThreeWayTypes$toLabel(tp), sum(types == tp)))
  legend(legPos, legend, inset = 0.01,
         col = ThreeWayTypes$toCol(legTypes), pch = ThreeWayTypes$toPCH(legTypes), pt.bg = ThreeWayTypes$toCol(legTypes))
}

PlotAllData <- function(trjList, grouping = "species") {
  par(mar = c(5, 4, 3, 1) + 0.1)
  layout(matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 5), byrow = TRUE, nrow = 2))
  PlotTrjsSubset(trjList, trjList$metaInfo$mimicType == MTP_MODEL, title = "Ants (n = %d)")
  PlotTrjsSubset(trjList, trjList$metaInfo$mimicType %in% MTP_MIMICS, title = "Mimics (n = %d)")
  PlotTrjsSubset(trjList, trjList$metaInfo$mimicType == MTP_NON_MIMIC, title = "Non-mimics (n = %d)")
  
  set.seed(1)
  md <- GetMorphoForPhotos(NULL, "Dorsal")
  PlotMorphoPCA(md[[grouping]], "Dorsal body shapes", legPos = "topright")
  ml <- GetMorphoForPhotos(NULL, "Lateral")
  PlotMorphoPCA(ml[[grouping]], "Lateral body shapes", legPos = "topright")
}

# PlotMorphoPCA(md$species, "Dorsal body shapes")
# PlotMorphoPCA(ml$species, "Lateral body shapes")

############################################################################################

