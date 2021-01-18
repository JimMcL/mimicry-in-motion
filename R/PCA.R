library(trajr)

# Returns list(var, contrib), var is variance explained by first 2 components,
# contrib is matrix of relative contributions of each variable to the first 2
# components
# 
plotMotionPCA <- function(trjs, cex = 1, pt.cex = 1, fill = FALSE, classifier = ThreeWayTypes, ellipses = TRUE, points = TRUE, legend = TRUE, labels = FALSE, ...) {
  .cHull <- function(pts, lineColour, fillColour) {
    ch <- chull(pts)
    # Close the polygon
    ch <- c(ch, ch[1])
    
    lines(pts[ch,], col = lineColour)
    polygon(pts[ch,], border = NA, col = fillColour)
  }
  .typeContour <- function(pts, type, ...) {
    contour(.typeDensity(pts, type), add = TRUE, col = classifier$toCol(type), nlevels = 6, drawlabels = FALSE, ...)
  }
  .typeHeatmap <- function(pts, types) {
    image(.typeDensity(pts, types), add = TRUE)
  }
  .typeCHull <- function(pts, type) {
    c <- classifier$toCol(type)
    .cHull(pts[pcaTypes == type,], lineColour = c, fillColour = JTransparentColour(c, 12))
  }
  .typeDensity <- function(pts, type) {
    kde2d(pts[pcaTypes == type,1], pts[pcaTypes == type,2], n = 100)
  }
  .confidenceEllipse <- function(pts, type, conf = .95) {
    idx <- which(pcaTypes == tp)
    if (length(idx) > 2) {
      col <- classifier$toCol(type)
      pts <- pts[idx, ]
      # From https://www.researchgate.net/post/How_to_draw_a_95_confidence_ellipse_to_an_XY_scatter_plot2
      centroid <- colMeans(pts)
      car::ellipse(center = centroid, shape = cov(pts), radius = sqrt(qchisq(.95, df=2)), col = col)
      points(centroid[1], centroid[2], pch = 3, cex = 0.3, col = col)
    }
  }
  
  ####
  
  stats <- GetAnalysableStats(trjs, TRUE)
  # Run the PCA. It is very important to scale the values
  pca <- prcomp(~ ., data = stats, scale. = TRUE)
  
  # Plot
  pcaTypes <- classifier$classify(trjs)
  pts <- pca$x[,1:2]
  typeList <- unique(pcaTypes)
  xlim <- extendrange(pts[,1])
  ylim <- extendrange(pts[,2])
  var <- pca$sdev^2
  var <- signif(100 * var/sum(var), 3)
  
  
  plot(NULL, xlim = xlim, ylim = ylim, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ...)
  title(xlab = sprintf('PC1 (%g%%)', var[1]), ylab = sprintf('PC2 (%g%%)', var[2]), line = 0.5 * cex, cex.lab = cex)

    # for(tp in typeList)
  #   .typeHeatmap(pts, tp)
  # for(tp in typeList)
  #   .typeCHull(pts, tp)
  # for(tp in typeList)
  #   .typeContour(pts, tp, xlim = xlim, ylim = ylim)
  bg <- NULL
  if (fill) {
    bg <- classifier$toCol(pcaTypes)
  }
  if (points) {
    points(pts, pch = classifier$toPCH(pcaTypes), col = classifier$toCol(pcaTypes), bg = bg, cex = pt.cex)
  }
  if (labels) {
    text(pts, trjs$metaInfo$scientificName, col = typeToCol(pcaTypes), cex = .75, pos = 1)
  }
  
  if (ellipses) {
    for(tp in typeList)
      .confidenceEllipse(pts, tp)
    #car::dataEllipse(pts[,1], pts[,2], pcaTypes)
  }
  
  # Legend
  if (legend) {
    typeCounts <- sapply(typeList, function(tp) sum(pcaTypes == tp))
    GroupTypeLegend(classifier, ltypes = typeList, typeCounts, "topright", fill = fill,
                    pch = classifier$toPCH(sort(classifier$toTypeFactor(typeList))),
                    cex = cex)
  }
  
  # Relative contributions of each variable to PC1 and PC2
  absRot <- abs(pca$rotation)
  contrib <- sweep(absRot, 2, colSums(absRot), "/")[,c(1,2)]
  
  # Return variance explained by first 2 components
  pcai <- summary(pca)$importance
  invisible(list(var = pcai[nrow(pcai), 2], contrib = contrib))
}

################################################################################################

.MPconfidenceEllipse <- function(pts, col, conf = .95) {
  # From https://www.researchgate.net/post/How_to_draw_a_95_confidence_ellipse_to_an_XY_scatter_plot2
  centroid <- colMeans(pts)
  car::ellipse(center = centroid, shape = cov(pts), radius = sqrt(qchisq(.95, df=2)), col = col)
  # Big centroid dot
  points(centroid[1], centroid[2], pch = 3, cex = 1, col = "black")
}


# Constructs a motion PCA object
MPPrepare <- function(trjs) {
  
  stats <- GetAnalysableStats(trjs, TRUE)
  # Run the PCA. It is very important to scale the values
  pca <- prcomp(~ ., data = stats, scale. = TRUE)
  # Extract some values
  pts <- pca$x[,1:2]
  xlim <- extendrange(pts[,1])
  ylim <- extendrange(pts[,2])
  var <- pca$sdev^2
  var <- signif(100 * var/sum(var), 3)
  
  ### Private methods
  
  .getTrjs <- function(newTrjs) {
    if (!is.null(newTrjs) && !identical(newTrjs, trjs)) {
      newTrjs
    } else {
      trjs
    }
  }
  
  # Returns the PC1 & PC2 points for the specified trajectories. These need not be the original trajectories
  .getPts <- function(newTrjs) {
    if (!is.null(newTrjs) && !identical(newTrjs, trjs)) {
      # Transform these trajectories using the original PCA transformation
      stats <- GetAnalysableStats(newTrjs, TRUE)
      pp <- predict(pca, stats)
      # Predict just returns the transformed points
      pts <- pp[,1:2]
    }
    pts    
  }
  
  ### Methods
  
  # Draws an empty plot with correct dimensions and axis labels
  emptyPlot <- function(cex = 1, ...) {
    plot(NULL, xlim = xlim, ylim = ylim, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', ...)
    title(xlab = sprintf('PC1 (%g%%)', var[1]), ylab = sprintf('PC2 (%g%%)', var[2]), line = 0.5 * cex, cex.lab = cex)
  }
  
  addPoints <- function(classifier = ThreeWayTypes, fill = TRUE, cex = 1, newTrjs = NULL) {
    ltrjs <- .getTrjs(newTrjs)
    lpts <- .getPts(ltrjs)
    classes <- classifier$classify(ltrjs)
    bg <- NULL
    if (fill) {
      bg <- classifier$toCol(classes)
    }
    points(lpts, pch = classifier$toPCH(classes), col = classifier$toCol(classes), bg = bg, cex = cex)
  }
  
  addEllipses <- function (classifier = ThreeWayTypes, newTrjs = NULL) {
    trjs <- .getTrjs(newTrjs)
    pts <- .getPts(trjs)
    classes <- classifier$classify(trjs)
    typeList <- unique(classes)
    for(tp in typeList) {
      idx <- which(classes == tp)
      if (length(idx) > 2) {
        tpCol <- classifier$toCol(tp)
        tpPts <- pts[idx, ]
        .MPconfidenceEllipse(tpPts, tpCol)
      }
    }
    #car::dataEllipse(pts[,1], pts[,2], classes)
  }
  
  addLegend <- function(classifier = ThreeWayTypes, fill = TRUE, cex = 1, newTrjs = NULL) {
    trjs <- .getTrjs(newTrjs)
    classes <- classifier$classify(trjs)
    typeList <- unique(classes)
    typeCounts <- sapply(typeList, function(tp) sum(classes == tp))
    GroupTypeLegend(classifier, ltypes = typeList, typeCounts, "topright", fill = fill,
                    pch = classifier$toPCH(sort(classifier$toTypeFactor(typeList))),
                    cex = cex)
    
  }
  
  list(emptyPlot = emptyPlot, addPoints = addPoints, addEllipses = addEllipses, addLegend = addLegend)
}


#####

.pcaMimicryComplexes <- function(trjList = trjInfo) {
  op <- par(mfrow = c(2, 2), mar = c(2, 2, 4, 1) + .1)
  pc <- MPPrepare(trjList)
  pc$emptyPlot(main = "All")
  pc$addPoints()
  pc$addEllipses()
  pc$addLegend()
  
  .pcaComplex <- function(title, species, genera) {
    pc$emptyPlot(main = title)
    idxs <- trjList$metaInfo$species %in% species | trjList$metaInfo$genus %in% genera
    complex <- SubsetTrjInfo(trjList, idxs)
    classifier <- BuildSpeciesClassifier(complex)
    pc$addPoints(classifier, newTrjs = complex)
    pc$addEllipses(classifier, newTrjs = complex)
    pc$addLegend(classifier, newTrjs = complex)
    
  }
  
  .pcaComplex("Geen ant complex", c("Amyciaea albomaculata", "Colobathristid1 sp1", "Cosmophasis bitaeniata", "Myrmarachne smaragdina"), "Oecophylla")
  .pcaComplex("Golden bum complex", c("Myrmarachne erythrocephala", "Myrmarachne luctuosa"), c("Polyrhachis", "Dolichoderus"))
  .pcaComplex("Strobe ant complex", c("Judalana lutea", "Myrmarachne bicolor", "Myrmarachne helensmithae"), c("Opisthopsis"))
  
  par(op)
}

if (FALSE) {
  library(JUtils)
  .pcaMimicryComplexes()
  JPlotToPNG("../output/mimicry-complexes-pca.png", .pcaMimicryComplexes(), units = "px", width = 800, aspectRatio = 1)
}
