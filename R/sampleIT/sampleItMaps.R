# R API for mapping sampleIt specimens.

library(ggmap)
library(maps)
library(mapdata)
library(sp)


# Basic long/lat projection
wgs84.proj4 <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

#######################################################################
# Mapping functions

# Uses the package ggmap to plot a google map of the specimens' collection sites.
# Note that google maps (optionally used by ggmap) requires you to register with google. 
# Refer to https://github.com/dkahle/ggmap for more information
# May require the latest github release of ggmap
SIGMapSpecimenSites <- function(specimens, title = NULL, xlimFrac = 0.5, ylimFrac = 0.5, source = "stamen") {
  
  # Determine unique locations, and specimen counts at each location
  cols <- c('decimalLatitude', 'decimalLongitude')
  sites <- aggregate(rep(1, nrow(specimens)), by = list(specimens$decimalLatitude, specimens$decimalLongitude), sum)
  names(sites) <- c(cols, 'Count')
  
  xlim <- extendrange(sites$decimalLongitude, f = xlimFrac)
  ylim <- extendrange(sites$decimalLatitude, f = ylimFrac)
  m <- get_map(c(xlim[1], ylim[1], xlim[2], ylim[2]), source = source)
  
  # Explicitly print so that it works when plotting to a device  
  print(ggmap(m) + 
          ggtitle(title) +
          geom_point(data = sites, aes(x = decimalLongitude, y = decimalLatitude, size = Count), 
                     color = 'black', fill= '#ff000044', pch = 21) + 
          scale_size_continuous(range = c(5, 20)) +
          theme(plot.margin = unit(c(0, 0, 0, 0), "cm")))
}


# Uses the maps and mapdata packages to plot a map of the specimens' collection sites.
# Note that google maps requires you to register with google. 
# Refer to https://github.com/dkahle/ggmap for more information
SIMapSpecimenSites <- function(specimens, xlimFrac = 0.5, ylimFrac = 0.5, showLegend = FALSE, title = NULL, cex = NULL, scaleSitesByCount = TRUE, ...) {
  
  # Determine unique locations, and specimen counts at each location
  cols <- c('decimalLatitude', 'decimalLongitude')
  sites <- aggregate(rep(1, nrow(specimens)), by = list(specimens$decimalLatitude, specimens$decimalLongitude), sum)
  names(sites) <- c(cols, 'Count')
  # Convert sites to spatial points
  coordinates(sites) <- c("decimalLongitude", "decimalLatitude")
  proj4string(sites) <- wgs84.proj4
  
  xlim <- extendrange(sites$decimalLongitude, f = xlimFrac)
  ylim <- extendrange(sites$decimalLatitude, f = ylimFrac)
  
  # Draw a map
  map("world2Hires", xlim = xlim, ylim = ylim, col = rgb(0.9, 0.85, 0.8), fill = TRUE)
  map("rivers", col = "blue", add = TRUE)
  title(main = title)
  
  # Draw the sites
  MAX_CEX <- 5
  .countToCex <- function(count) MAX_CEX * (log(count) + 1) / max(log(sites$Count) + 1)
  if (scaleSitesByCount) {
    cex <- .countToCex(sites$Count)
  }
  points(sites, cex = cex, ...)
  
  if (showLegend) {
    legend("topright", legend = c(1, 2, 5, 10), pt.cex = .countToCex(c(1, 2, 5, 10)), title = "No. of specimens", pch = 1)
  }
}


