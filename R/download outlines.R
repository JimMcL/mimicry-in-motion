# For reference only - this won't work without access to the sampleIT database

# Downloads outline images from my sampleIt database, and create a descriptive CSV file. 
# These are a subset of the outlines used in this project

library(JUtils)
source(SAMPLEIT_API)
source("mimic-types.R", local = TRUE)



DownloadOutlines <- function(dir, ...) {
  
  # Get all outline photos
  outlines <- SIQueryPhotos("ptype=Outline&imageable_type=Specimen", ...)
  # Only use "strict" outlines. Non-strict outlines attempted to adjust the outline based on pigmentation/colouring
  outlines <- outlines[grep("strict", outlines$description, ignore.case = TRUE), ]
  
  # Rename some columns that would otherwise be duplicated
  .rnCol <- function(df, new, old) { df[[new]] <- df[[old]]; df[[old]] <- NULL; df; }
  outlines <- .rnCol(outlines, "outlineId", "id")
  outlines <- .rnCol(outlines, "photoDescription", "description")

  # Fill in specimen info, species etc.
  specimens <- SIQuerySpecimensForPhotos(outlines)
  # Match specimens to photos
  fac <- specimens[match(outlines$imageableid, specimens$id),]
  
  # Download the files
  file <- JDownload(outlines$url, cacheDir = dir, filePattern = "outline")

  x <- cbind(outlines, file, fac)
  x
}

x <- DownloadOutlines(file.path(OUTLINE_DIR, "sampleIt"))
csvFile <- file.path(OUTLINE_DIR, "sampleIt", "outline-info.csv")
write.csv(x, csvFile, row.names = FALSE)
