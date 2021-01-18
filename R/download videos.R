# For reference only - this won't work without access to the sampleIT database

# Download trajectory videos
library("JUtils")
source(SAMPLEIT_API)
source("constants.R")


# Download all scale videos/photos
DownloadScales <- function() {
  # Get all scale photos
  # Note that ftype only specifies the type of URL to return, but doesn't act as a filter
  sp <- SIQueryPhotos("q=Scale for&ftype=photo")
  # get all scale videos
  sv <- SIQueryPhotos("q=Scale for&ftype=video")
  s <- rbind(sp[sp$url != "", ], sv[sv$url != "", ])

  if (!dir.exists(SCALE_DIR))
    dir.create(SCALE_DIR, recursive = TRUE)
  JDownload(s$url, 
            tempfileFn = function(pattern, tmpdir, fileext) file.path(tmpdir, paste0(make.names(s$description), fileext)), 
            cacheDir = SCALE_DIR)
  
}


# What videos have we got?
# Query for all photos
p <- SIQueryPhotos("ftype=video")
# Search for ones with ptype containing trajectory or motion
p <- p[grep("traject|motion", p$ptype, ignore.case = TRUE), ]
# Strip out scale videos (note that there are scale photos which don't show up here)
scales <- p[grep("scale", p$ptype, ignore.case = TRUE), ]
videos <- p[-grep("scale", p$ptype, ignore.case = TRUE), ]

Report <- function(videos) {
  specimens <- SIQuerySpecimensForPhotos(videos)
  # Summary by family
  print(table(specimens$family))
  # Report missing species names
  cat(sprintf("Specimens not id'ed to species: %s\n", paste(unique(specimens[specimens$species == "", "id"]), collapse = ", ")))
}

# Download all the wild trajectory videos
wild <- videos[grep("Wild trajectory", videos$ptype, ignore.case = TRUE), ]
if (!dir.exists(WILD_DIR))
  dir.create(WILD_DIR, recursive = TRUE)
JDownload(wild$url, 
          tempfileFn = function(pattern, tmpdir, fileext) file.path(tmpdir, paste0(wild$id, fileext)), 
          cacheDir = WILD_DIR)

# Download scale videos
DownloadScales()

# Download lab videos to another dir
lab <- videos[-grep("Wild trajectory", videos$ptype, ignore.case = TRUE), ]
if (!dir.exists(LAB_DIR))
  dir.create(LAB_DIR, recursive = TRUE)
lf <- JDownload(lab$url, 
                tempfileFn = function(pattern, tmpdir, fileext) file.path(tmpdir, paste0(lab$id, fileext)), 
                cacheDir = LAB_DIR)
