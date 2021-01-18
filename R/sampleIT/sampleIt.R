# "Shim" interface to my sampleIT database. 
# 
# R functions to query the samples database or locally cached results.
# The database contains information about specimens and their photos.
# 
# The database is queried using an HTTP interface.
# The host defaults to localhost, but can be specified by setting the global variable SI_HOST.
#
# See the file sampleItMaps.R for some mapping functionality.

library(openssl)

# This code can work in 3 modes 
SI_SHIM_MODES <- c(
  live = 1,        # Live database mode, requires access to the running database
  use.cache = 2,   # Uses pre-built locally cached files; doesn't require database access
  build.cache = 3  # Queries the live database, but also caches query results into local files; requires dbs access
)

SI_MODE <- SI_SHIM_MODES["use.cache"]
SI_CACHE_DIR <- "../data/sampleITcache"

##############################################################################
# Shim functions

# Given a sampleIT query URL, returns the name of the file it can be/is cached in
.siCacheFile <- function(url) {
  file.path(SI_CACHE_DIR, md5(url))
}

# Returns data from the database or the local cache, based on the value of SI_MODE
SIQuery <- function(...) {
  tryCatch({
    url <- SIUrl(...)
    cachedFile <- .siCacheFile(url)
    # Shim mode
    if (bitwAnd(SI_MODE, 1))
      r <- read.csv(url, stringsAsFactors = F, strip.white=TRUE)
    else {
      if (!file.exists(cachedFile))
        stop(sprintf("Locally cached file doesn't exist for URL %s", url))
      r <- read.csv(cachedFile, stringsAsFactors = F, strip.white=TRUE)
    }
    # Does the result now need to be cached?
    if (bitwAnd(SI_MODE, 2)) {
      if (!dir.exists(SI_CACHE_DIR)) {
        dir.create(SI_CACHE_DIR)
      }
      # Sanity check, assuming we only build the cache once. File should not exist
      if (file.exists(cachedFile))
        stop(sprintf("Error - cache file already exists for URL %s", url))
      write.csv(r, cachedFile, row.names = FALSE)
    }
    r
    },
    error = function(e) stop(sprintf("SIQuery failed with arguments (%s): %s", paste(..., sep = ", "), e))
  )
}


####
# Non-shim functions

# Returns the name of the host for database queries.
# If SI_HOST is set, returns it, otherwise returns 'localhost'.
SIChooseHost <- function() {
  if (exists('SI_HOST'))
    SI_HOST
  else
    'localhost'
}

# Returns a URL by pasting arguments on the end of 'http://SI_HOST/'
SIUrl <- function(...) {
  u <- sprintf("http://%s/%s", SIChooseHost(), paste0(...))
  #cat(paste0(u, '\n'))
  u
}

# Returns photo metadata from the database. Any arguments are used to construct the URL
SIQueryPhotos <- function(...) {
  SIQuery('photos.csv?', ...)
}

# Returns specimen metadata from the database. Any arguments are used to construct the URL
SIQuerySpecimens <- function(...) {
  SIQuery('specimens.csv?', ...)
}

# Given a set of specimen IDs, returns their specimen records
SIQuerySpecimensByIds <- function(ids) {
  SIQuerySpecimens(sprintf("id=[%s]", paste(ids, collapse=',')))
}

# Returns specimen metadata corresponding to a set of photos
SIQuerySpecimensForPhotos <- function(photos) {
  specimenIds <- unique(photos[photos$imageabletype == 'Specimen',]$imageableid)
  SIQuerySpecimensByIds(specimenIds)
}

# Returns a data frame containing details of all photos in the database with the specified viewing angle.
# The photo image files are downloaded to a local cache, and the file name is stored in the column "file".
# angle - 'dorsal' or 'lateral'
SIGetOutlines <- function(angle) {
  SIGetPhotoData("ptype=Outline&imageable_type=Specimen&view_angle=", angle, strictOnly = TRUE)
}

# Returns the specimen collection date time.
SIRecordedDateTime <- function(specimens) {
  date <- paste(specimens$year, specimens$month, specimens$day, sep = "-")
 strptime(paste(date, specimens$time), format = "%Y-%m-%d %H:%M")
}
