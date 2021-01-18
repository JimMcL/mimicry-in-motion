# Functions for handling traits in text fields.
# Trait format is: "Name: value[ units]", with each trait on its own line.
# ^[A-Z].*: .*[ .*]$


.SITraitsFromString <- function(s) {
  # Split into lines
  lines <- strsplit(s, "\n")
  traits <- lapply(lines, function(line) {
    m <- regexec("^([[:upper:]][^:]*):[[:space:]]+([[:graph:]]+)([[:space:]]+([[:graph:]]+))?", line)
    rm <- regmatches(line, m)
    do.call(rbind, lapply(rm, function(rm) rm[c(2, 3, 5)]))
  })
  # Get rid of NAs
  lapply(traits, function(t) na.omit(t))
}


.SIFlattenTraits <- function(traits) {
  names <- sapply(traits, function(t) t[,1])
  dim(names) <- NULL  # Flatten matrix
  names <- na.omit(unique(unlist(names)))
  
  rows <- lapply(traits, function(t) {
    do.call(cbind, lapply(names, function(n) {
      r <- data.frame(val = t[t[,1] == n, 2], units = t[t[,1] == n, 3])
      if (nrow(r) == 0)
        r <- data.frame(val = NA, units = NA)
      r
    }))
  })
  df <- do.call(rbind, rows)
  names(df) <- unlist(sapply(names, function(n) list(n, paste0(n, "-units"))))
  df
}

#' Extracts traits from the notes field of a set of rows.
#'
#' @return Data frame with one row per input row, and 2 columns for every uniquely
#'   named trait in the dataset. Each trait consists of 2 columns: the trait
#'   name and the trait name with "-units" appended.
#'   
#' @examples 
#' lizards <- SIQuerySpecimens("q=skink")
#' # Add traits as additional columns to the lizards data frame
#' lizards <- cbind(lizards, SITraitsFromNotes(lizards))
#' 
SITraitsFromNotes <- function(rows, notes = "notes") {
  .SIFlattenTraits(.SITraitsFromString(rows[[notes]]))
}

