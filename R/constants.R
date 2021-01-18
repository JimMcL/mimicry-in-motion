DATA_DIR <- "../data"
VIDEO_DIR <- file.path(DATA_DIR, "videos")
WILD_DIR <- file.path(VIDEO_DIR, "wild")
SCALE_DIR <- file.path(VIDEO_DIR, "scale")
LAB_DIR <- file.path(VIDEO_DIR, "lab")

OUTLINE_DIR <- file.path(DATA_DIR, "outlines")

#SAMPLEIT_DIR <- "../../../apps/sampleIt/API/R"
# Use local cached files created by querying the live database
SAMPLEIT_DIR <- "sampleIT"
SAMPLEIT_API <- file.path(SAMPLEIT_DIR, "sampleIt.R")

CACHED_TRJS <- "../data/TrajectoryData.rds"
