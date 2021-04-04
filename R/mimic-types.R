# Functions for manipulating mimic types
#
# Defines groups of trajectories. Groups can be used in statistical tests and plotting.
# Available groups are:
# MimicTypes - ants, mimetic spiders, mimetic insects and non-mimics
# TrajectoryTypes3 - wild, semi-wild, lab
# TrajectoryTypes2 - wild, lab
# FullGroupTypes - (ant, mimic, non-mimic) * (wild, lab)

# Allow blind analysis by randomly assigning type to trajectories
BLINDING <- FALSE


# Defines an object that encapsulates a classification scheme for consistent
# analysis and display of trajectories. Each object has the following
# properties:
#
# names - factor: available types
# classify - function: given a trajectory list (e.g. labTrjs), returns vector of (character) type names
# toCol - function: given type names, returns type colours
# toPCH - function: given type names, returns type point symbology
# toLTY - function: given type names, returns type line style
# toLabels - function: given type names, returns names pluralised and capitalised
# toTypeFactor - function: given type names, returns types as a factor with nicely ordered levels
.buildTypeSymbology <- function(names, classify, cols, pchs, ltys, pluralLabels, singLabels) {
  
  # Allow blind analysis
  if(BLINDING) {
    message("Applying blinding")
    classify <- function(trjList) sample(names, nrow(trjList$metaInfo), replace = TRUE)
  }
    
  .typeToVal <- function(type, vals) {
    # I was relying on numeric factor values to do this, but ran into mysterious cases 
    # when it didn't produce the correct results! This mechanism is more general
    idx <- match(type, names)
    ifelse(is.na(idx), vals[length(names) + 1], vals[idx])
  }
  # Construct a new mimic type factor for plotting. This does 2 things:
  # 1. removes any unused level (i.e. not in the data)
  # 2. orders the factor in the way I want it
  # Normally these things don't matter, but they do matter when plotting 
  .plottableTypeFactor <- function(type) {
    # Order of names is important here
    levels <- names[names %in% type]
    factor(type, levels = levels)
  }
  
  
  toCol <- function(type) .typeToVal(type, cols)
  toPCH <- function(type) .typeToVal(type, pchs)
  toLTY <- function(type) .typeToVal(type, ltys)
  toLabel <- function(type, plural = TRUE) {
    .typeToVal(type, c(if (plural) pluralLabels else singLabels, type))
  }
  list(names = names, classify = classify, toCol = toCol, toPCH = toPCH, toLTY = toLTY, toLabel = toLabel, toTypeFactor = .plottableTypeFactor)
}

#########################################################################
# The default type: moodel, mimetic insect, mimetic spider and non-mimic

# Mimic types
MTP_MODEL <- 'model'
MTP_SPIDER <- 'mimetic spider'
MTP_INSECT <- 'mimetic insect'
MTP_NON_MIMIC <- 'non-mimic'
MTP_NAMES <- c(MTP_MODEL, MTP_SPIDER, MTP_INSECT, MTP_NON_MIMIC)
names(MTP_NAMES) <- MTP_NAMES
MTP_MIMICS <- c(MTP_SPIDER, MTP_INSECT)


# Decide whether each specimen is an ant, a mimic insect, a mimic spider or a non-mimic
mimicTypeFromMetaInfo <- function(metaInfo) {
  ifelse(metaInfo$family == "Formicidae",
         MTP_MODEL,
         ifelse(grepl("mimic", metaInfo$other, ignore.case = TRUE),
                ifelse(metaInfo$class == "Arachnida",
                       MTP_SPIDER, MTP_INSECT),
                MTP_NON_MIMIC)
  )
}

# Construct a new mimic type factor for plotting. This does 2 things:
# 1. removes any unused level (e.g. "insect mimic")
# 2. orders the factor in the way I want it
# Normally these things don't matter, but they do matter when plotting 
AsPlottableMimicTypeFactor <- function(types) {
  # Order of MTP_NAMES is important here
  levels <- MTP_NAMES[MTP_NAMES %in% types]
  factor(types, levels = levels)
}

.typeToVal <- function(type, val, unknownTypeVal, names = MTP_NAMES) {
  # I was relying on numeric factor values to do this, but ran into mysterious cases 
  # when it didn't produce the correct results! This mechanism is more general
  idx <- match(type, names)
  ifelse(is.na(idx), unknownTypeVal, val[idx])
}

# Given a type (e.g. 'model', 'spider mimic', ...), returns a colour to use when plotting the type.
# Works using character comparisons, so factor values numeric values are not assumed.
typeToCol <- function(type) {
  .typeToVal(type, c("#662c91", "#ee2e2f", "#185aa9", "#008c48"), 'black')
}

# Given a type (e.g. 'model', 'spider mimic', ...), returns a line style to use when plotting the type
# Works using character comparisons, so factor values numeric values are not assumed.
typeToLty <- function(type) {
  .typeToVal(type, c(4, 1, 2, 3), 6)
}

# Given a type (e.g. 'model', 'spider mimic', ...), returns a line style to use when plotting the type
# Works using character comparisons, so factor values numeric values are not assumed.
typeToPch <- function(type) {
  #.typeToVal(type, c(2, 15, 16, 6), 43)
  .typeToVal(type, c(24, 15, 16, 25), 43)
}

typeToLabel <- function(type, plural = TRUE) {
  pl <- c('Ants', 'Mimetic spiders', 'Mimetic insects', 'Non-mimics')
  sing <- c('Ant', 'Mimetic spider', 'Mimetic insect', 'Non-mimic')
  .typeToVal(type, if(plural) pl else sing, type)
}


# Need a value for each type + 1 for unknown type, except for labels, which uses the type as the label
MimicTypes <- .buildTypeSymbology(MTP_NAMES, 
                                  function(ti) mimicTypeFromMetaInfo(ti$metaInfo),
                                  cols = c("#662c91", "#ee2e2f", "#185aa9", "#008c48", 'black'),
                                  pchs = c(24, 15, 16, 25, 43),
                                  ltys = c(4, 1, 2, 3, 6),
                                  pluralLabels = c('Models', 'Mimetic spiders', 'Mimetic insects', 'Non-mimics'),
                                  singLabels = c('Model', 'Mimetic spider', 'Mimetic insect', 'Non-mimic'))



##############################################################################
# Trajectory types 2 or 3

TTP_LAB <- "lab"
TTP_WILD <- "wild"
TTP_SEMI_WILD <- "semi-wild"
TTP3_NAMES <- c(TTP_LAB, TTP_WILD, TTP_SEMI_WILD)
names(TTP3_NAMES) <- TTP3_NAMES

# Returns the type of trajectory - "lab", "wild" or "semi-wild"
TrajectoryType3Fn <- function(trjList) {
  semiWild <- grepl("semi.wild", trjList$metaInfo$ptype, ignore.case = TRUE)
  wild <- grepl("wild", trjList$metaInfo$ptype, ignore.case = TRUE) & !semiWild
  ifelse(wild, TTP_WILD,
         ifelse(semiWild, TTP_SEMI_WILD, TTP_LAB))
}

TrajectoryTypes3 <- .buildTypeSymbology(TTP3_NAMES, 
                                        TrajectoryType3Fn,
                                        cols = c("#f47d23", "#b43894", "#008c48", "black"),
                                        pchs = c(0, 2, 5, 8),
                                        ltys = c(1, 3, 2, 4),
                                        pluralLabels = c("Lab trajectories", "Wild trajectories", "Semi-wild trajectories"),
                                        singLabels = c("Lab trajectory", "Wild trajectory", "Semi-wild trajectory"))

###

TTP2_NAMES <- c(TTP_LAB, TTP_WILD)
names(TTP2_NAMES) <- TTP2_NAMES

# Returns the type of trajectory - "lab" or "wild"
TrajectoryType2Fn <- function(trjList) {
  wild <- grepl("wild", trjList$metaInfo$ptype, ignore.case = TRUE)
  ifelse(wild, TTP_WILD, TTP_LAB)
}

TrajectoryTypes2 <- .buildTypeSymbology(TTP2_NAMES, 
                                        TrajectoryType2Fn,
                                        cols = c("#f47d23", "#b43894", "black"),
                                        pchs = c(0, 2, 8),
                                        ltys = c(1, 3, 4),
                                        pluralLabels = c("Lab trajectories", "Wild trajectories"),
                                        singLabels = c("Lab trajectory", "Wild trajectory"))

##############################################################################
# Three mimic-types, models, mimics and non-mimics

TWT_ANT <- "ant"
TWT_NON_ANT <- "non-mimic"
TWT_MIMIC <- "mimic"
TWT_NAMES <- c(TWT_ANT, TWT_MIMIC, TWT_NON_ANT)
names(TWT_NAMES) <- TWT_NAMES

# Returns the type of trajectory - ant, mimic or non-ant
ThreeWayTypeFromMimicType <- function(mimicType) {
  ants <- mimicType == MTP_MODEL
  mimics <- mimicType %in% MTP_MIMICS
  nonAnts <- mimicType == MTP_NON_MIMIC
  ifelse(ants, TWT_ANT,
         ifelse(mimics, TWT_MIMIC, TWT_NON_ANT))
}

ThreeWayTypeFn <- function(trjList) {
  ThreeWayTypeFromMimicType(trjList$metaInfo$mimicType)
}

ThreeWayClass <- function(type) {
  as.factor(ThreeWayTypeFromMimicType(type))
}

ThreeWayTypes <- .buildTypeSymbology(TWT_NAMES, 
                                     ThreeWayTypeFn,
                                     cols = c("#662c91", "#ee2e2f", "#185aa9", 'black'),
                                     #pchs = c(0, 2, 5, 8),
                                     pchs = c(22, 24, 23, 8),
                                     #pchs = c(15, 17, 19, 8),
                                     ltys = c(1, 3, 2, 4),
                                     pluralLabels = c("Ants", "Mimics", "Non-mimics"),
                                     singLabels = c("Ant", "Mimic", "Non-mimic"))

###########################################
# Grouped on both mimic type (ant, mimic, non-mimic) and trajectory type (wild, lab)

FG_NAMES <- sprintf("%s:%s", expand.grid(TWT_NAMES, TTP2_NAMES)[,1], expand.grid(TWT_NAMES, TTP2_NAMES)[,2])
names(FG_NAMES) <- FG_NAMES

FullGroupTypeFn <- function(trjList) {
  twt <- ThreeWayTypeFn(trjList)
  tt <- TrajectoryType2Fn(trjList)
  paste(twt, tt, sep = ":")
}

FullGroupTypes <- .buildTypeSymbology(FG_NAMES, 
                                      FullGroupTypeFn,
                                      cols = c("#662c91", "#ee2e2f", "#185aa9", "#008c48", "#daa410", "#10daa4", 'black'),
                                      pchs = c(22, 24, 23, 0, 2, 5, 43),
                                      ltys = c(1, 3, 2, 1, 2, 3, 4),
                                      pluralLabels = c("Ants/Lab", "Mimics/Lab", "Non-mimics/Lab", "Ants/Wild", "Mimics/Wild", "Non-mimics/Wild"),
                                      singLabels = c("Ant/Lab", "Mimic/Lab", "Non-mimic/Lab", "Ant/Wild", "Mimic/Wild", "Non-mimic/Wild"))



###########################################
# Grouped on species


BuildSpeciesClassifier <- function(trjList) {
  species <- sort(unique(trjList$metaInfo$species))
  n <- length(species)
  #cols <- hcl.colors(n)
  cols <- rainbow(n)
  pch <- rep(c(15, 16, 17), n / 3 + 2)[1:n]
  .buildTypeSymbology(species, 
                      function(t) t$metaInfo$species,
                      cols = c(cols, 'black'),
                      pchs = c(pch, 3),
                      ltys = rep(1, n + 1),
                      pluralLabels = species,
                      singLabels = species)
}