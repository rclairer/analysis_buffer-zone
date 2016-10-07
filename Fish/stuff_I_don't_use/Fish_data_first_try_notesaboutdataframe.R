#set working directory
setwd ('/Users/petelab/Dropbox/CRFL - Artificial Reefs/DATA/Fish')
getwd()
#Import data
fish <- read.csv("/Users/petelab/Dropbox/CRFL - Artificial Reefs/DATA/Fish/CRFL_AR_FISH_RAW_abp.csv")
fish
#how did this automatically became a data frame???????????




#fish_vector <- c(fish)
#fish_vector
#fish_matrix <- matrix (fish_vector, 1639, 22)
#fish_matrix <- matrix (fish_vector)
#fish_matrix
#dim(fish_matrix) <- c(1639, 22)
#dim(fish_matrix) <- c(22, 1639)
#dim(fish_matrix) <- c(22)
#fish_matrix
#fish_matrix2 <- matrix(fish, 1639, 22)
#results <- data.frame(row.names = integer(), ID = character(), Site = character(), Diver_Team = factor(), Date = integer(), Transect = factor(), Species_Code = factor(), Abundance = integer(), Size_cm = integer(), Min.Range_cm = integer(), Max.Range_cm = integer(), Biomass_kg.m3 = logical(), Data.entered.by = factor(), Data.read.by = factor(), NOTES = factor(), QAQC = factor(), QAQC.Line.Count = integer(), Errors = integer(), Type.of.error = factor(), NOTES.1 = factor(), Flagged.for.review = factor(), X = logical(), X.1 = factor())