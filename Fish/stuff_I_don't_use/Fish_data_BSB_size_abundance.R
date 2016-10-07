#FIND THE MEAN SIZE OF ALL BLACK SEA BASS using size and abundance
#Set working directory 
setwd("C:/Users/rclairer/Desktop/CRFL-AR/R_CR")
#read in data
fish <- read.csv ("CRFL_AR_Fish_Raw_abp.csv", header = T, sep= ",")
#install dplyr: first go to packages, install, search for package, then add to library
library(dplyr)
#creates a matrix of columns Species.code, Species, Size_cm
test2 <- select(fish, contains("Species"), contains("Size_cm"), contains ("Abundance"))
#creates a matrix of the above three columns with rows that contain Black Sea Bass in the Species column
BSB2 <- filter(test2, Species == "Black Sea Bass")
#filters BSB matrix for anything that is not blank in the Size_cm column
BSBnumeric2 <- filter(BSB2, Size_cm !="")
#now make sure that R knows that in Size_cm all values are numeric
BSBnumeric2$Size_cm <- as.numeric(BSBnumeric2$Size_cm)
#now make sure that R knows that in Abundance all values are numeric
BSBnumeric2$Abundance <- as.numeric(BSBnumeric2$Abundance)
#make sure that the structure says numeric for Size_cm
str(BSBnumeric2)
#find the mean of column Size_cm in BSBnumeric
mean (BSBnumeric2$Size_cm)
#create another column with data from other columns - on dplyr cheat sheet
BSBnumeric2withSizeAbundance <- mutate(BSBnumeric2, SizeAbundance = Size_cm * Abundance)
#find sum of the sizeabundance column
sumBSBSizeAbundance <- sum(BSBnumeric2withSizeAbundance$SizeAbundance)
sumBSBSizeAbundance
#find sum of abundance column
sumBSBAbundance <- sum(BSBnumeric2withSizeAbundance$Abundance)
sumBSBAbundance
#find the average by dividing the two
BSBaverage <- sumBSBSizeAbundance/sumBSBAbundance
BSBaverage
