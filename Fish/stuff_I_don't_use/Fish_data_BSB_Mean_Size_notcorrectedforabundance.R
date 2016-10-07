#FIND THE MEAN SIZE OF ALL BLACK SEA BASS
#Set working directory 
setwd("C:/Users/rclairer/Desktop/CRFL-AR/R_CR")
#read in data
fish <- read.csv ("CRFL_AR_Fish_Raw_abp.csv", header = T, sep= ",")
#install dplyr: first go to packages, install, search for package, then add to library
library(dplyr)
#creates a matrix of columns Species.code, Species, Size_cm
test <- select(fish, contains("Species"), contains("Size_cm"))
#creates a matrix of the above three columns with rows that contain Black Sea Bass in the Species column
BSB <- filter(test, Species == "Black Sea Bass")
#filters BSB matrix for anything that is not blank in the Size_cm column
BSBnumeric <- filter(BSB, Size_cm !="")
#now make sure that R knows that in Size_cm all values are numeric
BSBnumeric$Size_cm <- as.numeric(BSBnumeric$Size_cm)
#make sure that the structure says numeric for Size_cm
str(BSBnumeric)
#find the mean of column Size_cm in BSBnumeric
mean (BSBnumeric$Size_cm)