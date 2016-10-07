###Fish QAQC- Investigating Outliers ###
## Script prepared by Emily Pickering and Avery Paxton
## Goal: Create a scatter plot for each species of fish, date on the x-axis and size on the y-axis. Unusual sizes should appear and be selectable as outliers in the plot.#

### Import data
  # Set working directory
  #setwd('/Users/petelab/Dropbox/CRFL/DATA/Fish/2_QAQC')
  #getwd()
  
  setwd('/Users/rclairer/Desktop/CRFL-AR/R_CR/Analysis/Data/Fish')
  getwd()
  
  library(dplyr)
  
  # Import data under the name "BOEM_Fish.Data_with.QAQC". It is a csv-comma delimited file with 16 variables, 6494 observations.Heading checked "Yes".
  # BOEM_Fish.Data_with.QAQC <- read.csv("~/BOEM/DATA/Fish/Fish_QAQC/BOEM_Fish Data_with QAQC.csv")
  #fish <- read.csv("/Users/petelab/Dropbox/CRFL/DATA/Fish/2_QAQC/CRFL_Fish_RangeEdits_QAQC.csv")
  #head(fish)
  
  fish <- read.csv("/Users/rclairer/Desktop/CRFL-AR/R_CR/Analysis/Data/Fish/CRFL_AR_Fish_Raw_abp_2016-03-21.csv")
  head(fish)
  fishwithpositiveabundance <- filter(fish, Abundance !="", Abundance !="NO_FISH", Abundance !="N/A")
  radiatingfishwpa  <- filter (fishwithpositiveabundance, Transect_Type =="Radiating") 
  radiatingfishwpa$Size_cm <- as.numeric(radiatingfishwpa$Size_cm)
  #now make sure that R knows that in Abundance all values are numeric
  radiatingfishwpa$Abundance <- as.numeric(radiatingfishwpa$Abundance)
  #make sure that the structure says numeric for Size_cm


## Identify outliers in plots of species abundance vs species size
  
  # Identify species by unique identifiers in the column called species
  uniq <- unique(unlist(radiatingfishwpa$Species.code))  # saves unique species codes as object called 'uniq' so that can use them in the loop below 
  # Make data frame to store results of outlier detection
  results <- data.frame(row.names = integer(), ID = character(), Site = character(), Diver_Team = factor(), Date = integer(), Transect = factor(), Species_Code = factor(), Abundance = integer(), Size_cm = integer(), Min.Range_cm = integer(), Max.Range_cm = integer(), Biomass_kg.m3 = logical(), Data.entered.by = factor(), Data.read.by = factor(), NOTES = factor(), QAQC = factor(), QAQC.Line.Count = integer(), Errors = integer(), Type.of.error = factor(), NOTES.1 = factor(), Flagged.for.review = factor(), X = logical(), X.1 = factor())
#remove all rows in fish where abund = no_fish, N/A, or blank
  
  
# For loop to identify outliers for each species in the object uniq
  p=1 # p is the counter, initialize it at 1
  for (p in 1:length(uniq)){
    fish_sub <- subset(radiatingfishwpa, Species.code == uniq[p]) # subset data so that each species appears by itself for the plot and outlier detection below. Since it's a loop, this will repeat for all species in 'uniq'
    plot (fish_sub$Abundance, fish_sub$Size_cm, main=as.character(uniq[p]), cex=0.8, pch=18, col=84, xlab="Species Abundance", ylab="Species Size (cm)") # plot subset corresponding to each species
    # main=title
    #print(fish_sub[identify(fish_sub$Abundance, fish_sub$Size_cm, labels=row.names(fish_sub)),]) # identify outliers
    test<-print(fish_sub[identify(fish_sub$Abundance, fish_sub$Size_cm, labels=row.names(fish_sub)),]) # identify outliers
      # inside identify is the x, y, and labels
      # it is in [,] form to give row and and all columns for outliers once selected
      # print makes the results show up in the console screen
    results <- rbind(results, test) # this appends the output from the current counter on the loop to the overall / previous results  
    }
 
  # Export outliers and associated information in a csv file
  write.table(results, file = "fish_outlier_output.csv", row.names = FALSE, append = FALSE, col.names = TRUE, sep = ", ")


  
#### EMILY'S ORIGINAL CODE ####
#Equations below will select rows that include the species of fish labeled (Example- CEST plot), creating a new data source.
  # CEST <- BOEM_Fish.Data_with.QAQC[ which(BOEM_Fish.Data_with.QAQC$Species=='CEST'), ]

#Plot with main title, smaller points, diamonds, blue!, and set limits for x,y axes
  # plot(CEST$Abundance, CEST$Size_cm, main="CEST", cex=0.8, pch=18, col=84)

#Identify outliers- copy equation, click on points of interest, click "Finish" in  upper right
  #identify(CEST$Abundance, CEST$Size_cm, labels=row.names(CEST))#
  #CEST[identify(CEST$Abundance, CEST$Size_cm, labels=row.names(CEST)),]