#### Functions written to gather and process previously cleaned raw data ####

## Fish -------------------------------------------------------

#get_fish_data <- function(file_data, size){
  # Get fish data for all or specific sizes
  #data <- read.table(file = file_data, header = T, sep=',')
  #data$abun <-data[,size] # where abundance level is all fish, small, med, large, or apex
  #data
#}

combine_data_meta <- function(data, file_meta, merge_by){
  # Combine fish data and metadata
  metadata <- read.table(file = file_meta, header = T, sep=',')
  data_all <- merge(data, metadata, by = merge_by)
  data_all
}


make_fish_biomass_col <- function (data){
  j=1
  for (j in 1:nrow(data)){
    if (data$Size_cm[j] != 0)
      data$biomass[j] <- data$Abund[j] * data$a[j] * (data$Size_cm[j])^data$b[j]
    if (data$Size_cm[j] == 0)
      data$biomass[j] <- data$Abund[j] * data$a[j] * (mean(c(data$Max_Range_cm[j], data$Min_Range_cm[j])))^data$b[j]
  }
  data$biomass <- data$biomass / 1000
  data
}

make_fish_biomass_col_small <- function (data){
  j=1
  for (j in 1:nrow(data)){
    if (data$Size_cm[j] != 0)
      data$biomass[j] <- data$sm_1.10cm[j] * data$a[j] * (data$Size_cm[j])^data$b[j]
    if (data$Size_cm[j] == 0)
      data$biomass[j] <- data$sm_1.10cm[j] * data$a[j] * (mean(c(data$Max.Range_cm[j], data$Min.Range_cm[j])))^data$b[j]
  }
  data$biomass <- data$biomass / 1000
  data
}

make_fish_biomass_col_medium <- function (data){
  j=1
  for (j in 1:nrow(data)){
    if (data$Size_cm[j] != 0)
      data$biomass[j] <- data$md_11.29cm[j] * data$a[j] * (data$Size_cm[j])^data$b[j]
    if (data$Size_cm[j] == 0)
      data$biomass[j] <- data$md_11.29cm[j] * data$a[j] * (mean(c(data$Max.Range_cm[j], data$Min.Range_cm[j])))^data$b[j]
  }
  data$biomass <- data$biomass / 1000
  data
}

make_fish_biomass_col_large <- function (data){
  j=1
  for (j in 1:nrow(data)){
    if (data$Size_cm[j] != 0)
      data$biomass[j] <- data$lg_30.49cm[j] * data$a[j] * (data$Size_cm[j])^data$b[j]
    if (data$Size_cm[j] == 0)
      data$biomass[j] <- data$lg_30.49cm[j] * data$a[j] * (mean(c(data$Max.Range_cm[j], data$Min.Range_cm[j])))^data$b[j]
  }
  data$biomass <- data$biomass / 1000
  data
}

make_fish_biomass_col_apex <- function (data){
  j=1
  for (j in 1:nrow(data)){
    if (data$Size_cm[j] != 0)
      data$biomass[j] <- data$ap_50.cm[j] * data$a[j] * (data$Size_cm[j])^data$b[j]
    if (data$Size_cm[j] == 0)
      data$biomass[j] <- data$ap_50.cm[j] * data$a[j] * (mean(c(data$Max.Range_cm[j], data$Min.Range_cm[j])))^data$b[j]
  }
  data$biomass <- data$biomass / 1000
  data
}

remove_rare_species <- function (data){
  colSums(data[,4:ncol(data)] !=0) # count non-zero items in each columns that correspond to fish species
  fish_no_rare<-cbind(data[,c(1,3)], data[,colSums(data[,4:ncol(data)] !=0)>=5]) # Removes columns that correspond to species of fish where there were less than 5 occurrences over the ENTIRE sampling! (bascicaly takes away rare species!!)
  fish_no_rare
}


calc_fish_abund_matrix <- function(data, tax_level = c("Species_Code", "Family", "Functional_Group")){
  # Calculate fish abundance for all fish or specific sizes
  library(reshape2)
  data$group <-data[,tax_level]
  data_sum <- data %>% group_by(group, ID, Site, Subsite, Date, Transect_Type, Sampling_Period, Transect_Number, Transect_Number_Old, Structure_Present, Radiating_Structure) %>%
  summarise(data_abund = sum(Abundance)) %>% as.data.frame %>% dcast(ID + Site + Subsite + Date + Transect_Type + Sampling_Period + Transect_Number + Transect_Number_Old + Structure_Present + Radiating_Structure ~ group) # dcast makes wide
  data_sum
  data_sum[is.na(data_sum)]<-0
  data_sum
}

calc_fish_abund_matrix_new <- function(data, tax_level = c("Species_Code", "Family", "Functional_Group")){
  # Calculate fish abundance for all fish or specific sizes
  library(reshape2)
  data$group <-data[,tax_level]
  data_sum <- data %>% group_by(group, Site, Subsite, Date, Transect_Type, Sampling_Period, Transect_Number) %>%
    summarise(data_abund = sum(Abundance)) %>% as.data.frame %>% dcast(Site + Subsite + Date + Transect_Type + Sampling_Period + Transect_Number ~ group) # dcast makes wide
  data_sum
  data_sum[is.na(data_sum)]<-0
  data_sum
}


calc_fish_biom_matrix <- function(data, tax_level = c("Species_Code", "Family", "Functional_Group")){
  # Calculate fish abundance for all fish or specific sizes
  library(reshape2)
  data$group <-data[,tax_level]
  data_sum <- data %>% group_by(group, ID, Site, Subsite, Date, Transect_Type, Sampling_Period, Transect_Number, Transect_Number_Old, Structure_Present, Radiating_Structure) %>%
  summarise(data_biom = sum(biomass)) %>% as.data.frame %>% dcast(ID + Site + Subsite + Date + Transect_Type + Sampling_Period + Transect_Number + Transect_Number_Old + Structure_Present + Radiating_Structure ~ group) # dcast makes wide
  data_sum
  data_sum[is.na(data_sum)]<-0
  data_sum
}

# calc_fish_biom <- function(data, tax_level = c("Species_Code", "Family", "Functional_Group"), trans_level = c("indiv", "avgd")){
#   # Calculate fish abundance for all fish or specific sizes
#   library(reshape2)
#   data$group <-data[,tax_level]
#   if (trans_level == "indiv")
#     data_sum <- data %>% group_by(group, Site, Date, Transect, Sample) %>%
#     summarise(data_biom = sum(biomass)) %>% as.data.frame %>% dcast(Site + Date + Sample + Transect ~ group) # dcast makes wide
#   if (trans_level == "avgd")
#     data_sum <- data %>% group_by(group, Site, Date, Sample) %>%
#     summarise(data_biom = sum(biomass)) %>% as.data.frame %>% dcast(Site + Date + Sample ~ group) # dcast makes wide
#   data_sum
#   data_sum[is.na(data_sum)]<-0
#   data_sum
# }


subset_data <- function (data, sub_cat, sub_val){
  # Subset fish data by category and it's value of interest
  sub <- data[, sub_cat]
  data_sub <- data[sub == sub_val,]
  data_sub
}

## Benthos ----------------------------------------------------

get_ben_data <- function(file_data){
  # Get fish data for all or specific sizes
  data <- read.table(file = file_data, header = T, sep=',')
  data
}
 
# combine_data_meta is above in fish section

calc_ben_cover <- function(data, tax_level = c("species", "group", "broad_cat", "phyla", "maj_cat", "simple_maj_cat"), trans_level = c("indiv", "avgd", "quad"), start = c(4, 5, 6)){
  # Calculate fish abundance for all fish or specific sizes
  library(reshape2)
  data$group <-data[,tax_level]
  if (trans_level == "indiv")
    #pts_sum <- data %>% group_by (group, Site, Date, Transect, Sample) %>% sum(group)
    data_cov <- data %>% group_by(group, Site, Date, Transect, Sample) %>%
    summarise(data_cov = sum(cover)) %>% as.data.frame %>% dcast(Site + Date + Sample + Transect ~ group)
  if (trans_level == "avgd")
    data_cov <- data %>% group_by(group, Site, Date, Sample) %>%
    summarise(data_cov = sum(cover) ) %>% as.data.frame %>% dcast(Site + Date + Sample ~ group)
  if (trans_level == "quad")
    data_cov <- data %>% group_by(group, Site, Date, Transect, Sample, Quad) %>% 
    summarise(data_cov = sum(cover)) %>% as.data.frame %>% dcast(Site + Date + Sample + Transect + Quad ~ group)
  # find the sum of all points for each unit (quad, transect, or sample)
    start = start # row value for start (quad level is 6, sample is 4, transect is 5)
    cov_ids <- data_cov[, 1:start-1] # row ID values
    cov_vals <- data_cov[,start:ncol(data_cov)] # actual cover values
    pts_sum <- matrix(nrow = nrow(data_cov), ncol=1) # set up matrix to store row sums
    colnames(pts_sum)<-c("pts_sum") # name the matrix's only column as pts_sum
    cov_normalized <- matrix(nrow = nrow(cov_vals), ncol=ncol(cov_vals)) # set up matrix to store the percent cover values that have been normalized by row sums
    i=1 # counter initialization
    j=1 # counter initialization
    for (i in 1:nrow(data_cov)){
      for (j in 1:ncol(cov_vals)){
        pts_sum[i]<-sum(cov_vals[i,start:ncol(cov_vals)])
        cov_normalized[i,j]<-cov_vals[i,j] / pts_sum[i] * 100
        cov_normalized
     }} # double for loop to calculate normalized cover values as the original cover value divided by the total row sum and then multiplied by 100 to give the percent cover value
    ben_cover <- cbind (cov_ids, cov_normalized) # combine row ID values (site, etc) with percent cover values
    colnames(ben_cover)<-colnames(data_cov) # name the columns correctly
    ben_cover # output
}


## Complexity --------------------------------------------------

calc_comp_metrics <- function(data, trans_level = c("indiv", "avgd")){
  if (trans_level == "avgd")
    data_sum <- data %>% group_by(Site, Date, Sample) %>%
    summarize(DRR = mean(DRR), min_dep = mean(min_dep), max_dep = mean(max_dep), avg_dep = mean(avg_dep), C = mean(C), ver_rel = mean(ver_rel_m)) %>% as.data.frame
  data_sum
}

convert_date_format <- function(data){
  
}

## Sediment --------------------------------------------------

calc_sed_metrics <- function(data, trans_level = c("indiv", "avgd")){
  if (trans_level == "indiv")
    data_sum <- data %>% group_by (Site, Date, Sample, Transect) %>%
    summarize(sed_avg = mean(Sediment_depth_cm), sed_max = max(Sediment_depth_cm), sed_min = min(Sediment_depth_cm), sed_stdv = sd(Sediment_depth_cm))
  if (trans_level == "avgd")
    data_sum <- data %>% group_by (Site, Date, Sample) %>%
    summarize(sed_avg = mean(Sediment_depth_cm), sed_max = max(Sediment_depth_cm), sed_min = min(Sediment_depth_cm), sed_stdv = sd(Sediment_depth_cm))
  data_sum
}

## Temperature ------------------------------------------------

calc_temp_metrics <- function(data, trans_level = c("indiv", "avgd")){
  if (trans_level == "avgd")
    data_sum <- data %>% group_by(Site, Date, Sample) %>%
    summarize(temp_avg = mean(Temp_avg), temp_max = mean(Temp_max), temp_min = mean(Temp_min)) %>% as.data.frame
  data_sum
}

## Transform Data ---------------------------------------------

transform_data <- function(data, transform_type = c("square_root", "cube_root", "fourth_root", "pres_abs", "log", "square_root_plus_number", "box-cox", "log + 1")){
  if (transform_type == "square_root")
    data <- sqrt (data)
  if (transform_type == "cube_root")
    data <- data^(1/3)
  if (transform_type == "fourth_root")
    data <- data^(1/4)
  if (transform_type == "pres_abs")
    data <- decostand(data, method = "pa", MARGIN = 2)
  if (transform_type == "log")
    data <- log(data)
  if (transform_type == "square_root_plus_number")
    data <- sqrt(data)+ 0.3 # 0.11
  if (transform_type == "box-cox")####################
   data <- sqrt(data + 0.001) # c = 0.1, lambda = 0.5################
   #trying to make multivariate work
  #if (transform_type == "box-cox")
    #data <- (data + 0.1)^(1/4)
  #if (transform_type == "box-cox")
    #data <- (data + 1) ^ (-0.1414141)
  #if (transform_type == "box-cox")
   # data <- sqrt(data + 1)
  #if (transform_type == "box-cox")
   # data <- (data + 1)^(1/4) # c = 1, lambda = 0.25
  if (transform_type == "log + 1")
    data <- log(1 + data)
  data
}

#### Bitter End ####
