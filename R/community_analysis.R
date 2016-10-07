## Analysis scripts

## Community metrics ----------------------------------------------------------------------
calc_comm_metrics <- function(data, env_data, biom_data){
  S <- specnumber(data) # species richness (S)
  shan <- diversity (data, index = "shannon") # shannon diversity
  simp <- diversity (data, index = "simpson") # simpson's diversity
  simp_inv <- diversity (data, index = "invsimpson") # inverse simpson's diversity
  J <- shan / log (S) # pielou's evenness (J)
  abund <- rowSums (data, na.rm = FALSE, dims = 1) # total abundance for each sample
  biom <- rowSums (biom_data, na.rm = FALSE, dims = 1) # totaal biomass for each sample
  comm_mets <- cbind (abund, biom, S, shan, simp, simp_inv, J, env_data)  
  comm_mets
}

## Nonmetric multidimensional scaling -------------------------------------

calc_nms_ord <- function (species.data){
  set.seed(318) # for reproducibility
  species.bcd<-bcdist(species.data, rmzero=FALSE) # build distance matrix based on bray curtis
  species.nms.step <- nmds(species.bcd, nits=10, mindim=1, maxdim=6) # originally 10 nits and 6 dims; step down procedure to generate multiple ordinations; 6 sets of 10 ranging from 1dims to 6dims
  # Get stress values as a matrix
  species.nms.stress <- matrix(species.nms.step$stress, nrow=6, byrow=T)
  nms.stress.mean <- apply(species.nms.stress, 1, "mean")
  nms.stress.min <- apply(species.nms.stress, 1, "min")
  nms.stress.max <- apply(species.nms.stress, 1, "max")
  # Scree plot to find proper dimensions
  plot(1:6, nms.stress.mean, type="b", pch=19, lwd=2, xlab="Dimensions", ylim=c(0, 0.7), ylab="Stress")
  lines(1:6, nms.stress.min, type="l", lty=2)
  lines(1:6, nms.stress.max, type="l", lty=2)
  title("NMS Stepdown")
  # R2 values
  species.nms.r2 <- matrix(species.nms.step$r2, nrow=6, byrow=T)
  nms.r2.mean <- apply(species.nms.r2, 1, "mean")
  nms.r2.min <- apply(species.nms.r2, 1, "min")
  nms.r2.max <- apply(species.nms.r2, 1, "max")
  # Scree plot with R2
  plot(1:6, nms.r2.mean, type="b", pch=19, lwd=2, xlab="Dimensions", ylim=c(0, 1.0), ylab="R-squared")
  lines(1:6, nms.r2.min, type="l", lty=2)
  lines(1:6, nms.r2.max, type="l", lty=2)
  title("NMS Stepdown") #should this be labled R-squared Values?
  # Final ordination configuration
  species.nmds <- nmds(species.bcd,mindim=2,maxdim=2,nits=20) # 20 reps and 2 dimensions
  minso<-which.min(species.nmds$stress)
  minso # minimum stress ordination number
  stress <- min(species.nmds$stress) # minimum stress value
  species.nms <- nmds.min(species.nmds) # gives minimum stress and r2 value for best ordination
  # Rotate the ordination (forcing axis 1 to have most variance)
  nms.pca<-princomp(species.nms)
  print(nms.pca)
  summary(nms.pca) # gives % variation on each axis out of % variance captured in ordination (not out 100%)
  species.nms<-nms.pca$scores
  colnames(species.nms) <- c("NMS1", "NMS2")
  # Shepard diagram to check if linear 
  nms2.xod<-dist(species.nms)
  plot(nms2.xod,species.bcd,pch="*",xlab="Ordination Distance", ylab="Extended B-C Distance")
  abline(0,1,col="red") # put in the 1:1 line (intercept=0, slope=1)
  title("Shepard Diagram")
  # Get R2 values 
  nms.xod1 <- dist(species.nms[,1]) # pearson correlation 
  nms.xod2 <- dist(species.nms[,1:2]) # pearson correlation
  r1<-cor(species.bcd,nms.xod1)
  r2.1<-r1^2; r2.1
  # axis 2 is 2-D minus 1-D solution:
  r2<-cor(species.bcd,nms.xod2)
  r2.2<-r2^2; 
  r2.2<-r2.2-r2.1
  # Print what we need for next steps
  species.nms<-cbind(species.nms, env_data)
  returnlist = list(species.nms, r2.2, r2.1, stress) # so these things are stored as [[1]] through [[3]] in output object
}

corr_nms_with_env <- function(species.nms, env_data){
  corenv <- cor2m(species.nms, env_data)
  corenv
} 

get_wght_avg_spec <- function (species.nms, species.data){
  set.seed(318)
  species.wa <- wascores (species.nms[,1:2], species.data) # combine nms scores and species data
  species.wa <- as.data.frame(species.wa) # convert to a data frame for plotting
  species.wa <- cbind(species_code = rownames(species.wa), species.wa) # make species codes into column
  species.wa # return data frame of interest
}

get_env_vectors <- function (species.nms, env_data){
  nums <- sapply(env_data, is.numeric)
  env_num <- env_data[, nums] # select only numeric values
  vf <- envfit(species.nms[,1:2], env=env_num, alpha = 0.05) # may need to add na.rm=TRUE.... 
  pvals <- vf$vectors$pvals # extract pvals
  vect.vf <- as.data.frame(scores(vf, display = "vectors"))
  vect.vf <- cbind(vect.vf, Env = rownames(vect.vf), pvals)
  vect.vf <- subset(vect.vf, pvals<0.05) # subset to get only significant vectors with pvals < 0.05
  vect.vf
}

get_convex_hulls_reefs <- function(species.nms){
  grp.art <- species.nms[species.nms$Reef_Type == "Artificial",][chull(species.nms[species.nms$Reef_Type == "Artificial", c("NMS1", "NMS2")]), ]  # hull values for grp A
  grp.nat <- species.nms[species.nms$Reef_Type == "Natural", ][chull(species.nms[species.nms$Reef_Type == "Natural", c("NMS1", "NMS2")]), ]  # hull values for grp B
  hull.data <- rbind(grp.art, grp.nat)  #combine grp.a and grp.b
  hull.data
}

get_convex_hulls_depth <- function(species.nms){
  grp.shall <- species.nms[species.nms$Depth_general == "Shallow",][chull(species.nms[species.nms$Depth_general == "Shallow", c("NMS1", "NMS2")]), ]  # hull values for grp A
  grp.inter <- species.nms[species.nms$Depth_general == "Intermediate", ][chull(species.nms[species.nms$Depth_general == "Intermediate", c("NMS1", "NMS2")]), ]  # hull values for grp B
  grp.deep  <- species.nms[species.nms$Depth_general == "Deep", ][chull(species.nms[species.nms$Depth_general == "Deep", c("NMS1", "NMS2")]), ]  # hull values for grp B
  hull.data <- rbind(grp.shall, grp.inter, grp.deep)  #combine grps
  hull.data
}


## Summary stats to use in bar plots ----------------------------------------

get_summary_data_reef_type <- function (data, env_data){
  sums<-rowSums(data, na.rm=FALSE, dims = 1)
  data_sum <- cbind(env_data, sums)
  data_summary<-summarySE (data_sum, 
                          measurevar="sums", 
                          groupvars = "Reef_Type")
  data_summary$Reef_Type <- as.factor(data_summary$Reef_Type)
  returnlist = list(data_summary, data_sum) # so these things are stored as [[1]] through [[2]] in output object
}

get_summary_data_depth <- function (data, env_data){
  sums<-rowSums(data, na.rm=FALSE, dims = 1)
  data_sum <- cbind(env_data, sums)
  data_summary<-summarySE (data_sum, 
                           measurevar="sums", 
                           groupvars = "Depth_general")
  data_summary$Depth_general <- as.factor(data_summary$Depth_general)
  returnlist = list(data_summary, data_sum) # so these things are stored as [[1]] through [[2]] in output object
}

get_summary_data_reef_type_depth <- function (data, env_data){
  sums<-rowSums(data, na.rm=FALSE, dims = 1)
  data_sum <- cbind(env_data, sums)
  data_summary<-summarySE(data_sum, 
                          measurevar="sums", 
                          groupvars = c("Reef_type", "Depth_general"))
  data_summary$Reef_type <- factor(data_summary$Reef_type)
  data_summary$Depth_general <- factor(data_summary$Depth_general)
  returnlist = list(data_summary, data_sum) # so these things are stored as [[1]] through [[2]] in output object
}


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
  ## Summarizes data.--- from R COOKBOOK http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
  ## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
  ##   data: a data frame.
  ##   measurevar: the name of a column that contains the variable to be summariezed
  ##   groupvars: a vector containing names of columns that contain grouping variables
  ##   na.rm: a boolean that indicates whether to ignore NA's
  ##   conf.interval: the percent range of the confidence interval (default is 95%)
}

