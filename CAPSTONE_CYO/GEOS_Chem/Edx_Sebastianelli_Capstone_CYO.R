###############################################################################
##################                                       ######################
################## Introduction to GEOS-Chem Software    ######################
##################                                       ######################
###############################################################################

## Overview:
# From this Link: http://acmg.seas.harvard.edu/geos/geos_overview.html
#
# # GEOS-Chem enables simulations of atmospheric composition on local to global 
# scales. It can be used off-line as a 3-D chemical transport model driven by 
# assimilated meteorological observations from the Goddard Earth Observing System 
# (GEOS) of the NASA Global Modeling Assimilation Office (GMAO) . 
# It can also be used on-line as a chemical module coupled to weather and climate models. 
# GEOS-Chem is developed and used by hundreds of research groups worldwide as a 
# versatile tool for application to a wide range of atmospheric composition problems. 
# It is open-access and can be downloaded through github. It is also fully supported 
# for use on the Amazon Web Services cloud.

# From GEOS-Chem Wiki: 
# http://wiki.seas.harvard.edu/geos-chem/index.php/Main_Page
#
# GEOS-Chem Community MISSION: to advance understanding of human and natural 
# influences on the environment through a comprehensive, state-of-the-science, 
# readily accessible global model of atmospheric composition.

###############################################################################
###################                                    ########################
###################          FILES DESCRIPTION         ########################
###################                                    ########################
###############################################################################

# # The .nc4 files used to develop this script are available at the following 
#   link: 
#
#   https://data.aad.gov.au/metadata/records/AAS_4431_CAMMPCAN_GEOS_Chem_Model_AA_2017-18
#   
#   Citation: Fisher, J., Sebastianelli, P., Schofield, R. (2021) 
#   GEOS-Chem model output for 2017-2018 CAMMPCAN Aurora Australis voyages, 
#   Ver. 1, Australian Antarctic Data Centre - doi:10.26179/hna9-ab45, 
#   Accessed: 2021-07-27
#
## Brief description
#
# The dataset provides model output from the GEOS-Chem chemical transport model 
# to support the CAMMPCAN and MARCUS 2017-2018 voyages.
#
# The following output types are included (for variable names, see below):
#
# Regional daily means
#
# Filenames: GEOSChem.{DATA_TYPE}.{YYYYMM}01_regional.nc4
# {DATA_TYPE} can be any of SpeciesConc, StateMet, Aerosols, AerosolMass (see below).
# {YYYYMM} is the year and month for the data included in the file.
# The model output has been averaged on a daily timescale and is provided for 
# all gridboxes in the region bounded by 30-90°S (inclusive). 
# The resulting dataset is 4-dimensional (longitude, latitude, level, time). 
# These are netcdf files.
#  
# Output data types (correspond to filenames given above):
# 1. SpeciesConc: Concentrations of advected model species.
# 2. StateMet: Meteorological fields and other derived quantities.
# 
# References:
# The International GEOS-Chem User Community. (2020, May 21). 
# geoschem/geos-chem: GEOS-Chem 12.8.1 (Version 12.8.1). 
# Zenodo. http://doi.org/10.5281/zenodo.3837666
# 
# Travis, K. R., Heald, C. L., Allen, H. M., Apel, E. C., Arnold, S. R., Blake, 
# D. R., Brune, W. H., Chen, X., Commane, R., Crounse, J. D., Daube, B. C., Diskin, 
# G. S., Elkins, J. W., Evans, M. J., Hall, S. R., Hintsa, E. J., Hornbrook, R. S., 
# Kasibhatla, P. S., Kim, M. J., Luo, G., McKain, K., Millet, D. B., Moore, 
# F. L., Peischl, J., Ryerson, T. B., Sherwen, T., Thames, A. B., Ullmann, K., 
# Wang, X., Wennberg, P. O., Wolfe, G. M., and Yu, F.: 
# Constraining remote oxidation capacity with ATom observations, 
# Atmos. Chem. Phys., 20, 7753-7781, https://doi.org/10.5194/acp-20-7753-2020, 2020.
#

# Resource provider AU/AADC > Australian Antarctic Data Centre, Australia
# Publisher Australian Antarctic Data Centre

# Related People
# Processor, Principal investigator FISHER, JENNY
# Processor, Principal investigator SCHOFIELD, ROBYN
# Processor, Principal investigator SEBASTIANELLI, PAOLO
# Originator Australia
# Originator Australian Antarctic Division

###############################################################################
###################                                    ########################
###################          LICENSE                   ########################
###################                                    ########################
###############################################################################

# This data set conforms to the CCBY Attribution License
# (http://creativecommons.org/licenses/by/4.0/).
# Please follow instructions listed in the citation reference provided
# at http://data.aad.gov.au/aadc/metadata/citation.cfm?entry_id=AAS_4431_CAMMPCAN_GEOS_Chem_Model_AA_2017-18
# when using these data.
# Please also contact Jenny Fisher at the University of Wollongong before
# using these data. Jennyf at uow dot edu dot au


###############################################################################
########################                            ###########################   
########################   SCRIPT IN R - START      ###########################
########################                            ###########################
###############################################################################

#### These are the packages I need to explore and manipulate the data set 

if(!require(tidyverse))install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ncdf4))install.packages("ncdf4")("ncdf4", repos = "http://cran.us.r-project.org")
if(!require(raster))install.packages("raster")("raster", repos = "http://cran.us.r-project.org")
if(!require(rgdal))install.packages("rgdal")("rgdal", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))install.packages("ggplot2")("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(RNetCDF))install.packages("RNetCDF")("RNetCDF", repos = "http://cran.us.r-project.org")
if(!require(tidync))install.packages("tidync")("tidync", repos = "http://cran.us.r-project.org")
if(!require(maps))install.packages("maps")("maps", repos = "http://cran.us.r-project.org")
if(!require(devtools))install.packages("devtools")("devtools", repos = "http://cran.us.r-project.org")
if(!require(stars))install.packages("stars")("stars", repos = "http://cran.us.r-project.org")
if(!require(dplyr))install.packages("dplyr")("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr))install.packages("tidyr")("tidyr", repos = "http://cran.us.r-project.org")
if(!require(readr))install.packages("readr")("readr", repos = "http://cran.us.r-project.org")
if(!require(RCurl))install.packages("RCurl")("RCurl", repos = "http://cran.us.r-project.org")
if(!require(rgeos))install.packages("rgeos")("rgeos", repos = "http://cran.us.r-project.org")
if(!require(glmnet))install.packages("glmnet")("glmnet", repos = "http://cran.us.r-project.org")
if(!require(ranger))install.packages("ranger")("ranger", repos = "http://cran.us.r-project.org")
if(!require(randomForest))install.packages("randomForest")("randomForest", repos = "http://cran.us.r-project.org")
if(!require(kernlab))install.packages("kernlab")("kernlab", repos = "http://cran.us.r-project.org")
if(!require(e1071))install.packages("e1071")("e1071", repos = "http://cran.us.r-project.org")

###If needed

# devtools::install_github("ropenscilabs/rnaturalearth")
# install.packages("rnaturalearthhires",
#                  repos = "http://packages.ropensci.org",
#                  type = "source")


#### These are the libraries I need to explore and manipulate the data set

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
# library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(RNetCDF)
library(tidync)
library(fields)
library(maps)
library(dplyr)
library(ncmeta)
library(viridis)
library('rnaturalearth')
library("rnaturalearthdata")
# library("rnaturalearthhires")
library(tidyverse)
library(dslabs)
library(caret)
library(randomForest)
library(rpart)
library(matrixStats)
library(corrplot)
library(ggcorrplot)
library(Rborist)
library("plot3D")
library(ranger)
library(xgboost)
library(plyr)
library(readr)
library(RCurl)
library(rgeos)
library(glmnet)
library(kernlab)
library(e1071)

##### Look at the package you have installed on your system
search()

# ###################################### PART ONE ############################
# ### All this "Part ONE" is about the nc4 file opening procedure and the   ##
# ### extraction of data from the netCDF file with the aim of building a    ##
# ### new dataframe, if the the nc4 files are on the local system. 
##### 
##### NOTE: Since the nc4 files are really big (more than 5Gb) in some 
#####       case I processed them to obtain a csv that can be read directly
#####       from my github repository. 
#####
#####       Link: https://raw.githubusercontent.com/PaoloSebas/
#####                    DATA_SCIENCE/main/GC_tot_Dat3.txt
###
# ############################################################################
# 
# ### OPENING DATA FILE using **nc_open** function from [ncdf4] library
# ### This step is done to extract the values of latitude and longitude
# ### and storing them in th object "Data_ncdf4_type"
# 
# Data_ncdf4_type <- nc_open('GEOSChem.SpeciesConc.20180301_regional.nc4')
# class(Data_ncdf4_type)  ### "ncdf4"
# 
# #### Extracting the values for longitude and latitude using ncvar_get function 
# #### from ncdf4 package. Storing them in two arrays
# 
# lon <- ncvar_get(Data_ncdf4_type, "lon")  ### between -180 and 177.5 degrees
# lat <- ncvar_get(Data_ncdf4_type, "lat")  ### between -89.5 and -30.0  degrees
# 
# ##############################################################################
############
# ## OPTIONAL ##################################################################
############
# ###### If one wants it is possible subsetting the longitude and latitude values
# ###### using indexes., In this example a piece of the world map is taken between 
# ###### Australia and Antartic
# #
# # lonIdx <- which(Data_ncdf4_type$dim$lon$vals > 100 & 
# #                 Data_ncdf4_type$dim$lon$vals < 170)
# # latIdx  <- which(Data_ncdf4_type$dim$lat$vals > -90 & 
# #                 Data_ncdf4_type$dim$lat$vals < 0)
# #
# # lon <- ncvar_get(Data_ncdf4_type, 
# #                 "lon", start=c(min(lonIdx)), 
# #                  count=c((max(lonIdx)-min(lonIdx)+1)))  
# # nlon <- dim(lon)  
# # nlon # 27
# #
# # lat <- ncvar_get(Data_ncdf4_type, "lat", 
# #                  verbose = F, start=c(min(latIdx)), 
# #                  count=c((max(latIdx)-min(latIdx)+1)))
# # nlat <- dim(lat)  
# # nlat # 45
# #############################################################################
# 
# ### Tidync package is a better choice to extract the data one wants
# ### Opening data file using functions from tidync library
# 
# ### Source file for CHEMICAL SPECIES CONCENTRATION values
# 
# source <- tidync('GEOSChem.SpeciesConc.20180301_regional.nc4')
# class(source)
# 
# ### METEREOLOGICAL STATE description
# 
# state_met <- tidync('GEOSChem.StateMet.20180301_regional.nc4')
# 
# ################################
# ###    SOURCE - DESCRIPTION ####
# ################################
# 
# class(source)  ### "tidync"
# 
# print(source2)
# 
# ### There are 5 dimension, 4 of them are active: 
# ### (D0) time, (D1) lon, (D2) lat, (D3)lev (level)
# ### The time spans 31 days of simulation
# ### There are 47 levels to be analyzed, starting from the world surface
# ### 31 latitudes points from -89.5 to 30 degrees
# ### 144 longitudes points from -180 to 177.5
# ### 203 variables
# ### 4464 observations (144*31)
# 
# ####################################
# ###    STATE MET  - DESCRIPTION ####
# ####################################
# 
# print(state_met)
# 
# ### There are 5 dimension, 4 of them are active: 
# ### (D0) time, (D1) lon, (D2) lat, (D3)lev (level)
# ### The time spans 31 days of simulation
# ### There are 47 levels to be analyzed, starting from the world surface 
# ### 31 latitudes points from -89.5 to 30 degrees
# ### 144 longitudes points from -180 to 177.5
# ### 36 variables
# ### 4464 observations (144*31)
# 
# ### Activating, again, the dimensions that are of my interest (just in case)
# 
# source <- tidync('GEOSChem.SpeciesConc.20180301_regional.nc4') %>% 
#                  activate("D1,D2,D3,D0")
# state_met <- tidync('GEOSChem.StateMet.20180301_regional.nc4') %>% 
#                     activate("D1,D2,D3,D0")
# 
# #############################################################################
# #############                                           #####################
# #############           SLICING DATA                    #####################
# #############################################################################
# 
# ### The data set is sliced to obtain information
# ### about the "surface" level (lev = 0.992) during the "first" (time = 720) 
# ### day of simulation
# 
# source_slice <- source %>% 
#                 hyper_filter(time = time == 7.2e+2, 
#                             lev = lev > 0.980, 
#                             lat =  dplyr::between(index,1, 31), 
#                             lon =  dplyr::between(index, 1, 144))
# state_met_slice <- state_met %>% 
#                   hyper_filter(time = time == 7.2e+2, 
#                                lev = lev > 0.980, 
#                               lat =  dplyr::between(index, 1, 31), 
#                                lon =  dplyr::between(index, 1, 144))
# 
# ### The data set is sliced to obtain information
# ### about the "surface" level (lev = 0.992) during the "second" (time = 2160) 
# ### day of simulation
# 
# source_slice2 <- source %>% 
#                   hyper_filter(time = time == 2160  , 
#                   lev = lev > 0.980, 
#                   lat =  dplyr::between(index, 1, 31), 
#                   lon =  dplyr::between(index, 1, 144))

# state_met_slice2 <- state_met %>% 
#                     hyper_filter(time = time == 2160, 
#                     lev = lev > 0.980, 
#                     lat =  dplyr::between(index, 1, 31), 
#                     lon =  dplyr::between(index, 1, 144))
# 
# ### If you want, you can check the time values with this commented cod3
# ##  Data_ncdf4_type$dim$time
# 
# #### In case of subsetting one can change lat and lon 
# # source_slice <- source %>% 
#                 hyper_filter(time = time == 7.2e+2, 
#                 lev = lev > 0.980, 
#                 lat =  dplyr::between(index, 1, 45), 
#                 lon =  dplyr::between(index, 114, 140))
# 
# ### If you want see the difference between source and source_slice:
# # print(source_slice)
# # print(state_met_slice)
# 
# #### CREATING DATA FRAMES FROM TIDYNC OBJECTS 
# #### using hyper_tibble() function from the package tidync
# 
# src_slc_dataframe <- source_slice %>% hyper_tibble()
# stm_slc_dataframe <- state_met_slice %>% hyper_tibble()
# 
# # src_slc_dataframe2 <- source_slice2 %>% hyper_tibble()
# # stm_slc_dataframe2 <- state_met_slice2 %>% hyper_tibble()
# 
# ### Binding the data frames. 
# ### They share the same lat, lon, time and lev values.
# 
# total_dataframe <- cbind(src_slc_dataframe, 
#                          stm_slc_dataframe, deparse.level = 1)
# total_dataframe2 <- cbind(src_slc_dataframe2, 
#                          stm_slc_dataframe2, deparse.level = 1)
# # 
# #### If you want transform the tidync object in an array you can use 
#     hyper_array() function
# #### src_slc_array <- source_slice %>% hyper_array()
# 
# ###Check if there are duplicated columns
# 
# duplicated(t(total_dataframe)) #lon, lat and lev are a duplicated column
# 
# ## Eliminating duplicated columns 
# total_dataframe <- total_dataframe[!duplicated(as.list(total_dataframe))]  ### Eliminating lon,lat,lev duplicated columns
# 
# # total_dataframe2 <- total_dataframe2[!duplicated(as.list(total_dataframe2))]  ### Eliminating lon,lat,lev duplicated columns
# 
# #### Are they columns with NA values?
# 
# names(which(sapply(total_dataframe, anyNA)))  #### None
# 
# write.table(total_dataframe,"GC_tot_Dat3.txt",sep="\t",row.names=FALSE)


###############################################################################
#################                                                  ############
#################  STARTING FROM HERE WE CAN WORK ON THE CSV FILE  ############
#################                                                  ############
###############################################################################

##### Loading data from the csv that is in my github page
##### 

url <- "https://raw.githubusercontent.com/PaoloSebas/DATA_SCIENCE/main/GC_tot_Dat3.txt"
dest_file <- "GC_tot_Dataframe.txt"
download.file(url, destfile = dest_file)
GC_total_dataframe <- read.table(header = TRUE, dest_file)

#### Are they columns with 0 variance? 

nzv <- nearZeroVar(GC_total_dataframe)
col_index <- setdiff(1:ncol(GC_total_dataframe), nzv)
print(nzv)

#### Finding the names of the columns that nearzerovar proposes to drop

colnames(GC_total_dataframe)[nzv]   

#### Filtering the total_dataframe

drops <- c("lev","time", "FracOfTimeInTrop", "Met_DTRAIN","SpeciesConc_H1301", "SpeciesConc_CFC114" )
total_dataframe_filtered <- GC_total_dataframe[ , !(names(GC_total_dataframe) %in% drops)]

##### DATA ANALYSIS

##### Sulfur Dioxide - Study 

##### The SO2 concentration (the outcome) want to be predicted 
##### using the concentration values of the other chemical species  
##### and the meteorological parameters (the predictors).

# #### Each grid cell is one training sample.

###    There are 234 features:
###    The 203 species concentrations + the remaining meteorological variables.
### The analysis has been restricted to the surface (lowest model level) 
### to facilitate the analysis.
### Calculating (long×lat×lev=144×31×1) = 4464 training samples.
### To build the validation data set a randomly selected 
### 10% of the locations has been withdrawn.

### Storing in a vector the SO2 values from the total_dataframe_filtered
SO2_total <- total_dataframe_filtered$SpeciesConc_SO2

### Calculating the mean value at surface level
SO2_mean <- mean(SO2_total)
SO2_mean  

### The mean Dry mixing ratio of species SO2 
### is 2.577628e-11 mol*mol-1 dry air or 25 ppb (parts per billion)

### Visualizing the distribution of SO2 values.
### Effectively what is seen is that the distribution 
### is centered near the values x*e-11

qplot(log10(SO2_total), bins = 100, color = I("black"), 
      main="Distribution of SO2 concentration",
      xlab="SO2 conc - log scale", ylab="# of occurences")

### According the longitude 

total_dataframe_filtered %>% ggplot(aes(x=lon, y = SpeciesConc_SO2)) +
  geom_point() +
  ggtitle("SO2: concentration") +
  labs(x="Longitude", y = "SO2 concentration") +
  geom_line(aes(lon, SO2_mean), col="red")

### According the latitude 

total_dataframe_filtered %>% ggplot(aes(x=lat, y = SpeciesConc_SO2)) +
  geom_point() +
  ggtitle("SO2: concentration") +
  labs(x="Latitude", y = "SO2 concentration") +
  geom_line(aes(lat, SO2_mean), col="red")

### Total 3D visualization 

scatter3D(total_dataframe_filtered$lon,total_dataframe_filtered$lat, total_dataframe_filtered$SpeciesConc_SO2,colvar = NULL, col = "blue",
          pch = 19, cex = 0.5, margin = 0.1) 

### Following plot shows a picture of the 2D SO2 concentration 
### Creating an object "world" using ne_countries() from rnaturalearth library

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf() +
  geom_point(data = total_dataframe_filtered, aes(x = lon, y = lat, size = SpeciesConc_SO2), alpha = 4/10) +
  coord_sf(xlim = c(-180, 180), ylim = c(-89.5,89.5), expand = FALSE)+
  theme_bw()

#### Where is the maximum [SO2] concentration?

### The maximum value of [SO2] is at -30° latitude and 30° longitude
##  Ingwe Local Municipality, South Africa

which.max(total_dataframe_filtered$SpeciesConc_SO2)  ### row 4405
total_dataframe_filtered$lat[4405] # -30
total_dataframe_filtered$lon[4405] # 30 

##############################################################
######
######    CORRELATION BETWEEN VARIABLES
######
##############################################################

sum(is.na(total_dataframe_filtered))  ### no NA value in total_dataframe

### Determining correlation between variables in train_set
corr <- cor(total_dataframe_filtered)

### Plotting the correlation matrix
### You have to export the image at high resolution if you want 
### appreciate the details. 
### The matrix is as png in my github page
### link: 

ggcorrplot(corr,  outline.col = "white") +
  theme(text = element_text(size = 6),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

### Since we are interested how SO2 is correlated with the others
### Storing of the data for SO2

SO2_corr <- as.data.frame(corr['SpeciesConc_SO2',])
colnames(SO2_corr) <- c("Correlation")

####Plotting the result
ggplot(SO2_corr, aes(x = row.names(SO2_corr), y = Correlation)) + 
  geom_point() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#### Filtering with correlation > 0.55
SO2_corr_filtered <- SO2_corr %>% filter(Correlation > 0.55)
ggplot(SO2_corr_filtered, aes(x = row.names(SO2_corr_filtered), y = Correlation)) + 
  geom_point() +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

### Storing the variables for future studies 
rownames_SO2 <- SO2_corr_filtered %>% row.names()
view(rownames_SO2)


##############################################################################
#########                                                       ##############
#########                 MACHINE LEARNING                      ##############
##############################################################################

### Generating an 'outcome' object for the createDataPartition() function
### total_dataframe_filtered will be partitionated (90:10) in two subsets
### the train_set and test_set

outcome <- total_dataframe_filtered$SpeciesConc_SO2
set.seed(123)
test_index <- createDataPartition(outcome, times = 1, p = 0.1, list = FALSE) 
## p = 0.1 means 10% to the test_set

## Building the train and test data sets

test_set <- total_dataframe_filtered[test_index, ]
train_set <- total_dataframe_filtered[-test_index, ]

#### Metrics for comparison: RMSE error and NRMSE
### NMRSE will be RMSE / standard deviation of [SO2] in train_set

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### Calculating the RMSE associated at the "pure average" algorithm
RMSE_average <- RMSE(test_set$SpeciesConc_SO2, SO2_mean) 
RMSE_average
### 2.627619e-11 the error is bigger than the mean value

SO2_sd_train <- sd(train_set$SpeciesConc_SO2)  

### Normalized RMSE on the train sd value 
NRMSE_average <- (RMSE_average/SO2_sd_train)*100 
### This is not a good result: 49% far from the reality

#################  IMPLEMENTING REGRESSION ALGORITHMS #########################

### Being CONTINOUS VARIABLE what is needed is a REGRESSION ALGORITHM

### LINEAR REGRESSION
#### Training with caret train() function a "lm" model 

### Repeated cross-validation (3 times)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
set.seed(seed)

fit_lm <- train(SpeciesConc_SO2 ~ ., 
                data = train_set, 
                method="lm", 
                preProcess = c("center","scale"),
                trControl=control)

print(fit_lm)

###Predicting with lm 
y_clm = predict(fit_lm, newdata = test_set)
print(y_clm)

RMSE(y_clm, test_set$SpeciesConc_SO2)

### Normalized RMSE (on standard deviation)
NRMSE_lm <- (RMSE(y_clm, test_set$SpeciesConc_SO2)/SO2_sd_train)*100  ### 25,48%

####Plotting the result

test_set %>%
  ggplot() +
  geom_point(aes(x = seq_along(SpeciesConc_SO2), SpeciesConc_SO2)) +
  geom_line(aes(seq_along(SpeciesConc_SO2), y_clm), col="red")

#### Training a glmnet model from the caret package 

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "RMSE"
set.seed(seed)

fit_glmnet <- train(SpeciesConc_SO2 ~ . , 
                         data=train_set, 
                         method="glmnet", 
                         trControl=control, 
                         preProcess = c("center","scale"),
                         do.trace=TRUE)

print(fit_glmnet)
plot(fit_glmnet)


#### Predicting with glmnet
y_glmnet = predict(fit_glmnet, newdata = test_set)

####Plotting the result

test_set %>%
  ggplot() +
  geom_point(aes(x = seq_along(SpeciesConc_SO2), SpeciesConc_SO2)) +
  geom_line(aes(seq_along(SpeciesConc_SO2), y_glmnet), col="red")

##### NRMSE for ridge 
NRMSE_glmnet <- (RMSE(y_glmnet,test_set$SpeciesConc_SO2)/SO2_sd_train)*100

#### RANDOM FOREST PACKAGE
#### TUNING RF 

seed <- 7
metric <- "RMSE"
set.seed(seed)

fit_rf <- randomForest(SpeciesConc_SO2 ~ ., 
                       data=train_set, 
                       ntree = 55,  
                       mtry = 72,
                       preProcess = c("center","scale"),
                       do.trace = TRUE)
plot(fit_rf)
print(fit_rf)

y_rf = predict(fit_rf, newdata = test_set)

##### NRMSE for random forest

NRMSE_rf <- (RMSE(y_rf,test_set$SpeciesConc_SO2)/SO2_sd_train)*100 ### 18.25%

### TUNING ranger

fit_ranger <- ranger(SpeciesConc_SO2 ~ ., 
                     data = train_set,
                     num.trees = 55,
                     mtry = 71
                )

y_ranger <- predict(fit_ranger, data = test_set)

##### NRMSE for random forest

NRMSE_ranger <- (RMSE(y_ranger$predictions,test_set$SpeciesConc_SO2)/SO2_sd_train)*100

### Visualizing the result 

test_set %>%
  ggplot() +
  geom_point(aes(x = seq_along(SpeciesConc_SO2), SpeciesConc_SO2)) +
  geom_line(aes(seq_along(SpeciesConc_SO2), y_ranger), col="red")

#### Variable importance 

imp <- varImp(fit_rf)
which.max(imp$Overall)
imp[which.max(imp$Overall),]  ### SpeciesConc_pFe      3.319607e-18

### END

