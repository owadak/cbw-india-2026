##Day 2 - Practical Exercise 4

##Hello everyone. Thank you for participating in this workshop.
##Lines that start with a # character represent comments in an R file,
##which means that they are not treated as R code.These lines will all appear
##in pale green text.

##For each of the lines of code in the exercise, you will need to press
##the CTRL and ENTER keys together to execute that line of code.


## ---------------------------------------------------------------------------------------
##
# Getting started with Practical Exercise materials in RStudio
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
#Enter the directory where you extracted this R file
#inside the quotes below.
#Note: You will need to use the / character in the path instead of \
setwd("")


## ---------------------------------------------------------------------------------------
# Load all the required packages for this practical exercise
library(terra)
library(tidyterra)
library(sf)
library(dplyr)
library(biomod2)


## ---------------------------------------------------------------------------------------
##
# Creating a species distribution model (SDM)
##
## ---------------------------------------------------------------------------------------


#Add "_ex4" to the rds filename if you have not created it in Practical Exercise 3
## ---------------------------------------------------------------------------------------
maharashtra_districts <- st_read("data/Maharashtra_Districts_WGS1984.shp")
occ_species_sf <- readRDS("data/gbif_anser_indicus_maharashtra.rds")
predictors <- rast('data/sdm_inputs.tif')


## ---------------------------------------------------------------------------------------
occ_coordinates <- st_coordinates(occ_species_sf)
occ_df <- data.frame(lat = occ_coordinates[,'Y'],
                     lon = occ_coordinates[,'X'],
                     presence = 1)


## ---------------------------------------------------------------------------------------
species_name <- 'Anser indicus'
myBiomodData.r <- BIOMOD_FormatingData(resp.var = occ_df$presence,
                                       expl.var = predictors,
                                       resp.xy = occ_df[, c('lon', 'lat')],
                                       resp.name = species_name,
                                       PA.nb.rep = 1,
                                       PA.nb.absences = 1000,
                                       PA.strategy = 'disk',
                                       filter.raster = TRUE)


## ---------------------------------------------------------------------------------------
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData.r,
                                    modeling.id = 'AllModels',
                                    models = c("GBM" , "XGBOOST", "MAXNET", "RF"),
                                    CV.strategy = 'random',
                                    CV.nb.rep = 2,
                                    CV.perc = 0.8,
                                    var.import = 3,
                                    metric.eval = c('TSS','AUCroc'),
                                    seed = 42)


## ---------------------------------------------------------------------------------------
bm_PlotEvalMean(myBiomodModelOut, dataset='calibration')
bm_PlotEvalMean(myBiomodModelOut, dataset='validation')


## ---------------------------------------------------------------------------------------
bm_PlotVarImpBoxplot(bm.out = myBiomodModelOut, group.by = c('expl.var', 'algo', 'algo'))


## ---------------------------------------------------------------------------------------
##
# Creating an ensemble SDM
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      models.chosen = 'all',
                                      em.by = 'all',
                                      em.algo = c('EMmean'),
                                      metric.eval = c('TSS', 'AUCroc'),
                                      var.import = 3,
                                      seed = 42)


## ---------------------------------------------------------------------------------------
get_evaluations(myBiomodEM)


## ---------------------------------------------------------------------------------------
bm_PlotVarImpBoxplot(bm.out = myBiomodEM, group.by = c('algo', 'expl.var', 'merged.by.run'))


## ---------------------------------------------------------------------------------------
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM',
                                             new.env = predictors,
                                             models.chosen = 'all',
                                             metric.binary = 'all',
                                             metric.filter = 'all')


## ---------------------------------------------------------------------------------------
em_raster <- get_predictions(myBiomodEMProj)

ggplot() +
  geom_spatraster(data = em_raster[[2]]) +
  scale_fill_viridis_c(na.value = NA)+
  geom_sf(data = occ_species_sf, color = "red") +
  labs(title=paste0("SDM Ensemble Model and GBIF occurrence data for ", species_name),
       x = "Longitude", y = "Latitude")


## ---------------------------------------------------------------------------------------
# END OF PRACTICAL EXERCISE 4
## ---------------------------------------------------------------------------------------