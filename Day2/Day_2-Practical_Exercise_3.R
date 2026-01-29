##Day 2 - Practical Exercise 3

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
library(sf)
library(rgbif)
library(dplyr)
library(ggplot2)


## ---------------------------------------------------------------------------------------
##
# Retrieving input data
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
# Extract extent of Maharashtra state
maharashtra_districts <- st_read("data/Maharashtra_Districts_WGS1984.shp")
maharashtra_extent <- ext(maharashtra_districts)

wkt_polygon <- paste0("POLYGON((",
                      maharashtra_extent[1], " ", maharashtra_extent[3], ", ",
                      maharashtra_extent[2], " ", maharashtra_extent[3], ", ",
                      maharashtra_extent[2], " ", maharashtra_extent[4], ", ",
                      maharashtra_extent[1], " ", maharashtra_extent[4], ", ",
                      maharashtra_extent[1], " ", maharashtra_extent[3], "))")


## ---------------------------------------------------------------------------------------
species_name <- 'Anser indicus'
gbif_data_species <- occ_data(scientificName = species_name,
                              geometry = wkt_polygon,
                              limit = 500,
                              hasCoordinate = TRUE)


## ---------------------------------------------------------------------------------------
head(gbif_data_species$data)


## ---------------------------------------------------------------------------------------
occ_species <- gbif_data_species$data %>%
               dplyr::select(lon = decimalLongitude,
                             lat = decimalLatitude)


## ---------------------------------------------------------------------------------------
occ_species_sf <- st_as_sf(occ_species,
                           coords = c("lon", "lat"),
                           crs = st_crs(maharashtra_districts))


## ---------------------------------------------------------------------------------------
occ_species_maha_sf <- st_filter(occ_species_sf, maharashtra_districts)

ggplot() +
  geom_sf(data = maharashtra_districts) +
  geom_sf(data = occ_species_maha_sf, color = "red") +
  labs(title = paste0("GBIF Occurrence Records for Maharashtra: ", species_name),
       x = "Longitude", y = "Latitude")


## ---------------------------------------------------------------------------------------
saveRDS(occ_species_maha_sf,
        "data/gbif_anser_indicus_maharashtra.rds")


## ---------------------------------------------------------------------------------------
predictors <- rast('data/sdm_inputs.tif')
plot(predictors)


## ---------------------------------------------------------------------------------------
# END OF PRACTICAL EXERCISE 3
## ---------------------------------------------------------------------------------------