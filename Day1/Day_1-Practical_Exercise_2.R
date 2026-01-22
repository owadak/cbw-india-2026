##Day 1 - Practical Exercise 2

##Hello everyone. Thank you for participating in this workshop.
##Lines that start with a # character represent comments in an R file,
##which means that they are not treated as R code.These lines will all appear
##in pale green text.

##For each of the lines of code in the exercise, you will need to press
##the CTRL and ENTER keys together to execute that line of code.


## ---------------------------------------------------------------------------------------
##
# Loading source map data
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
#Enter the directory where you extracted this R file
#inside the quotes below.
#Note: You will need to use the / character in the path instead of \
setwd("")


## ---------------------------------------------------------------------------------------
#Load terra package and read the BIO1 Bioclim raster.
library(terra)
bioclim_1 <- rast("./data/wc2.1_5m_bio_1.tif")


## ---------------------------------------------------------------------------------------
#Load the simple feature package and read the state level vector map of India
library(sf)
sfIndiaStates <- st_read("./data/India_State_Boundary_WGS1984.shp")


## ---------------------------------------------------------------------------------------
##
# Cropping and overlaying map data
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
#Crop Bioclim1 map to area of India
cropped_bioclim_1 <- crop(bioclim_1, sfIndiaStates, mask = TRUE)


## ---------------------------------------------------------------------------------------
#Plot cropped map and then overlay India state boundaries map
plot(cropped_bioclim_1)
plot(sfIndiaStates['geometry'], add = T)


## ---------------------------------------------------------------------------------------
##
# Loading and filtering case data
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
#Load WAHIS data
wahis_data <- read.csv("data/Outbreaks_India_(WAHIS).csv")


## ---------------------------------------------------------------------------------------
#Look at sample of data
head(wahis_data)


## ---------------------------------------------------------------------------------------
#The dplyr package is used for manipulating data in R.
#Filter data to only keep outbreaks of Highly Pathogenic Avian Influenza
library(dplyr)
wahis_aiv <- dplyr::filter(wahis_data, grepl('HPAI', diseaseName))
  

## ---------------------------------------------------------------------------------------
#Look at sample of filtered data 
head(wahis_aiv)


## ---------------------------------------------------------------------------------------
##
# Creating point data and visualising
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
#Convert WAHIS data into simple feature data type and use same projection as India state map
sp_wahis_aiv <- st_as_sf(wahis_aiv, coords = c("longitude", "latitude"), crs = st_crs(sfIndiaStates))
head(sp_wahis_aiv)


## ---------------------------------------------------------------------------------------
#Plot outbreak coordinates on top of state map
#This plot uses the 'ggplot2' package to allow more flexible
#visualisation of data.
#Note that the package name includes the number 2, i.e. ggplot2, but the
#plotting function is just ggplot().
#Each element to be plotted using ggplot must be specified with a function
#that corresponds to its data type in order to be interpreted correctly.
#In this case 'geom_sf' is used because both elements are simple feature objects.
#'data' specifies which data to plot
#'pch' value determines symbol used to plot each coordinate.
#pch=4 plots coordinates using an 'x' symbol
#'color' value determines symbol color used to plot each coordinate
library(ggplot2)
ggplot()+
  geom_sf(data = sfIndiaStates) +
  geom_sf(data = sp_wahis_aiv, pch = 4, color = "red")


## ---------------------------------------------------------------------------------------
##
# Rasterising point data and aggregating raster data
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
#Rasterise AIV point data using same extent as BioClim1 raster
#The 'fun' parameter sets how to treat multiple coordinates within the same
#raster cell. Using 'sum' calculates the sum of all coordinate values within
#each raster cell.
r_AIV <- rasterize(sp_wahis_aiv, cropped_bioclim_1, fun='sum')


## ---------------------------------------------------------------------------------------
#Create basic plot of rasterised map of AIV case data
plot(r_AIV)

  
## ---------------------------------------------------------------------------------------
#The previous plot showed that the raster data are difficult to see on the map
#so we will aggregate the data, which converts it to a coarser spatial resolution.
#The 'fact' parameter specifies the factor of aggregating. Setting to 10 means
#the resulting raster will combine every 10x10 area of raster cells to produce
#a single raster cell in the output raster.
#The 'fun' parameter specifies how to process all the values within that 10x10
#area to produce a new value. Using 'sum' will sum all the cell values together.
#The 'na.rm' parameter specifies whether to exclude N/A values within each 10x10
#area in the calculation of the new raster values. This is set to TRUE to ignore
#the values. Choosing FALSE will result in an output value of NA if at least one
#cell in the 10x10 area is NA.
r_AIV_aggregated <- aggregate(r_AIV, fact = 10, fun=sum, na.rm=TRUE)


## ---------------------------------------------------------------------------------------
#Create basic plot of aggregated rasteris map of AIV case data
plot(r_AIV_aggregated)


## ---------------------------------------------------------------------------------------
#Set the name of the raster map
names(r_AIV_aggregated) <- "AIV_cases"


## ---------------------------------------------------------------------------------------
##
# Advanced visualisation
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
#Create plot combining the India state areas, the rasterised case data,
#and the point case data.
#The tidyterra package is required here to use the 'geom_spatraster' function
#for using spatraster data with ggplot.
#The 'scale_fill_viridis_c' function is for setting colour fill using one of
#the viridis colour schemes.
#The 'na.value' parameter specifies how to interpret raster values of NA when
#plotting the raster on the map. Setting to NA ensures these cells are not
#plotted.
#This time the point data uses pch=16 instead of 4, which draws a solid circle.
#The 'cex' parameter is used to scale the size of the point data symbol.
#Setting this to 0.5 makes it half of the default size.
library(tidyterra)
ggplot()+
  geom_sf(data = sfIndiaStates) +
  geom_spatraster(data = r_AIV_aggregated) +
  scale_fill_viridis_c(na.value = NA) +
  geom_sf(data = sp_wahis_aiv, col = 'red', cex = 0.5, pch = 16)


## ---------------------------------------------------------------------------------------
#Create plot of bioclim1
#In 'scale_fill_viridis_c' function, the 'Option' parameter specifies the
#colour scheme - in this case 'turbo'.
#Assigning a call to ggplot() to a variable stores the plot for use in other
#functions.
bioclim_1_plot <- ggplot() + 
  geom_spatraster(data=cropped_bioclim_1) +
  scale_fill_viridis_c(option = "turbo", na.value = NA)


## ---------------------------------------------------------------------------------------
#Create plot of AIV raster on top of state map of India
r_avi_plot <- ggplot() +
  geom_sf(data = sfIndiaStates) +
  geom_spatraster(data = r_AIV_aggregated) +
  scale_fill_viridis_c(na.value = NA)


## ---------------------------------------------------------------------------------------
#Combine the two plots using two columns and set plot labels using
#ggarange and the two ggplot variables created above.
#The 'align' parameter specifies how to align two maps. Setting to 'v' means
#align vertically.
#The 'ncol' parameter specifies how many columns to use for the input data.
#The 'labels' parameter allows for headings to be added to each plot.
library(ggpubr)
ggarrange(
  bioclim_1_plot, r_avi_plot,
  align = "v", ncol = 2,
  labels = c("Bioclim1", "AIV cases raster")
) 


## ---------------------------------------------------------------------------------------
# END OF PRACTICAL EXERCISE 2
## ---------------------------------------------------------------------------------------