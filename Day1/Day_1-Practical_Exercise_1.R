##Day 1 - Practical Exercise 1

##Hello everyone. Thank you for participating in this workshop.
##Lines that start with a # character represent comments in an R file,
##which means that they are not treated as R code.These lines will all appear
##in pale green text.

##For each of the lines of code in the exercise, you will need to press
##the CTRL and ENTER keys together to execute that line of code.


## ---------------------------------------------------------------------------------------
##
# Setting working directory
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
#Enter the directory where you extracted this R file
#inside the quotes below.
#Note: You will need to use the / character in the path instead of \
setwd("")


## ---------------------------------------------------------------------------------------
#
getwd()


## ---------------------------------------------------------------------------------------
##
# Working with variables and data structures
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
# assign a single value
var <- 1
var


## ---------------------------------------------------------------------------------------
var <- "hello world"
class(var)


## ---------------------------------------------------------------------------------------
# assign a vector
vec <- c(1,2,3,4)
vec


## ---------------------------------------------------------------------------------------
# assign a dataframe
df <- data.frame(col1 = c(1,2,3,4),
                 col2 = c("a","b","c","d"))
df


## ---------------------------------------------------------------------------------------
##
# Installing and loading packages
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
# The terra package is for reading and manipulating raster files
install.packages("terra", dependencies = TRUE)


## ---------------------------------------------------------------------------------------
library(terra)


## ---------------------------------------------------------------------------------------
?terra


## ---------------------------------------------------------------------------------------
?rasterize


## ---------------------------------------------------------------------------------------
packages_vector <- c( "tidyterra",  # Plotting of raster data
                      "sf",         # Work with vector data
                      "raster",     # Raster data manipulation
                      "dplyr",      # Data manipulation
                      "biomod2",    # Species distribution modelling
                      "rgbif",      # Species occurrence data
                      "lavaan",     # Latent variable analysis and SEM
                      "lavaanPlot", # Additional functionalities for SEM
                      "performance",# Measures SEM performances
                      "stringr",    # String operations
                      "tidyr",      # Data manipulation
                      "ggplot2",    # Visualising data
                      "ggpubr")     # Advanced layout of plots

install.packages(packages_vector, dependencies = TRUE)


## ---------------------------------------------------------------------------------------
##
# Reading and visualising spatial data
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
#Shape file: India_State_Boundary_WGS1984.shp
#Provided by Dr Chanda
#Load state level vector map of India as simple feature vector
library(sf)
sfIndiaStates <- st_read("./data/India_State_Boundary_WGS1984.shp")


## ---------------------------------------------------------------------------------------
#See first rows of vector
head(sfIndiaStates)


## ---------------------------------------------------------------------------------------
#Plot map
plot(sfIndiaStates)


## ---------------------------------------------------------------------------------------
#WorldClim provides a number of climatic raster maps of the world
#for temperature and precipitation called BioClim rasters.
#In this case, we will load the BIO1 raster,
#which describes Annual Mean Temperature. 
#See the below source for the full list of BioClim rasters.
#Source: https://worldclim.org/data/bioclim.html
library(terra)
bioclim_1 <- rast("./data/wc2.1_5m_bio_1.tif")


## ---------------------------------------------------------------------------------------
#View summary of raster variable
bioclim_1


## ---------------------------------------------------------------------------------------
#Create basic plot of raster
plot(bioclim_1)


## ---------------------------------------------------------------------------------------
# END OF PRACTICAL EXERCISE 1
## ---------------------------------------------------------------------------------------