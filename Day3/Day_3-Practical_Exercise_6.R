##Day 3 - Practical Exercise 6

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
library(lavaan)
library(lavaanPlot)
library(performance)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggpubr)


## ---------------------------------------------------------------------------------------
##
# Preparing projected data for SEM 
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
species_name <- "Anser indicus"
wildbird_distribution <- rast('data/anser_indicus_distribution.tif')
projected_wildbird_distribution <- rast('data/anser_indicus_distribution_cc.tif')
current_exposures <- rast('data/sem_current_exposure_anser_indicus.tif')
sem_inputs <- rast('data/sem_inputs.tif')


## ---------------------------------------------------------------------------------------
wildbird_distribution_change <- (projected_wildbird_distribution - wildbird_distribution) / wildbird_distribution

ggplot() +
  geom_spatraster(data = wildbird_distribution_change) +
  scale_fill_viridis_c(na.value = NA) +
  labs(title="Percentage change in predicted wildbird distribution in Maharashtra",
       x = "Longitude", y = "Latitude")



## ---------------------------------------------------------------------------------------
combined_sem_inputs <- c(sem_inputs,
                         projected_wildbird_distribution)
combined_sem_inputs.scaled <- scale(combined_sem_inputs)
df <- as.data.frame(combined_sem_inputs.scaled,
                    na.rm = FALSE,
                    xy = TRUE)
df[is.na(df)] = 0


## ---------------------------------------------------------------------------------------
##
# Creating a climate change projection SEM
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
sem_model <- '
              ### Latent variable definitions
            
              Wildbird_exposure =~ cases_wildbirds 
              Poultry_farm_exposure =~ cases_domestic_birds 
              Human_exposure =~ cases_human_influenza_a 
              
              ### Regressions

              Wildbird_exposure ~ projected_wildbird_distribution
              Poultry_farm_exposure ~ Wildbird_exposure + Chicken_Distribution + Duck_Distribution
              Human_exposure ~ Poultry_farm_exposure + Proportion_Urban + Population_Density
             '

## ---------------------------------------------------------------------------------------
sem_output <- sem(sem_model, data = df)


## ---------------------------------------------------------------------------------------
lavaanPlot(model = sem_output, coefs = TRUE, stars = "regress")


## ---------------------------------------------------------------------------------------
##
# Plotting current and projected exposure risk
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
coefficients <- lavaan::coef(sem_output)

dfcoeff <- as.data.frame(coefficients) %>%
  filter(!str_detect(rownames(.), '~~')) %>%
  mutate(regression = rownames(.)) %>%
  separate(col =  regression, into = c('exposure', 'param'), sep = '~') 

exposures <- distinct(dfcoeff, exposure)

list_weights <- list()
for (exp in exposures$exposure){
  sub <- dplyr::filter(dfcoeff, exposure == exp)
  plist = list()
  for (param in sub$param){
    plist[param] = sub[sub$param == param, 'coefficients']
  }
  list_weights[[exp]] = plist
}


## ---------------------------------------------------------------------------------------
Projected_Wildbird_exposure = 
  combined_sem_inputs.scaled$projected_wildbird_distribution *
  list_weights$Wildbird_exposure$projected_wildbird_distribution

Projected_Poultry_farm_exposure = 
  Projected_Wildbird_exposure * list_weights$Poultry_farm_exposure$Wildbird_exposure +
  combined_sem_inputs.scaled['Chicken_Distribution'] * list_weights$Poultry_farm_exposure$Chicken_Distribution +
  combined_sem_inputs.scaled['Duck_Distribution'] * list_weights$Poultry_farm_exposure$Duck_Distribution

Projected_Human_exposure = 
  Projected_Poultry_farm_exposure * list_weights$Human_exposure$Poultry_farm_exposure + 
  combined_sem_inputs.scaled$Proportion_Urban * list_weights$Human_exposure$Proportion_Urban + 
  combined_sem_inputs.scaled$Population_Density * list_weights$Human_exposure$Population_Density


## ---------------------------------------------------------------------------------------
wildbird_exposure_plot <- ggplot() +
  geom_spatraster(data = current_exposures[[1]]) +
  scale_fill_viridis_c(na.value = NA) +
  labs(title=paste0("Current wildbird exposure (", species_name, ")\nin Maharashtra"),
       x = "Longitude", y = "Latitude")

projected_wildbird_exposure_plot <- ggplot() +
  geom_spatraster(data = Projected_Wildbird_exposure) +
  scale_fill_viridis_c(na.value = NA) +
  labs(title=paste0("Projected wildbird exposure (", species_name, ")\nin Maharashtra in 2041-2060"),
       x = "Longitude", y = "Latitude")

ggarrange(
  wildbird_exposure_plot, projected_wildbird_exposure_plot,
  align = "v", ncol = 2)


## ---------------------------------------------------------------------------------------
poultry_farm_exposure_plot <- ggplot() +
  geom_spatraster(data = current_exposures[[2]]) +
  scale_fill_viridis_c(na.value = NA) +
  labs(title="Current poultry farm exposure\nin Maharashtra",
       x = "Longitude", y = "Latitude")

projected_poultry_farm_exposure_plot <- ggplot() +
  geom_spatraster(data = Projected_Poultry_farm_exposure) +
  scale_fill_viridis_c(na.value = NA) +
  labs(title="Projected poultry farm exposure\nin Maharashtra in 2041-2060",
       x = "Longitude", y = "Latitude")

ggarrange(
  poultry_farm_exposure_plot, projected_poultry_farm_exposure_plot,
  align = "v", ncol = 2)


## ---------------------------------------------------------------------------------------
human_exposure_plot <- ggplot() +
  geom_spatraster(data = current_exposures[[3]]) +
  scale_fill_viridis_c(na.value = NA) +
  labs(title="Current human exposure",
       x = "Longitude", y = "Latitude")

projected_human_exposure_plot <- ggplot() +
  geom_spatraster(data = Projected_Human_exposure) +
  scale_fill_viridis_c(na.value = NA) +
  labs(title="Projected human exposure in 2041-2060",
       x = "Longitude", y = "Latitude")

ggarrange(
  human_exposure_plot, projected_human_exposure_plot,
  align = "v", ncol = 2)


## ---------------------------------------------------------------------------------------
# END OF PRACTICAL EXERCISE 6
## ---------------------------------------------------------------------------------------