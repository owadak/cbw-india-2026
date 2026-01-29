##Day 3 - Practical Exercise 5

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
# Preparing data for SEM 
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
species_name <- "Anser indicus"
wildbird_distribution <- rast('data/anser_indicus_distribution.tif')
bird_aiv_cases <- readRDS('data/bird_aiv_case_coordinates.rds')
sem_inputs <- rast('data/sem_inputs.tif')


## ---------------------------------------------------------------------------------------
combined_sem_inputs <- c(sem_inputs,
                         wildbird_distribution)
plot(combined_sem_inputs)


# ----------------------------------------------------------------------------------------
combined_sem_inputs.scaled <- scale(combined_sem_inputs)
df <- as.data.frame(combined_sem_inputs.scaled,
                    na.rm = FALSE,
                    xy = TRUE)
df[is.na(df)] = 0


## ---------------------------------------------------------------------------------------
##
# Creating an SEM 
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
sem_model <- '
              ### Latent variable definitions
            
              Wildbird_exposure =~ cases_wildbirds 
              Poultry_farm_exposure =~ cases_domestic_birds 
              Human_exposure =~ cases_human_influenza_a 
              
              ### Regressions

              Wildbird_exposure ~ wildbird_distribution
              Poultry_farm_exposure ~ Wildbird_exposure + Chicken_Distribution + Duck_Distribution
              Human_exposure ~ Poultry_farm_exposure + Proportion_Urban + Population_Density
             '

## ---------------------------------------------------------------------------------------
sem_output <- sem(sem_model, data = df)


## ---------------------------------------------------------------------------------------
lavaanPlot(model = sem_output, coefs = TRUE, stars = "regress")


## ---------------------------------------------------------------------------------------
##
# Evaluating an SEM 
##
## ---------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------
summary(sem_output, standardized = TRUE)


## ---------------------------------------------------------------------------------------
model_performance(sem_output, metrics = c('CFI', 'RMSEA'))


## ---------------------------------------------------------------------------------------
##
# Plotting exposure risk based on the SEM 
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
Wildbird_exposure = 
  combined_sem_inputs.scaled$wildbird_distribution *
    list_weights$Wildbird_exposure$wildbird_distribution

Poultry_farm_exposure = 
  Wildbird_exposure * list_weights$Poultry_farm_exposure$Wildbird_exposure +
  combined_sem_inputs.scaled['Chicken_Distribution'] * list_weights$Poultry_farm_exposure$Chicken_Distribution +
  combined_sem_inputs.scaled['Duck_Distribution'] * list_weights$Poultry_farm_exposure$Duck_Distribution

Human_exposure = 
  Poultry_farm_exposure * list_weights$Human_exposure$Poultry_farm_exposure + 
  combined_sem_inputs.scaled$Proportion_Urban * list_weights$Human_exposure$Proportion_Urban + 
  combined_sem_inputs.scaled$Population_Density * list_weights$Human_exposure$Population_Density


## ---------------------------------------------------------------------------------------
wildbird_cases  <- bird_aiv_cases %>%
                   filter(animalCategory == 'wild')
 
ggplot() +
   geom_spatraster(data = Wildbird_exposure) +
   scale_fill_viridis_c(na.value = NA) +
   geom_sf(data = wildbird_cases['totalCases'], color = "red") +
   labs(title=paste0("Wildbird exposure (", species_name, ") and wildbird AIV cases in Maharashtra"),
        x = "Longitude", y = "Latitude")
 

## ---------------------------------------------------------------------------------------
domestic_bird_cases  <- bird_aiv_cases %>%
                        filter(animalCategory == 'domestic')

ggplot() +
  geom_spatraster(data = Poultry_farm_exposure) +
  scale_fill_viridis_c(na.value = NA) +
  geom_sf(data = domestic_bird_cases['totalCases'], color = "red") +
  labs(title="Poultry farm exposure and domestic bird AIV cases in Maharashtra",
       x = "Longitude", y = "Latitude")


## ---------------------------------------------------------------------------------------
human_cases <- combined_sem_inputs.scaled$cases_human_influenza_a

Human_exposure_plot <- ggplot() +
  geom_spatraster(data = Human_exposure) +
  scale_fill_viridis_c(na.value = NA) +
  labs(title="Human exposure",
       x = "Longitude", y = "Latitude")


human_cases_plot <- ggplot() +
  geom_spatraster(data = human_cases) +
  scale_fill_viridis_c(na.value = NA) +
  labs(title="Human Influenza A cases",
       x = "Longitude", y = "Latitude")

ggarrange(
  Human_exposure_plot, human_cases_plot,
  align = "v", ncol = 2) 


## ---------------------------------------------------------------------------------------
# END OF PRACTICAL EXERCISE 5
## ---------------------------------------------------------------------------------------