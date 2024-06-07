rm(list = ls())
useCaseName <- "Nigeria"
climate = c("precipitation", "tempMax", "tempMin", "solaRadiation", "relativeHumidity")

# source the soil, topography, climate and lime scripts 
source("~/Ferilizer_prediction/Scripts/getSpatialSoil.R")
source("~/Ferilizer_prediction/Scripts/getSpatialTopography.R")
source("~/Ferilizer_prediction/Scripts/getSpatialClimate.R")

# pass parameter and run functions for preparing data for data driven modelling
getSpatialSoil(useCaseName)
getSpatialTopography(useCaseName)
getSpatialClimate (climate, useCaseName)
  
# pass parameter and develop the machine learning model(Random forest)
run_RF_ML <- function(useCaseName)
