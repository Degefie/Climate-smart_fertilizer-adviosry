#' -----------------------------------------------------------------------------
#' Using functions get analysis ready data on soil, topography and 
#' climate and combine with  field trial points to prepare for data driven analysis.
#'  Further prepare prediction grid with three scenarios (above, ormal and below)for fertilizer prediction
#' -----------------------------------------------------------------------------

getAnalysisReadyData <- function(useCaseName){
 
   packages_required <- c("terra", "sf", "tidyverse", "readxl")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # list all the climate variables
  climate = c("precipitation", "tempMax", "tempMin", "solaRadiation", "relativeHumidity")
  
  # source the soil, topography, climate and lime scripts 
  source("~/Ferilizer_prediction/Scripts/getSpatialSoil.R")
  source("~/Ferilizer_prediction/Scripts/getSpatialTopography.R")
  source("~/Ferilizer_prediction/Scripts/getSpatialClimate.R")
  
  getSpatialSoil(useCaseName)
  getSpatialTopography(useCaseName)
  
  # loop through each climate variables to extract using gps to create climate 
  # scenario 
  for(i in 1:length(climate)){
    getSpatialClimate(climate[i], useCaseName)
  }
  
  # ------------------------------------------------------------------------------
  # bind all the rds files with the trial data and make the data ready for the 
  # data driven approach
  pathIn_trial <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                        useCaseName, "input/field_data", sep = "/")
  trial <- readxl::read_xlsx(paste(pathIn_trial, "final_pts_filtered.xlsx", sep = "/"), 
                             col_names = T)
  
  pathIn_gps<- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                     useCaseName, "intermediate/gps", sep = "/")
  rds_files <- list.files(path = pathIn_gps, pattern = "*.rds", full.names = T)
  
  for(i in 1:length(rds_files)){
    rds <- readRDS(rds_files[i])
    trial <- trial |> dplyr::inner_join(rds, by = c("unique_id" = "unique_id"))
  }
  # prepare the final prediction raster for three different scenarios
  pathIn_Soil <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                       useCaseName, "intermediate/geospatial/soil", sep = "/")
  pathIn_Topo <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                       useCaseName, "intermediate/geospatial/topography", sep = "/")
  pathIn_climate <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                          useCaseName, "intermediate/geospatial/climate", sep = "/")
  
  soil_sp <- list.files(path = pathIn_Soil, pattern = "*.tif", full.names = T)|> 
    rast() 
  
 
  soil_lyr <- c(soil_sp)
  
  topo_sp <- list.files(path = pathIn_Topo, pattern = "*.tif", full.names = T) |> 
    rast() 
  topo_lyr <- c(topo_sp) |> terra::resample(soil_lyr)
  
  # read the climates of three different scenarios
  # above scenario
  climate_sp_abv <- list.files(path = pathIn_climate, pattern = "*above.tif", full.names = T)
  climate_lyr_abv <- rast()
  for(i in 1:length(climate_sp_abv)){
    print(i)
    r <- terra::rast(climate_sp_abv[[i]]) |> terra::resample(soil_lyr)
    add(climate_lyr_abv) <- r
  }
  
  # normal scenario
  climate_sp_norm <- list.files(path = pathIn_climate, pattern = "*normal.tif", full.names = T)
  climate_lyr_norm <- rast()
  for(i in 1:length(climate_sp_norm)){
    print(i)
    r <- terra::rast(climate_sp_norm[[i]]) |> terra::resample(soil_lyr[[1]])
    add(climate_lyr_norm) <- r
  }
  
  # below scenario
  climate_sp_bel <- list.files(path = pathIn_climate, pattern = "*below.tif", full.names = T)
  climate_lyr_bel <- rast()
  for(i in 1:length(climate_sp_bel)){
    print(i)
    r <- terra::rast(climate_sp_bel[[i]]) |> terra::resample(soil_lyr[[1]])
    add(climate_lyr_bel) <- r
  }
  
  # create a prediction grid for all scenarios including soil and topography
  scenario_above <- c(topo_lyr, soil_lyr, climate_lyr_abv)
  scenario_normal <- c(topo_lyr, soil_lyr, climate_lyr_norm)
  scenario_below <- c(topo_lyr, soil_lyr,  climate_lyr_bel)
  
  # write the final outputs of data frame and prediction layers for ML
  pathOut_Final <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                         useCaseName, "output", sep = "/")
  if(!exists(pathOut_Final)){
    suppressWarnings(dir.create(pathOut_Final, recursive = T))
  }
  saveRDS(trial, paste(pathOut_Final, "covariates_GPS_ML2.rds", sep = "/"))
  
  terra::writeRaster(
    scenario_above,
    filename = paste(pathOut_Final,"scenario_above.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    scenario_normal,
    filename = paste(pathOut_Final,"scenario_normal.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    scenario_below,
    filename = paste(pathOut_Final,"scenario_below.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
}

