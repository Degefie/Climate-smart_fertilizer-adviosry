# ------------------------------------------------------------------------------
#' Function to get elvation and derived vraibles like slope, tri and tpi and extracting
#' using field trial data to prepare for data driven modelling
# ------------------------------------------------------------------------------ 
getSpatialTopography <- function(useCaseName){
  
  packages_required <- c("terra", "sf", "geodata", "tidyverse", "readxl","rgeoboundaries","raster","elevatr")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # read the elevation data 
  
  dem_o <- rgeoboundaries::geoboundaries(useCaseName)
  dem <- elevatr::get_elev_raster(locations = dem_o, z = 9, clip = "locations")
  
   
  # # derive slope and aspect mask the rasters with growing area of the crop 
  # crop_mask <- crop_mask |> terra::resample(dem)
  # dem <- dem |> terra::crop(crop_mask) |> terra::mask(crop_mask)
   slope <- terra::terrain(dem, v = 'slope')
   tpi <- terra::terrain(dem, v = 'TPI')
   tri <- terra::terrain(dem, v = 'TRI')
   
  topo<-c(dem,slope,tpi,tri)
  
  # read the trial data and extract the topography data
  pathIn_trial <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                        useCaseName, "input/field_data", sep = "/")
  
  trial <- readxl::read_xlsx(paste(pathIn_trial, "final_pts_filtered.xlsx", sep = "/"), 
                             col_names = T)  
  points <- terra::vect(trial, geom = c("longitude", "latitude"), crs = "epsg:4326")
  
  
  points_topo <- topo |> terra::extract(points)
  points_topo = subset(points_topo, select = -(ID))
  extracted_topo <- trial |> dplyr::select(unique_id, longitude, latitude, date) |> cbind(points_topo)
  
    # export the results to intermediate path  
  pathOut_gps <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                       useCaseName, "intermediate/gps", sep = "/")
  if(!dir.exists(pathOut_gps)){
    suppressWarnings(dir.create(pathOut_gps, recursive = T))
  }
  pathOut_layers <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                          useCaseName, "intermediate/geospatial/topography", sep = "/")
  if(!dir.exists(pathOut_layers)){
    suppressWarnings(dir.create(pathOut_layers, recursive = T))
  }
  
  saveRDS(extracted_topo, paste(pathOut_gps, "topography.rds", sep = "/"))
  terra::writeRaster(
    dem,
    filename = paste(pathOut_layers,"dem.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    slope,
    filename = paste(pathOut_layers,"slope.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )

}
