# ------------------------------------------------------------------------------
#'Function to get soil nuitrient covariates from ISDA soil profile data and extract 
#'using field trial data to prepare for data driven modelling work
# ------------------------------------------------------------------------------ 

getSpatialSoil <- function(useCaseName){
  
  packages_required <- c("terra", "sf", "geodata", "tidyverse", "readxl","AOI")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # read soil properities data from ISDA soil at 0-20 cm depth (e.g Soil organic carbon (OC), Bulk density (BLKD), pH, total nitrogen (Ntot), Extractable potassium)
  
   OC <- soil_af_isda(var="OC", depth=20, path=tempdir())
   BLKD <- soil_af_isda(var="db.od", depth=20, path=tempdir())
   pH <- soil_af_isda(var="pH.H2O", depth=20, path=tempdir())
   Ntot <- soil_af_isda(var="N.tot", depth=20, path=tempdir())
   P <- soil_af_isda(var="P", depth=20, path=tempdir())
   K <- soil_af_isda(var="K", depth=20, path=tempdir())
  
  
  #Cropping by country boundary 
  crop_mask = aoi_get(country = useCaseName)
  
  OC1 <-crop(OC,crop_mask,mask=T)
  BLKD1 <-crop(BLKD,crop_mask,mask=T)
  pH1 <-crop(pH,crop_mask,mask=T)
  Ntot1 <-crop(Ntot,crop_mask,mask=T)
  P1 <-crop(P,crop_mask,mask=T)
  K1 <-crop(K,crop_mask,mask=T)
 
  #concatenate all soil property layer
  soil<-c(OC1,BLKD1,pH1,Ntot1,P1,K1)
  
  pathOut_layers <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                          useCaseName, "intermediate/geospatial/soil", sep = "/")
  
  if(!dir.exists(pathOut_layers)){
    suppressWarnings(dir.create(pathOut_layers, recursive = T))
  }
  
  
  # read the trial data and extract the soil data
  pathIn_trial <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                        useCaseName, "input/field_data", sep = "/")
  trial <- readxl::read_xlsx(paste(pathIn_trial, "final_pts_filtered.xlsx", sep = "/"), 
                             col_names = T)
  
  points <- terra::vect(trial, geom = c("longitude", "latitude"), crs = "epsg:4326")
  
  points_soil <- soil |> terra::extract(points) |> select(-(ID))
  extracted_soil <- trial |> dplyr::select(unique_id, longitude, latitude, date) |> cbind(points_soil)
  
  # export the results to intermediate path  
  pathOut_gps <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                       useCaseName, "intermediate/gps", sep = "/")
  if(!dir.exists(pathOut_gps)){
    suppressWarnings(dir.create(pathOut_gps, recursive = T))
  }
  pathOut_layers <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                          useCaseName, "intermediate/geospatial/soil", sep = "/")
  if(!dir.exists(pathOut_layers)){
    suppressWarnings(dir.create(pathOut_layers, recursive = T))
  }
  
  saveRDS(extracted_soil, paste(pathOut_gps, "soil.rds", sep = "/"))
  terra::writeRaster(
    OC1,
    filename = paste(pathOut_layers,"OC.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    BLKD1,
    filename = paste(pathOut_layers,"BLKD.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    pH1,
    filename = paste(pathOut_layers,"pH.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    Ntot1,
    filename = paste(pathOut_layers,"Ntot.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    P1,
    filename = paste(pathOut_layers,"P.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
  terra::writeRaster(
    K1,
    filename = paste(pathOut_layers,"K.tif", sep = "/"),
    filetype = "GTiff",
    overwrite = T
  )
}

