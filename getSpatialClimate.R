#' Function to  access raster monthly climate data (precipitation, Tmax, Tmin, Solar radtion
#' Relative humidity) from online sources and extract using field trial data to prepare for data driven modelling.
#' Further prepare scenario rasters (Above, Normal and Below) for prediction 
#' 
getSpatialClimate <- function(climate, useCaseName){
  
  # install & load packages
  packages_required <- c("terra", "tidyverse", "sf", "lubridate", "plyr","geodata")
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # read trial sample data
  
  pathIn_trial <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                        useCaseName, "input/field_data", sep = "/")
 
  
  trial <- readxl::read_xlsx(paste(pathIn_trial, "final_pts_filtered.xlsx", sep = "/"), 
                             col_names = T) 
  
  
  
  points <- vect(trial, geom = c("longitude", "latitude"), crs = "epsg:4326")
  
  specialnames <- setdiff(names(trial), c("longitude", "latitude","date"))
  
  
  # working on the sowing start and growing length by the trial points data
  
  trial<-trial %>% mutate(sowing_start = 121)
  
  trial<-trial %>% mutate(grow_length = 120)
  
  trial_with_grow_len <- trial |> 
    dplyr::mutate(full_date = as.Date(paste(date, "01", "01", sep = "-"))) |>
    dplyr::mutate(pl_date = ymd(full_date) + days(sowing_start)) |>
    dplyr::mutate(hv_date = ymd(full_date) + days(sowing_start+grow_length))|>na.omit() 
  
  
  # read the climate monthly data from different directories
  
  if(climate == "precipitation"){
    ras <- worldclim_country(useCaseName, var="prec", path=tempdir())
  }else if(climate == "tempMax"){
      ras <- worldclim_country(useCaseName, var="tmin", path=tempdir())
  }else if(climate == "tempMin"){
      ras <- worldclim_country(useCaseName, var="tmax", path=tempdir())
  }else if(climate == "solaRadiation"){
       ras <- worldclim_country(useCaseName, var="srad", path=tempdir())
  }else if(climate == "relativeHumidity"){
       ras <- worldclim_country(useCaseName, var="srad", path=tempdir())
  }else{
      print("Please enter a valid climate variable name")
  }
  
  
  unique_grow_len <- trial_with_grow_len |> dplyr::select(date, pl_date, hv_date) |>
    unique()
  
  # loop unique planting and harvesting date to extract the climate data
  f_df <- c()
  for(i in 1:nrow(unique_grow_len)){
    print(i)
    d <- trial_with_grow_len |> dplyr::filter(pl_date == unique_grow_len$pl_date[i] & 
                                                hv_date == unique_grow_len$hv_date[i])
    pl_date <- d$pl_date[1]
    hv_date <- d$hv_date[1]
    year <- d$date[1]
    d <- vect(d, geom = c("longitude", "latitude"), crs = "epsg:4326")
    if(pl_date < hv_date){
      month_start <- as.numeric(format(pl_date, "%m"))
      month_end <- as.numeric(format(hv_date, "%m"))
      grow_len_r <- ras[[month_start:month_end]]
    }else if(pl_date > hv_date){
      month_start <- as.numeric(format(pl_date, "%m"))
      month_end <- as.numeric(format(hv_date, "%m"))
      
      grow_len_r1 <- ras[[month_start:12]]
      grow_len_r2 <- ras[[13:month_end + 12]]
      grow_len_r <- c(grow_len_r1, grow_len_r2)
    }
    r_df <- terra::extract(grow_len_r, d) |> dplyr::select(-ID)
    colnames(r_df) <- paste0(climate, "_g_len_", seq(1,ncol(r_df),1))
    f_df <- rbind.fill(f_df, r_df) # row binds df with different column numbers
  }
  
  
 
    # fill na values with row means - with different growing lengths some areas have 3,
    # others have 4, others have 5. The shorter ones have NA values to match the larger
    # ones. The NA values will be filled by row means
    
    # k <- which(is.na(f_df), arr.ind = TRUE)
    # f_df[k] <- rowMeans(f_df, na.rm = TRUE)[k[, 1]]
    
    final_df <- trial_with_grow_len |> dplyr::select(unique_id, longitude, latitude, date, pl_date, hv_date) |>
      cbind(f_df)
   
    
    # ----------------------------------------------------------------------------
    # create a prediction scenario raster
    min_pl_date <- min(format(as.Date(final_df$pl_date,format="%d/%m/%Y"),"%m")) |>
      as.numeric()
    max_hv_date <- max(format(as.Date(final_df$hv_date,format="%d/%m/%Y"),"%m")) |>
      as.numeric()
    clim_rast <- ras
    
    q2_clim <- rast()
    q1_clim <- rast()
    q3_clim <- rast()
    
    
      q1 <- terra::quantile(clim_rast, probs = 0.25)
      add(q1_clim) <- q1
      q2 <- terra::quantile(clim_rast, probs = 0.5)
      add(q2_clim) <- q2
      q3 <- terra::quantile(clim_rast, probs = 0.75)
      add(q3_clim) <- q3
    
    
    clim_below <- q1_clim
    clim_normal <- q2_clim
    clim_above <- q3_clim
    names(clim_above) <- names(clim_normal) <- names(clim_above) <-
      paste0(climate, "_g_len_", seq(1,nlyr(clim_below),1))
   
    # export the results to the crop path  
    pathOut_gps <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                         useCaseName, "intermediate/gps", sep = "/")
    
    if(!dir.exists(pathOut_gps)){
      suppressWarnings(dir.create(pathOut_gps, recursive = T))
    }
    
    pathOut_layers <- paste("~/Ferilizer_prediction/dataops/datasourcing/Data", 
                            useCaseName, "intermediate/geospatial/climate", sep = "/")
    
    if(!dir.exists(pathOut_layers)){
      suppressWarnings(dir.create(pathOut_layers, recursive = T))
    }
    
    final_df <- final_df |> dplyr::select(-c(hv_date, pl_date))
    saveRDS(final_df, paste(pathOut_gps, paste0(climate, ".rds"), sep = "/"))
    
    terra::writeRaster(
      clim_above,
      filename = paste(pathOut_layers,paste0(climate, "_above", ".tif"), sep = "/"),
      filetype = "GTiff",
      overwrite = T
    )
    terra::writeRaster(
      clim_normal,
      filename = paste(pathOut_layers,paste0(climate, "_normal", ".tif"), sep = "/"),
      filetype = "GTiff",
      overwrite = T
    )
    terra::writeRaster(
      clim_below,
      filename = paste(pathOut_layers,paste0(climate, "_below", ".tif"), sep = "/"),
      filetype = "GTiff",
      overwrite = T
    )
  }
  
    
     
    
