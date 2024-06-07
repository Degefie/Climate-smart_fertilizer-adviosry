#' Developing Random forest ML model for  response curve for N_rate, P_rate and K_rate
#' to yield. It uses the anlayis ready data as an input and trai RF model


RF_ML <- function(useCaseName){
  
  # 1. Sourcing required packages -------------------------------------------
  
  packages_required <- c("doParallel", "foreach", "h2o", "tidyverse", "dplyr", "stringr", "ggplot2", "ggpmisc", "Metrics")
  
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))
  
  #  data accesing for model input 
  
  ML_inputData <- readRDS(paste("~/Ferilizer_prediction/dataops/datasourcing/Data", useCaseName, "output", "covariates_GPS_ML2.rds", sep="/")) 
  
  
  # setting up a local h2o cluster
  h2o.init()
  
  # convert our dataframe into a special object that h2o can recognize
  ML_inputData.h2o <- as.h2o(ML_inputData)
  
  
  #create a random training-test split of our data
  ML_inputData.h2o <- as.h2o(ML_inputData)
  ML_inputData_split <- h2o.splitFrame(data = ML_inputData.h2o, ratios = 0.8, seed = 1234)
  training_data <- ML_inputData_split[[1]]
  test_data <- ML_inputData_split[[2]]
  
  
  ## define the target and feature variables
  listVars=NULL
  if(!is.null(listVars)){
    predictors <- listVars
  }else{
    predictors <- c("n_rate2","p_rate2", "k_rate2", "n.tot.ncs_0-20cm", "p_0-20cm", "k_0-20cm", "oc_0-20cm", "precipitation_g_len_1", "precipitation_g_len_2", "precipitation_g_len_3", "precipitation_g_len_4",  
                    "tempMax_g_len_1", "tempMax_g_len_2","tempMax_g_len_3", "tempMax_g_len_4","tempMin_g_len_1", "tempMin_g_len_2","tempMin_g_len_3", "tempMin_g_len_4",
                    "solaRadiation_g_len_1", "solaRadiation_g_len_2", "solaRadiation_g_len_3", "solaRadiation_g_len_4","relativeHumidity_g_len_1","relativeHumidity_g_len_2","relativeHumidity_g_len_3","relativeHumidity_g_len_4","dem","slope", "TPI", "TRI", "db.od_0-20cm", "ph.h2o_0-20cm")
  }
  
  response <- "yield"
  #response2 <- "yield2"
  
  
  ## Specify the hyper-parameter grid
  hyper_params <- list(
    ntrees = seq(500, 1200, 100),
    max_depth = seq(4, 12, 2),
    mtries = c(2, 3, 4, 5, 6)
  )
  
  # Train and tune the random forest model
  grid_rf <- h2o.grid(
    algorithm = "randomForest",
    x = predictors,
    y = response,
    grid_id = "rf_grid",
    hyper_params = hyper_params,
    training_frame = training_data,
    validation_frame = test_data,
    seed = 444
  )
  
  
  pathOut <- paste("~/Ferilizer_prediction/ML_output",useCaseName, "output", sep = "/")
  if(!exists(pathOut)){
    suppressWarnings(dir.create(pathOut, recursive = T))
  }
  
  saveRDS(grid_rf, paste(pathOut, "grid_rf_hyperParam.rds", sep=""))
  # Get the best model from the grid search
  best_model <- h2o.getModel(grid_rf@model_ids[[1]])
  
  ML_randomForest <- h2o.randomForest(x = predictors,
                                      y = response,
                                      ntrees =  best_model@parameters$ntrees,
                                      max_depth = best_model@parameters$max_depth,
                                      mtries =   best_model@parameters$mtries,
                                      training_frame = training_data,
                                      validation_frame = test_data,
                                      keep_cross_validation_predictions = TRUE,
                                      nfolds = 5,
                                      seed = 444)
  
  model_path <- h2o.saveModel(object = ML_randomForest, path = pathOut, force = TRUE)
  saved_model <- h2o.loadModel(model_path)
  
  rmse_r2_randomforest <- data.frame( mae = round(h2o.mae(ML_randomForest, train=TRUE, valid=TRUE), 0),
                                      rmse =h2o.rmse(ML_randomForest, train=TRUE, valid=TRUE)[[2]],
                                      R_sq = c(h2o.r2(ML_randomForest, train=TRUE, valid=TRUE)[[2]]))
  saveRDS(rmse_r2_randomforest, paste(pathOut, "rmse_r2_randomforest.RDS"))
  
  rmse_r2_randomforest
  
  print("Model performance evaluation plots")
  
  print("1. residual plot)")
  h2o.residual_analysis_plot(ML_randomForest,test_data)
  
  print("2. variable importance plot)")
  h2o.varimp_plot(ML_randomForest)
  
  print("3. shap summary")
  h2o.shap_summary_plot(ML_randomForest, test_data)
  
  print("4. partial plot")
  h2o.partialPlot(object = ML_randomForest, newdata = test_data, cols = "n_rate2")
  h2o.partialPlot(object = ML_randomForest, newdata = test_data, cols = "p_rate2")
  h2o.partialPlot(object = ML_randomForest, newdata = test_data, cols = "n_rate2")
  
  h2o.ice_plot(ML_randomForest,test_data,column = "n_rate2")
  h2o.ice_plot(ML_randomForest,test_data,column = "p_rate2")
  h2o.ice_plot(ML_randomForest,test_data,column = "k_rate2")
  
  print("5. validating by prediction")
  rf_valid <- test_data
  rf_valid$predResponse <- h2o.predict(object = ML_randomForest, newdata = test_data)
  rf_valid <- as.data.frame(rf_valid)
  rf_valid$Response <- rf_valid[,which(names(rf_valid)==response)]
  
  gg2 <-  ggplot(rf_valid, aes(Response, predResponse)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "blue") +
    xlab("Measured Yield") + ylab("predicted yield")+
    ggtitle("Random forest") +
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(size=12))
  
  ggsave(paste(pathOut, "randomForest_Validation.pdf", sep=""), gg2)
  
  print(gg2)
  
 
  h2o.residual_analysis_plot(ML_randomForest,test_data)
  h2o.varimp_plot(ML_randomForest)
  h2o.shap_explain_row_plot(ML_randomForest,test_data,row_index = 1)
  h2o.shap_summary_plot(ML_randomForest,test_data)
  h2o.pd_plot(ML_randomForest,test_data,column = "n_rate2")
  h2o.ice_plot(ML_randomForest,test_data,column = "n_rate2")
  
   return(ML_randomForest)
  
}







