# Created by: Luca Poll
# Created on: 11.09.2021

# input parameters
# - data: dataset
# - depvar: name of dependent variable
# - subset: subset of variables that should be used
# - no_var: should within variation be averaged out?
# - oos_periods: range of periods for which out-of-sample forecast should be conducted
# - p: number of variables that will be evaluated at each node (mtry)
# - imbal: is the input data imbalanced and should be adjusted? (undersampling)
# - trees: size of the forest
# - return: should only the AUCs be returned or additionally also all of the models?


# note: input data needs
#           "time" column (date format),
#            "municipio" column,
#            dependent variable (needs to be factor),
#            all regressors have to start with "rhs_",
#            regressors that are not structural need to start with "rhs_tv_"
#

RunModel <- function(data, dependent_var, subset = NULL,
                     no_var = FALSE, oos_periods, imbal = "none",
                     p = "default", trees = 150, return = "auc"){

  # 0) make that depvar can be used as such and other setup
  print(paste0("Start time:", Sys.time()))

  depvar <- as.name(dependent_var)

   # 1) prepare output of function
  output <- data.frame(matrix(nrow = length(oos_periods), ncol = 4))
  colnames(output) <- c("period", "AUC", "AUCPR", "imbalance")
  output$period <- oos_periods

  if (return == "all"){
    models <- vector(mode = "list", length = length(oos_periods))
    names(models) <- oos_periods
  }


  # 2) prepare data that is used
  # get rid of within variation
  if (no_var == TRUE){
    data <- data %>%
      select(c(time, municipio, all_of(depvar), starts_with("rhs_"))) %>%
      group_by(municipio) %>%
      mutate(across(-c(time, all_of(depvar), starts_with("rhs_tv_")), mean)) %>%
      ungroup()
  }

  # choose set of regressors
  if (is.null(subset)){
    data <- data %>% select(c(time, all_of(depvar), starts_with("rhs_")))
  } else if (!is.null(subset)){
      data <- data %>% select(c(time, all_of(depvar), all_of(subset)))
  }

  # drop NAs
  data <- data %>% drop_na()

  # hyperparameter tuning p (mtry)
  if (p == "default"){
    p <- floor(sqrt(ncol(data)-1))
  } else if (p == "tune") {
    print("starting to find optimal mtry...")
    tune_data <- data %>%
      filter(time < min(oos_periods)) %>%
      select(-time)
    control <- trainControl(method = "cv", number = 5)
    tunegrid <- expand.grid(.mtry = round(seq(2, ceiling(sqrt(ncol(tune_data))*2),
                                              length.out = if_else(ceiling(sqrt(ncol(tune_data))*2) > 15,
                                                                    15, ceiling(sqrt(ncol(tune_data))*2))))) %>% unique()
    weights <- ifelse(tune_data[[dependent_var]] == "yes",
                   (1/table(tune_data[[dependent_var]])[1])*0.5,
                   (1/table(tune_data[[dependent_var]])[2])*0.5)
    form <- as.formula(paste(depvar, "~ ."))
    modeltune <- train(form, data = tune_data,
                       method = "rf", metric = "Accuracy", weights = weights,
                       tuneGrid = tunegrid, ntree = 100, trControl = control)
     p <- modeltune$results$mtry[modeltune$results$Accuracy == max(modeltune$results$Accuracy)]
    print(paste0("found p = ", p, ". Starting to run model now:"))
  }



  # 3) loop over periods to get oos forecasts
  for (period in oos_periods){

    print(paste0("Computing period ", match(period, oos_periods), "/",
                 length(oos_periods), "..."))

    # 4) split data into training and test data
    train_data <- data %>% filter(time < period) %>% select(-time)
    oos_data <- data %>% filter(time == period) %>% select(-time)

    # 5) run model
    i <- match(dependent_var, names(train_data))
    # account for imbalancedness of data
    if (imbal == "weight"){
      model <- randomForest(x = train_data[-i], y = train_data[[i]],
                            mtry = p, ntree = trees,
                            classwt = c("yes" = 1,
                                        "no" = sum(train_data[[dependent_var]] == "no")/nrow(train_data)),
                            importance = FALSE, proximity = FALSE)
    } else if (imbal == "downsample"){
      model <- randomForest(x = train_data[-i], y = train_data[[i]],
                            mtry = p, ntree = trees,
                            strata = train_data[[dependent_var]],
                            sampsize = rep(min(table(train_data[[dependent_var]])), 2),
                            importance = FALSE, proximity = FALSE)
    } else if (imbal == "none"){
      model <- randomForest(x = train_data[-i], y = train_data[[i]],
                            mtry = p, ntree = trees, importance = FALSE, proximity = FALSE)
    }


    # 6) compute AUC
    pred <- predict(model, newdata = oos_data, type = "prob")
    perf <- prediction(pred[,2], oos_data[,depvar])
    auc <- round(performance(perf, "auc")@y.values[[1]], digits = 4)
    aucpr <- round(performance(perf, "aucpr")@y.values[[1]], digits = 4)



    # 7) save output
    output[output$period == period, 2] <- auc
    output[output$period == period, 3] <- aucpr
    output[output$period == period, 4] <- 1 - table(train_data[[dependent_var]])[2]/table(train_data[[dependent_var]])[1]

    if (return == "all"){
       models[[match(period, oos_periods)]] <- model
    }
  }

  print(paste0("End time:", Sys.time()))

  # 8) assign output of function
  if (return == "auc"){
      return(output)
  }
  else if (return == "all") {
     return(list(models = models, output = output))
  }

}








RunRegressionModel <- function(data, subset = NULL, oos_periods, p = "default", trees = 150){

  # 0) make that depvar can be used as such and other setup
  print(paste0("Start time:", Sys.time()))

  # 1) prepare output of function
  output <- data.frame(matrix(nrow = length(oos_periods), ncol = 4))
  colnames(output) <- c("period", "MSE", "r2", "Var")
  output$period <- oos_periods

  # 2) prepare data that is used
  # choose set of regressors
  if (is.null(subset)){
    data <- data %>% select(c(time, municipio, "dv_violent_events_F1", starts_with("rhs_")))
  } else if (!is.null(subset)){
      data <- data %>% select(c(time, municipio, "dv_violent_events_F1", all_of(subset)))
  }

  # drop NAs
  data <- data %>% drop_na()

  # hyperparameter tuning p (mtry)
  if (p == "default"){
    p <- floor(sqrt(ncol(data)-1))
  } else if (p == "tune") {
    print("not implemented for regression yet")
  }

  # 3) loop over periods to get oos forecasts
  for (period in oos_periods){

    print(paste0("Computing period ", match(period, oos_periods), "/",
                 length(oos_periods), "..."))

    # 4) split data into training and test data
    train_data <- data %>% filter(time < period) %>% select(-time)
    # compute average level of violence during observed periods by municipio
    mean_viol <- train_data %>% group_by(municipio) %>%
      summarise(violence_mean = mean(dv_violent_events_F1))
    train_data <- train_data %>% left_join(mean_viol, by = "municipio") %>%
      mutate(y_violence_demeaned_F1 = dv_violent_events_F1 - violence_mean) %>%
      select(-c(dv_violent_events_F1, violence_mean, municipio)) %>% ungroup()
    # compute out of sample deviation from in sample mean
    oos_data <- data %>% filter(time == period) %>% select(-time) %>%
      left_join(mean_viol, by = "municipio") %>%
      mutate(y_violence_demeaned_F1 = dv_violent_events_F1 - violence_mean) %>%
      select(-c(dv_violent_events_F1, violence_mean, municipio)) %>% ungroup()

    # 5) run model
    i <- match("y_violence_demeaned_F1", names(train_data))
    model <- randomForest(x = train_data[-i], y = train_data[[i]],
                          mtry = p, ntree = trees, importance = FALSE, proximity = FALSE)

    # 6) compute MSE and r^2
    fitted <- as.double(predict(model, newdata = oos_data, type = "response"))
    y <- as.double(oos_data$y_violence_demeaned_F1)
    mse <- mean((y - fitted)^2)
    var <- sum((y - mean(y))^2)/(length(y)-1)
    r2 <- 1 - (sum((y - fitted)^2) / sum((y - mean(y))^2))



    # 7) save output
    output[output$period == period, 2] <- mse
    output[output$period == period, 3] <- r2
    output[output$period == period, 4] <- var

  }

  print(paste0("End time:", Sys.time()))

  # 8) assign output of function
  return(output)

}




