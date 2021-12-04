# Created by: Luca Poll
# Created on: 27.07.2021

# ------------------------------------------------------------------------------
# Part O: setup
# ------------------------------------------------------------------------------

# libraries used
library(tidyverse)
library(doParallel)
library(ggsci)
library(randomForest)
library(caret)
library(ROCR)

# color palette
colpal <- pal_nejm("default")(8)

# set working directory
setwd("C:/Users/Luca Poll/Google Drive/TSE/Thesis/Code/Model")

# load data
# bazzi data
load("DATA/bazzi_data.RData")
# master dataset
load("DATA/master_data.RData")

# load model functions
source("CODE/RandomForest.R")

# register number of cores for parallel computing
registerDoParallel(makePSOCKcluster(2))


################################################################################
# note: due to the stochastic nature of the random forest algorithm are the  ###
#       results probably not going to be exactly the same as reported in the ###
#       paper since 250 trees still inherit some randomness. I therefore en- ###
#       courage to run the models several times to see that the reported     ###
#       results are conservative reports.                                    ###
################################################################################

# set of variables that are available:
# regressors:
# - y_violence_orig: indicator for violence one period ahead in/from bazzi dataset
# - y_violence_F1: indicator for violence one period ahread (CNMH data)
# - y_violence_F2: indicator for violence two periods ahread (CNMH data)
# - y_violence_F3: indicator for violence three period ahread (CNMH data)
# - y_violence_hotspot_F1: indicator for more than five violent incidences five periods ahead
# - y_violence_demeaned_F1: number of violent events demeaned by municipio

# other possible dependent variables : dv_victims, dv_total_victims, dv_injured,
#                  dv_total_injured, dv_total_hostages, dv_violent_events
#                  -> as F1, F2 & F3

# regressors:
# - allbazzi: all variables from Bazzi et al (2021)
# - structural: structural variables from Bazzi et al (2021)
# - sentiment: icews sentiment measures (rhs_tv_sent_ ...)
# - quad: icews quad category counts (rhs_tv_quad_ ...)
# - cameo: icews cameo category counts (rhs_tv_cameo_ ...)
# - violence_history: data on violent event counts, victims, injured and captured
#                  as well as the winning side, the attacker and the type of
#                  violence with the respective first three lags (L0, L1, L2, L3)

# compare outcomes of models with different specifications

# 2 different outcomes: y_violence_F1 and y_violence_hotspot_F1

# 4 different specifications: structural, structural sentiment,
#                             structural violence selected,
#                             structural violence selected sentiment


# ------------------------------------------------------------------------------
# Part 1: Models
# ------------------------------------------------------------------------------


################################################################################
# run violence hotpots specification

filename <- "results_violence_hotspot_downsampled_tuned"
specs <- list(c(ar), c(structural_selected),
              c(structural_selected, violence_selected, sentiment),
              c(structural_selected, violence_history),
              c(structural_selected, violence_selected, quad))
spec_names <- c("ar", "structural", "structural_violence_selected_sentiment",
                "structural_violence_history",
                "structural_violence_selected_quad")

results <- data.frame(matrix(nrow = length(spec_names), ncol = 4))
names(results) <- c("model", "mean_AUC", "mean_AUCPR", "mean_imbalance")
count <- 0

for (specification in specs){

  count <- count + 1
  print(paste0("loop ", count,"/", length(specs)))
  name <- paste0("rural_", spec_names[count], "_downs")
  model <-  RunModel(
    data = master_data,
    dependent_var = "y_violence_hotspot_F1",
    subset = specification,
    no_var = FALSE,
    oos_periods = seq(as.Date("2011-01-01"), as.Date("2014-10-01"), by = "quarter"),
    imbal = "downsample",
    p = "tune",
    trees = 1000)
  results[count,2] <- model %>% summarise(AUC = mean(AUC))
  results[count,3] <- model %>% summarise(AUCPR = mean(AUCPR))
  results[count,4] <- model %>% summarise(imbalance = mean(imbalance))
  results[count,1] <- spec_names[count]
  save(results, file = paste0(filename, ".RData"))
}





################################################################################
# run violence outcome

spec_name <- c("structural_violence_history", "structural_violence",
                "structural_violence_sentiment", "ar", "structural",
                "structural_sentiment", "structural_violence_selected_quad")

filename <- "results_violence_downsampled_tuned_short"
specs <- list(c(ar),
              c(structural_selected),
              c(structural_selected, violence_selected, sentiment),
              c(structural_selected, violence_history),
              c(structural_selected, violence_selected, quad))
spec_names <- c("ar", "structural", "structural_violence_selected_sentiment",
                "structural_violence_history",
                "structural_violence_selected_quad")
results <- data.frame(matrix(nrow = length(spec_names), ncol = 4))
names(results) <- c("model", "mean_AUC", "mean_AUCPR", "mean_imbalance")
count <- 0

for (specification in specs){

  count <- count + 1
  print(paste0("loop ", count,"/", length(specs)))
  name <- paste0("rural_", spec_names[count], "_downs")
  model <-  RunModel(
              data = master_data,
              dependent_var = "y_violence_F1",
              subset = specification,
              no_var = FALSE,
              oos_periods = seq(as.Date("2011-01-01"), as.Date("2014-10-01"), by = "quarter"),
              imbal = "downsample",
              p = "tune",
              trees = 100)
  results[count,1] <- spec_names[count]
  results[count,2] <- model %>% summarise(AUC = mean(AUC))
  results[count,3] <- model %>% summarise(AUCPR = mean(AUCPR))
  results[count,4] <- model %>% summarise(imbalance = mean(imbalance))
  save(results, file = paste0("OUTPUT/", filename, ".RData"))
}







################################################################################
# violence onset

master_data <- master_data %>%
  group_by(municipio) %>% arrange(time) %>%
  mutate(y_onset = factor(if_else(y_violence_F1 == "yes" &
                                    lag(y_violence_F1) == "no" &
                                    lag(y_violence_F1, 2) == "no" &
                                    lag(y_violence_F1, 3) == "no" &
                                    lag(y_violence_F1, 4) == "no",
                                  "yes", "no"))) %>%
  ungroup()

onset_data <- master_data %>%
                filter(rhs_tv_viol_violent_events_L0 == 0,
                       rhs_tv_viol_violent_events_L1 == 0,
                       rhs_tv_viol_violent_events_L2 == 0,
                       rhs_tv_viol_violent_events_L3 == 0)


# run on violence onset
filename <- "results_violence_onset_downsampled_tuned"
specs <- list(c(structural_selected),
              c(structural_selected, violence_selected, sentiment),
              c(structural_selected, violence_history),
              c(structural_selected, violence_selected, quad))
spec_names <- c("structural",
                "structural_violence_selected_sentiment",
                "structural_violence_history",
                "structural_violence_selected_quad")
results <- data.frame(matrix(nrow = length(spec_names), ncol = 4))
names(results) <- c("model", "mean_AUC", "mean_AUCPR", "mean_imbalance")
count <- 0

for (specification in specs){

  count <- count + 1
  print(paste0("loop ", count,"/", length(specs)))
  name <- paste0("rural_", spec_names[count], "_downs")
  model <-  RunModel(
              data = onset_data,
              dependent_var = "y_onset",
              subset = specification,
              no_var = FALSE,
              oos_periods = seq(as.Date("2011-01-01"), as.Date("2014-10-01"), by = "quarter"),
              imbal = "downsample",
              p = "tune",
              trees = 250)
  results[count,1] <- spec_names[count]
  results[count,2] <- model %>% summarise(AUC = mean(AUC))
  results[count,3] <- model %>% summarise(AUCPR = mean(AUCPR))
  results[count,4] <- model %>% summarise(imbalance = mean(imbalance))
  save(results, file = paste0("OUTPUT/", filename, ".RData"))
}





################################################################################

# violence onset 2
master_data <- master_data %>%
  group_by(municipio) %>% arrange(time) %>%
  mutate(y_onset2 = factor(if_else(y_violence_F1 == "yes" &
                                    lag(y_violence_F1) == "no" &
                                    lag(y_violence_F1, 2) == "no" &
                                    lag(y_violence_F1, 3) == "no" &
                                    lag(y_violence_F1, 4) == "no" &
                                    lag(y_violence_F1, 5) == "no" &
                                    lag(y_violence_F1, 6) == "no" &
                                    lag(y_violence_F1, 7) == "no" &
                                    lag(y_violence_F1, 8) == "no",
                                  "yes", "no"))) %>%
  ungroup()

onset2_data <- master_data %>% arrange(time) %>%
                filter(rhs_tv_viol_violent_events_L0 == 0,
                       rhs_tv_viol_violent_events_L1 == 0,
                       rhs_tv_viol_violent_events_L2 == 0,
                       rhs_tv_viol_violent_events_L3 == 0,
                       lag(rhs_tv_viol_violent_events_L3 == 0),
                       lag(rhs_tv_viol_violent_events_L3 == 0, 2),
                       lag(rhs_tv_viol_violent_events_L3 == 0, 3),
                       lag(rhs_tv_viol_violent_events_L3 == 0, 4))

table(master_data$y_onset, master_data$time)
table(master_data$y_onset2, master_data$time)

# run on violence onset
filename <- "results_violence_onset2_downsampled_tuned"
specs <- list(c(structural_selected), c(structural_selected, violence_history),
              c(structural_selected, violence_selected, sentiment),
              c(structural_selected, violence_selected, quad))
results <- data.frame(matrix(nrow = length(spec_names), ncol = 4))
names(results) <- c("model", "mean_AUC", "mean_AUCPR", "mean_imbalance")
count <- 0

for (specification in specs){

  count <- count + 1
  print(paste0("loop ", count,"/", length(specs)))
  name <- paste0("rural_", spec_names[count], "_downs")
  model <-  RunModel(
              data = onset2_data,
              dependent_var = "y_onset2",
              subset = specification,
              no_var = FALSE,
              oos_periods = seq(as.Date("2011-01-01"), as.Date("2013-10-01"), by = "quarter"),
              imbal = "downsample",
              p = "tune",
              trees = 1000)
  results[count,1] <- spec_names[count]
  results[count,2] <- model %>% summarise(AUC = mean(AUC))
  results[count,3] <- model %>% summarise(AUCPR = mean(AUCPR))
  results[count,4] <- model %>% summarise(imbalance = mean(imbalance))
  save(results, file = paste0("OUTPUT/", filename, ".RData"))
}




stopCluster(makePSOCKcluster(2))

































