# KNN Tuning 

library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(stacks)
tidymodels_prefer()

set.seed(5)

load("attempt_5/results/initial_setup.rda")
load("attempt_5/results/recipe_9.rda")

#########################
# Parallel processing
registerDoMC(cores = 6)
#########################
# define model engine 

knn_model <- nearest_neighbor(mode = "regression",
                              neighbors = tune()) %>% 
  set_engine("kknn")

knn_params <- extract_parameter_set_dials(knn_model) 

knn_grid <- grid_regular(knn_params, levels = 5) 

knn_workflow <- workflow() %>% 
  add_model(knn_model) %>% 
  add_recipe(recipe_9)

########################################################################
# Tune grid 
# clear and start timer
tic.clearlog()
tic("K Nearest Neighbors")

metric <- metric_set(rmse)
ctrl_grid <- control_stack_grid()

knn_tune <- tune_grid(
  knn_workflow,
  resamples = data_folds,
  grid = knn_grid,
  control = ctrl_grid,
  metric = metric_set
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

knn_tictoc <- tibble(model = time_log[[1]]$msg,
                     runtime = time_log[[1]]$toc - time_log[[1]]$tic)


save(knn_tune, knn_tictoc, knn_workflow,
     file = "attempt_5/results/knn_9_tuned.rda" )


