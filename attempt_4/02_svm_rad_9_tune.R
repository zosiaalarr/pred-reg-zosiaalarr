# SVM Tuning 

library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(stacks)
tidymodels_prefer()
set.seed(5)

load("attempt_4/results/initial_setup.rda")
load("attempt_4/results/recipe_7.rda")

#########################
# Parallel processing
registerDoMC(cores = 8)
#########################
# define model engine 

svm_radial_model <- svm_rbf(
  mode = "regression", 
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab")

svm_radial_params <- extract_parameter_set_dials(svm_radial_model) %>% 
  update(cost = cost(range = c(8,45), trans = NULL), 
         rbf_sigma = rbf_sigma(range = c(.00200, .004500), trans = NULL))

svm_radial_grid <- grid_regular(svm_radial_params, levels = 5) 

svm_radial_workflow <- workflow() %>% 
  add_model(svm_radial_model) %>% 
  add_recipe(recipe_7)

# Tune grid 
# clear and start timer
tic.clearlog()
tic("SVM Radial")

metric <- metric_set(rmse)
ctrl_grid <- control_stack_grid()

svm_radial_tune <- tune_grid(
  svm_radial_workflow,
  resamples = data_folds,
  grid = svm_radial_grid,
  metrics = metric, 
  control = ctrl_grid
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

svm_radial_tictoc <- tibble(model = time_log[[1]]$msg,
                            runtime = time_log[[1]]$toc - time_log[[1]]$tic)


save(svm_radial_tune, svm_radial_tictoc, svm_radial_workflow,
     file = "attempt_4/results/svm_rad_9_tuned.rda" )
