# SVM Tuning 


library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(stacks)
tidymodels_prefer()
set.seed(25)

load("attempt_5/results/initial_setup.rda")
load("attempt_5/results/recipe_9.rda")

#########################
# Parallel processing
registerDoMC(cores = 8)
#########################
# define model engine 

svm_poly_model <- svm_poly(
  mode = "regression",
  cost = tune(),
  degree = tune(),
  scale_factor = tune()
) %>%
  set_engine("kernlab")

svm_poly_params <- extract_parameter_set_dials(svm_poly_model) 

svm_poly_grid <- grid_regular(svm_poly_params, levels = 5) 

svm_poly_workflow <- workflow() %>% 
  add_model(svm_poly_model) %>% 
  add_recipe(recipe_9)

# Tune grid 
# clear and start timer
tic.clearlog()
tic("SVM Polynomial")


metric <- metric_set(rmse)
ctrl_grid <- control_stack_grid()

svm_poly_tune <- tune_grid(
  svm_poly_workflow,
  resamples = data_folds,
  grid = svm_poly_grid,
  metrics = metric, 
  control = ctrl_grid
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

svm_poly_tictoc <- tibble(model = time_log[[1]]$msg,
                          runtime = time_log[[1]]$toc - time_log[[1]]$tic)


save(svm_poly_tune, svm_poly_tictoc, svm_poly_workflow,
     file = "attempt_5/results/svm_poly_9_tuned.rda" )
