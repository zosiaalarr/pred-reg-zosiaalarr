# SVM Tuning 

library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
tidymodels_prefer()

load("attempt_3/results/initial_setup.rda")
load("attempt_3/results/recipe_3.rda")

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
  update(rbf_sigma = rbf_sigma(c(.002, .0035)), trans = NULL)



svm_radial_grid <- grid_regular(svm_radial_params, levels = 5) 

svm_radial_workflow <- workflow() %>% 
  add_model(svm_radial_model) %>% 
  add_recipe(recipe_3)

# Tune grid 
# clear and start timer
tic.clearlog()
tic("SVM Radial")

svm_radial_tune <- tune_grid(
  svm_radial_workflow,
  resamples = data_folds,
  grid = svm_radial_grid,
  control = control_grid(save_pred = TRUE, # creates extra column for each prediction 
                         save_workflow = TRUE, # lets you use extract_workflow 
                         parallel_over = "everything") # this helps with parallel processing
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

svm_radial_tictoc <- tibble(model = time_log[[1]]$msg,
                            runtime = time_log[[1]]$toc - time_log[[1]]$tic)


save(svm_radial_tune, svm_radial_tictoc, svm_radial_workflow,
     file = "attempt_3/results/svm_rad_1_tuned.rda" )
