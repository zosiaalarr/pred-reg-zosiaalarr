

library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
tidymodels_prefer()

load("attempt_2/results/initial_setup.rda")
load("attempt_2/results/recipe_3.rda")

#########################
# Parallel processing
registerDoMC(cores = 8)
#########################
# define model engine 

nn_model <- mlp(
  mode = "regression", # or regression
  hidden_units = tune(),
  penalty = tune()
) %>%
  set_engine("nnet")

nn_params <- extract_parameter_set_dials(nn_model) 

nn_grid <- grid_regular(nn_params, levels = 5) 

nn_workflow <- workflow() %>% 
  add_model(nn_model) %>% 
  add_recipe(recipe_4)

# Tune grid 
# clear and start timer
tic.clearlog()
tic("Multilayer Perception Neural Network")

nn_tune <- tune_grid(
  nn_workflow,
  resamples = data_folds,
  grid = nn_grid,
  control = control_grid(save_pred = TRUE, # creates extra column for each prediction 
                         save_workflow = TRUE, # lets you use extract_workflow 
                         parallel_over = "everything") # this helps with parallel processing
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

nn_tictoc <- tibble(model = time_log[[1]]$msg,
                    runtime = time_log[[1]]$toc - time_log[[1]]$tic)


save(nn_tune, nn_tictoc, nn_workflow,
     file = "attempt_2/results/nn_4_tuned.rda" )
