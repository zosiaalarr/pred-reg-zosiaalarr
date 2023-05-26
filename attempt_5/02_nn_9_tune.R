

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
  add_recipe(recipe_9)

# Tune grid 
# clear and start timer
tic.clearlog()
tic("Multilayer Perception Neural Network")

metric <- metric_set(rmse)
ctrl_grid <- control_stack_grid()

nn_tune <- tune_grid(
  nn_workflow,
  resamples = data_folds,
  grid = nn_grid,
  control = ctrl_grid,
  metric = metric_set
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

nn_tictoc <- tibble(model = time_log[[1]]$msg,
                    runtime = time_log[[1]]$toc - time_log[[1]]$tic)


save(nn_tune, nn_tictoc, nn_workflow,
     file = "attempt_5/results/nn_9_tuned.rda" )
