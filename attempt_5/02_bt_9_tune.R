# Boosted Tree Tuning 

library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(stacks)
tidymodels_prefer()


load("attempt_5/results/initial_setup.rda")
load("attempt_5/results/recipe_9.rda")

#########################
# Parallel processing
registerDoMC(cores = 6)
#########################
# define model engine 
bt_model <- boost_tree(mode = "regression",
                       min_n = tune(),
                       mtry = tune(),
                       learn_rate = tune()) %>%
  set_engine("xgboost", importance = "impurity")


bt_params <- extract_parameter_set_dials(bt_model) %>% 
  update(mtry = mtry(range = c(1, 25)))

bt_grid <- grid_regular(bt_params, levels = 5)

bt_workflow <- workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(recipe_9)

########################################################################
# Tune grid 
# clear and start timer
tic.clearlog()
tic("Boosted Tree")

metric <- metric_set(rmse)
ctrl_grid <- control_stack_grid()

bt_tune <- tune_grid(
  bt_workflow,
  resamples = data_folds,
  grid = bt_grid,
  control = ctrl_grid, 
  metric = metric_set
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

bt_tictoc <- tibble(model = time_log[[1]]$msg,
                    runtime = time_log[[1]]$toc - time_log[[1]]$tic)


save(bt_tune, bt_tictoc, bt_workflow,
     file = "attempt_5/results/bt_9_tuned.rda" )


