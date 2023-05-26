# SVM Tuning 

library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
library(earth)
library(stacks)
tidymodels_prefer()

load("attempt_5/results/initial_setup.rda")
load("attempt_5/results/recipe_9.rda")

#########################
# Parallel processing
registerDoMC(cores = 6)
#########################
# define model engine 

mars_model <- mars(
  mode = "regression", # or "regression"
  num_terms = tune(),
  prod_degree = tune()
) %>%
  set_engine("earth")

mars_params <- extract_parameter_set_dials(mars_model) %>% 
  update(num_terms = num_terms(range = c(1, 25)))

mars_grid <- grid_regular(mars_params, levels = 5) 

mars_workflow <- workflow() %>% 
  add_model(mars_model) %>% 
  add_recipe(recipe_9)

# Tune grid 
# clear and start timer
tic.clearlog()
tic("MARS")

metric <- metric_set(rmse)
ctrl_grid <- control_stack_grid()

mars_tune <- tune_grid(
  mars_workflow,
  resamples = data_folds,
  grid = mars_grid,
  control = ctrl_grid,
  metric = metric_set
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

mars_tictoc <- tibble(model = time_log[[1]]$msg,
                      runtime = time_log[[1]]$toc - time_log[[1]]$tic)


save(mars_tune, mars_tictoc, mars_workflow,
     file = "attempt_5/results/mars_9_tuned.rda" )
