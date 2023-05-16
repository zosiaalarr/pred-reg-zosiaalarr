library(tidymodels)
library(tidyverse)
library(naniar)
library(doMC)
library(tictoc)
set.seed(25)

registerDoMC(cores = 8)

load("attempt_1/results/initial_setup.rda")
load("attempt_1/results/recipe_4.rda")

rf_model <- rand_forest(mtry = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")


# set-up tuning grid ----

rf_params <- extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(c(1, 25)))



# define tuning grid
rf_grid <- grid_regular(rf_params, levels = 5)

# workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe_4)


# Tuning/fitting ----

rf_tune_4 <- tune_grid(
  rf_workflow,
  resamples = data_folds,
  grid = rf_grid,
  control = control_grid(save_pred = TRUE,
                         save_workflow = TRUE,
                         parallel_over = "everything") # this helps with parrallel processing
)

tic("Random Forest Recipe 4")

toc(log = TRUE)

# save runtime info
time_log <- tic.log(format = FALSE)

rf_4_tictoc <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_timex
)


save(rf_tune_4, rf_4_tictoc, rf_workflow,
     file = "attempt_1/results/rf_4_tuned.rda" )

