library(tidymodels)
library(tidyverse)
library(naniar)
library(doMC)
library(tictoc)

registerDoMC(cores = 8)

load("results/initial_setup.rda")
load("results/recipe_5.rda")

rf_model <- rand_forest(mtry = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")


# set-up tuning grid ----

rf_params <- extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(c(1, 9)))



# define tuning grid
rf_grid <- grid_regular(rf_params, levels = 5)

# workflow ----
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(recipe_5)


# Tuning/fitting ----

rf_tune_5 <- tune_grid(
  rf_workflow,
  resamples = data_folds,
  grid = rf_grid,
  control = control_grid(save_pred = TRUE,
                         save_workflow = TRUE,
                         parallel_over = "everything") # this helps with parrallel processing
)

# tic("Random Forest Recipe 5")
# 
# toc(log = TRUE)
# 
# # save runtime info
# time_log <- tic.log(format = FALSE)
# 
# rf_5_tictoc <- tibble(
#   model = time_log[[1]]$msg,
#   start_time = time_log[[1]]$tic,
#   end_time = time_log[[1]]$toc,
#   runtime = end_time - start_timex
# )


save(rf_tune_5, rf_workflow,
     file = "results/rf_5_tuned.rda" )

