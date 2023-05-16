# Elastic Net Tuning 

library(tidymodels)
library(tidyverse)
library(tictoc)
library(doMC)
tidymodels_prefer()

load("attempt_2/results/initial_setup.rda")
load("attempt_2/results/recipe_2.rda")


#########################
# Parallel processing
registerDoMC(cores = 8)
#########################
# define model engine and workflow

en_model <- linear_reg(mixture = tune(),
                       penalty = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

en_params <- extract_parameter_set_dials(en_model) 

en_grid <- grid_regular(en_params, levels = 5) 


en_workflow <- workflow() %>% 
  add_model(en_model) %>% 
  add_recipe(recipe_2)

########################################################################
# Tune grid 
# clear and start timer
tic.clearlog()
tic("Elastic Net")

en_tune <- tune_grid(
  en_workflow,
  resamples = data_folds,
  grid = en_grid,
  control = control_grid(save_pred = TRUE,
                         save_workflow = TRUE,
                         parallel_over = "everything") # this helps with parrallel processing
)

toc(log = TRUE)

time_log <- tic.log(format = FALSE)

en_tictoc <- tibble(model = time_log[[1]]$msg,
                    runtime = time_log[[1]]$toc - time_log[[1]]$tic)
en_tictoc

save(en_tune, en_tictoc, en_workflow,
     file = "attempt_2/results/en_2_tuned.rda" )


