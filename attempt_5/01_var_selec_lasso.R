library(tidymodels) 
library(tidyverse)
library(vip)
library(randomForest)
library(caret)
load("attempt_5/results/initial_setup.rda")

load("attempt_5/results/init_recipe.rda")

set.seed(5)

lasso_mod <- linear_reg(mode = "regression",
                        penalty = tune(),
                        mixture = 1) %>% 
  set_engine("glmnet")

lasso_params <- extract_parameter_set_dials(lasso_mod) 



penalty()

lasso_grid <- grid_regular(lasso_params, levels = 5)

lasso_workflow <- workflow() %>% 
  add_model(lasso_mod) %>% 
  add_recipe(init_recipe)

lasso_tune <- lasso_workflow %>% 
  tune_grid(resamples = data_folds,
            grid = lasso_grid)

lasso_wkflw_final <- lasso_workflow %>% 
  finalize_workflow(select_best(lasso_tune, metric = "rmse"))

lasso_fit <- fit(lasso_wkflw_final, data = train)

save(lasso_fit, file = "data/lasso_variables.rda")


lasso_vars_zero <- lasso_fit %>% 
  tidy() %>% 
  filter(estimate == 0) %>% 
  pull(term)

save(lasso_vars_zero, file = "attempt_5/results/lasso_vars_zero.rda")

#put those term in a step_rm() step_rm(all_of(lasso_vars_zero))


