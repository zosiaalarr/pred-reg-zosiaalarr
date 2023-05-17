library(tidymodels) 
library(tidyverse)
library(vip)
library(randomForest)
library(caret)
load("attempt_3/results/initial_setup.rda")

set.seed(5)

# init_recipe <- recipe(y ~., data = train_data) %>% 
#   #step_rm(id) %>% 
#   step_nzv(all_predictors()) %>% 
#   step_normalize(all_numeric_predictors()) %>% 
#   step_impute_mean(all_numeric_predictors())
# 
# init_recipe %>% 
#   prep() %>% 
#   bake(new_data = NULL)

set.seed(23)
lasso_mod <- linear_reg(mode = "regression",
                        penalty = tune(),
                        mixture = 1) %>% 
  set_engine("glmnet")

lasso_params <- extract_parameter_set_dials(lasso_mod) %>% 
  update(penalty = penalty(range = c(0,5)))

lasso_grid <- grid_regular(lasso_params, levels = 5)

lasso_workflow <- workflow() %>% 
  add_model(lasso_mod) %>% 
  add_recipe(init_recipe)

lasso_tune <- lasso_workflow %>% 
  tune_grid(resamples = data_folds,
            grid = lasso_grid)

lasso_wkflw_final <- lasso_workflow %>% 
  finalize_workflow(select_best(lasso_tune, metric = "rmse"))

lasso_fit <- fit(lasso_wkflw_final, data = train_data)

save(lasso_fit, file = "data/lasso_variables.rda")

load("data/lasso_variables.rda")

lasso_tidy <- lasso_fit %>% 
  tidy() %>% 
  filter(estimate != 0)

View(lasso_tidy)



######################################################################
# rf 
rf_mod <- rand_forest(mode = "regression",
                      mtry = tune()) %>% 
  set_engine("ranger", importance = "impurity")

rf_params <- extract_parameter_set_dials(rf_mod) %>% 
  update(mtry = mtry(range = c(1,5)))

rf_grid <- grid_regular(rf_params, levels = 3)

rf_workflow <- workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(init_recipe)

rf_tune <- rf_workflow %>% 
  tune_grid(resamples = data_folds,
            grid = rf_grid)

rf_wkflw_final <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

rf_fit <- fit(rf_wkflw_final, data = train_data)

# view estimate parameters
rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip::vip()

#####################################################
# build rf model 
rf_import <- randomForest(y ~., data = train, ntree = 1000)

# get variable importance 
var_importance <- varImp(rf_import)

# extract var names 
var_names <- rownames(var_importance)

# sort variable names based on importance 
sorted_var_names <- var_names[order(var_importance$Overall, decreasing = TRUE)]

# print sorted varible names 
print(sorted_var_names)

save(sorted_var_names, file = "attempt_3/results/sorted_var_names")

