library(tidymodels)
library(tidyverse)
library(naniar)
library(doMC)
set.seed(5)
registerDoMC(cores = 8)


train_1 <- read_csv("data/raw/train.csv")

test_2 <- read_csv("data/raw/test.csv")

load("attempt_5/results/lasso_vars_zero.rda")

my_split <- initial_split(train_1, prop = .75, strata = y)

train <- training(my_split)

test <- testing(my_split)



data_folds <- vfold_cv(train, folds = 5, repeats = 3)

save(train_1, test_2, test, train, data_folds, file = "attempt_5/results/initial_setup.rda")

#############
# init recipe 
init_recipe <- recipe(y ~., data = train) %>% 
  step_rm(id) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr() %>% 
  step_impute_mean(all_numeric_predictors())

init_recipe %>% 
  prep() %>% 
  bake(new_data = NULL)


save(init_recipe, file = "attempt_5/results/init_recipe.rda")

####################################
recipe_8 <- recipe(y ~., data = train) %>% 
  step_rm(!!lasso_vars_zero) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr() %>% 
  #step_pca(0.97)
  step_impute_median(all_numeric_predictors()) 

recipe_8 %>% 
  prep() %>% 
  bake(new_data = NULL)

lightgbm
lightgbm

save(recipe_8, file = "attempt_5/results/recipe_8.rda")

