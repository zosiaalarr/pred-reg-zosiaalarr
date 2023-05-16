# we don't know the true y variable but kaggle does know the trut 
# do feature engeneering and get a prediction 
# sbmit your prediction with id collumn and y variable 
# when you re train your data youy should use a new set of folds, just run folds again 

library(tidymodels)
library(tidyverse)
library(naniar) 
library(doMC)
set.seed(5)
registerDoMC(cores = 8)



train_1 <- read_csv("data/raw/train.csv")

test_2 <- read_csv("data/raw/test.csv")

my_split <- initial_split(train_1, prop = .75, strata = y)

train <- training(my_split)

test <- testing(my_split)

data_folds <- vfold_cv(train, folds = 5, repeats = 3)

save(train_1, test_2, test, train, data_folds, file = "attempt_1/results/initial_setup.rda")



###########################################################
# eda

ggplot(train_data, aes(x = y)) + 
  geom_histogram()

ggplot(train_data, aes(x = sqrt(y))) + 
  geom_histogram()

miss_table <- naniar::miss_var_summary(train_data)

View(miss_table) # we can impute all 

##########################################################
# basic recipie to find important vars 

init_recipe <- recipe(y ~., data = train_data) %>% 
  step_rm(id) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors())

init_recipe %>% 
  prep() %>% 
  bake(new_data = NULL)

save(test_data, train_data, data_folds, init_recipe, file = "results/initial_setup.rda")

###########################################################
# second recipe

recipe_2 <- recipe(y ~ x146 + x102 + x014 + x687 + x696 + x420 + x427 + x105 + x355 + x378, data = train_data) %>% 
  #step_rm(id) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors())

recipe_2 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe_2, file = "results/recipe_2.rda")

###############################################################
# third recipe, from lasso
recipe_3 <- recipe(y ~ x014 + x017 + x022 + x043 + x086 + x102 + x105 + x108 + x111 + x116 + x135 + x146 +
                   x186 + x253 + x265 + x284 + x286 + x302 + x317 + x328 + x343 + x365 + x366 + x369 +
                   x425 + x427 + x447, data = train_data) %>% 
  #step_rm(id) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_corr(all_predictors())

recipe_3 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe_3, file = "results/recipe_3.rda")
#########################################################
# fourth recipe, from lasso all variables 

recipe_4 <- recipe(y ~ x014 + x017 + x022 + x043 + x086 + x102 + x105 + x108 + x111 + x116 + x135 + x146 +
                   x186 + x253 + x265 + x284 + x286 + x302 + x317 + x328 + x343 + x365 + x366 + x369 +
                     x425 + x427 + x447 + x454 + 	
                     x466 + x477 + 	
                     x487 + x488 + 	
                     x500 + x532 + x561 + x567 + x568 + x581 + x591 + x619 + x622 + x626 + x660 + x668 + x685 + 
                     x699 + x702 + x717 + x734 + x740 + x750 + x752, data = train) %>% 
  #step_rm(id) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_bag(all_numeric_predictors()) %>% 
  step_corr(all_predictors())

recipe_4 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe_4, file = "attempt_1/results/recipe_4.rda")

##################################################################################
# log version 

recipe_5 <- recipe(y ~ x014 + x017 + x102 + x105 + x108 + x146 + x186 +
                     x477 + x561 + x567 + x568, x581 + x619 +x622 + x740 +x750, data = train_data) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_corr(all_predictors())

recipe_5 %>% 
  prep() %>% 
  bake(new_data = NULL)
View(recipe_5)

save(recipe_5, file = "results/recipe_5.rda")

