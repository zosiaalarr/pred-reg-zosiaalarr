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

save(train_1, test_2, test, train, data_folds, file = "attempt_4/results/initial_setup.rda")

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


save(init_recipe, file = "attempt_4/results/init_recipe.rda")

###############
# recipe 1, lasso variable, penalty range 0-5 

recipe_1 <- recipe(y ~ x014 + x017 + x102+ x105+ x108 + x146 +  x186 + 
                     x477 + x561 + x567 + x568 + x581 + x619 + x622 + x740 + x750, data = train) %>% 
  #step_rm(id) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr() %>% 
  step_impute_mean(all_numeric_predictors())

recipe_1 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe_1, file = "attempt_4/results/recipe_1.rda")
############################
# recipe 2, rf variables 
recipe_2 <- recipe(y ~ x146 + x355 + x014 + x105 + x102 
                   + x488 + x670 + x561 + x619 + x096, data = train) %>% 
  #step_rm(id) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr() %>% 
  step_impute_mean(all_numeric_predictors())

recipe_2 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe_2, file = "attempt_4/results/recipe_2.rda")

###############################
# recipe 3, rf+ lasso vars 
recipe_3 <- recipe(y ~ x014 + x017 + x102+ x105+ x108 +  x186 + x146 + x355 + x488 + x670 + x096 +
                     x477 + x561 + x567 + x568 + x581 + x619 + x622 + x740 + x750, data = train) %>% 
  #step_rm(id) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr() %>% 
  step_impute_mean(all_numeric_predictors())

recipe_3 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe_3, file = "attempt_4/results/recipe_3.rda")
##################################
# ryans vars 
recipe_4 <- recipe(y ~ x014 + x017 + x102 + x105 + x108 + x146 + x186 + x477 + 
x561 + x567 + x568 + x581 + x619 + x622 + x740 + x750 + 
x355 + x755 + x670 + x687 + x721, data = train) %>%  
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr() %>% 
  step_impute_bag(all_numeric_predictors())

recipe_4 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe_4, file = "attempt_4/results/recipe_4.rda")

###############################################################
#recipe 7, rf + lasso (recipe 3)
recipe_7 <- recipe(y ~ x014 + x017 + x022 + x043 + x086 + x102 + x105 + x108 + x111 + x116 + x135 + x146 +
                     x186 + x253 + x265 + x284 + x286 + x302 + x317 + x328 + x343 + x365 + x366 + x369 +
                     x425 + x427 + x447 + x146 + x105 +  x755 +  x059 +  x702 +  x753 +  x203 +  x561 +  x724 +  x118 +  x014 +
                     x073  + x420 +  x670 +  x548 +  x366  + x244  + x253  + x147 +  x257  + x725 +  x365 +  x619 + 
                     x636, data = train) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_knn(all_numeric_predictors()) %>% 
  step_corr(all_predictors()) %>%  
  step_YeoJohnson()

recipe_7 %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe_7, file =  "attempt_4/results/recipe_7.rda" )



