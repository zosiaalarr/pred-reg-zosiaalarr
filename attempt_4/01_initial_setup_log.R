library(tidymodels)
library(tidyverse)
library(naniar)
library(doMC)
set.seed(5)
registerDoMC(cores = 8)


train_1 <- read_csv("data/raw/train.csv")

test_2 <- read_csv("data/raw/test.csv")

my_split <- initial_split(train_1, prop = .75, strata = y)

train_log <- training(my_split) %>% 
  mutate(y = log(y))

test <- testing(my_split)



data_folds <- vfold_cv(train_log, folds = 5, repeats = 3)

save(train_1, test_2, test, train_log, data_folds, file = "attempt_4/results/initial_setup_log.rda")

###########################################################
#recipe 7, rf + lasso (recipe 3)
recipe_log <- recipe(y ~ x014 + x017 + x022 + x043 + x086 + x102 + x105 + x108 + x111 + x116 + x135 + x146 +
                     x186 + x253 + x265 + x284 + x286 + x302 + x317 + x328 + x343 + x365 + x366 + x369 +
                     x425 + x427 + x447 + x146 + x105 +  x755 +  x059 +  x702 +  x753 +  x203 +  x561 +  x724 +  x118 +  x014 +
                     x073  + x420 +  x670 +  x548 +  x366  + x244  + x253  + x147 +  x257  + x725 +  x365 +  x619 + 
                     x636, data = train_log) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_knn(all_numeric_predictors()) %>% 
  step_corr(all_predictors()) %>%  
  step_YeoJohnson()

recipe_log %>% 
  prep() %>% 
  bake(new_data = NULL)

save(recipe_log, file =  "attempt_4/results/recipe_log.rda" )