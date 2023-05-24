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
######################################################
# most important from lasso
recipe_9 <- recipe(y ~  x001   +       x006     +     x015    +      x021+          x028    +    
                     x031       +   x033+          x035+          x037  +      x044    +      x058    +    
                     x079     +   +  x087+          x089+          x092  +        x094     +     x096  +      
                    x099     +     x100   +       x101   +       x105     +     x111      +    x114     +   
                    x116      +    x120    +      x121    +      x128      +    x131+          x146      +  
                  x149        +  x155       +   x156       +   x159         + x164   +       x168  +      
                   x172        +  x175       +   x186       +   x192         + x202   +       x203  +      
                     x205       +   x207      +    x208      +    x215        +  x217  +        x218 +       
                     x219        +  x223       +   x225       +   x228         + x229   +       x239  +      
                   x242 +         x251          +x252          +x253+          x261      +    x265     +   
                  x270   +       x271 +         x283+          x284  +        x288        +  x289       + 
                    x293  +        x294 +         x296+          x302 +         x314       +   x323      +  
                    x334   +       x336  +        x341+          x342  +        x350        +  x353       + 
                   x357     +     x364    +      x366  +        x368   +       x387          +x390        +
                   x398      +    x409     +     x417   +       x427    +      x429 +         x431        +
                     x437     +     x448    +      x451  +        x463   +       x472+          x475       + 
                    x477       +   x480      +    x509    +      x510     +     x512  +        x524        +
                 x526   +       x529          +x533        +  x534         + x540      +    x545        +
                     x549   +       x553  +        x555     +     x556      +    x561   +       x563     +   
                     x567    +      x575    +      x576      +    x578       +   x579    +      x581      +  
                     x598     +     x614     +     x619       +   x622        +  x627     +     x631       + 
                     x645      +    x655      +    x661        +  x662         + x664      +    x665        +
                    x668        +  x669    +      x670          +x679 +         x684        +  x695        +
                     x704   +       x713       +   x717   +       x724 +         x736        +  x739        +
                   x741      +    x744          +x749      +    x752    +      x755,  data = train ) %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr() %>% 
  step_impute_mean(all_numeric_predictors())

recipe_9 %>% 
  prep() %>% 
  bake(new_data = NULL)
save(recipe_9, file = "attempt_5/results/recipe_9.rda")

