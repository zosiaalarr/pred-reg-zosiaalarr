library(tidymodels)
library(tidyverse)
library(naniar)
library(doMC)
set.seed(5)
registerDoMC(cores = 8)

test_data <- read_csv("data/raw/test.csv") 

train_data <- read_csv("data/raw/train.csv") 

data_folds <- vfold_cv(train_data, folds = 5, repeats = 3)

# best practice is to split the training data to try and prevent overfitting 

my_split <- initial_split(train_data, prop = .75, strata = y)

sub_train <- training(my_split)

sub_test <- testing(my_split)

##########################################################################
# Functions for exploration 

boxplot_fun <- function(var = NULL){
  ggplot(sub_train, aes(x = factor(!!sym(var)), y = y)) + 
    geom_boxplot()
}

boxplot_log_fun <- function(var = NULL){
  ggplot(sub_train, aes(x = factor(!!sym(var)), y = log(y))) + 
    geom_boxplot()
}


##########################################################################
# distribution of y 

ggplot(sub_train, aes(x = y)) + 
  geom_histogram()

MASS::boxcox(lm(y ~ 1, sub_train))
# recommends a log transformation 

ggplot(sub_train, aes(x = log(y))) + 
  geom_histogram()

##########################################################################
# missingness 

missing_lst <- list()


for(var in colnames(sub_train)){
  missing_lst[var] <- sub_train %>% 
    select(any_of(var)) %>% 
    filter(is.na(!!sym(var))) %>%  # !!sym turns character var into something useful, unquotes the value
    summarize(num_missing = n())
}

missing_tbl <- enframe(unlist(missing_lst))
View(missing_tbl)

missing_tbl %>% 
  mutate(pct = value/4034) %>% 
  arrange(desc(pct))

##########################################################################
##########################################################################
# see if there are constant variable to get rid of
# remove zero_var, also could be done with step_zv()

var_lst <- list()


for(var in colnames(sub_train)){
  var_lst[var] <- sub_train %>% 
    select(any_of(var)) %>% 
    summarize(sd = sd(!!sym(var), na.rm = TRUE)) 
}

var_tbl <- enframe(unlist(var_lst))
View(var_tbl)

# remove zero variance 
# mental note that high variance might benefit from a tranformation 

zero_var <- var_tbl %>% 
  filter(value == 0) %>% 
  pull(name) # tidyverse version of data$variable, vector of var of interest  

# update training data to remove unwanted variables 
sub_train %>% 
  select(!all_of(zero_var))

##########################################################################
##########################################################################
# high correlation 
# could remove with step_corr 


##########################################################################
##########################################################################
# miscoded categorical variables 
cat_lst <- list()


for(var in colnames(sub_train)){
  cat_lst[var] <- sub_train %>% 
    select(any_of(var)) %>% 
    # count of unique values in variable 
    summarize(unique = length(unique(!!sym(var)))) 
}

cat_tbl <- enframe(unlist(cat_lst))
View(cat_tbl)

cat_var <- cat_tbl %>% 
  filter(value <= 10) %>% 
  pull(name)

boxplot_fun(var = "x025")

boxplot_log_fun(var = "x025")

map(cat_var, boxplot_fun)

# turn to factor with mutate
# could write a function for scatter plots to explore relation with y 

# from here, save "clean data" 

write_rds(sub_train, "data/processed/sub_train.rds")
write_rds(sub_test, "data/processed/sub_test.rds")

