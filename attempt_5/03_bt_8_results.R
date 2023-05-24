library(tidymodels)
library(tidyverse)
library(bonsai)
library(lightgbm)

tidymodels_prefer()

load("attempt_5/results/bt_8_tuned.rda")
load("attempt_5/results/initial_setup.rda")
load("attempt_5/results/recipe_8.rda")

bt_workflow <- bt_workflow %>% 
  finalize_workflow(select_best(bt_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(bt_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_29 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_29)

show_best(bt_tune)

write_csv(sub_29, "attempt_5/submissions/submission_29.csv")
