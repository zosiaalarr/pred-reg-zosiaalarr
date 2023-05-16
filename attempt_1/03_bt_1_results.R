library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("results/bt_1_tuned.rda")
load("results/initial_setup.rda")

bt_workflow <- bt_workflow %>% 
  finalize_workflow(select_best(bt_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(bt_workflow, train_data)


data_pred <- predict(final_fit, test_data) %>% 
  bind_cols(test_data %>% select(id))

sub_2 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_2)

write_csv(sub_2, "submissions/submission_2.csv")
