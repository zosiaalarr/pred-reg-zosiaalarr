library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("results/rf_3_tuned.rda")
load("results/initial_setup.rda")

rf_workflow <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune_3, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(rf_workflow, train_data)


data_pred <- predict(final_fit, test_data) %>% 
  bind_cols(test_data %>% select(id))

sub_7 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_7)

write_csv(sub_7, "submissions/submission_7.csv")
