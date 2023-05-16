library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("results/rf_5_tuned.rda")
load("results/initial_setup.rda")

rf_workflow <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune_5, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(rf_workflow, train_data)


data_pred <- predict(final_fit, test_data) %>% 
  bind_cols(test_data %>% select(id))

sub_9 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_9)

write_csv(sub_9, "submissions/submission_9.csv")
