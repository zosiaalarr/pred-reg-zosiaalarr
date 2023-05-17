library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_3/results/rf_1_tuned.rda")
load("attempt_3/results/initial_setup.rda")

rf_workflow <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rmse"))



# fit training data to final workflow 

final_fit <- fit(rf_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_16 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)



write_csv(sub_16, "attempt_3/submissions/submission_12.csv")


