library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_2/results/rf_3_tuned.rda")
load("attempt_2/results/initial_setup.rda")

rf_workflow <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(rf_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_9 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_9)

write_csv(sub_9, "attempt_2/submissions/submission_9.csv")
