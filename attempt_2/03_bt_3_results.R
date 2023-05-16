library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_2/results/bt_3_tuned.rda")
load("attempt_2/results/initial_setup.rda")

bt_workflow <- bt_workflow %>% 
  finalize_workflow(select_best(bt_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(bt_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_10 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_10)

write_csv(sub_10, "attempt_2/submissions/submission_10.csv")
