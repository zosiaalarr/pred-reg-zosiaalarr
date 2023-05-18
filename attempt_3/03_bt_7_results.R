library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_3/results/bt_7_tuned.rda")
load("attempt_3/results/initial_setup.rda")

bt_workflow <- bt_workflow %>% 
  finalize_workflow(select_best(bt_tune, metric = "rmse"))



# fit training data to final workflow 

final_fit <- fit(bt_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

 sub_23 <- data_pred %>% 
  tibble() %>%
  rename(y = .pred)

bt_tune %>% 
  show_best() 

write_csv(sub_23, "attempt_3/submissions/submission_23.csv")


