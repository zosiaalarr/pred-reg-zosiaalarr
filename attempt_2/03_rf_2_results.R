library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_2/results/rf_2_tuned.rda")
load("attempt_2/results/initial_setup.rda")

rf_workflow <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(rf_workflow, train)


data_pred <- predict(final_fit, test) %>% 
  bind_cols(test %>% select(id))

sub_4 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_4)

write_csv(sub_4, "attempt_2/submissions/submission_4.csv")
