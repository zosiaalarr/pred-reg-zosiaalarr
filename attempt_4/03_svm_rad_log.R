library(tidymodels)
library(tidyverse)
set.seed(5)

tidymodels_prefer()

load("attempt_4/results/svm_rad_log_tuned.rda")
load("attempt_4/results/initial_setup.rda")

svm_radial_workflow <- svm_radial_workflow %>% 
  finalize_workflow(select_best(svm_radial_tune, metric = "rmse"))



# fit training data to final workflow 

final_fit <- fit(svm_radial_workflow, train)


data_pred <- predict(final_fit, test) %>% 
  bind_cols(test %>% select(id)) 

sub_log <- data_pred %>% 
  mutate(y = exp(.pred)) %>% 
  select(-c(.pred))
 

svm_radial_tune %>% 
  show_best() 



write_csv(sub_log, "attempt_4/submissions/submission_28.csv")


