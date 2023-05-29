library(tidymodels)
library(tidyverse)
set.seed(5)

tidymodels_prefer()

load("attempt_4/results/svm_rad_11_tuned.rda")
load("attempt_4/results/initial_setup.rda")

svm_radial_workflow <- svm_radial_workflow %>% 
  finalize_workflow(select_best(svm_radial_tune, metric = "rmse"))



# fit training data to final workflow 

final_fit <- fit(svm_radial_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_31 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)

svm_radial_tune %>% 
  show_best() 



write_csv(sub_31, "attempt_4/submissions/submission_31.csv")


