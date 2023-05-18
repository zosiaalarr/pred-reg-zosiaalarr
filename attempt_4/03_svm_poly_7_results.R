library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_4/results/svm_poly_7_tuned.rda")
load("attempt_4/results/initial_setup.rda")

svm_poly_workflow <- svm_poly_workflow %>% 
  finalize_workflow(select_best(svm_poly_tune, metric = "rmse"))



# fit training data to final workflow 

final_fit <- fit(svm_poly_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_20 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)

svm_poly_tune %>% 
  show_best() 

write_csv(sub_20, "attempt_4/submissions/submission_20.csv")


