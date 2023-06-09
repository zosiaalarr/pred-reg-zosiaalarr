library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_3/results/svm_poly_tuned.rda")
load("attempt_3/results/initial_setup.rda")

svm_poly_workflow <- svm_poly_workflow %>% 
  finalize_workflow(select_best(svm_poly_tune, metric = "rmse"))



# fit training data to final workflow 

final_fit <- fit(svm_poly_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_14 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)

svm_poly_tune %>% 
  show_best() 

write_csv(sub_14, "attempt_3/submissions/submission_14.csv")


