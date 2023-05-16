library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("results/svm_poly_1_tuned.rda")
load("results/initial_setup.rda")

svm_poly_workflow <- svm_poly_workflow %>% 
  finalize_workflow(select_best(svm_poly_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(svm_poly_workflow, train_data)


data_pred <- predict(final_fit, test_data) %>% 
  bind_cols(test_data %>% select(id))

sub_6 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_6)

write_csv(sub_6, "submissions/submission_6.csv")
