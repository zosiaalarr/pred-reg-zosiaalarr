library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("results/svm_radial_1_tuned.rda")
load("results/initial_setup.rda")

svm_radial_workflow <- svm_radial_workflow %>% 
  finalize_workflow(select_best(svm_radial_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(svm_radial_workflow, train_data)


data_pred <- predict(final_fit, test_data) %>% 
  bind_cols(test_data %>% select(id))

sub_6 <- data_pred %>% 
  tibble() %>% 
  rsvm_radialame(y = .pred)
View(sub_6)

write_csv(sub_6, "submissions/submission_6.csv")
