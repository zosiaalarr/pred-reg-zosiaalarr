library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("results/knn_1_tuned.rda")
load("results/initial_setup.rda")

knn_workflow <- knn_workflow %>% 
  finalize_workflow(select_best(knn_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(knn_workflow, train_data)


data_pred <- predict(final_fit, test_data) %>% 
  bind_cols(test_data %>% select(id))

sub_3 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_3)

write_csv(sub_3, "submissions/submission_3.csv")
