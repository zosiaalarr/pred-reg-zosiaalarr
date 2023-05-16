library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_2/results/knn_3_tuned.rda")
load("attempt_2/results/initial_setup.rda")

knn_workflow <- knn_workflow %>% 
  finalize_workflow(select_best(knn_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(knn_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_11 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_11)

write_csv(sub_11, "attempt_2/submissions/submission_11.csv")
