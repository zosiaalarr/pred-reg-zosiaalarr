library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_2/results/knn_1_tuned.rda")
load("attempt_2/results/initial_setup.rda")

knn_workflow <- knn_workflow %>% 
  finalize_workflow(select_best(knn_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(knn_workflow, train)


data_pred <- predict(final_fit, test) %>% 
  bind_cols(test %>% select(id))

sub_5 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_5)

write_csv(sub_5, "attempt_2/submissions/submission_5.csv")
