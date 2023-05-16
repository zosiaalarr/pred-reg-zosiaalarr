library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("results/nn_1_tuned.rda")
load("results/initial_setup.rda")

nn_workflow <- nn_workflow %>% 
  finalize_workflow(select_best(nn_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(nn_workflow, train_data)


data_pred <- predict(final_fit, test_data) %>% 
  bind_cols(test_data %>% select(id))

sub_5 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_5)

write_csv(sub_5, "submissions/submission_5.csv")
