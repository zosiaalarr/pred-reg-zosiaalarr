library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("results/en_1_tuned.rda")
load("results/initial_setup.rda")

en_workflow <- en_workflow %>% 
  finalize_workflow(select_best(en_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(en_workflow, train_data)


data_pred <- predict(final_fit, test_data) %>% 
  bind_cols(test_data %>% select(id))

sub_1 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_1)

write_csv(sub_1, "submissions/submission_2.csv")
