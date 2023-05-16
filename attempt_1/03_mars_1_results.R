library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("results/mars_1_tuned.rda")
load("results/initial_setup.rda")

mars_workflow <- mars_workflow %>% 
  finalize_workflow(select_best(mars_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(mars_workflow, train_data)


data_pred <- predict(final_fit, test_data) %>% 
  bind_cols(test_data %>% select(id))

sub_4 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_4)

write_csv(sub_4, "submissions/submission_4.csv")
