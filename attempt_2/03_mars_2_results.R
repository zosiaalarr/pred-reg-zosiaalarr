library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_2/results/mars_1_tuned.rda")
load("attempt_2/results/initial_setup.rda")

mars_workflow <- mars_workflow %>% 
  finalize_workflow(select_best(mars_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(mars_workflow, train)


data_pred <- predict(final_fit, test) %>% 
  bind_cols(test %>% select(id))

sub_6 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_6)

write_csv(sub_6, "attempt_2/submissions/submission_6.csv")
