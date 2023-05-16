library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_2/results/en_2_tuned.rda")
load("attempt_2/results/initial_setup.rda")

en_workflow <- en_workflow %>% 
  finalize_workflow(select_best(en_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(en_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_2 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_2)

write_csv(sub_2, "attempt_2/submissions/submission_2.csv")
