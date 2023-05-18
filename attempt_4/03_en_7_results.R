library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_4/results/en_7_tuned.rda")
load("attempt_4/results/initial_setup.rda")

en_workflow <- en_workflow %>% 
  finalize_workflow(select_best(en_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(en_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

sub_18 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_18)

show_best(en)

write_csv(sub_18, "attempt_4/submissions/submission_18.csv")
