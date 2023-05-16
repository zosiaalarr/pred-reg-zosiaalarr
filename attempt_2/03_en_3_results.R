library(tidymodels)
library(tidyverse)

tidymodels_prefer()

load("attempt_2/results/en_3_tuned.rda")
load("attempt_2/results/initial_setup.rda")

en_workflow <- en_workflow %>% 
  finalize_workflow(select_best(en_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(en_workflow, train)


data_pred <- predict(final_fit, test) %>% 
  bind_cols(test %>% select(id))

sub_3 <- data_pred %>% 
  tibble() %>% 
  rename(y = .pred)
View(sub_3)

write_csv(sub_3, "attempt_2/submissions/submission_3.csv")
