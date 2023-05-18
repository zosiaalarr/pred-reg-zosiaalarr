library(tidymodels)
library(tidyverse)

tidymodels_prefer()

set.seed(5)

load("attempt_4/results/nn_7_tuned.rda" )
load("attempt_4/results/initial_setup.rda")

nn_workflow <- nn_workflow %>% 
  finalize_workflow(select_best(nn_tune, metric = "rmse"))

# fit training data to final workflow 

final_fit <- fit(nn_workflow, train_1)


data_pred <- predict(final_fit, test_2) %>% 
  bind_cols(test_2 %>% select(id))

# sub_9 <- data_pred %>% 
#   tibble() %>% 
#   rename(y = .pred)
# View(sub_9)

show_best(nn_tune)

#write_csv(sub_9, "attempt_2/submissions/submission_9.csv")
