library(tidymodels)
library(tidyverse)
library(kableExtra)
set.seed(5)

tidymodels_prefer()

load("attempt_4/results/svm_rad_8_tuned.rda")
load("attempt_4/results/bt_7_tuned.rda")
load("attempt_4/results/en_7_tuned.rda")
load("attempt_4/results/rf_7_tuned.rda")
load("attempt_4/results/initial_setup.rda")

model_set <- as_workflow_set(
  "elastic_net" = en_tune,
  "rand_forest" = rf_tune, 
  "boosted_tree" = bt_tune,
  "svm_radial" = svm_radial_tune
)

## Table of results
model_results <- model_set %>% 
  group_by(wflow_id) %>% 
  mutate(best = map(result, show_best, metric = "rmse", n = 1)) %>% 
  select(best) %>% 
  unnest(cols = c(best)) %>% 
  select(wflow_id, .metric, mean)

ggplot(model_results, aes (x = wflow_id, y = mean, color = wflow_id)) +
  geom_point() +
  labs( y = "RMSE", x = "Model") +
  geom_text(aes(y = mean - 0.01, label = wflow_id, color = wflow_id)) +
  ggtitle(label = "Best Results") +
  theme(legend.position = "none")


first_8 <- data.frame(
  model = c("MARS", "SVM poly", "SVM radial", 
            "EN", "RF", "KNN", 
            "NN", "BT"),
  value = c(9.90, 9.82, 9.67, 9.58, 9.55, 10.02, 10.07, 9.73)
)
ggplot(first_8, aes(x = model, y = value)) + 
  geom_point(aes(color = model)) + 
  geom_text(aes(y = value - 0.05, label = model, color = model)) + 
  theme_bw() + 
  theme(legend.position = "none") 

svm_radial_tictoc %>% 
  mutate(runtime = runtime/60) %>% 
  kbl() %>% 
  kable_classic() %>% 
  save_kable("svm_time.png", zoom = 10)



  
