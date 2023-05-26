# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# Load candidate model info ----
load("attempt_5/results/svm_rad_9_tuned.rda")
load("attempt_5/results/en_9_tuned.rda" )
load("attempt_5/results/rf_9_tuned.rda")
load("attempt_5/results/bt_9_tuned.rda")
load("attempt_5/results/nn_9_tuned.rda")
load("attempt_5/results/knn_9_tuned.rda")
#load("attempt_5/results/mars_9_tuned.rda")
load("attempt_5/results/svm_poly_9_tuned.rda")

# Load split data object & get testing data
load("attempt_5/results/initial_setup.rda")

#wildfires_test <- wildfires_split %>% testing()

# Create data stack ----
stacks()

data_st <- 
  stacks() %>%
  add_candidates(en_tune) %>% 
  #add_candidates(knn_tune)%>%
  #add_candidates(mars_tune) %>% 
  #add_candidates(nn_tune)
  add_candidates(bt_tune)# 15 models 
  add_candidates(rf_tune) %>% # 25 models 
  add_candidates(svm_radial_tune) %>%  # 1 model 
  add_candidates(svm_poly_tune)

# looks at all these models, asigns some to 0 and a coeficient to others 

data_st 

as_tibble(data_st)


# Fit the stack ----
# penalty values for blending (set penalty argument when blending)
blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2)

# higher penalty values will force more things to 0, more selective on what values to chose 



# Blend predictions using penalty defined above (tuning step, set seed)
set.seed(9876)

data_st <-
  data_st %>%
  blend_predictions(penalty = blend_penalty) 

plot1 <- autoplot(data_st, type = "weights") + 
  labs(title = "Figure 1")



# Save blended model stack for reproducibility & easy reference (Rmd report)
save(data_st, file = "attempt_5/results/fit_stack.rmd")




# fit to ensemble to entire training set ----
data_model_fit <-
  data_st %>%
  fit_members()

# Save trained ensemble model for reproducibility & easy reference (Rmd report)

save(data_model_fit, file = "attempt_5/results/data_model_fit.rdm")


# Explore and assess trained ensemble model
data_test <- 
  test_2 %>%
  bind_cols(predict(data_model_fit, .)) %>%
  select(id, .pred) %>% 
  rename(y = .pred)
write_csv(data_test,"attempt_5/submissions/submission_3.csv")

