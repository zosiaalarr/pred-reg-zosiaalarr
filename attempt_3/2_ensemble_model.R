# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# Load candidate model info ----
load("attempt_3/results/svm_rad_3_tuned.rda")
load("attempt_3/results/rf_1_tuned.rda" )
load("attempt_3/results/svm_poly_tuned.rda")

# Load split data object & get testing data
load("attempt_3/results/initial_setup.rda")

#wildfires_test <- wildfires_split %>% testing()

# Create data stack ----
stacks()

data_st <- 
  stacks() %>%
  add_candidates(rf_tune) %>% # 15 models 
  add_candidates(svm_poly_tune) %>% # 25 models 
  add_candidates(svm_radial_tune) # 1 model 

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



# Save blended model stack for reproducibility & easy reference (Rmd report)
save(data_st, file = "attempt_3/results/fit_stack.rda")


# Explore the blended model stack
autoplot(data_st)

autoplot(data_st, type = "members")

plot1 <- autoplot(data_st, type = "weights") + 
  labs(title = "Figure 1") # shows optimal tuning parameter with the dif belnd that it chose 
# optimal blend has linear reg with 7 members 

ggsave("plot1.png")


# fit to ensemble to entire training set ----
data_model_fit <-
  data_st %>%
  fit_members()

collect_parameters(data_st, "svm_res")


# Save trained ensemble model for reproducibility & easy reference (Rmd report)

save(data_model_fit, file = "attempt_3/results/data_model_fit.rdm")


# Explore and assess trained ensemble model
data_test <- 
  test_2 %>%
  bind_cols(predict(data_model_fit, .)) %>%
  select(id, .pred) %>% 
  rename(y = .pred)
write_csv(data_test,"attempt_3/submissions/submission_17.csv")

# scatter plot
plot2 <- ggplot(data_test) +
  aes(x = burned, 
      y = .pred) +
  geom_point() + 
  coord_obs_pred() + 
  theme_bw() + 
  labs(title = "Figure 2", subtitle = "Predicted vs. actual values")
#SAVE

ggsave("plot2.png")

#show each member/model prediction 
member_preds <- 
  data_test %>%
  select(y) %>%
  bind_cols(predict(data_model_fit, data_test, members = TRUE)) %>% 
  rename(ensamble = .pred)
# shows the predicted values from each chosen model and then blends them into a .pred value 

map(member_preds, rmse_vec, truth = member_preds$burned) %>%
  as_tibble()
library(kableExtra)
member_preds %>% 
  map_df(rmse, truth = burned, data = member_preds) %>% 
  mutate(member = colnames(member_preds)) %>% 
  filter(member != "burned") %>% 
  arrange(.estimate) %>% 
  head(n = 7) %>% 
  kbl(caption = "Model Results Table") %>% 
  kable_classic() %>% 
  save_kable("result_table.png", zoom = 10)


#SAVE AS A NICE TABLE 

member_preds %>% 
  kbl(caption = "Model Results Table") %>% 
  kable_classic() %>% 
  save_kable("result_table.png", zoom = 10)

# .pred is the stacked model, it preformed the best 
  

