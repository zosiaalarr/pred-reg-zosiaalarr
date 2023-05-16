# Load package(s) ----
library(tidymodels)
library(tidyverse)
library(stacks)

# Handle common conflicts
tidymodels_prefer()

# Load candidate model info ----
load("model_info/knn_res.rda")
load("model_info/svm_res.rda")
load("model_info/lin_reg_res.rda")

# Load split data object & get testing data
load("data/wildfires_split.rda")

wildfires_test <- wildfires_split %>% testing()

# Create data stack ----
stacks()

wildfires_data_st <- 
  stacks() %>%
  add_candidates(knn_res) %>% # 15 models 
  add_candidates(lin_reg_res) %>% # 25 models 
  add_candidates(svm_res) # 1 model 

# looks at all these models, asigns some to 0 and a coeficient to others 

wildfires_data_st 

as_tibble(wildfires_data_st)


# Fit the stack ----
# penalty values for blending (set penalty argument when blending)
blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2)

# higher penalty values will force more things to 0, more selective on what values to chose 



# Blend predictions using penalty defined above (tuning step, set seed)
set.seed(9876)

wilfires_model_st <-
  wildfires_data_st %>%
  blend_predictions(penalty = blend_penalty) 



# Save blended model stack for reproducibility & easy reference (Rmd report)
save(wilfires_model_st, file = "model_info/fit_stack.rmd")


# Explore the blended model stack
autoplot(wilfires_model_st)

autoplot(wilfires_model_st, type = "members")

plot1 <- autoplot(wilfires_model_st, type = "weights") + 
  labs(title = "Figure 1") # shows optimal tuning parameter with the dif belnd that it chose 
# optimal blend has linear reg with 7 members 

ggsave("plot1.png")


# fit to ensemble to entire training set ----
wilfires_model_fit <-
  wilfires_model_st %>%
  fit_members()

collect_parameters(wilfires_model_st, "svm_res")


# Save trained ensemble model for reproducibility & easy reference (Rmd report)

save(wilfires_model_fit, file = "model_info/wilfires_model_fit.rmd")


# Explore and assess trained ensemble model
wildfires_test <- 
  wildfires_test %>%
  bind_cols(predict(wilfires_model_fit, .))

# scatter plot
plot2 <- ggplot(wildfires_test) +
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
  wildfires_test %>%
  select(burned) %>%
  bind_cols(predict(wilfires_model_fit, wildfires_test, members = TRUE)) %>% 
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


