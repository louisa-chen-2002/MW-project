######## Task Performance Analyses ########

################ Data ################
MW_all <- read.csv(file = "MW_all.csv", 
                   header = TRUE)  

############## Packages ################
library(lme4)
library(dplyr)

############## Analyses ################
# data cleaning
MW_all <- MW_all %>%
  mutate(task = factor(task, 
                       levels = c(1, 2, 3), 
                       labels = c("control", "rest-break", "task-switch")),
         intervention = factor(intervention, 
                               levels = c("before", "after")))

######### Model #########

#### Accuracy ####
model_all_acc <- glmer(
  correctness ~ task * intervention + (1 + intervention | participant),
  data = MW_all,
  family = binomial
)
summary(model_all_acc)
# beta value and 95% CI
std_model <- standardize_parameters(model_all_acc) # 95% CI
print(std_model, digits = 3)

# pairwise comparison within each condition from before to after
emmeans(model_all_acc, pairwise ~ intervention | task, type = "response")


#### Reaction Time #### 

# filter go correct reaction time 
MW_all_correct_rt <- MW_all %>%
  filter(correctness == 1 & condition == 1) 

model_all_correct_RT <- lmer(
  RT ~ task * intervention + (1 + intervention | participant),
  data = MW_all_correct_rt,
) 
summary(model_all_correct_RT)

# beta value and 95% CI
std_model <- standardize_parameters(model_all_correct_RT) # 95% CI
print(std_model, digits = 3)

# pairwise comparison within each condition from before to after
emmeans(model_all_correct_RT, pairwise ~ intervention | task, type = "response")
