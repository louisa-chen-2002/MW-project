######## Pre-probe analysis ########

################ Data ################
MW <- read.csv(file = "MW.csv", header = TRUE)

############## Packages ################
library(lme4)
library(dplyr)

############## Analyses ################

# data cleaning
MW <- MW %>%
  mutate(
    participant  = factor(participant),
    task = factor(task, levels = c(1, 2, 3), labels = c("control", "rest-break", "task-switch")),
    intervention = factor(intervention, levels = c("before", "after"))
  )

# filter different mental state
MW_on_task <- MW %>%
  filter(MW_label == 0)
MW_off_task <- MW %>%
  filter(MW_label == 1)


# filter different mental state in go correct condition
MW_on_task_correct <- MW %>%
  filter(MW_label == 0 & correctness == 1 & condition == "space") 
MW_off_task_correct <- MW %>%
  filter(MW_label == 1 & correctness == 1 & condition == "space") 


######## Models for ON Task ########

#### Accuracy ####
model_acc_on_task <- glmer(
  correctness ~ intervention * task + (1 + intervention | participant),
  data = MW_on_task,
  family = binomial
)
summary(model_acc_on_task)
# beta value and 95% confidence interval
stad <- standardize_parameters(model_acc_on_task)
print(stad, digits = 3)

# pairwise comparison within each condition from before to after
emmeans(model_acc_on_task, pairwise ~ intervention | task, type = "response")


#### RT ####
model_RT_on_task <- lmer(
  RT ~ intervention * task + (1 + intervention | participant),
  data = MW_on_task_correct
)
summary(model_RT_on_task)

# beta value and 95% confidence interval
stad <- standardize_parameters(model_RT_on_task)
print(stad, digits = 3) 

# pairwise comparison within each condition from before to after
emmeans(model_RT_on_task, pairwise ~ intervention | task, type = "response")


######## Models for OFF Task ########

#### Accuracy ####
model_acc_off_task <- glmer(
  correctness ~ intervention * task + (1 + intervention | participant),
  data = MW_off_task,
  family = binomial
)
summary(model_acc_off_task)

# beta value and 95% confidence interval
stad <- standardize_parameters(model_acc_off_task)
print(stad, digits = 3) 

# pairwise comparison within each condition from before to after
emmeans(model_acc_off_task, pairwise ~ intervention | task, type = "response")
 
#### RT ####
model_RT_off_task <- lmer(
  RT ~ intervention * task + (1 + intervention | participant),
  data = MW_off_task_correct
)
summary(model_RT_off_task)

# beta value and 95% confidence interval
stad <- standardize_parameters(model_RT_off_task)
print(stad, digits = 3) 

# pairwise comparison within each condition from before to after
emmeans(model_RT_off_task, pairwise ~ intervention | task, type = "response")
