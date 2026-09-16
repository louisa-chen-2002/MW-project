######## Task Performance Analyses ########

################ Data ################
project_dir <- "/Users/siyuchen/Desktop/UW/CCDL/Louisa-MindWanderingProject/MW_project_2.0/MW_project_code"

MW_all <- read.csv(file = file.path(project_dir, "data", "MW_all.csv"),
                   header = TRUE)
MW <- read.csv(file = file.path(project_dir, "data", "MW.csv"),
               header = TRUE)

############## Packages ################
library(lme4)
library(dplyr)
library(emmeans)
library(parameters)

############## Analyses ################
# data cleaning
MW_all <- MW_all %>%
  dplyr::mutate(task = factor(task, 
                       levels = c(1, 2, 3), 
                       labels = c("control", "rest-break", "task-switch")),
         intervention = factor(intervention, 
                               levels = c("before", "after")))

View(MW_all)

############## Anova for Before Intervention ################
# filter before intervention 
MW_before <- MW_all %>%
  dplyr::filter(intervention == "before")
# ANOVA to compare if there is a task different


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

# RTCV model 
data_correct_RTCV <- MW_all_correct_rt %>%
  dplyr::group_by(participant, task, intervention) %>%
  dplyr::summarise(RTCV = sd(RT) / mean(RT), .groups = "drop") 

model_all_correct_RTCV <- lmer(
  RTCV ~ task * intervention + (1 | participant),
  data = data_correct_RTCV,
)

summary(model_all_correct_RTCV)
# beta value and 95% CI
std_model <- standardize_parameters(model_all_correct_RTCV) # 95% CI
print(std_model, digits = 3)
