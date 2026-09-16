####### MW Frequency Analysis #######

################ Data ################
MW <- read.csv(file = "/Users/siyuchen/Desktop/UW/CCDL/Louisa-MindWanderingProject/MW_project_2.0/MW_project_code/data/MW.csv", header = TRUE)
View(MW)

############## Packages ################
library(lme4)
library(dplyr)
library(ggplot2)

############## Analyses ################
# calculate MW proportion
MW_prop <- MW %>%
  select(participant, Block, MW_index, MW_label, task) %>%
  filter(!is.na(MW_label)) %>%
  group_by(participant, Block, MW_index, task) %>%
  summarize(
    MW_label = mean(MW_label),
    n_unique = n_distinct(MW_label),
    .groups = "drop"
  )

MW_prop <- MW_prop %>%
  mutate(
    participant = factor(participant),
    Block = factor(Block),
    task = factor(task)
  )

MW_prop$task <- factor(
  MW_prop$task,
  levels = c(1, 2, 3),
  labels = c("No-Break", "Rest-Break", "Task-Switch")
)

MW_prop$task <- relevel(
  factor(MW_prop$task),
  ref = "No-Break"
)

MW_prop$Block <- relevel(
  factor(MW_prop$Block),
  ref = "1"
)

mf_model <- glmer(
  MW_label ~ task * Block + (1 | participant),
  data = MW_prop,
  family = binomial(link = "logit"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
)
summary(mf_model)

library(emmeans)

mw_emm <- emmeans(
  mf_model,
  ~ Block | task,
  type = "response"
)

data_summary_mw_prop <- as.data.frame(mw_emm)

data_summary_mw_prop$Block <- factor(
  data_summary_mw_prop$Block,
  labels = c("B1", "B2", "B3", "B4")
)

data_summary_mw_prop$task <- factor(
  data_summary_mw_prop$task,
  labels = c("no-break", "rest-break", "task-switch")
)
