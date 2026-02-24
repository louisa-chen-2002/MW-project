####### MW Frequency Analysis #######

################ Data ################
MW <- read.csv(file = "MW.csv", header = TRUE)

############## Packages ################
library(lme4)
library(dplyr)

############## Analyses ################
# calculate MW proportion
MW_prop <- MW %>%
  select(participant, Block, MW_index, MW_label, task) %>%
  dplyr::group_by(participant, Block, MW_index, task) %>%
  dplyr::summarize(avg_MW_label = mean(MW_label), .groups = 'drop') 

# calculate the mean MW frequency for each participant per block and task
data_participant_level <- MW_prop %>%
  dplyr::group_by(participant, Block, task) %>%
  dplyr::summarize(
    mean_MW = mean(avg_MW_label),
    .groups = 'drop'
  )
# factorize Block and task
data_participant_level$Block <- as.factor(data_participant_level$Block)
data_participant_level$task  <- as.factor(data_participant_level$task)

# using linear mixed model to compare MW frequency between groups
mf_model <- 
  lmer(mean_MW ~ task + Block + (1 | participant), data = data_participant_level)
summary(mf_model)

# pairwise comparison
mf_emmeans <- emmeans(mf_model, pairwise ~ Block)
summary(mf_emmeans)
