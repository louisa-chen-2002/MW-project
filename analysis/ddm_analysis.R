######## DDM Analysis ########

################ Data ################
before <- read.csv("sart_param_before_summary.csv")
after <- read.csv("sart_param_after_summary.csv") 

before_on_task  <- read.csv("sart_param_before_on_task_summary.csv")
before_off_task <- read.csv("sart_param_before_off_task_summary.csv")
after_on_task   <- read.csv("sart_param_after_on_task_summary.csv")
after_off_task  <- read.csv("sart_param_after_off_task_summary.csv")

############## Packages ################
library(dplyr)
library(lme4)
library(lmerTest)
library(effectsize)

############## Data Cleaning ################
# data cleaning for full trial parameters
before$condition <- "before"
after$condition <- "after"
mw_ddm_p1 <- rbind(before, after)
mw_ddm_p1$task <- ifelse(substr(mw_ddm_p1$subject, 1, 1) == "1", "task-switch", 
                         ifelse(substr(mw_ddm_p1$subject, 1, 1) == "2", "rest-break", "control"))
mw_ddm_p1$task <- as.factor(mw_ddm_p1$task)
mw_ddm_p1$subject <- as.factor(mw_ddm_p1$subject)
mw_ddm_p1$condition <- as.factor(mw_ddm_p1$condition)


# data cleaning for pre-probe parameters

before_on_task$condition  <- "before"
before_on_task$state      <- "on-task"

before_off_task$condition <- "before"
before_off_task$state     <- "off-task"

after_on_task$condition   <- "after"
after_on_task$state       <- "on-task"

after_off_task$condition  <- "after"
after_off_task$state      <- "off-task"

# combine all the data
mw_ddm_p2 <- rbind(before_on_task, before_off_task, after_on_task, after_off_task)
first_digit <- substr(as.character(mw_ddm_p2$subject), 9, 9)
mw_ddm_p2$task <- ifelse(first_digit == "1", "task-switch",
                         ifelse(first_digit == "2", "rest-break",
                                "control"))

mw_ddm_p2$task      <- as.factor(mw_ddm_p2$task)
mw_ddm_p2$subject   <- as.factor(mw_ddm_p2$subject)
mw_ddm_p2$condition <- as.factor(mw_ddm_p2$condition)
mw_ddm_p2$state     <- as.factor(mw_ddm_p2$state)


############## Model ################
#### full parameters ####
# model for v.ng
model_lmr_ddm_v.ng <- lmer(
  v.ng ~ task * condition + (1 | subject),
  data = mw_ddm_p1
)
summary(model_lmr_ddm_v.ng)
standardize_parameters(model_lmr_ddm_v.ng) 
emmeans(model_lmr_ddm_v.ng, pairwise ~ condition | task, type = "response")

# model for v.go
model_lmr_ddm_v.go <- lmer(
  v.go ~ task * condition + (1 | subject),
  data = mw_ddm_p1
)
summary(model_lmr_ddm_v.go)
standardize_parameters(model_lmr_ddm_v.go) 
emmeans(model_lmr_ddm_v.go, pairwise ~ condition | task, type = "response")

# model for a
model_lmr_ddm_a <- lmer(
  a ~ task * condition + (1 | subject),
  data = mw_ddm_p1
)
summary(model_lmr_ddm_a)
standardize_parameters(model_lmr_ddm_a) 
emmeans(model_lmr_ddm_a, pairwise ~ condition | task, type = "response")


#### pre_probe ####
mw_ddm_p2_on_task <- mw_ddm_p2 %>%
  filter(state == "on-task") %>%
  mutate(
    task = factor(task, 
                  levels = c("control", "rest-break", "task-switch")),
    condition = factor(condition, 
                       levels = c("before", "after"))
  )

contrasts(mw_ddm_p2_on_task$task) <- contr.treatment(3)
contrasts(mw_ddm_p2_on_task$condition) <- contr.treatment(2)

# change the dependent variable (v.ng /v.go / a) 
model_lmr_ddm_on_task <- lmer(
  v.ng ~ task * condition + (1 | subject),
  data = mw_ddm_p2_on_task
)

summary(model_lmr_ddm_on_task)

# standardized beta value and 95% CI
standardize_parameters(model_lmr_ddm_on_task)
emmeans(model_lmr_ddm_on_task, pairwise ~ condition | task, type = "response", adjust = "bonferroni")

# off-task

mw_ddm_p2_off_task <- mw_ddm_p2 %>%
  filter(state == "off-task") %>%
  mutate(
    task = factor(task, 
                  levels = c("control", "rest-break", "task-switch")),
    condition = factor(condition, 
                       levels = c("before", "after"))
  )

contrasts(mw_ddm_p2_off_task$task) <- contr.treatment(3)
contrasts(mw_ddm_p2_off_task$condition) <- contr.treatment(2)


# change the dependent variable (v.ng /v.go / a) 
model_lmr_ddm_off_task <- lmer(
  v.ng ~ task * condition + (1  | subject),
  data = mw_ddm_p2_off_task
)
summary(model_lmr_ddm_off_task)
standardize_parameters(model_lmr_ddm_off_task)
emmeans(model_lmr_ddm_off_task, pairwise ~ condition | task, type = "response", adjust = "bonferroni")
