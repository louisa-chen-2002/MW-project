######## Performance On and Off Task ########

################ Data ################
MW <- read.csv(file = "MW.csv", 
                   header = TRUE)  

############## Packages ################
library(dplyr)

############## Analyses ################
# data cleaning
# change MW_label to factor
MW$MW_label <- factor(MW$MW_label, 
                      levels = c(0, 1))

MW_subj_accuracy <- MW %>%
  dplyr::group_by(participant, MW_label) %>%
  dplyr::summarise(
    mean_correct  = mean(correctness,   na.rm = TRUE),
  )

# filter correct RT join data together
MW_subj_RT <- MW %>%
  dplyr::filter(condition == "space" & correctness == 1) %>%
  dplyr::group_by(participant, MW_label) %>%
  dplyr::summarise(
    mean_RT       = mean(RT,            na.rm = TRUE),
    RTCV = sd(RT, na.rm = TRUE) / mean(RT, na.rm = TRUE)
  )

# join MW_subj_accuracy and MW_subj_RT together by participant and MW_label
MW_subj <- left_join(MW_subj_accuracy, MW_subj_RT, by = c("participant", "MW_label"))

MW_wide <- MW_subj %>%
  pivot_wider(
    id_cols = participant,
    names_from = MW_label,
    values_from = c(mean_correct, mean_RT, RTCV),
    names_prefix = "MW_"
  )

View(MW_wide)

# paired t-test on measures
# Accuracy 
t.test(
  MW_wide$mean_correct_MW_0,
  MW_wide$mean_correct_MW_1,
  paired = TRUE
)
# mean for accuracy
mean(MW_wide$mean_correct_MW_0, na.rm = TRUE)
mean(MW_wide$mean_correct_MW_1, na.rm = TRUE)

# RT 
t.test(
  MW_wide$mean_RT_MW_0,
  MW_wide$mean_RT_MW_1,
  paired = TRUE
)
# mean for RT
mean(MW_wide$mean_RT_MW_0, na.rm = TRUE)
mean(MW_wide$mean_RT_MW_1, na.rm = TRUE)

# RTCV 
t.test(
  MW_wide$RTCV_MW_0,
  MW_wide$RTCV_MW_1,
  paired = TRUE
) 
# mean for RTCV
mean(MW_wide$RTCV_MW_0, na.rm = TRUE)
mean(MW_wide$RTCV_MW_1, na.rm = TRUE)
