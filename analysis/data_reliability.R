### Statistical Test on Reliability ###

# Data
MW <- read.csv("MW_all.csv", header = TRUE, stringsAsFactors = FALSE)

# Package
library(splithalf)

# filter RT that condition = 1 and correctness = 1
MW_correct_RT <- MW[MW$condition == 1 & MW$correctness == 1, ]
MW_correct_RT_noNA <- MW_correct_RT[!is.na(MW_correct_RT$RT), ]

correct_RT_MW <- splithalf(data = MW_correct_RT_noNA,
                            outcome = "RT",
                            score = "average",
                            conditionlist = c("1"),
                            halftype = "random",
                            permutations = 5000,
                            var.RT = "RT",
                            var.condition = "condition",
                            var.participant = "participant",
                            average = "mean",
                            plot = TRUE)
correct_RT_MW
acc_MW <- splithalf(data = MW,
                               outcome = "accuracy",
                               score = "average",
                               conditionlist = c("1", "0"),
                               halftype = "random",
                               permutations = 5000,
                               var.ACC = "correctness",
                               var.condition = "condition",
                               var.participant = "participant",
                               # var.compare = "trial_type",
                               # compare1 = "congruent",
                               # compare2 = "incongruent",
                               average = "mean",
                               plot = TRUE) 

acc_MW
