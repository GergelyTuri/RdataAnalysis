library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(caTools)

#Import data (Don't forget to check your path!)
setwd("~/Data/mice_ptsd/RdataAnalysis/RdataAnalysis")
PTSD1_freezing <- read.csv("./data/PTSD1_freezing.csv")
PTSD2_freezing <- read.csv("./data/PTSD2_freezing.csv")

PTSD_freezing <- rbind(PTSD1_freezing, PTSD2_freezing) #combine cohorts

#Let us create a quick plot to see what we deal with
#plot frequency of freezing times on each days (control vs ptsd)
PTSD_freezing %>% ggplot(aes(x = freezing, fill = condition)) + 
                  geom_dotplot() +
                  facet_wrap(~day)
ggsave("./data/plots/freezing-control_vs_ptsd.pdf")

#We need to encode categorical data for regression

#day
PTSD_freezing$day <- factor(PTSD_freezing$day,
                             levels = c("sefla", "seflb", 
                                          "recall1", "recall2", "recall3",
                                            "recall4", "recall5", "recall6"),
                              labels = c(1, 2, 3, 4, 5, 6, 7, 8))

#condition
PTSD_freezing$condition <- factor(PTSD_freezing$condition,
                             levels = c("control", "ptsd"),
                             labels = c(1, 2))

#sex
PTSD_freezing$sex <- factor(PTSD_freezing$sex,
                             levels = c("f", "m"),
                             labels = c(1, 2))
#cohort
PTSD_freezing$cohort <- factor(PTSD_freezing$cohort,
                             levels = c("PTSD1", "PTSD2"),
                             labels = c(1, 2))

#We will need a test set to evaluate our regression so we split the dataset
#caTools library is used
set.seed(42) #to make this step reproducible
split <- sample.split(PTSD_freezing$freezing, SplitRatio = 0.85)
PTSD_training_set <- subset(PTSD_freezing, split == TRUE)
PTSD_test_set <- subset(PTSD_freezing, split == FALSE)

#We fit MLR to the trainng set
regressor <- lm(formula = freezing ~ day + condition + cohort, 
                data = PTSD_training_set) #sex is not included as all mice are male

#Let us have a look at how the regressor looks like
summary(regressor)

#Predict test set in order to test our regressor
PTSD_predict <- predict(regressor, newdata = PTSD_test_set)

#####################################

# For the next regression we need to reshape data
# so first I recreate the original dataset
# minus the empty columns (behaviors and note)
PTSD_freezing <- rbind(PTSD1_freezing, PTSD2_freezing) %>% select(-c(behaviors, note))

# And move it to wide format
PTSD_freezing <- PTSD_freezing %>% pivot_wider(names_from = day, values_from = freezing)
write.csv2(PTSD_freezing, "./data/PTSD_freezing_wide.csv")

#We need to encode categorical data for regression again
# day is not one of them any more!

#condition
PTSD_freezing$condition <- factor(PTSD_freezing$condition,
                                  levels = c("control", "ptsd"),
                                  labels = c(1, 2))

#sex
PTSD_freezing$sex <- factor(PTSD_freezing$sex,
                            levels = c("f", "m"),
                            labels = c(1, 2))
#cohort
PTSD_freezing$cohort <- factor(PTSD_freezing$cohort,
                               levels = c("PTSD1", "PTSD2"),
                               labels = c(1, 2))

#Split data into training set and test set
split <- sample.split(PTSD_freezing$id, SplitRatio = 0.9)
PTSD_training_set <- subset(PTSD_freezing, split == TRUE)
PTSD_test_set <- subset(PTSD_freezing, split == FALSE)

#We fit MLR to the trainng set
regressor <- lm(formula = recall6 ~ condition + cohort + sefla + seflb +
                  recall1 + recall2 + recall3 + recall4 + recall5, 
                data = PTSD_training_set) #sex is not included as all mice are male

#Let us have a look at how the regressor looks like
summary(regressor)

#Predict test set in order to test our regressor
PTSD_predict <- predict(regressor, newdata = PTSD_test_set)
