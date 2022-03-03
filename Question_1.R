Question 1 

#Data Wrangling

#Load Packages
library(tidyverse)
library(table1)
library(ggdark)
library(lme4)
library(lmerTest)
library(performance)
library(emmeans)
library(sjPlot)

#Read in data
assignment_data <- read.csv("assignment1_data1.csv")

#Snapshot of data
head(assignment_data)

#Problem 1 - Rename column labels
tidied_data <- rename(assignment_data, c(Subject = subj,
                                         Item = item,
                                         RT = DV,
                                         Condition = condition))

#Problem 2 - Remove specific strings from elements within the data
tidied_data <- tidied_data %>%
  mutate_at("Item", str_replace, "I", "") %>%
  mutate_at("Subject", str_replace, "S", "")

#Problem 3 - Problem 3 - Mutate relevant variables to factors
tidied_data <- tidied_data %>%
  mutate(Subject = factor(Subject),
         Item = factor(Item),
         Condition = factor(Condition))

#Data Exploration 

#Descriptive Statistics
label(tidied_data$RT)<-"Response Time"
table1(~RT| Condition, data = tidied_data, overall=F)

#Violin Plot
set.seed(1234)
tidied_data %>%
  ggplot(aes(x = Condition, y = RT, colour = Condition)) +
  geom_violin(width = .5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "white") +
  geom_jitter(width = .1, alpha = .15) +
  guides(colour = "none") +
  labs(x = "Word Context (IV)",
       y = "Reaction Time (ms.)",
       title = "Examining the effect of Word Context on Reaction Time ") +
  dark_theme_light() +
  coord_flip()

#Data Analysis 

#Model 1 (Too complex)
model_1 <- lmer(RT ~ Condition + (1 + Condition | Subject) 
                + (1 + Condition | Item), data =tidied_data)

#Model 2 
model_2 <- lmer(RT ~ Condition + (1 | Subject) 
                + (1 | Item), data = tidied_data) 

#Check model assumptions
check_model(model_2)

#Likliehood Ratio Test
model_null <- lmer(RT ~ (1 | Subject) + (1 | Item), 
                   data = tidied_data) 
anova(model_2, model_null)

#Results
summary(model_2)

#Post hoc testing
emmeans(model_2, pairwise ~ Condition)

#Table summary 
tab_model(model_2, show.std = TRUE)

