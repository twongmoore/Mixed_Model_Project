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
library(fitdistrplus)

#Read in data
factorial_data <- read.csv("assignment1_data2.csv")

#Snapshot of data
head(factorial_data)

#Problem 1 - Rename column labels
factorial_data_tidied <- rename(factorial_data, 
                                c(Story_Emotion = StoryEmotion,
                                  Facial_Expression = FaceExpression))

#Problem 3 - Problem 3 - Mutate relevant variables to factors
factorial_data_tidied <- factorial_data_tidied %>%
  mutate(Subject = factor(Subject), 
         Vignette = factor(Vignette),
         Story_Emotion = factor(Story_Emotion), 
         Facial_Expression = factor(Facial_Expression))

Data Visualisation

#Descriptive Statistics
table_data <- factorial_data_tidied %>%
  mutate_at("Story_Emotion", str_replace, "Anger", "Anger (Story
            Emotion)") %>%
  mutate_at("Story_Emotion", str_replace, "Fear", "Fear (Story
            Emotion)")
table1::label(table_data$RT)<-"Response Time"
table1::table1(~RT| Story_Emotion:Facial_Expression, data = table_data, overall=F)

#Violin Plot
set.seed(1234)
factorial_data_tidied %>%
  ggplot(aes(x = Story_Emotion:Facial_Expression, 
             y = RT, colour = Story_Emotion:Facial_Expression)) +
  geom_violin(width = .5) +
  stat_summary(fun.data = "mean_cl_boot", colour = "white") +
  geom_jitter(width = .1, alpha = .15) +
  guides(colour = "none") +
  labs(x = "Story Emotion X Facial Expression (IVs)",
       y = "Reaction Time (ms.)",
       title = "Examining the effect of the interaction between Story Emotion and Facial Expression on Reaction Times ") +
  dark_theme_light() +
  coord_flip()

#Data Analysis 

#Set deviation coding
contrasts(factorial_data_tidied$Story_Emotion) <- matrix(c(.5, -.5))
contrasts(factorial_data_tidied$Facial_Expression) <- matrix(c(.5, -.5))

#Model 1 (Too complex) 
factorial_model <- lmer(RT ~ Story_Emotion * Facial_Expression + 
                          (1 + Story_Emotion + Facial_Expression | Subject)+
                          (1 + Story_Emotion + Facial_Expression |Vignette),
                        data = factorial_data_tidied)

#Model 2 
factorial_model_2 <- lmer(RT ~ Story_Emotion * Facial_Expression + 
                            (1 | Subject) +
                            (1 | Vignette),
                          data = factorial_data_tidied)

#Check model assumptions (Failed)
check_model(factorial_model_2)

#Further checks of assumptions
descdist(factorial_data_tidied$RT)

#Gamma models
gamma_model_story <- glmer(RT ~ 
                             Story_Emotion * Facial_Expression +
                             (1 + Story_Emotion | Subject) +
                             (1 + Story_Emotion | Vignette), 
                           family = Gamma,
                           nAGQ = 0,
                           data = factorial_data_tidied)

gamma_model_face <- glmer(RT ~ 
                            Story_Emotion * Facial_Expression +
                            (1 + Facial_Expression | Subject) +
                            (1 + Facial_Expression | Vignette), 
                          family = Gamma,
                          nAGQ = 0,
                          data = factorial_data_tidied)

#Likliehood Ratio Test
gamma_model_story_null <- glmer(RT ~ (1 + Story_Emotion | Subject) +
                                  (1 + Story_Emotion | Vignette), 
                                family = Gamma,
                                nAGQ = 0,
                                data = factorial_data_tidied)

anova(gamma_model_story, gamma_model_story_null)

gamma_model_face_null <- glmer(RT ~ (1 + Facial_Expression | Subject) +
                                 (1 + Facial_Expression | Vignette), 
                               family = Gamma,
                               nAGQ = 0,
                               data = factorial_data_tidied)

anova(gamma_model_face, gamma_model_face_null)

#Results
summary(gamma_model_face)
summary(gamma_model_story)

#Post hoc testing
emmeans(gamma_model_story, pairwise ~ Story_Emotion*Facial_Expression, adjust = "none")

#Table summary 
tab_model(gamma_model_story, gamma_model_face, show.std = TRUE, dv.labels = c("Gamma Model Story", "Gamma Model Face"))

