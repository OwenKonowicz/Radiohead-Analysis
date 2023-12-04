## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
mean(data$Mood_of_Song)
sd(data$Mood_of_Song)
table(data$Mood_of_Song)
describe(data$Mood_of_Song)
summary(data$Mood_of_Song)

mean(data$Album)
sd(data$Album)
table(data$Album)
describe(data$Album)
summary(data$Album)

mean(data$Rating)
sd(data$Rating)
table(data$Rating)
describe(data$Rating)
summary(data$Rating)

mean(data$Number_of_Streams)
sd(data$Number_of_Streams)
table(data$Number_of_Streams)
describe(data$Number_of_Streams)
summary(data$Number_of_Streams)
##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Mood_of_Song, data$Album)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(table(data$Mood_of_Song, data$Album))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova <- aov(Mood_of_Song ~ Rating, data = data)
summary(anova)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################

cor(data$Rating, data$Number_of_Streams)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Number_of_Streams ~ Rating, data = data)
##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
plot(data$Rating, data$Number_of_Streams)
abline(linear_relationship, col = "red")
abline(v = 6.4 , col = "green")
abline(h = 125836264, col = "green")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Rating, residuals(linear_relationship))
abline(h = 0, col = "red")