## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: Final Project
# Date:      2024_1_17
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
data <- read_delim("Final_Project.csv")
mean(data$Song_length)
sd(data$Song_length)
hist(data$Song_length)
summary(data$Song_length)
table(data$Mood)
table(data$`Feature `)
1/##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
sd(data$Release)
summary(data$Release)
mean(data$Release)
##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
table(data$mood, data$release)
table(data$mood, data$feature)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
table(data$mood, data$feature)
table(data$)
# BOX PLOT
ggplot(raw_data, aes(x = data$mood, y = data$release)) +
  geom_boxplot() +
  labs(title = "Box Plot of mood by release",
       x = mood,
       y = release) +
  theme_minimal()
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################

data <- read_delim("Final_Project")
##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
chisq.test(data$mood, data$`feature `)
table(data$mood, data$`feature `)

# BOX PLOT
ggplot(Final_Project_Sheet1_4, aes(x = data$mood, y =data$release)) +
  geom_boxplot() +
  labs(title = "Box Plot of mood by release",
       x = "mood",
       y = "release") +
  theme_minimal()


##### STEP 1: Examine the scatter plot
# showing the relationship between release year and song length


linear_plot <- plot(data$release, data$song_length)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$release)
meanx <- mean(data$song_length)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")



##### STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot
linear_relationship <- lm(song_length ~ release, data = data)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

##### STEP 3: Plot the residuals

# Plot the residuals
plot(data$release, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##### STEP 1: Examine the scatter plot
# showing the relationship between campaign spending and observed vote share 


linear_plot <- plot(vote_table$campaign_spending, vote_table$observed_vote_share)
print(linear_plot)

# add x line and y line for means
meany <- mean(vote_table$campaign_spending)
meanx <- mean(vote_table$observed_vote_share)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")



##### STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot
linear_relationship <- lm(observed_vote_share ~ campaign_spending, data = vote_table)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")

##### STEP 3: Plot the residuals

# Plot the residuals
plot(vote_table$campaign_spending, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


