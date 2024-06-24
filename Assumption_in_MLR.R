getwd()
data=read.csv("C:/Users/I C Sathsara/Desktop/GWP.csv")
data
#setwd("C:\\Users\\I C Sathsara\\Desktop")
#install.packages("tidyverse")
library(tidyverse)
glimpse(GWP)
names(GWP)
#Categorical variables
GWP$quarter<-as.factor(GWP$quarter)
GWP$department<-as.factor(GWP$department)
GWP$day<-as.factor(GWP$day)
#Rename categories in the department variable.
levels(GWP$department) <- list(sweing="sweing", finishing="finishing", finishing="finishing ")
unique(GWP$department)
#Finding missing values.
colSums(is.na(GWP))
#Replace the missing values with zero
GWP$wip[is.na(GWP$wip)] <- 0
#Find is there any duplication.
sum(duplicated(GWP))#if this zero we can say no duplication.


###### Checking the assumptions of the MLR model #######

#01.Linearity between the response variable and predictors 
#01.Scatterplot of residuals vs fitted values


library(MASS)

#without two way interactions
attach(GWP)
lm_full1=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers+day)

step_model=stepAIC(lm_full1, direction = "both", trace = FALSE)
summary(step_model)

plot(step_model)

#back_model=stepAIC(lm_full1, direction = "backward", trace = FALSE)
#summary(back_model)

#step_model=stepAIC(lm_full1, direction = "both", trace = FALSE)
#summary(step_model)

# Load required libraries
library(ggplot2)
library(car)

# Calculate standardized residuals
std_resid <- rstandard(step_model)#back_model#step_model
# Create a data frame containing fitted values and standardized residuals
data <- data.frame(Fitted = fitted(step_model), Standardized_Residuals = std_resid)
# Create a scatterplot
ggplot(data, aes(x = Fitted, y = Standardized_Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Fitted Values vs. Standardized Residuals Scatterplot")




#with two way interactions

lm_full1=lm(actual_productivity~quarter+department+targeted_productivity+smv+wip+over_time+incentive+idle_time+idle_men+no_of_style_change+no_of_workers+day+quarter*department+quarter*day+quarter*targeted_productivity+quarter*smv+quarter*wip+quarter*over_time+quarter*incentive+quarter*idle_time+quarter*idle_men+quarter*no_of_style_change+quarter*no_of_workers+department*day+department*targeted_productivity+department*smv+department*wip+department*over_time+department*incentive+department*idle_time+department*idle_men+department*no_of_style_change+department*no_of_workers+day*targeted_productivity+day*smv+day*wip+day*over_time+day*incentive+day*idle_time+day*idle_men+day*no_of_style_change+day*no_of_workers+targeted_productivity*smv+targeted_productivity*wip+targeted_productivity*over_time+targeted_productivity*incentive+targeted_productivity*idle_time+targeted_productivity*idle_men+targeted_productivity*no_of_style_change+targeted_productivity*no_of_workers+smv*wip+smv*over_time+smv*incentive+smv*idle_time+smv*idle_men+smv*no_of_style_change+smv*no_of_workers+wip*over_time+wip*incentive+wip*idle_time+wip*idle_men+wip*no_of_style_change+wip*no_of_workers+over_time*incentive+over_time*idle_time+over_time*idle_men+over_time*no_of_style_change+over_time*no_of_workers+incentive*idle_time+incentive*idle_men+incentive*no_of_style_change+incentive*no_of_workers+idle_time*idle_men+idle_time*no_of_style_change+idle_time*no_of_workers+idle_men*no_of_style_change+idle_men*no_of_workers+no_of_style_change*no_of_workers)

step_model=stepAIC(lm_full1, direction = "both", trace = FALSE)
summary(step_model)
plot(step_model)


# Calculate standardized residuals
std_resid <- rstandard(step_model)#back_model#step_model
# Create a data frame containing fitted values and standardized residuals
data <- data.frame(Fitted = fitted(step_model), Standardized_Residuals = std_resid)
# Create a scatterplot
ggplot(data, aes(x = Fitted, y = Standardized_Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Fitted Values vs. Standardized Residuals Scatterplot")



