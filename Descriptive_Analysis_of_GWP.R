#Import the data set to R and Set the Directory

data=read.csv("C:/Users/I C Sathsara/Desktop/ST 3008 Group project/GWP.csv")
data
getwd()

##### DATA CLEANING #####

#install.package("tidyverse")
library(tidyverse)

#Information of the variables....

glimpse(GWP)
names(GWP)

#Categorical variable
unique(GWP$quarter)
unique(GWP$department)
unique(GWP$day)

GWP$quarter<-as.factor(GWP$quarter)
GWP$department<-as.factor(GWP$department)
GWP$day<-as.factor(GWP$day)

levels(GWP$quarter)
levels(GWP$department)
levels(GWP$day)

#Rename the levels of department variable
levels(GWP$department) <- list(sweing="sweing", finishing="finishing", finishing="finishing ")
unique(GWP$department)

#Missing Values
colSums(is.na(GWP))
#We can see that wip variable has 506 missing values.
plot(GWP$wip,GWP$actual_productivity)
summary(GWP$wip)

#Replace the missing values with zero
GWP$wip[is.na(GWP$wip)] <- 0

#Duplicate Values
sum(duplicated(GWP))

#Actual Productivity greater than 1. 
filtered_data <- GWP%>%
  filter(GWP$actual_productivity > 1)
print(filtered_data)


#### DESCRIPTIVE ANALYSIS FOR QULITATIVE VARIABLE ####

# Frequency table for categorical variables

freq_day <- GWP %>%
  count(day)
print(freq_day)

freq_quarter <- GWP %>%
  count(quarter )
print(freq_quarter )


freq_department <- GWP %>%
  count(department)
print(freq_department)

#BAR CHART FOR CATEGORICAL VARIABLES

ggplot(GWP, aes(x = day)) +
  geom_bar() +
  labs(title = "Frequency of Days", x = "Day of the Week", y = "Frequency")

ggplot(GWP, aes(x = quarter)) +
  geom_bar() +
  labs(title = "Frequency of Quarters", x = "Quarter", y = "Frequency")


ggplot(GWP, aes(x = department)) +
  geom_bar() +
  labs(title = "Frequency of Departments", x = "Department", y = "Frequency")

#CROSS TABULATION

library(dplyr)
cross_tab <- table(GWP$day, GWP$quarter,GWP$department)
print(cross_tab)


#Response variable vs categorical predictor variables – Box plots

box_plots_categorical <- data %>%
  select(actual_productivity, day, quarter, department) %>%
  gather(variable, value, -actual_productivity) %>%
  ggplot(aes(x = value, y = actual_productivity)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Box Plots: Response vs. Categorical Predictors", x = "Variable Value", y = "Actual Productivity")
print(box_plots_categorical)


#One way ANOVA Test for Categorical variables (5% Significance Level) OR KRUSKAL-WALLIS TEST

## Normality Assumption
# Create histogram
hist(GWP$actual_productivity, main = "Histogram")

# Create Q-Q plot 
qqnorm(GWP$actual_productivity)
qqline(GWP$actual_productivity)

#Homogeneity of Variances (Homoscedasticity)
install.packages("car")
library(car)

levene_test_result <- leveneTest(actual_productivity ~ day, data = GWP)
print(levene_test_result)

levene_test_result <- leveneTest(actual_productivity ~ quarter, data = GWP)
print(levene_test_result)

levene_test_result <- leveneTest(actual_productivity ~ department, data = GWP)
print(levene_test_result)


## Kruskal-Wallis Test

kruskal_test_result <- kruskal.test(actual_productivity ~ day, data = GWP)
print(kruskal_test_result)

kruskal_test_result <- kruskal.test(actual_productivity ~ quarter, data = GWP)
print(kruskal_test_result)

kruskal_test_result <- kruskal.test(actual_productivity ~ department, data = GWP)
print(kruskal_test_result)

#### DESCRIPTIVE ANALYSIS FOR QUANTITATIVE VARIABLE ####

#FIVE NUMBER SUMMARY
summary(GWP[, c("targeted_productivity", "smv", "wip","over_time","incentive","idle_time",
                "idle_men","no_of_style_change","no_of_workers")])

#HISTOGRAM FOR QUANTITATIVE VARIABLES
TP<-hist(GWP$targeted_productivity,
        main = "Histogram of targeted_productivity", 
        xlab = "targeted_productivity", 
        ylab = "Frequency")

SM<-hist(GWP$smv,          
        main = "Histogram of smv", 
        xlab = "smv", 
        ylab = "Frequency")

WI<-hist(GWP$wip,
        main = "Histogram of wip", 
        xlab = "wip", 
        ylab = "Frequency")
OT<-hist(GWP$over_time,
        main = "Histogram of over_time", 
        xlab = "over_time", 
        ylab = "Frequency")

SC<-hist(GWP$no_of_style_change,
        main = "Histogram of no_of_style_change", 
        xlab = "no_of_style_change", 
        ylab = "Frequency")

IM<-hist(GWP$idle_men,
        main = "Histogram of idle_men", 
        xlab = "idle_men", 
        ylab = "Frequency")

INC<-hist(GWP$incentive,
        main = "Histogram of incentive", 
        xlab = "incentive", 
        ylab = "Frequency")

IT<-hist(GWP$idle_time,
        main = "Histogram of idle_time", 
        xlab = "idle_time", 
        ylab = "Frequency")


NW<-hist(GWP$no_of_workers,
        main = "Histogram of no_of_workers", 
        xlab = "no_of_workers", 
        ylab = "Frequency")

frequency_values <- TP$counts
print(frequency_values)
frequency_values <- SM$counts
print(frequency_values)
frequency_values <- WI$counts
print(frequency_values)
frequency_values <- OT$counts
print(frequency_values)
frequency_values <- SC$counts
print(frequency_values)
frequency_values <- IM$counts
print(frequency_values)
frequency_values <- INC$counts
print(frequency_values)
frequency_values <- IT$counts
print(frequency_values)
frequency_values <- NW$counts
print(frequency_values)


#Correlation Analysis

# Print the correlation matrix

cor_matrix <- cor(GWP[, c("actual_productivity", "targeted_productivity", "smv", "wip","over_time","incentive","idle_time",
                          "idle_men","no_of_style_change","no_of_workers")])
print(cor_matrix)

library(corrplot)
corrplot(cor_matrix, method = "color" )


corrplot(cor_matrix, method = "number" )




