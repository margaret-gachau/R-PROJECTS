# Load library "tidyverse" and "here"
library(tidyverse)
library(here)
# load "Project-diabetes-data.csv" in folder "PROJECT" 
projectdata<- read_csv(here("PROJECT", "Project-diabetes-data.csv"))
#calculate descriptive statitics
summary(projectdata)

#1.Handling missing variables & cleaning
# Convert "Diabetes_outcome" (character data) to factor
projectdata$Diabetes_Outcome <- factor(projectdata$Diabetes_Outcome)
# Convert "pregnancies" (character data) to factor
projectdata$pregnancies <- factor(projectdata$pregnancies)
# Convert "Skinthickness" (character data) to factor
projectdata$Skinthickness <- factor(projectdata$Skinthickness)
# Convert "Diabetespedigreefunction" (character data) to factor
projectdata$Diabetespedigreefunction <- factor(projectdata$Diabetespedigreefunction)
#Summary statistics
summary(projectdata)

#Rename "Glucose" to "Glucose_level"
projectdata <- rename(projectdata, "Glucose_Level" = "Glucose")

#print unique values in Diabetes_outcome
table(projectdata$Diabetes_Outcome)
#print unique values in pregnancies
table(projectdata$pregnancies)
#print unique values in Skinthickness
table(projectdata$Skinthickness)
#print unique values in Diabetespedigreefunction
table(projectdata$Diabetespedigreefunction)

##2.Mutating data

# Initialize "Age_Cat" with all missing values
projectdata$Age_Cat<- NA
#calculate summary
summary(projectdata$Age)
# Set "Age_Cat" to "Young" if "Age" <= 35
projectdata$Age_Cat[projectdata$Age <= 35] <- "Young"
# Set "Age_Cat" to "Middle Aged" if "Age" > 35 and <= 55
projectdata$Age_Cat[projectdata$Age > 35 & projectdata$Age <= 55] <- "Middle Aged"
# Set "Age_Cat" to "Old" if "Age" > 55
projectdata$Age_Cat[projectdata$Age> 55] <- "Old"
#print unique values in Age_Cat
table(projectdata$Age_Cat)
#convert "Age_Cat" (character data) to factor
projectdata$Age_Cat <- factor(projectdata$Age_Cat)

# DESCRIPTIVE STATISTICS FOR NUMERIC VALUES
summary(projectdata)
#plot Age, BMI, BloodPressure, Glucose_Level
# Plot "Age" (numeric data) using default styling, bin width 5, and bins beginning at 0
ggplot(projectdata, aes(x = Age)) + geom_histogram(binwidth = 5, boundary = 0)
# Plot "BMI" (numeric data) using default styling, bin width 5, and bins beginning at 0
ggplot(projectdata, aes(x = BMI)) + geom_histogram(binwidth = 5, boundary = 0)
# plot "BloodPressure" (numeric data) using default styling, bin width 5, and bins beginning at 0
ggplot(projectdata, aes(x = BloodPressure)) + geom_histogram(binwidth = 5, boundary = 0)
# plot "Glucose_Level" (numeric data) using default styling, bin width 5, and bins beginning at 0
ggplot(projectdata, aes(x = Glucose_Level)) + geom_histogram(binwidth = 5, boundary = 0)

#Correlation 
## Plot "BMI" and "BloodPressure" (both numeric)
ggplot(projectdata, aes(x = BMI, y = BloodPressure)) + geom_point()
## Perform correlation test to assess if "BMI" and "Blood pressure" are correlated
cor.test(projectdata$BMI, projectdata$BloodPressure)
# Result: Yes,"BMI" and "Blood pressure" are weakly positively correlated (r = 0.28, p < 0.05)

## Plot "Age" and "glucoselevel" (both numeric)
ggplot(projectdata, aes(x = Age, y = Glucose_Level)) + geom_point()
## Perform correlation test to assess if "Age" and "Glucose level" are correlated
cor.test(projectdata$Age, projectdata$Glucose_Level)
# Result: Yes,"Age" and "Glucose level" are weakly positively correlated (r = 0.26, p < 0.05)

# PLot "Age" and "BloodPressure" (both numeric)
ggplot(projectdata, aes(x = Age, y = BloodPressure)) + geom_point()
## Perform correlation test to assess if "Age" and "Blood pressure" are correlated
cor.test(projectdata$Age, projectdata$BloodPressure)
# Result: Yes,"Age" and "Blood pressure" are weakly positively correlated (r = 0.24, p < 0.05)

#CATEGORICAL VARIABLES
# Plot Diabetes_Outcome  (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(projectdata, aes(x = Diabetes_Outcome)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot Skinthickness (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(projectdata, aes(x = Skinthickness)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
## Plot Diabetespedigreefunction (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(projectdata, aes(x = Diabetespedigreefunction)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Plot Age_Cat
ggplot(projectdata, aes(x = Age_Cat)) + geom_bar(fill = "navyblue") + labs(title = "Age Category Distribution", x = "Age Category", y = "Frequency")
# Plot pregnancies
ggplot(projectdata, aes(x = pregnancies)) + geom_bar(fill = "navyblue") + labs(title = "Pregnancies Distribution", x = "Pregnancies", y = "Frequency")

#INDEPENDENCE TEST -CHI -SQUARE TEST
# Count unique values of "Diabetes_Outcome" and "pregnancies"
table(projectdata$Diabetes_Outcome, projectdata$pregnancies)
# Perform chi-square test to test if "Diabetes_Outcome" and "pregnancies" (unordered factors) affect each other
chisq.test(table(projectdata$Diabetes_Outcome, projectdata$pregnancies))
# Result: Yes, we reject H0 and conclude that "Diabetes_Outcome" and "pregnancies" affect each other (p < 0.05)

# Count unique values of "Diabetes_Outcome" and "Skinthickness"
table(projectdata$Diabetes_Outcome, projectdata$Skinthickness)
# Perform chi-square test to test if "Diabetes_Outcome" and "Skinthickness" (unordered factors) affect each other
chisq.test(table(projectdata$Diabetes_Outcome, projectdata$Skinthickness))
# Result: Yes,we reject H0 and conclude that "Diabetes_Outcome" and "Skinthickness" affect each other (p < 0.05)

# Count unique values of "Diabetes_Outcome" and "Diabetespedigreefunction"
table(projectdata$Diabetes_Outcome, projectdata$Diabetespedigreefunction)
# Perform chi-square test to test if "Diabetes_Outcome" and "Diabetespedigreefunction" (unordered factors) affect each other
chisq.test(table(projectdata$Diabetes_Outcome, projectdata$Diabetespedigreefunction))
# Result: Yes, we reject H0 and conclude that "Diabetes_Outcome" and "Diabetespedigreefunction" affect each other (p < 0.05)

#COUNT UNIQUE VALUES OF "Diabetes_Outcome" AND "Age_Cat"
table(projectdata$Diabetes_Outcome, projectdata$Age_Cat)
# Perform chi-square test to test if "Diabetes_Outcome" and "Age_Cat" (unordered factors) affect each other
chisq.test(table(projectdata$Diabetes_Outcome, projectdata$Age_Cat))
# Result: Yes, we reject H0 and conclude that "Diabetes_Outcome" and "Age_Cat" affect each other (p < 0.05)


#MEAN DIFFERENCE TEST  -T TEST
# Analysis of "Diabetes Outcome " (unordered factor w/2 levels) and "BMI" (numeric data)
# Calculate descriptive statistics for each level of "Diabetes Outcome"
tapply(projectdata, projectdata$Diabetes_Outcome, summary)
# Plot the distribution of "BMI" for each level of "Diabetes Outcome"
ggplot(projectdata, aes(x = Diabetes_Outcome, y = BMI)) + geom_boxplot()
# Perform t-test to assess if the mean of "BMI" is significantly different across both levels of "Diabetes Outcome"
t.test(projectdata$BMI ~ projectdata$Diabetes_Outcome)
t.test(data = projectdata, BMI ~ Diabetes_Outcome)
# Result: Yes, the mean of "BMI" is significantly different across both levels of "Diabetes Outcome" (p < 0.05)

# Analysis of "Diabetes Outcome " (unordered factor w/2 levels) and "BloodPressure" (numeric data)
# Calculate descriptive statistics for each level of "Diabetes Outcome"
tapply(projectdata, projectdata$Diabetes_Outcome, summary)
# Plot the distribution of "BloodPressure" for each level of "Diabetes Outcome"
ggplot(projectdata, aes(x = Diabetes_Outcome, y = BloodPressure)) + geom_boxplot()
# Perform t-test to assess if the mean of "BloodPressure" is significantly different across both levels of "Diabetes Outcome"
t.test(projectdata$BloodPressure ~ projectdata$Diabetes_Outcome)
t.test(data = projectdata, BloodPressure ~ Diabetes_Outcome)
# Result: Yes, the mean of "BloodPressure" is significantly different across both levels of "Diabetes Outcome" (p < 0.05)

# ANOVA 
# Analysis of "Age_Cat " (unordered factor w/3 levels) and "insulin" (numeric data)
# Calculate descriptive statistics for each level of "Age_Cat"
tapply(projectdata, projectdata$Age_Cat, summary)
# Plot the distribution of "Insulin" for each level of "Age_Cat"
ggplot(projectdata, aes(x = Age_Cat, y = Insulin)) + geom_boxplot()
# Perform ANOVA to assess if the mean of "insulin" is significantly different across levels of "Age_Cat"
summary(aov(data = projectdata, Insulin ~ Age_Cat))
# Result: No, the mean of "insulin" isn't significantly different across levels of "Age_Cat" (p > 0.05)

# Analysis of "Age_Cat " (unordered factor w/3 levels) and "BMI" (numeric data)
# Calculate descriptive statistics for each level of "Age_Cat"
tapply(projectdata, projectdata$Age_Cat, summary)
# Plot the distribution of "BMI" for each level of "Age_Cat"
ggplot(projectdata, aes(x = Age_Cat, y = BMI)) + geom_boxplot()
# Perform ANOVA to assess if the mean of "BMI" is significantly different across levels of "Age_Cat"
summary(aov(data = projectdata, BMI ~ Age_Cat))
# Result: No, the mean of "BMI" isn't significantly different across levels of "Age_Cat" (p > 0.05)

# Analysis of "Age_Cat " (unordered factor w/3 levels) and "BloodPressure" (numeric data)
# Calculate descriptive statistics for each level of "Age_Cat"
tapply(projectdata, projectdata$Age_Cat, summary)
# Plot the distribution of "BloodPressure" for each level of "Age_Cat"
ggplot(projectdata, aes(x = Age_Cat, y = BloodPressure)) + geom_boxplot()
# Perform ANOVA to assess if the mean of "BloodPressure" is significantly different across levels of "Age_Cat"
summary(aov(data = projectdata, BloodPressure ~ Age_Cat))
# Result: No, the mean of "BloodPressure" isn't significantly different across levels of "Age_Cat" (p > 0.05)
