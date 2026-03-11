# Load library "tidyverse" and "here"
library(tidyverse)
library(here)
# load "Developer Survey Results.csv" in folder "21-Final Project"
devsurvey <- read_csv(here("21-Final Project", "Developer Survey Results.csv"))
# Calculate descriptive statistics
summary(devsurvey)

# Filter by compTotalUSD 
# Create a subset with rows where "compTotalUSD" is greater than 0
devsurvey_subset <- filter(devsurvey, CompTotalUSD > 0)

#Filter by JobSat
# Create a subset with rows where "JobSat" is greater than 0
devsurvey_subset <- filter(devsurvey_subset, JobSat > 0)

#Years Code
# print unique values in "YearsCode"
table(devsurvey_subset$YearsCode)
#Replace "Less than 1 year" with "0" in "YearsCode"
devsurvey_subset$YearsCode[devsurvey_subset$YearsCode == "Less than 1 year"] <- "0"
#Replace "More than 50 years" with "50" in "YearsCode"
devsurvey_subset$YearsCode[devsurvey_subset$YearsCode == "More than 50 years"] <- "50"
#Convert "YearsCode" (character data) to numeric
devsurvey_subset$YearsCode <- as.numeric(devsurvey_subset$YearsCode)
# Calculate descriptive statistics
summary(devsurvey_subset$YearsCode)
# Print unique values in "YearsCode"
table(devsurvey_subset$YearsCode)

#YearsCodePro
# print unique values in "YearsCodePro"
table(devsurvey_subset$YearsCodePro)
#Replace "Less than 1 year" with "0" in "YearsCodePro"
devsurvey_subset$YearsCodePro[devsurvey_subset$YearsCodePro == "Less than 1 year"] <- "0"
#Replace "More than 50 years" with "50" in "YearsCodePro"
devsurvey_subset$YearsCodePro[devsurvey_subset$YearsCodePro == "More than 50 years"] <- "50"
#Convert "YearsCodePro" (character data) to numeric
devsurvey_subset$YearsCodePro <- as.numeric(devsurvey_subset$YearsCodePro)
# Calculate descriptive statistics
summary(devsurvey_subset$YearsCodePro)

# Explode the variable Employment 
devsurvey_subset <- separate_rows(devsurvey_subset, Employment, sep = ";") 
devsurvey_subset$value <- TRUE 
devsurvey_subset <- pivot_wider(devsurvey_subset, names_from = Employment, 
                        names_prefix = "Employment_", values_from = value, 
                        values_fill = FALSE) 
colnames(devsurvey_subset) 

##1.Handling missing variables & cleaning
# Convert "Age" (character data) to factor
devsurvey_subset$Age <- factor(devsurvey_subset$Age)
#CONVERT "Age" (character data) to CHARACTER
devsurvey_subset$Age <- as.character(devsurvey_subset$Age)
#CONVERT TO ORDERED FACTOR
devsurvey_subset$Age <- factor(devsurvey_subset$Age, levels = c("Under 18 years old", "18-24 years old", "25-34 years old", "35-44 years old", "45-54 years old", "55-64 years old", "65 years or older", "Prefer not to say"), ordered = TRUE)
# Print unique values in "Age"
table(devsurvey_subset$Age)
# Count number of unique values in "Age"
length(table(devsurvey_subset$Age))

#convert "RemoteWork" (character data) to unordered factor
devsurvey_subset$RemoteWork <- factor(devsurvey_subset$RemoteWork)
# Print unique values in "RemoteWork"
table(devsurvey_subset$RemoteWork)


#convert EdLevel  (character data) to factor
devsurvey_subset$EdLevel <- factor(devsurvey_subset$EdLevel)
#convert EdLevel (factor) to character
devsurvey_subset$EdLevel <- as.character(devsurvey_subset$EdLevel)
#convert EdLevel (character data) to ordered factor
devsurvey_subset$EdLevel <- factor(devsurvey_subset$EdLevel, levels = c("Something else", "Primary/elementary school","Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)", "Some college/university study without earning a degree", "Associate degree (A.A., A.S., etc.)", "Bachelor’s degree (B.A., B.S., B.Eng., etc.)", "Bachelor's degree", "Master’s degree (M.A., M.S., M.Eng., MBA, etc.)", "Professional degree (JD, MD, Ph.D, Ed.D, etc.)"), ordered = TRUE)
# Print unique values in "EdLevel"
table(devsurvey_subset$EdLevel)
# Count number of unique values in "EdLevel"
length(table(devsurvey_subset$EdLevel))

#convert Industry (character data) to factor
devsurvey_subset$Industry <- factor(devsurvey_subset$Industry)
# Print unique values in "Industry"
table(devsurvey_subset$Industry)
# Count number of unique values in "Industry"
length(table(devsurvey_subset$Industry))

#rename "Employment_Employed, full-time" to "Employment_Employed_full_time"
devsurvey_subset <- rename(devsurvey_subset, "Employment_Employed_full_time" = "Employment_Employed, full-time")
#convert Employment_Employed, full-time (character data) to factor
devsurvey_subset$Employment_Employed_full_time <- factor(devsurvey_subset$Employment_Employed_full_time)
# Print unique values in "Employment_Employed_full_time"
table(devsurvey_subset$Employment_Employed_full_time)
# Count number of unique values in "Employment_Employed_full_time"
length(table(devsurvey_subset$Employment_Employed_full_time))

#rename "Employment_Student, full-time" to "Employment_Student_full_time"
devsurvey_subset <- rename(devsurvey_subset, "Employment_Student_full_time" = "Employment_Student, full-time")
#convert Employment_Student, full-time (logical) to undordered factor
devsurvey_subset$Employment_Student_full_time <- factor(devsurvey_subset$Employment_Student_full_time)
# Print unique values in "Employment_Student_full_time"
table(devsurvey_subset$Employment_Student_full_time)

#rename "Employment_Independent contractor, freelancer, or self-employed" to "Employment_Independent_contractor"
devsurvey_subset <- rename(devsurvey_subset, "Employment_Independent_contractor" = "Employment_Independent contractor, freelancer, or self-employed")
#convert Employment_Independent contractor, freelancer, or self-employed (character data) to factor
devsurvey_subset$Employment_Independent_contractor <- factor(devsurvey_subset$Employment_Independent_contractor)
# Print unique values in "Employment_Independent_contractor"
table(devsurvey_subset$Employment_Independent_contractor)

#rename Employment_Not employed, but looking for work to Employment_Not_employed_looking
devsurvey_subset <- rename(devsurvey_subset, "Employment_Not_employed_looking" = "Employment_Not employed, but looking for work")
#convert Employment_Not employed, but looking for work (character data) to factor
devsurvey_subset$Employment_Not_employed_looking <- factor(devsurvey_subset$Employment_Not_employed_looking)
# Print unique values in "Employment_Not_employed_looking"
table(devsurvey_subset$Employment_Not_employed_looking)

#rename "Employment_Employed, part-time" to "Employment_Employed_part_time"
devsurvey_subset <- rename(devsurvey_subset, "Employment_Employed_part_time" = "Employment_Employed, part-time")
#convert Employment_Employed, part-time (character data) to factor
devsurvey_subset$Employment_Employed_part_time <- factor(devsurvey_subset$Employment_Employed_part_time)
# Print unique values in "Employment_Employed_part_time"
table(devsurvey_subset$Employment_Employed_part_time)
# Count number of unique values in "Employment_Employed_part_time"
length(table(devsurvey_subset$Employment_Employed_part_time))

## Initialize "JobSat_Cat" with all missing values
devsurvey_subset$JobSat_Cat <- NA
# Set "JobSat_Cat" to "Very dissatisfied" if "JobSat" <= 3
devsurvey_subset$JobSat_Cat[devsurvey_subset$JobSat <= 3] <- "Very dissatisfied"
# Set "JobSat_Cat" to "Dissatisfied" if "JobSat" > 3 and <= 6
devsurvey_subset$JobSat_Cat[devsurvey_subset$JobSat > 3 & devsurvey_subset$JobSat <= 6] <- "Dissatisfied"
# Set "JobSat_Cat" to "Neutral" if "JobSat" > 6 and <= 8
devsurvey_subset$JobSat_Cat[devsurvey_subset$JobSat > 6 & devsurvey_subset$JobSat <= 8] <- "Neutral"
# Set "JobSat_Cat" to "Satisfied" if "JobSat" > 8 and <= 10
devsurvey_subset$JobSat_Cat[devsurvey_subset$JobSat > 8 & devsurvey_subset$JobSat <= 10] <- "Satisfied"
#convert "JobSat_Cat" (character data) to ordereed factor
devsurvey_subset$JobSat_Cat <- factor(devsurvey_subset$JobSat_Cat)
#convert "JobSat_Cat" (ordered factor) to character
devsurvey_subset$JobSat_Cat <- as.character(devsurvey_subset$JobSat_Cat)
#convert "JobSat_Cat" (character data) to ordered factor
devsurvey_subset$JobSat_Cat <- factor(devsurvey_subset$JobSat_Cat, levels = c("Very dissatisfied", "Dissatisfied", "Neutral", "Satisfied"), ordered = TRUE)

#Descriptive statistics for numerical Variables
# Calculate descriptive statistics
summary(devsurvey_subset)
#plot YearsCode, YearsCodePro, CompTotalUSD, jobsat
# Plot "YearsCode" (numeric data) using default styling, bin width 5, and bins beginning at 0
ggplot(devsurvey_subset, aes(x = YearsCode)) + geom_histogram(binwidth = 5, boundary = 0)
# Plot "YearsCodePro" (numeric data) using default styling, bin width 5, and bins beginning at 0
ggplot(devsurvey_subset, aes(x = YearsCodePro)) + geom_histogram(binwidth = 5, boundary = 0)
# Plot "CompTotalUSD" (numeric data) using default styling, bin width 5000, and bins beginning at 0
ggplot(devsurvey_subset, aes(x = CompTotalUSD)) + geom_histogram(binwidth = 1000000, boundary = 0)


#print unique values in "CompTotalUSD"
table(devsurvey_subset$CompTotalUSD)

#correralation
## Plot "YearsCode" and "CompTotalUSD" (both numeric)
ggplot(devsurvey_subset, aes(x = YearsCode, y = CompTotalUSD)) + geom_point()
# Perform correlation test to test if "YearsCode" and "CompTotalUSD" (both numeric) are correlated
cor.test(devsurvey_subset$YearsCode, devsurvey_subset$CompTotalUSD)
# Result: Yes, "YearsCode" and "CompTotalUSD" are weakly positively correlated (r = 0.15, p < 0.05)

# Plot "Jobsat" and "CompTotalUSD" (both numeric)
ggplot(devsurvey_subset, aes(x = JobSat, y = CompTotalUSD)) + geom_point()
# Perform correlation test to test if "JobSat" and "CompTotalUSD" (both numeric) are correlated
cor.test(devsurvey_subset$JobSat, devsurvey_subset$CompTotalUSD)
# cor 0.0495
# Result: Yes, "JobSat" and "CompTotalUSD" are weakly positively correlated (r = 0.0495, p < 0.05)

# Plot "YearsCodePro" and "CompTotalUSD" (both numeric)
ggplot(devsurvey_subset, aes(x = YearsCodePro, y = CompTotalUSD)) + geom_point()
# Perform correlation test to test if "YearsCodePro" and "CompTotalUSD" (both numeric) are correlated
cor.test(devsurvey_subset$YearsCodePro, devsurvey_subset$CompTotalUSD)
# Result: Yes, "YearsCodePro" and "CompTotalUSD" are weakly positively correlated (r = 0.15, p < 0.05)

#CATEGORICAL VARIABLES
# Plot Age  (ordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = Age)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#calculate value counts
table(devsurvey_subset$Age)

# Plot remoteWork (unordered factor) using default styling, rotate the x-axis labels by 45 degrees\
ggplot(devsurvey_subset, aes(x = RemoteWork)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#calculate value counts
table(devsurvey_subset$RemoteWork)

# Plot Industry (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = Industry)) + geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Calculate value counts
table(devsurvey_subset$Industry)

# Plot EdLevel (ordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = EdLevel)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Calculate value counts
table(devsurvey_subset$EdLevel)

# Plot Employment_Employed_full_time (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = Employment_Employed_full_time)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#calculate Value counts
table(devsurvey_subset$Employment_Employed_full_time)

# Plot Employment_Student_full_time (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = Employment_Student_full_time)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Calculate value counts
table(devsurvey_subset$Employment_Student_full_time)


# Plot Employment_Independent_contractor (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = Employment_Independent_contractor)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Calculate value counts
table(devsurvey_subset$Employment_Independent_contractor)


# Plot Employment_Not_employed_looking (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = Employment_Not_employed_looking)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Calculate value counts
table(devsurvey_subset$Employment_Not_employed_looking)


# Plot Employment_Retired (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = Employment_Retired)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Calculate value counts
table(devsurvey_subset$Employment_Retired)
# Plot Jobsat (unordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = JobSat)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot JobSat_Cat (ordered factor) using default styling, rotate the x-axis labels by 45 degrees
ggplot(devsurvey_subset, aes(x = JobSat_Cat)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))



#INDEPENDENCE TEST -CHI -SQUARE TEST
#1.Remote Work and JobSat
# Count unique values in Jobsat_cat
table(devsurvey_subset$JobSat_Cat)
# Count unique values in RemoteWork
table(devsurvey_subset$RemoteWork)
# Count unique values in RemoteWork and JobSat_Cat
table(devsurvey_subset$RemoteWork, devsurvey_subset$JobSat_Cat)
# Perform chi-square test to test if "RemoteWork" and "JobSat_Cat" (unordered factors) affect each other
chisq.test(table(devsurvey_subset$RemoteWork, devsurvey_subset$JobSat_Cat))

#2. Age and JobSat_cat
# Count unique values in Age
table(devsurvey_subset$Age)
# Count unique values in JobSat_cat
table(devsurvey_subset$JobSat_Cat)
# Count unique values in Age and JobSat_cat
table(devsurvey_subset$Age, devsurvey_subset$JobSat_Cat)
# Perform chi-square test to test if "Age" and "JobSat_Cat" (unordered factors) affect each other
chisq.test(table(devsurvey_subset$Age, devsurvey_subset$JobSat_Cat))

#country and JobSat_cat
#calculate descriptive statistics
summary(devsurvey_subset)
#covert "Country" (character data) to factor
devsurvey_subset$Country <- factor(devsurvey_subset$Country)
# Count unique values in Country
table(devsurvey_subset$Country)
# Count unique values in JobSat_cat
table(devsurvey_subset$JobSat_Cat)
# Count unique values in Country and JobSat_cat
table(devsurvey_subset$Country, devsurvey_subset$JobSat_Cat)
# Perform chi-square test to test if "Country" and "JobSat_Cat" (unordered factors) affect each other
chisq.test(table(devsurvey_subset$Country, devsurvey_subset$JobSat_Cat))

##Mean Difference Test -T test
# Analysis of "Employment_Employed_full_time " (unordered factor w/2 levels) and "compTotalUSD" (numeric data)
#1. compTotalUSD and Employment_Employed_full_time
# Calculate descriptive statistics for each level of "Employment_Employed_full_time"
tapply(devsurvey_subset, devsurvey_subset$Employment_Employed_full_time, summary)
# Plot the distribution of "compTotalUSD" for each level of "Employment_Employed_full_time"
ggplot(devsurvey_subset, aes(x = Employment_Employed_full_time, y = CompTotalUSD)) + geom_boxplot()
# Perform t-test to assess if the mean of "compTotalUSD" is significantly different across both levels of "Employment_Employed_full_time"
t.test(devsurvey_subset$CompTotalUSD ~ devsurvey_subset$Employment_Employed_full_time)
t.test(data = devsurvey_subset, CompTotalUSD ~ Employment_Employed_full_time)
# Result: Yes, the mean of "compTotalUSD" is significantly different across both levels of "Employment_Employed_full_time" (p < 0.05)

#2. compTotalUSD and Employment_Student_full_time
# Calculate descriptive statistics for each level of "Employment_Student_full_time"
tapply(devsurvey_subset, devsurvey_subset$Employment_Student_full_time, summary)
# Plot the distribution of "compTotalUSD" for each level of "Employment_Student_full_time"
ggplot(devsurvey_subset, aes(x = Employment_Student_full_time, y = CompTotalUSD)) + geom_boxplot()
# Perform t-test to assess if the mean of "compTotalUSD" is significantly different across both levels of "Employment_Student_full_time"
t.test(devsurvey_subset$CompTotalUSD ~ devsurvey_subset$Employment_Student_full_time)
t.test(data = devsurvey_subset, CompTotalUSD ~ Employment_Student_full_time)
# Result: Yes, the mean of "compTotalUSD" is significantly different across both levels of "Employment_Student_full_time" (p < 0.05)

#3. compTotalUSD and Employment_Independent_contractor
# Calculate descriptive statistics for each level of "Employment_Independent_contractor"
tapply(devsurvey_subset, devsurvey_subset$Employment_Independent_contractor, summary)
# Plot the distribution of "compTotalUSD" for each level of "Employment_Independent_contractor"
ggplot(devsurvey_subset, aes(x = Employment_Independent_contractor, y = CompTotalUSD)) + geom_boxplot()
# Perform t-test to assess if the mean of "compTotalUSD" is significantly different across both levels of "Employment_Independent_contractor"
t.test(devsurvey_subset$CompTotalUSD ~ devsurvey_subset$Employment_Independent_contractor)
t.test(data = devsurvey_subset, CompTotalUSD ~ Employment_Independent_contractor)
# Result: Yes, the mean of "compTotalUSD" is NOT significantly different across both levels of "Employment_Independent_contractor" (p < 0.7672)

#4. compTotalUSD and Employment_Not_employed_looking
# Calculate descriptive statistics for each level of "Employment_Not_employed_looking"
tapply(devsurvey_subset, devsurvey_subset$Employment_Not_employed_looking, summary)
# Plot the distribution of "compTotalUSD" for each level of "Employment_Not_employed_looking"
ggplot(devsurvey_subset, aes(x = Employment_Not_employed_looking, y = CompTotalUSD)) + geom_boxplot()
# Perform t-test to assess if the mean of "compTotalUSD" is significantly different across both levels of "Employment_Not_employed_looking"
t.test(devsurvey_subset$CompTotalUSD ~ devsurvey_subset$Employment_Not_employed_looking)
t.test(data = devsurvey_subset, CompTotalUSD ~ Employment_Not_employed_looking)
# Result: Yes, the mean of "compTotalUSD" is significantly different across both levels of "Employment_Not_employed_looking" (p < 0.05)

#5. compTotalUSD and Employment_Employed_part_time
# Calculate descriptive statistics for each level of "Employment_Employed_part_time"
tapply(devsurvey_subset, devsurvey_subset$Employment_Employed_part_time, summary)
# Plot the distribution of "compTotalUSD" for each level of "Employment_Employed_part_time"
ggplot(devsurvey_subset, aes(x = Employment_Employed_part_time, y = CompTotalUSD)) + geom_boxplot()
# Perform t-test to assess if the mean of "compTotalUSD" is significantly different across both levels of "Employment_Employed_part_time"
t.test(devsurvey_subset$CompTotalUSD ~ devsurvey_subset$Employment_Employed_part_time)
t.test(data = devsurvey_subset, CompTotalUSD ~ Employment_Employed_part_time)
# Result: Yes, the mean of "compTotalUSD" is significantly different across both levels of "Employment_Employed_part_time" (p < 0.05)

#6. compTotalUSD and Employment_Retired
# Calculate descriptive statistics for each level of "Employment_Retired"
tapply(devsurvey_subset, devsurvey_subset$Employment_Retired, summary)
# Plot the distribution of "compTotalUSD" for each level of "Employment_Retired"
ggplot(devsurvey_subset, aes(x = Employment_Retired, y = CompTotalUSD)) + geom_boxplot()
# Perform t-test to assess if the mean of "compTotalUSD" is significantly different across both levels of "Employment_Retired"
t.test(devsurvey_subset$CompTotalUSD ~ devsurvey_subset$Employment_Retired)
t.test(data = devsurvey_subset, CompTotalUSD ~ Employment_Retired)
# Result: Yes, the mean of "compTotalUSD" is significantly different across both levels of "Employment_Retired" (p < 0.05)

## ANOVA
# Analysis of "RemoteWork" (ordered factor w/3levels) and "CompTotalUSD" (numeric data)
# Calculate descriptive statistics for each level of "RemoteWork"
tapply(devsurvey_subset, devsurvey_subset$RemoteWork, summary)
# Plot the distribution of "CompTotalUSD" for each level of "RemoteWork"
ggplot(devsurvey_subset, aes(x = RemoteWork, y = CompTotalUSD)) + geom_boxplot()
# Perform ANOVA to assess if the mean of "CompTotalUSD" is significantly different across levels of "RemoteWork"
summary(aov(data = devsurvey_subset, CompTotalUSD ~ RemoteWork))
# Result: Yes, the mean of "CompTotalUSD" is significantly different across levels of "RemoteWork" (p < 0.05)

#Perform Tukey's post-hoc test to assess which levels of "RemoteWork" are significantly different
TukeyHSD(aov(data = devsurvey_subset, CompTotalUSD ~ RemoteWork))
# Result: The mean is significantly different across all levels of "RemoteWork", except between "Hybrid" and "Fully remote"