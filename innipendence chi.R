#reading original dataset into data
data <- read.csv("C:/Users/vidhi/Desktop/Crime_Data_from_2020_to_Present.csv", header = TRUE)

install.packages("tidyverse")
library(tidyverse)


#View(data)
#glimpse(data)
#unique(data$AREA.NAME)
#unique(data$Vict.Age)
#unique(data$Vict.Sex)
#unique(data$Weapon.Desc)
#unique(data$TIME.OCC)
#unique(data$Premis.Desc)
#unique(data$Status.Desc)


#missing values in original data set(data)
missing_values <- colSums(is.na(data))
missing_values <- missing_values[order(missing_values, decreasing = TRUE)]
print(missing_values)

#here we replace blank values with NA and store in data_1
library(dplyr)
data_1 <- data %>%
  mutate_all(~ifelse(. == "" | . == "0", NA, .))

#here we replace NA values in Weapon.Desc to UNKNOWN WEAPON/OTHER WEAPON
data_1 <- data_1 %>%
  mutate(Weapon.Desc = ifelse(is.na(Weapon.Desc), "UNKNOWN WEAPON/OTHER WEAPON",
                              Weapon.Desc))


#removing columns not needed for analysis and removing missing values from 
#remaining columns
data_2 <- data_1 %>%
  select(DR_NO, Date.Rptd, DATE.OCC, TIME.OCC, AREA.NAME, Rpt.Dist.No, Part.1.2, Crm.Cd.Desc, 
         Crm.Cd, Vict.Age, Vict.Sex, Vict.Descent, Premis.Desc, Weapon.Desc, Status.Desc,
         LAT, LON) %>% 
  na.omit()


#missing values in data_2
missing_values <- colSums(is.na(data_2))
missing_values <- missing_values[order(missing_values, decreasing = TRUE)]
print(missing_values)


#-----------------------------------------------------------------------------------

#Chi-Squared Test of Independence
#null hypo : there is no difference
#alternate: there is difference 

times <- data_2$TIME.OCC[1:100]
print(times)
categorize_time <- function(time) {
  if (time >= 0 && time <= 400) {
    return("Night")
  } else if (time >= 401 && time <= 1200) {
    return("Morning")
  } else if (time >= 1201 && time <= 1900) {
    return("Afternoon")
  } else if (time >= 1901 && time <= 2400) {
    return("Night")
  } 
}
# Apply the categorize_time function to the times
time_categories <- sapply(times, categorize_time)
# Create a data frame with the original times and their categories
time_df <- data.frame(Time = times, Category = time_categories)
# Print the result
print(time_df)




gender <- data_2$Vict.Sex[1:100]
print(gender)
categorize_gender <- function(gender) {
  if (gender == "F") {
    return("Female")
  } else if (gender == "M") {
    return("Male")
  } else {
    return("Other/Unknown")
  }
}
# Apply the categorize_gender function to the 'gender' vector
gender_categories <- sapply(gender, categorize_gender)
# Create a data frame with the original 'gender' values and their categories
gender_df <- data.frame(Gender = gender, Category = gender_categories)
# Print the result
print(gender_df)



genders <- c(gender_df$Category)
time_of_day <- c(time_df$Category)
contingency_table <- table(genders, time_of_day)
chi_squared_test <- chisq.test(contingency_table)
print(chi_squared_test)

# Define the significance level (alpha)
alpha <- 0.05

# Determine whether to accept or reject the null hypothesis based on p-value
if (chi_squared_test$p.value < alpha) {
  cat("Reject the null hypothesis: There is a significant association between gender and time of day.")
} else {
  cat("Accept the null hypothesis: There is no significant association between gender and time of day.")
}







library(ggplot2)

# Assuming you have already performed the chi-squared test and stored the results in 'chi_squared_test'

# Create a data frame from the contingency table for plotting
contingency_df <- as.data.frame(chi_squared_test$observed)

# Add row and column names for better labeling
contingency_df$Gender <- rownames(contingency_df)
rownames(contingency_df) <- NULL

# Reshape the data for plotting
library(tidyr)
contingency_melted <- gather(contingency_df, key = "TimeOfDay", value = "Count", -Gender)

# Create a clustered bar chart and add a box plot
ggplot(data = contingency_melted, aes(x = Gender, y = Count, fill = TimeOfDay)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_boxplot(aes(group = Gender), position = position_dodge(width = 0.9), alpha = 0.5, width = 0.2) +
  labs(title = "Relationship Between Gender and Time of Day",
       x = "Gender",
       y = "Count") +
  scale_fill_manual(values = c("Morning" = "lightblue", "Afternoon" = "lightgreen", "Night" = "lightpink"),
                    name = "Time of Day") +
  theme_minimal()