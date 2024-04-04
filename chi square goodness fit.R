#reading original dataset into data
data <- read.csv("C:/Users/vidhi/Desktop/Crime_Data_from_2020_to_Present.csv", header = TRUE)

install.packages("tidyverse")
library(tidyverse)

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

View(data_2)


freq<-table(data_2$Vict.Descent)
print("frequency")
freq_df <- as.data.frame(freq)
print(freq_df)
print(freq_df$Freq)

#---------------------------------------------------------------------------------------------------

#chi squared goodness of fit test 

# Create a table of observed frequencies for Descent (ethnicity)
descent_table <- table(data_2$Vict.Descent)
# Convert the table to a dataframe
descent_df <- as.data.frame(descent_table)
# Create a vector of unique descent types
descent_types <- unique(data_2$Vict.Descent)
# Extract the observed frequencies
observed_freq <- descent_df$Freq


# Define your expected probabilities (e.g., uniform distribution)
expected_probabilities <- rep(1/length(descent_types), length(descent_types))

# Perform the chi-squared goodness of fit test
chi_sq_test <- chisq.test(observed_freq, p = expected_probabilities)
# Set your significance level (alpha)
alpha <- 0.05
# Print the results
print(chi_sq_test)

# Make a decision based on the p-value
if (chi_sq_test$p.value <= alpha) {
  cat("Reject the null hypothesis. There is a significant difference between observed and expected frequencies.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference between observed and expected frequencies.\n")
}

barplot(rbind(observed_freq, expected_probabilities), beside = TRUE,
        col = c("blue", "red"), legend.text = TRUE)

