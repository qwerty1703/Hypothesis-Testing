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
  select(DR_NO, Date.Rptd, DATE.OCC, TIME.OCC, AREA, AREA.NAME, Rpt.Dist.No, Part.1.2, Crm.Cd.Desc, 
         Crm.Cd, Vict.Age, Vict.Sex, Vict.Descent, Premis.Desc, Weapon.Desc, Status.Desc,
         LAT, LON) %>% 
  na.omit()


#missing values in data_2
missing_values <- colSums(is.na(data_2))
missing_values <- missing_values[order(missing_values, decreasing = TRUE)]
print(missing_values)

View(data_2)

library(openxlsx)
write.xlsx(data_2, "C:/Users/vidhi/Desktop/cleaned dataset.xlsx", asTable = FALSE, overwrite = TRUE)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Create the vector with numbers.
v <- c(data_2$AREA)
# Calculate the mode using the user function.
result <- getmode(v)
print(result)
# Create the vector with characters.
charv <- c("o","it","the","it","it")
# Calculate the mode using the user function.
result <- getmode(charv)
print(result)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Create the vector with numbers.
v <- c(data_2$AREA)
# Calculate the mode using the user function.
result <- getmode(v)
print(result)