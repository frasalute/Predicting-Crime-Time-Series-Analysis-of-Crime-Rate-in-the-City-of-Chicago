rm(list =ls())
crime <- read.csv("insert_file_position_crime_data")
install.packages("Amelia")
library(Amelia)
library(dplyr)
library(lubridate)
library(ggplot2)

# ANALYSE AND EXPLORE THE DATASET

# Observe the first 6 rows of data
head(crime)

# Names of all the columns present in the dataset
names(crime)

# Understanding basic statistic and structure

# Check total number of rows in dataset
num_rows <- nrow(crime)
print(num_rows)

str(crime)

summary (crime)

crime$Date <- as.POSIXct(crime$Date, format="%m/%d/%Y %I:%M:%S %p") # Date saved as Date and Time
crime$Updated.On <- as.POSIXct(crime$Updated.On, format="%m/%d/%Y %I:%M:%S %p") # Updated on as Date and Time
crime$Arrest <- as.logical(crime$Arrest) # Since true or false made into binary
crime$Domestic <- as.logical(crime$Domestic)

str(crime) # Check again

# Check which columns have missing values
missing_values <- colSums(is.na(crime))
print(missing_values)

# Check for empty strings 
sum(crime$Case.Number == "")
sum(crime$Block == "")
sum(crime$IUCR == "")
sum(crime$Primary.Type == "")
sum(crime$Description == "")
sum(crime$Location.Description == "")
sum(crime$FBI.Code == "")
sum(crime$Location == "")

# Visualize missing data
missmap(crime, main = "Missing Data Map", col = c("blue", "pink"), legend = TRUE)

# Since missing values account only for 1%, the decision is to drop them

# Identify rows with NA values
rows_with_na <- !complete.cases(crime)

# Identify rows with empty strings in specific columns
rows_with_empty_strings <- apply(crime, 1, function(row) {
  any(row["Case.Number"] == "" | 
        row["Block"] == "" | 
        row["IUCR"] == "" | 
        row["Primary.Type"] == "" | 
        row["Description"] == "" | 
        row["Location.Description"] == "" | 
        row["FBI.Code"] == "" | 
        row["Location"] == "")
})

# Combine the conditions
rows_to_drop <- rows_with_na | rows_with_empty_strings

# Exclude rows with NA values or empty strings
crime_cleaned <- crime[!rows_to_drop, ]

num_rows <- nrow(crime_cleaned)
print(num_rows)
missmap(crime_cleaned, main = "Clean Map", col = c("blue", "pink"), legend = TRUE)

# Check for duplicates
duplicates <- sum(duplicated(crime_cleaned))
print(duplicates)
crime_cleaned <- crime_cleaned[!duplicated(crime_cleaned), ]
View(crime_cleaned)


# Selecting only columns we actually need
crime_cleaned <- crime_cleaned %>%
  select(Date, Primary.Type)

-----------------------------------------------------------------------------------------------------------
  
# TOTAL CRIME 
  
crime_time <- crime %>%
  mutate(Date = floor_date(Date, unit = "hours")) # Round down to the nearest hour

total_crime_time <- crime_time %>%
  group_by(Date) %>%
  summarise(Total_Crime_Count = n()) %>%
  ungroup()

print(total_crime_time) # Check before saving the results as a cvs

# Save the resulting dataset
write.csv(total_crime_time, "total_crime_time.csv", row.names = FALSE) 

ggplot(total_crime_time, aes(x = Date, y = Total_Crime_Count)) +
  geom_line() +
  labs(title = "Total Crime Counts in Chicago through the Years", x = "Time", y = "Total Crime Count") +
  theme_minimal()

# Moving into TotalCrimeTime.R for analysis on Total Crime per Date
--------------------------------------------------------------------------------------------------------------------

# TOTAL CRIME PER YEAR-MONTH 

crime_YM <- crime_cleaned %>%
  mutate(YearMonth = format(Date, "%Y-%m")) # Impose the time as year-month

total_crime <- crime_YM %>%
  group_by(YearMonth) %>%
  summarise(Total_Crime_Count = n()) %>% # I want the total count of crime, no discrimination between the type of crime
  ungroup()

total_crime$YearMonth <- as.Date(paste0(total_crime$YearMonth, "-01")) # This is to save total crime in the correct format
print(total_crime) # Check before saving the results as a cvs

# Save the resulting dataset
write.csv(total_crime, "total_crime.csv", row.names = FALSE)

ggplot(total_crime, aes(x = YearMonth, y = Total_Crime_Count)) +
  geom_line() +
  labs(title = "Total Monthly Crime Counts in Chicago through the Years", x = "Month-Year", y = "Total Crime Count") +
  theme_minimal()

----------------------------------------------------------------------------------------------------------  

# TOP 5 CRIMES
types_crime <- unique(crime_cleaned$Primary.Type)
print(types_crime)

crime_counts <- crime_cleaned %>%
  group_by(Primary.Type) %>%
  summarise(Crime_Count = n()) %>%
  arrange(desc(Crime_Count))

top_5_crimes <- head(crime_counts, 5)

print(top_5_crimes)

# Plot for the occurrences of the top 5 crime types
ggplot(top_5_crimes, aes(x = reorder(Primary.Type, Crime_Count), y = Crime_Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Most Frequent Crime Types in Chicago",
       x = "Crime Type",
       y = "Number of Occurrences") +
  theme_minimal()



