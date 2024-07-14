library(tidyverse)
library(readr)
library(purrr)


 


data$`['1002023007735', 'Theft', 'Over', '$500', 'Suspended', '08/01/2023', '1:00', 'AM', 'Grant', 'North', '1250', 'Grant', 'Dr', 'N', 'NIU', 'Department', 'of']`
data <- data %>%
  rename(values = `['1002023007735', 'Theft', 'Over', '$500', 'Suspended', '08/01/2023', '1:00', 'AM', 'Grant', 'North', '1250', 'Grant', 'Dr', 'N', 'NIU', 'Department', 'of']`)

data_long <- data %>%
  mutate(row = row_number()) %>%  # Create a row identifier
  separate_rows(values, sep = ",") %>%  # Separate into multiple rows based on commas
  group_by(row) %>%
  mutate(colname = paste("value", row_number(), sep = "_")) %>%  # Create column names
  pivot_wider(names_from = colname, values_from = values) %>%  # Pivot to wide format
  select(-row)  # Remove the temporary row identifier



df <- data 
clean_text <- function(text) {
  # Extracts the last 12 digits from the string
  str_extract(text, "\\d{12}$")
}

# Assuming your data frame is named df and the column you want to modify is named 'column_name'
df$value_1 <- sapply(df$value_1, clean_text)


df %>%
  distinct(value_2)


df %>%
  distinct(value_3)


df %>%
  distinct(value_4)

df %>%
  filter(value_5 == "Real")


df <- df %>%
  select(-value_3)  # This excludes the value_3 column from the dataframe


# Assuming your data frame is named df

df <- df %>%
  mutate(
    Date = if_else(str_detect(value_2, "^\\d{1,2}/\\d{1,2}/\\d{4}$"), value_2, NA_character_),  # Extract dates to new column
    value_2 = if_else(is.na(Date), value_2, NA_character_)  # Replace dates in the original column with NA
  )


# Assuming your data frame is named df and you already have a Date column
df <- df %>%
  mutate(
    Time = if_else(str_detect(value_3, "^\\d{1,2}:\\d{2}$"), value_3, NA_character_),  # Detect and extract times
    Date = if_else(str_detect(value_3, "^\\d{1,2}/\\d{1,2}/\\d{4}$"), value_3, Date),  # Extract dates to Date column, preserve existing dates
    value_3 = if_else(is.na(Time) & is.na(Date), value_3, NA_character_)  # Keep text, replace times and dates with NA
  )

specific_values <- c("Trespass", "Sexual", "Battery", "Damage", "Disseminat", 
                     "Conduct", "Over", "$500", "w/Sec", "Cannabis", "Vehicle", 
                     "Cannabis>10", "to", "Card", "Possession", "By", "Scene", 
                     "Consumption", "of", "Domestic", "w/Security", "Use", "Liquor", 
                     "through", "Within", "Burglary", "Defacement", "State", 
                     "Crime", "Warrant", "Assault", "Notify", "Of", "Practices")


library(tidyverse)

df <- df %>%
  mutate(
    value_2 = if_else(value_3 %in% specific_values, paste(value_2, value_3, sep=" "), value_2),
    value_3 = if_else(value_3 %in% specific_values, NA_character_, value_3)
  )



disposition_values <- c("Closed", "Open", "Suspended", "Cleared", "Unfounded")

library(tidyverse)

df <- df %>%
  mutate(
    Disposition = if_else(value_3 %in% disposition_values, value_3, NA_character_),  # Move specific values to Disposition
    value_3 = if_else(value_3 %in% disposition_values, NA_character_, value_3)  # Set value_3 to NA where condition is met
  )



# Assuming your data frame is named df and you already have a Date column
df <- df %>%
  mutate(
    Date = if_else(str_detect(value_4, "^\\d{1,2}/\\d{1,2}/\\d{4}$"), value_4, Date),  # Extract dates to Date column, preserve existing dates
    value_3 = if_else(is.na(Date), value_4, NA_character_)  # Keep text, replace times and dates with NA
  )

date_pattern <- "^\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Remove hyphens by replacing them with a blank space
    value_4 = str_replace_all(value_4, "-", " "),
    # Identify dates and move them to the Date column
    Date = if_else(str_detect(value_4, date_pattern), value_4, Date),
    # Replace dates in value_4 with NA to denote a blank space
    value_4 = if_else(str_detect(value_4, date_pattern), NA_character_, value_4)
  )


specific_values <- c("To", "Assault", "Private", "$500", "Damage/Unatteded", 
                     "Fire", "<10", "Theft", "gm", "Notify", "Fraud", "Of", 
                     "Telephone", "Property", "State", "Orders", "Battery", "by", 
                     "Electronic", "Battery/Strangle", "Communication", "Alcohol", 
                     "The", "Abuse", "to", "Warrant", "Drug", "Fire.", "Lost", 
                     "Give", "or", "and")

library(tidyverse)

df <- df %>%
  mutate(
    value_2 = if_else(value_4 %in% specific_values, paste(value_2, value_4, sep=" "), value_2),
    value_4 = if_else(value_4 %in% specific_values, NA_character_, value_4)
  )


status_values <- c("Open", "Closed", "Suspended", "Cleared", "By")

library(tidyverse)

df <- df %>%
  mutate(
    Disposition = if_else(value_4 %in% status_values, paste(Disposition, value_4, sep=" "), Disposition),
    value_4 = if_else(value_4 %in% status_values, NA_character_, value_4)
  )



df %>%
  filter(t == "002024002071")

time_values <- c("pm", "am", "10:30")
remove_entry <- "Open 002024002071"

library(tidyverse)

df <- df %>%
  mutate(
    Time = if_else(value_4 %in% time_values, paste(Time, value_4, sep=" "), Time),
    value_4 = if_else(value_4 %in% time_values | value_4 == remove_entry, NA_character_, value_4)
  )

df <- df %>%
  select(-value_4)  # This excludes the value_4 column from the data frame


df %>%
  distinct(value_5)


date_pattern <- "^\\d{1,2}/\\d{1,2}/\\d{4}$"  # Regex for dates like 08/04/2023
time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16


library(tidyverse)

df <- df %>%
  mutate(
    # Concatenate dates to Date column if they match the date pattern
    Date = if_else(str_detect(value_5, date_pattern), paste(Date, value_5, sep=" "), Date),
    # Concatenate times to Time column if they match the time pattern
    Time = if_else(str_detect(value_5, time_pattern), paste(Time, value_5, sep=" "), Time),
    # Replace value_5 entries with NA if they are moved to Date or Time
    value_5 = if_else(str_detect(value_5, date_pattern) | str_detect(value_5, time_pattern), NA_character_, value_5)
  )


concat_values <- c("Real", "Sexual", "Property", "By", "Under", "Vehicle", "Life", 
                   "gm(Ordinance/Civil", "Damage/Unattended", "Residence", "Alcohol", 
                   "or", "Damage", "Warrant", "Of", "Weapon", "Minor", "City", 
                   "State", "Or", "Equipment", "Battery", "CommunicationsOpen", 
                   "Govern.", "Adult", "Notice", "Storage")

library(tidyverse)

df <- df %>%
  mutate(
    value_1 = if_else(value_5 %in% concat_values, paste(value_1, value_5, sep=" "), value_1),
    value_5 = if_else(value_5 %in% concat_values, NA_character_, value_5)
  )

library(tidyverse)

df <- df %>%
  mutate(
    # Extract additional text following the 12-digit string
    extra_text = str_extract(value_1, "(?<=^\\d{12}).*$"),
    
    # Concatenate extracted text to value_2, if any
    value_2 = if_else(!is.na(extra_text) & extra_text != "", paste(value_2, extra_text, sep=" "), value_2),
    
    # Remove the extra text from value_1
    value_1 = str_replace(value_1, "(?<=^\\d{12}).*$", "")
  )

move_values <- c("Suspended", "Closed", "Open", "Referred", "to")


library(tidyverse)

df <- df %>%
  mutate(
    Disposition = if_else(value_5 %in% move_values, paste(Disposition, value_5, sep=" "), Disposition),
    value_5 = if_else(value_5 %in% move_values, NA_character_, value_5)
  )

df %>%
  distinct(value_5) %>%
  print(n=100)

df %>%
  filter(value_5 == "Exception01/19/2024")


library(tidyverse)

df <- df %>%
  mutate(
    # Extract the text component
    text_part = str_extract(value_5, "^[A-Za-z]+"),
    # Extract the date component
    date_part = str_extract(value_5, "\\d{1,2}/\\d{1,2}/\\d{4}$"),
    
    # Move the text part to Disposition, concatenating it with existing content
    Disposition = if_else(!is.na(text_part), paste(Disposition, text_part, sep=" "), Disposition),
    
    # Concatenate the date part to the Date column
    Date = if_else(!is.na(date_part), paste(Date, date_part, sep=" "), Date),
    
    # Clear the original value_5 field
    value_5 = NA_character_
  )

df <- df %>%
  select(-value_5)  # This excludes the value_5 column from the data frame


df %>%
  distinct(value_6)


df%>%
  distinct(Disposition) %>%
  print(n=50)

library(tidyverse)

df <- df %>%
  mutate(
    # Extract "Faraday" and assign it to the new column 'Location'
    Location = if_else(str_detect(Disposition, "Open Faraday"), "Faraday", NA_character_),
    
    # Remove "Faraday" from the 'Disposition' column
    Disposition = str_replace(Disposition, "Open Faraday", "Open")
  )

# Define the specific values to be moved
keywords <- c("Suspended", "Open", "Closed", "Referred", "to", "Closed By Arrest", "By", "Exception", "Cleared", "Unfounded")

# Create a regex pattern that matches these keywords even within longer strings
# Regex needs to account for spaces or other characters before/after the keywords
pattern <- paste0("\\b(", paste(keywords, collapse="|"), ")\\b")

library(tidyverse)

df <- df %>%
  mutate(
    # Concatenate matched keywords to Disposition
    Disposition = if_else(str_detect(value_6, pattern), 
                          paste(Disposition, str_extract(value_6, pattern), sep=" "), 
                          Disposition),
    
    # Replace the keywords in value_6 with NA where they are moved
    value_6 = str_replace_all(value_6, pattern, ""),
    
    # Clean up value_6 by trimming excess whitespace after removal
    value_6 = str_trim(value_6)
  ) %>%
  # Additional step to set value_6 to NA where it becomes empty after cleaning
  mutate(
    value_6 = if_else(value_6 == "", NA_character_, value_6)
  )


df %>%
  distinct(value_6) %>%
  print(n = 50)

# Regex pattern for standard date and time formats
date_pattern <- "\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b"  # Matches dates in mm/dd/yyyy format
time_pattern <- "\\b\\d{1,2}:\\d{2}\\b"           # Matches times in hh:mm format

library(tidyverse)

df <- df %>%
  mutate(
    # Concatenate detected dates to the Date column
    Date = if_else(str_detect(value_6, date_pattern), paste(Date, str_extract(value_6, date_pattern), sep=" "), Date),
    
    # Concatenate detected times to the Time column
    Time = if_else(str_detect(value_6, time_pattern), paste(Time, str_extract(value_6, time_pattern), sep=" "), Time),
    
    # Remove detected dates and times from value_6
    value_6 = str_replace_all(value_6, date_pattern, ""),
    value_6 = str_replace_all(value_6, time_pattern, ""),
    
    # Clean up value_6 by trimming excess whitespace
    value_6 = str_trim(value_6)
  ) %>%
  # Convert value_6 to NA if it becomes empty after extraction
  mutate(
    value_6 = if_else(value_6 == "", NA_character_, value_6)
  )

df %>%
  filter(value_6 == "VehicleSuspended")

library(tidyverse)

dftest <- df


library(stringr)

dftest <- dftest %>%
  mutate(
    value_2 = if_else(str_detect(value_6, regex("property", ignore_case = TRUE)), 
                      paste(value_2, "Property", sep=" "), value_2),
    value_6 = if_else(str_detect(value_6, regex("property", ignore_case = TRUE)), 
                      NA_character_, value_6)
  )

df %>%
  distinct(value_6) %>%
  print(n=50)



values_to_move <- c("Property", "Mislaid", "Systems", "Viol)", "100", "Life", "Protection", "Veh.", "Vehicle", "Sup.")

library(dplyr)

df <- df %>%
  mutate(
    # Concatenate selected values from value_6 to value_2
    value_2 = if_else(value_6 %in% values_to_move, paste(value_2, value_6, sep=" "), value_2),
    
    # Set value_6 to NA where values were moved
    value_6 = if_else(value_6 %in% values_to_move, NA_character_, value_6)
  )


library(tidyverse)

# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"


df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_6, date_pattern), 
                   paste(Date, str_extract(value_6, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_6
    value_6 = str_remove(value_6, date_pattern)
  )


library(tidyverse)

df <- df %>%
  mutate(
    # Append "Suspended" to Disposition if it exists in value_6
    Disposition = if_else(str_detect(value_6, "Suspended$"), 
                          paste(Disposition, "Suspended", sep=" "), 
                          Disposition),
    
    # Remove "Suspended" from value_6
    value_6 = str_remove(value_6, "Suspended$")
  )

# Concatenated words to look for
suffixes <- c("Open", "Unfounded", "Referred")
suffix_pattern <- paste0(suffixes, "$", collapse="|")  # Creates "Open$|Unfounded$|Referred$"


library(tidyverse)

df <- df %>%
  mutate(
    # Extract the suffix and append it to Disposition if present
    Disposition = if_else(str_detect(value_6, suffix_pattern), 
                          paste(Disposition, str_extract(value_6, suffix_pattern), sep=" "), 
                          Disposition),
    
    # Remove the suffix from value_6
    value_6 = str_remove(value_6, suffix_pattern)
  )


df %>%
  filter(value_6 == "To")

df %>%
  distinct(value_6)


library(tidyverse)

# Assuming df is your data frame
df <- df %>%
  mutate(
    # Concatenate "Vehicle" or "Accident" to value_2 if found in value_6
    value_2 = if_else(value_6 %in% c("Exception", "Arrest"), 
                      paste(value_2, value_6, sep=" "), 
                      value_2),
    
    # Set value_6 to NA where "Vehicle" or "Accident" was moved
    value_6 = if_else(value_6 %in% c("Exception", "Arrest"), 
                      NA_character_, 
                      value_6)
  )


df <- df %>%
  mutate(
    Disposition = if_else(value_6 %in% c("Exception", "Arrest"),
                          paste(Disposition, value_6, sep = " "),
                          Disposition),
    value_6 = if_else(value_6 %in% c("Exception", "Arrest"),
                      NA_character_,
                      value_6)
  )

df <- df %>%
  mutate(
    Location = if_else(value_6 %in% c("Hall", "Lot", "Patterson"),
                          paste(Location, value_6, sep = " "),
                          Location),
    value_6 = if_else(value_6 %in% c("Hall", "Lot", "Patterson"),
                      NA_character_,
                      value_6)
  )


df <- df %>%
  mutate(
    Time = if_else(value_6 %in% c("AM", "PM"),
                       paste(Time, value_6, sep = " "),
                       Time),
    value_6 = if_else(value_6 %in% c("AM", "PM"),
                      NA_character_,
                      value_6)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_6 %in% c("Imgs", "of"),
                   paste(value_2, value_6, sep = " "),
                   value_2),
    value_6 = if_else(value_6 %in% c("Imgs", "of"),
                      NA_character_,
                      value_6)
  )

df <- df %>%
  mutate(
    Disposition = if_else(value_6 %in% c("To", "Adult"),
                      paste(Disposition, value_6, sep = " "),
                      Disposition),
    value_6 = if_else(value_6 %in% c("To", "Adult"),
                      NA_character_,
                      value_6)
  )


df <- df %>% 
  mutate(
    value_6 = if_else(value_6 == "PM 002023014932", NA_character_, value_6)
  )


df %>%
  distinct(value_7) %>%
  print(n = 100)


# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_7, date_pattern), 
                   paste(Date, str_extract(value_7, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_7
    value_7 = str_remove(value_7, date_pattern)
  )


Time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16

df <- df %>%
  mutate(
    # Extract the Time and concatenate it with the existing Time column
    Time = if_else(str_detect(value_7, Time_pattern), 
                   paste(Time, str_extract(value_7, Time_pattern), sep=" "), 
                   Time),
    
    # Remove the Time part from value_7
    value_7 = str_remove(value_7, Time_pattern)
  )


residence_hall_names = c("Anderson", "Patterson", "Neptune", "Stevenson", "Peters", "Altgeld", "Faraday", "Grant", "Recreation", "DeKalb", "Northern", "Fraternity", "NIU", "Hall", "Lot",
                         "Normal", "Rd", "Center", "Complex", "View", "Cir", "Holmes", "Student", "Center", "Annie", "Glidden", "N", "Carroll",
                         "Ave", "Garden", "200", "26D", "Convocation", "Founders", "Central", "Monsanto", "Williston",
                         "West", "Gilbert", "Art", "Huskie", "Stadium", "Campus", "Davis", "North", "Stevenon", "University",
                         "Rec", "Barsema", "South", "Lincoln", "Sigma", "La", "Swen", "Parking", "IL", "House", "SWANS", "New", "Emergency", "Management",
                         "Wirtz", "Lucinda", "Deck", "And", "Planning", "Pay", "East", "Residence", "College", "Building",
                         "Park", "Drive", "Parson", "Locust", "County", "Dining", "Life", "Community", "Center", "DeKalbIL",
                         "Dusable", "Alpha", "Sorority", "Tourette", "Red", "Memorial", "Gabel", "Library", "Hillcrest", "Dekalb",
                         "Dinning", "Dr", "Department", "Courthouse", "Health", "Mu", "Montgomery", "Laundry", "Douglas",
                         "Complex", "Chicago", "Kishwaukee", "Lobby", "Wellness", "Newman", "Aurora", "Bethany", "Office", "Arends",
                         "Sammys")
disposition_keywords <- c("Suspended", "Open", "Closed", "Referred", "to", "Closed By Arrest", "By", "Exception", "Cleared", "Unfounded", "Arrest", "Other", "by", "To", "Agency", "Adult")
value_2_words <- c("Land", "Property", "Accident", "Minor", "Systems", "Safety", "System", "Weapons", "gm(Misdemeanor)", "Vehicle",
                   "Disorderly", "Conduct", "Disorderly Conduct", "Battery", "Aggravated", "Theft", "Assault", "Criminal", "Unlawful", "Stalking",
                   "Non-consensual", "Notify", "Burglary", "Domestic", "Tampering", "Robbery", "Trespass", "Tamper", "Sexual",
                   "Possession", "Telephone", "Threat", "Mob", "Action", "Criminal", "Damage", "To", "Property", "Theft",
                   "Over", "$500", "Illegal", "Consumption", "Alcohol", "WAggravated", "WBattery", "Possess", "Cannabis", "All", "Disorderly", "Conduct",
                   "NFalse", "Police", "Report", "Harassment", "Through", "Electronic", "NUnlawful", "Use", "Of", "Weapon", "NBattery", "Traffic", "Armed", "Robbery",
                   "Criminal","Damage", "Institutional", "Vandalism", "SFraud", "In", "State", "Warrant", "Deceptive", "Practices",
                   "False", "Fire", "Intimidation", "Under")
time_words <- c("AM", "PM", "am", "pm")

df <- df %>%
  mutate(
    Location = if_else(value_7 %in% residence_hall_names,
                          paste(Location, value_7, sep = " "),
                          Location),
    value_7 = if_else(value_7 %in% residence_hall_names,
                      NA_character_,
                      value_7)
  )



df <- df %>%
  mutate(
    Disposition = if_else(value_7 %in% disposition_keywords,
                       paste(Disposition, value_7, sep = " "),
                       Disposition),
    value_7 = if_else(value_7 %in% disposition_keywords,
                      NA_character_,
                      value_7)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_7 %in% value_2_words,
                          paste(value_2, value_7, sep = " "),
                          value_2),
    value_7 = if_else(value_7 %in% value_2_words,
                      NA_character_,
                      value_7)
  )


df %>% 
  filter(value_7 == "gm(Misdemeanor)")


library(tidyverse)

df <- df %>%
  mutate(
    # Append "Open" to Disposition if it exists in value_7
    Disposition = if_else(str_detect(value_7, "Open$"), 
                          paste(Disposition, "Open", sep=" "), 
                          Disposition),
    
    # Remove "Open" from value_7
    value_7 = str_remove(value_7, "Open$")
  )


df <- df %>%
  mutate(
    Time = if_else(value_7 %in% time_words,
                       paste(Time, value_7, sep = " "),
                       Time),
    value_7 = if_else(value_7 %in% time_words,
                      NA_character_,
                      value_7)
  )

df <- df %>%
  select(-value_6)
  

df <- df %>%
  mutate(
    value_2 = if_else(value_6 %in% value_2_words,
                      paste(value_2, value_6, sep = " "),
                      value_2),
    value_6 = if_else(value_6 %in% value_2_words,
                      NA_character_,
                      value_6)
  )
df %>%
  distinct(value_6)



# value 8 


df <- df %>%
  mutate(
    Location = if_else(value_8 %in% residence_hall_names,
                       paste(Location, value_8, sep = " "),
                       Location),
    value_8 = if_else(value_8 %in% residence_hall_names,
                      NA_character_,
                      value_8)
  )



df <- df %>%
  mutate(
    Disposition = if_else(value_8 %in% disposition_keywords,
                          paste(Disposition, value_8, sep = " "),
                          Disposition),
    value_8 = if_else(value_8 %in% disposition_keywords,
                      NA_character_,
                      value_8)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_8 %in% value_2_words,
                      paste(value_2, value_8, sep = " "),
                      value_2),
    value_8 = if_else(value_8 %in% value_2_words,
                      NA_character_,
                      value_8)
  )



Time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16

df <- df %>%
  mutate(
    # Extract the Time and concatenate it with the existing Time column
    Time = if_else(str_detect(value_8, Time_pattern), 
                   paste(Time, str_extract(value_8, Time_pattern), sep=" "), 
                   Time),
    
    # Remove the Time part from value_8
    value_8 = str_remove(value_8, Time_pattern)
  )

# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_8, date_pattern), 
                   paste(Date, str_extract(value_8, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_8
    value_8 = str_remove(value_8, date_pattern)
  )

df <- df %>%
  mutate(
    # Append "Open" to Disposition if it exists in value_8
    Disposition = if_else(str_detect(value_8, "Closed$"), 
                          paste(Disposition, "Closed", sep=" "), 
                          Disposition),
    
    # Remove "Open" from value_8
    value_8 = str_remove(value_8, "Closed$")
  )


df %>%
  distinct(value_8) %>%
  print(n =50)


df %>%
  filter(value_8 == "Adult")



# value 9

df <- df %>%
  mutate(
    Location = if_else(value_9 %in% residence_hall_names,
                       paste(Location, value_9, sep = " "),
                       Location),
    value_9 = if_else(value_9 %in% residence_hall_names,
                      NA_character_,
                      value_9)
  )



df <- df %>%
  mutate(
    Disposition = if_else(value_9 %in% disposition_keywords,
                          paste(Disposition, value_9, sep = " "),
                          Disposition),
    value_9 = if_else(value_9 %in% disposition_keywords,
                      NA_character_,
                      value_9)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_9 %in% value_2_words,
                      paste(value_2, value_9, sep = " "),
                      value_2),
    value_9 = if_else(value_9 %in% value_2_words,
                      NA_character_,
                      value_9)
  )



Time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16

df <- df %>%
  mutate(
    # Extract the Time and concatenate it with the existing Time column
    Time = if_else(str_detect(value_9, Time_pattern), 
                   paste(Time, str_extract(value_9, Time_pattern), sep=" "), 
                   Time),
    
    # Remove the Time part from value_9
    value_9 = str_remove(value_9, Time_pattern)
  )

# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_9, date_pattern), 
                   paste(Date, str_extract(value_9, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_9
    value_9 = str_remove(value_9, date_pattern)
  )

df <- df %>%
  mutate(
    # Append "Open" to Disposition if it exists in value_9
    Disposition = if_else(str_detect(value_9, "Closed$"), 
                          paste(Disposition, "Closed", sep=" "), 
                          Disposition),
    
    # Remove "Open" from value_9
    value_9 = str_remove(value_9, "Closed$")
  )


df %>%
  distinct(value_9) %>%
  print(n =50)

df %>%
  filter(value_9 == "Memorial")

#value 10

df <- df %>%
  mutate(
    Location = if_else(value_10 %in% residence_hall_names,
                       paste(Location, value_10, sep = " "),
                       Location),
    value_10 = if_else(value_10 %in% residence_hall_names,
                      NA_character_,
                      value_10)
  )



df <- df %>%
  mutate(
    Disposition = if_else(value_10 %in% disposition_keywords,
                          paste(Disposition, value_10, sep = " "),
                          Disposition),
    value_10 = if_else(value_10 %in% disposition_keywords,
                      NA_character_,
                      value_10)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_10 %in% value_2_words,
                      paste(value_2, value_10, sep = " "),
                      value_2),
    value_10 = if_else(value_10 %in% value_2_words,
                      NA_character_,
                      value_10)
  )



Time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16

df <- df %>%
  mutate(
    # Extract the Time and concatenate it with the existing Time column
    Time = if_else(str_detect(value_10, Time_pattern), 
                   paste(Time, str_extract(value_10, Time_pattern), sep=" "), 
                   Time),
    
    # Remove the Time part from value_10
    value_10 = str_remove(value_10, Time_pattern)
  )

# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_10, date_pattern), 
                   paste(Date, str_extract(value_10, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_10
    value_10 = str_remove(value_10, date_pattern)
  )

df <- df %>%
  mutate(
    # Append "Open" to Disposition if it exists in value_10
    Disposition = if_else(str_detect(value_10, "Closed$"), 
                          paste(Disposition, "Closed", sep=" "), 
                          Disposition),
    
    # Remove "Open" from value_10
    value_10 = str_remove(value_10, "Closed$")
  )


df %>%
  distinct(value_10) %>%
  print(n =100)

df %>%
  filter(value_10 == "Laundry")


df %>%
  filter(is.na(Location))


#value 11

df <- df %>%
  mutate(
    Location = if_else(value_11 %in% residence_hall_names,
                       paste(Location, value_11, sep = " "),
                       Location),
    value_11 = if_else(value_11 %in% residence_hall_names,
                       NA_character_,
                       value_11)
  )



df <- df %>%
  mutate(
    Disposition = if_else(value_11 %in% disposition_keywords,
                          paste(Disposition, value_11, sep = " "),
                          Disposition),
    value_11 = if_else(value_11 %in% disposition_keywords,
                       NA_character_,
                       value_11)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_11 %in% value_2_words,
                      paste(value_2, value_11, sep = " "),
                      value_2),
    value_11 = if_else(value_11 %in% value_2_words,
                       NA_character_,
                       value_11)
  )



Time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16

df <- df %>%
  mutate(
    # Extract the Time and concatenate it with the existing Time column
    Time = if_else(str_detect(value_11, Time_pattern), 
                   paste(Time, str_extract(value_11, Time_pattern), sep=" "), 
                   Time),
    
    # Remove the Time part from value_11
    value_11 = str_remove(value_11, Time_pattern)
  )

# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_11, date_pattern), 
                   paste(Date, str_extract(value_11, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_11
    value_11 = str_remove(value_11, date_pattern)
  )

df <- df %>%
  mutate(
    # Append "Open" to Disposition if it exists in value_11
    Disposition = if_else(str_detect(value_11, "Closed$"), 
                          paste(Disposition, "Closed", sep=" "), 
                          Disposition),
    
    # Remove "Open" from value_11
    value_11 = str_remove(value_11, "Closed$")
  )


df %>%
  distinct(value_11) %>%
  print(n =110)


#value_12

df <- df %>%
  mutate(
    Location = if_else(value_12 %in% residence_hall_names,
                       paste(Location, value_12, sep = " "),
                       Location),
    value_12 = if_else(value_12 %in% residence_hall_names,
                       NA_character_,
                       value_12)
  )



df <- df %>%
  mutate(
    Disposition = if_else(value_12 %in% disposition_keywords,
                          paste(Disposition, value_12, sep = " "),
                          Disposition),
    value_12 = if_else(value_12 %in% disposition_keywords,
                       NA_character_,
                       value_12)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_12 %in% value_2_words,
                      paste(value_2, value_12, sep = " "),
                      value_2),
    value_12 = if_else(value_12 %in% value_2_words,
                       NA_character_,
                       value_12)
  )



Time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16

df <- df %>%
  mutate(
    # Extract the Time and concatenate it with the existing Time column
    Time = if_else(str_detect(value_12, Time_pattern), 
                   paste(Time, str_extract(value_12, Time_pattern), sep=" "), 
                   Time),
    
    # Remove the Time part from value_12
    value_12 = str_remove(value_12, Time_pattern)
  )

# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_12, date_pattern), 
                   paste(Date, str_extract(value_12, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_12
    value_12 = str_remove(value_12, date_pattern)
  )

df <- df %>%
  mutate(
    # Append "Open" to Disposition if it exists in value_12
    Disposition = if_else(str_detect(value_12, "Closed$"), 
                          paste(Disposition, "Closed", sep=" "), 
                          Disposition),
    
    # Remove "Open" from value_12
    value_12 = str_remove(value_12, "Closed$")
  )


df %>%
  distinct(value_12) %>%
  print(n =100)

df %>%
  filter(value_12 == "Telephone")


df %>%
  filter(is.na(Location))



process_row <- function(row) {
  # Check if value_2 is blank or empty
  if (row$value_2 %in% c("", NA_character_)) {
    # Iterate over the columns from value_8 to value_18
    for (col_name in paste0("value_", 8:18)) {
      # Check if the column's value is in value_2_words
      if (row[[col_name]] %in% value_2_words) {
        # Concatenate the found value to value_2
        row$value_2 <- paste(na.omit(c(row$value_2, row[[col_name]])), collapse=" ")
        # Remove the value from the original field
        row[[col_name]] <- NA
      }
    }
  }
  return(row)
}

df <- df %>%
  rowwise() %>%
  mutate(new_row = list(process_row(cur_data()))) %>%
  ungroup() %>%
  # To unpack the list into normal columns
  mutate(across(everything(), ~ if (is.list(.)) .[[1]] else .)) %>%
  select(-new_row)  # Optionally remove temporary 'new_row' if added


#value_13

df <- df %>%
  mutate(
    Location = if_else(value_13 %in% residence_hall_names,
                       paste(Location, value_13, sep = " "),
                       Location),
    value_13 = if_else(value_13 %in% residence_hall_names,
                       NA_character_,
                       value_13)
  )



df <- df %>%
  mutate(
    Disposition = if_else(value_13 %in% disposition_keywords,
                          paste(Disposition, value_13, sep = " "),
                          Disposition),
    value_13 = if_else(value_13 %in% disposition_keywords,
                       NA_character_,
                       value_13)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_13 %in% value_2_words,
                      paste(value_2, value_13, sep = " "),
                      value_2),
    value_13 = if_else(value_13 %in% value_2_words,
                       NA_character_,
                       value_13)
  )



Time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16

df <- df %>%
  mutate(
    # Extract the Time and concatenate it with the existing Time column
    Time = if_else(str_detect(value_13, Time_pattern), 
                   paste(Time, str_extract(value_13, Time_pattern), sep=" "), 
                   Time),
    
    # Remove the Time part from value_13
    value_13 = str_remove(value_13, Time_pattern)
  )

# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_13, date_pattern), 
                   paste(Date, str_extract(value_13, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_13
    value_13 = str_remove(value_13, date_pattern)
  )

df <- df %>%
  mutate(
    # Append "Open" to Disposition if it exists in value_13
    Disposition = if_else(str_detect(value_13, "Closed$"), 
                          paste(Disposition, "Closed", sep=" "), 
                          Disposition),
    
    # Remove "Open" from value_13
    value_13 = str_remove(value_13, "Closed$")
  )


df %>%
  distinct(value_13) %>%
  print(n =100)


#

df <- df %>%
  mutate(
    Location = if_else( %in% residence_hall_names,
                       paste(Location, value_14, sep = " "),
                       Location),
    value_14 = if_else(value_14 %in% residence_hall_names,
                       NA_character_,
                       value_14)
  )



df <- df %>%
  mutate(
    Disposition = if_else(value_14 %in% disposition_keywords,
                          paste(Disposition, value_14, sep = " "),
                          Disposition),
    value_14 = if_else(value_14 %in% disposition_keywords,
                       NA_character_,
                       value_14)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_14 %in% value_2_words,
                      paste(value_2, value_14, sep = " "),
                      value_2),
    value_14 = if_else(value_14 %in% value_2_words,
                       NA_character_,
                       value_14)
  )



Time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16

df <- df %>%
  mutate(
    # Extract the Time and concatenate it with the existing Time column
    Time = if_else(str_detect(value_14, Time_pattern), 
                   paste(Time, str_extract(value_14, Time_pattern), sep=" "), 
                   Time),
    
    # Remove the Time part from value_14
    value_14 = str_remove(value_14, Time_pattern)
  )

# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_14, date_pattern), 
                   paste(Date, str_extract(value_14, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_14
    value_14 = str_remove(value_14, date_pattern)
  )

df <- df %>%
  mutate(
    # Append "Open" to Disposition if it exists in value_14
    Disposition = if_else(str_detect(value_14, "Closed$"), 
                          paste(Disposition, "Closed", sep=" "), 
                          Disposition),
    
    # Remove "Open" from value_14
    value_14 = str_remove(value_14, "Closed$")
  )


df %>%
  distinct(value_14) %>%
  print(n =100)


#value_15

df <- df %>%
  mutate(
    Location = if_else(value_15 %in% residence_hall_names,
                       paste(Location, value_15, sep = " "),
                       Location),
    value_15 = if_else(value_15 %in% residence_hall_names,
                       NA_character_,
                       value_15)
  )



df <- df %>%
  mutate(
    Disposition = if_else(value_15 %in% disposition_keywords,
                          paste(Disposition, value_15, sep = " "),
                          Disposition),
    value_15 = if_else(value_15 %in% disposition_keywords,
                       NA_character_,
                       value_15)
  )


df <- df %>%
  mutate(
    value_2 = if_else(value_15 %in% value_2_words,
                      paste(value_2, value_15, sep = " "),
                      value_2),
    value_15 = if_else(value_15 %in% value_2_words,
                       NA_character_,
                       value_15)
  )



Time_pattern <- "^\\d{1,2}:\\d{2}$"           # Regex for times like 11:16

df <- df %>%
  mutate(
    # Extract the Time and concatenate it with the existing Time column
    Time = if_else(str_detect(value_15, Time_pattern), 
                   paste(Time, str_extract(value_15, Time_pattern), sep=" "), 
                   Time),
    
    # Remove the Time part from value_15
    value_15 = str_remove(value_15, Time_pattern)
  )

# Regex pattern to match the date in the format mm/dd/yyyy
date_pattern <- "\\d{1,2}/\\d{1,2}/\\d{4}$"

df <- df %>%
  mutate(
    # Extract the date and concatenate it with the existing Date column
    Date = if_else(str_detect(value_15, date_pattern), 
                   paste(Date, str_extract(value_15, date_pattern), sep=" "), 
                   Date),
    
    # Remove the date part from value_15
    value_15 = str_remove(value_15, date_pattern)
  )

df <- df %>%
  mutate(
    # Append "Open" to Disposition if it exists in value_15
    Disposition = if_else(str_detect(value_15, "Closed$"), 
                          paste(Disposition, "Closed", sep=" "), 
                          Disposition),
    
    # Remove "Open" from value_15
    value_15 = str_remove(value_15, "Closed$")
  )


df %>%
  distinct(value_15) %>%
  print(n =100)


library(dplyr)


# Look for values in column 2 that are NA and look to the consecutive rows to see if any values can fill 
library(dplyr)

test <- data %>%
  rowwise() %>%  # Apply operations row by row
  mutate(value_2 = if (is.na(value_2) || value_2 == "") {
    # Initialize the result as the current value_2
    result <- value_2
    # Iterate over columns value_8 to value_18
    for (col_name in paste0("value_", 5:19)) {
      # Check for the presence of any keywords
      if (get(col_name) %in% value_2_words) {
        # Concatenate the found word to value_2 if not empty
        result <- if (nzchar(result)) paste(result, get(col_name), sep=" ") else get(col_name)
        # Remove the word from the original column
        assign(col_name, NA, envir = .env)
        # Removed break statement to continue searching and concatenating all matches
      }
    }
    result  # Return the updated value_2
  } else {
    value_2  # Return the current value_2 if no changes needed
  }) %>%
  ungroup()  # Ensure the data frame is no longer grouped rowwise


test_df %>%
  filter(is.na(Location$Location))


clean_value <- function(value) {
  # Vectorized condition to check if the value without the first character is in value_2_words
  ifelse(substr(value, 2, nchar(value)) %in% value_2_words,
         substr(value, 2, nchar(value)),  # Return value without the first character
         value)  # Return the original value if no match
}

library(dplyr)
library(stringr)

df <- df %>%
  mutate(across(.cols = value_8:value_18, .fns = ~ clean_value(.)))


test_df <- df


process_row <- function(row) {
  # Check if Location is blank or NA
  if (is.na(row$Location) || row$Location == "") {
    # Iterate through columns value_8 to value_18
    for (i in 8:18) {
      col_name <- paste0("value_", i)
      # Check if the value is in residence_hall_names
      if (row[[col_name]] %in% residence_hall_names) {
        # Update Location and clear the original field
        row$Location <- paste(na.omit(c(row$Location, row[[col_name]])), collapse=" ")
        row[[col_name]] <- NA
      }
    }
  }
  return(row)
}

test_df <- test_df %>%
  rowwise() %>%
  mutate(Location = process_row(cur_data())) %>%
  ungroup()



new_df <- df %>%
  select(row, value_1, value_2, Date, Time, Disposition, Location)


# Assuming your data frame is named 'new_df' and the column names are 'value_2', 'column', and 'Location'
new_df$value_2 <- gsub("^NA\\s*", "", new_df$value_2)
new_df$column <- gsub("^NA\\s*", "", new_df$column)
new_df$Location <- gsub("^NA\\s*", "", new_df$Location)


new_df %>%
  group_by(crimes_normalized) %>%
  transmute(count = n()) %>%
  arrange(desc(count)) %>%
  distinct() %>%
  print(n = 250)


new_df %>%
  select(value_2) %>%
  distinct() %>%
  print(n = 300) 


library(dplyr)
library(stringr)

# Sample dataframe
data_test <- data.frame(
  crimes = c("NBattery", "WBattery", "Battery", "Theft Over $500", "Theft $500 and Under", "Criminal Trespass To Real Property")
)

# Function to normalize crime names
normalize_crimes <- function(crime) {
  crime <- tolower(crime)  # Convert to lower case for uniformity
  
  # Remove numerical values and special characters
  crime <- gsub("[0-9]", "", crime)
  crime <- gsub("[>$,]", "", crime)
  
  # Trim extra spaces
  crime <- str_trim(crime)
  
  # Normalize variations of the same term
  crime <- ifelse(grepl("battery", crime), "battery", crime)
  crime <- ifelse(grepl("theft", crime), "theft", crime)
  crime <- ifelse(grepl("criminal trespass", crime), "criminal trespass", crime)
  
  crime
}

# Apply normalization
new_df$crimes_normalized <- sapply(new_df$value_2, normalize_crimes)

# View the transformed data
print(data)


show_df <- new_df %>%
  select(value_1, value_2, Location, crimes_normalized)

write.csv(new_df, "new_df.csv")
write_csv(df, "updated_df.csv")

library(stringr)

# Function to clean column values based on partial match
clean_column_values <- function(value, words) {
  for (word in words) {
    if (str_detect(value, word)) {
      return(word)  # Replace the value with the word if a match is found
    }
  }
  return(value)  # Returns the original value if no match is found
}

# Define the words you're looking for


# Apply the function to each element in the dataframe
data[] <- lapply(data, function(column) sapply(column, clean_column_values, value_2_words))


library(stringr)

# Function to clean values based on finding the keyword and removing surrounding characters
clean_column_values_regex <- function(value, words) {
  pattern <- paste0(".*(", paste(words, collapse = "|"), ").*")
  str_replace(value, pattern, "\\1")
}


# Define the words you're looking for
variable_2_words <- c("Theft")  # Add other words as necessary

# Apply the function to each element in the dataframe
data[] <- lapply(data, function(column) if(is.character(column)) sapply(column, clean_column_values_regex, value_2_words) else column)




# Assuming your data frame is named 'new_df' and the column names are 'value_2', 'column', and 'Location'
test$value_2 <- gsub("^NA\\s*", "", new_df$value_2)
test$Location <- gsub("^NA\\s*", "", new_df$Location)

