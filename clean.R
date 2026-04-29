# ################################################################# 
# Cleaning
# ################################################################# 
# Elyse Gross 
# Mar 30, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

# Clear 
rm(list = ls())

# Set working directory
setwd("~/Desktop/EKGThesis/Data")

# Install and load packages
required_packages <- c("dplyr")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Import data
raw_data <- read.csv("raw_data_numeric_Mar4.csv")

# ################################################################# 
# 2. Clean Data

# Remove filler rows (repeat titles, not responses)
raw_data <- raw_data[-c(1,2), ]

# Remove unneeded columns
raw_data <- raw_data %>% select(-c(StartDate, EndDate, Status, IPAddress, Progress, 
                                   Duration..in.seconds., Finished, RecordedDate, 
                                   RecipientLastName, RecipientFirstName, 
                                   RecipientEmail, ExternalReference, LocationLatitude, 
                                   LocationLongitude, DistributionChannel, UserLanguage))

# Dropping any rows where they answer no to being a nutritionist 
raw_data <- raw_data[raw_data$Nutritionist.y.n != "2", ]
# Or if they did not answer anything
raw_data <- raw_data[
  !( (is.na(raw_data$Nutritionist.y.n) | raw_data$Nutritionist.y.n == "") &
       (is.na(raw_data$job.function) | raw_data$job.function == "") )
  , ]
# Or if they quit after the screening
raw_data <- raw_data[
  !(raw_data$Nutritionist.y.n == "1" &
      (is.na(raw_data$job.function) | raw_data$job.function == "")
  ), ]


# Make the specific replacements in the "number of cows" column that aren't numeric
raw_data$number.of.cows <- gsub("^15k$", "15000", raw_data$number.of.cows, ignore.case = TRUE)
raw_data$number.of.cows <- gsub("^75K$", "75000", raw_data$number.of.cows, ignore.case = TRUE)
raw_data$number.of.cows <- gsub("^Last estimate was indirect.*$", "50000", raw_data$number.of.cows, ignore.case = TRUE)

# Replace Qualtrics response IDs with anonymous numeric IDs
raw_data$ResponseId <- seq_len(nrow(raw_data))

# Save this cleaned data
write.csv(raw_data, "clean_data.csv", row.names = FALSE)