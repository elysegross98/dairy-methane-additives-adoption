# ################################################################# 
# Summary Statistics
# ################################################################# 
# Elyse Gross 
# April 1, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & data

rm(list = ls()) 

setwd("~/Desktop/EKGThesis")

# Load libraries
required_packages <- c("multcompView", "dplyr", "tidyr", "ggplot2", "kableExtra", "rstatix", "forcats")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Import Data
clean_data <- read.csv("Data/clean_data.csv")

# Keep only the columns for perceptions, etc. & demographics
clean_data <- clean_data %>%
  select(ResponseId, Nutritionist.y.n, job.function, decision_1, decision_2, decision_3, job.function, X3NOP.familiarity, X3NOP.effective, X3NOP.sell, X3NOP.use, seaweed.familiarity, seaweed.effective, seaweed.sell, seaweed.use, EO.familiarity, EO.effective, EO.sell, EO.use, tannins.familiarity, tannins.effective, tannins.sell, tannins.use, age, gender, education, region, number.of.cows, attitude.climate, attitude.methane, attitude.climate.mm)

# ################################################################# 
# 2. NUMBER OF COWS ##

# Correcting number of cows to be able to count & summing
clean_data$number.of.cows <- as.numeric(
  gsub(",", "", clean_data$number.of.cows)
)

total_cows <- sum(clean_data$number.of.cows, na.rm=TRUE)

# Num cows row 
cows_row <- data.frame(
  Variable = "Number of cows",
  Count = total_cows,
  Percent = NA
)

total_cows

# By region
cows_by_region <- clean_data %>%
  mutate(Region = case_when(
    region == 1 ~ "Northeast",
    region == 3 ~ "Midwest",
    region == 5 ~ "South",
    region == 8 ~ "West",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Region)) %>%
  group_by(Region) %>%
  summarise(Count = sum(number.of.cows, na.rm = TRUE), .groups = "drop") %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  rename(Variable = Region)

# Add a total row
cows_region_table <- bind_rows(
  cows_by_region,
  data.frame(Variable = "Total", Count = sum(cows_by_region$Count), Percent = 100)
)

# ################################################################# 
# 3. AGE ##
# Frequencies:
clean_data$age <- as.numeric(clean_data$age)
class(clean_data$age)
mean(clean_data$age, na.rm=TRUE)

# Average Age (estimate)
age_counts <- c(5, 15, 11, 9, 14)   
age_midpoints <- c(23.5, 34.5, 44.5, 54.5, 65) # Midpoints of age bins (adjust if needed)
avg_age <- weighted.mean(age_midpoints, age_counts, na.rm = TRUE) # Compute weighted mean age

# Age table
age_table <- clean_data %>%
  mutate(Age = case_when(
    age == 1 ~ "18–29 years old",
    age == 2 ~ "30–39 years old",
    age == 3 ~ "40–49 years old",
    age == 4 ~ "50–59 years",
    age == 5 ~ "60 years or older",
    age == 6 ~ "Prefer not to say",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Age)) %>%
  count(Age) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(
    Variable = Age,
    Count = n
  )
# Add average age row
age_table <- age_table %>%
  add_row(
    Variable = "Approximate Average Age",
    Count = round(avg_age, 1),
    Percent = NA
  )

# ################################################################# 
# 4. GENDER ##
clean_data$gender <- as.numeric(clean_data$gender)
class(clean_data$gender)
mean(clean_data$gender, na.rm=TRUE)

# Gender table
gender_table <- clean_data %>%
  mutate(Gender = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    gender == 3 ~ "Prefer not to say",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Gender)) %>%
  count(Gender) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(
    Variable = Gender,
    Count = n
  )

# ################################################################# 
# 5. EDUCATION ##

# Education table
education_table <- clean_data %>%
  mutate(Education = case_when(
    education == 1 ~ "Less than a Bachelor's Degree",
    education == 2 ~ "Bachelor's Degree",
    education == 3 ~ "Graduate or Professional Degree",
    education == 4 ~ "Prefer not to say",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Education)) %>%
  count(Education) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(
    Variable = Education,
    Count = n
  )

# ################################################################# 
# 6. REGION ##

# Region table
region_table <- clean_data %>%
  mutate(Region = case_when(
    region == 1 ~ "Northeast",
    region == 3 ~ "Midwest",
    region == 5 ~ "South",
    region == 8 ~ "West",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Region)) %>%
  count(Region) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(
    Variable = Region,
    Count = n
  )

# ################################################################# 
# 7. JOB FUNCTION ##

## Job function for all
jobfunction_flags <- clean_data %>%
  select(ResponseId, job.function) %>%
  mutate(
    job.function = trimws(job.function),
    has_nutr = grepl("(^|,)1(,|$)", job.function),
    has_prob = grepl("(^|,)2(,|$)", job.function),
    has_risk = grepl("(^|,)3(,|$)", job.function),
    has_all  = grepl("(^|,)4(,|$)", job.function),
    has_cost = grepl("(^|,)5(,|$)", job.function),
    has_rev  = grepl("(^|,)6(,|$)", job.function)
  )

N_resp <- nrow(clean_data)

jobfunction_table <- jobfunction_flags %>%
  summarise(
    Nutrition = sum(has_nutr | has_all),
    Problem = sum(has_prob | has_all),
    Risk = sum(has_risk | has_all),
    Cost = sum(has_cost | has_all),
    Revenue = sum(has_rev | has_all)
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Count"
  ) %>%
  mutate(
    Variable = case_when(
      Variable == "Nutrition" ~ "Nutrition education and advice",
      Variable == "Problem"    ~ "Problem solving",
      Variable == "Risk"       ~ "Risk mitigation",
      Variable == "Cost"       ~ "Cost control",
      Variable == "Revenue"    ~ "Revenue generation",
      TRUE ~ Variable
    ),
    Percent = round(100 * Count / N_resp, 1)
  ) %>%
  select(Variable, Count, Percent)

# ################################################################# 
# 8. COMBINING TABLES

make_section <- function(df, header_name) {
  header <- data.frame(Variable = header_name, Count = NA, Percent = NA)
  blank  <- data.frame(Variable = "",           Count = NA, Percent = NA)
  bind_rows(header, df, blank)
}

Table_summary_freq_clean <- bind_rows(
  gender_table,
  age_table,
  region_table,
  cows_region_table,
  education_table,
  jobfunction_table
) %>%
  mutate(Count   = as.character(Count),
         Percent = as.character(Percent)) %>%
  mutate(across(everything(), ~replace_na(., ""))) %>%
  mutate(Percent = ifelse(Percent == "", Percent, paste0(Percent, "\\%")))

# ################################################################# 
# 9. SAVE TABLE OUTPUT

save_kable(
  kable(
    Table_summary_freq_clean,
    format    = "latex",
    escape    = FALSE,
    align     = "lcc",
    col.names = c("Variable", "Count", "Percent"),
    caption   = "Summary Statistics",
    booktabs  = TRUE
  ) %>%
    footnote(
      general        = "Notes: The number of respondents who completed the Demographic portion of the survey was 56. The number of respondents who completed the Job Function portion was 69. ``Prefer not to say'' responses were omitted and some respondents left questions blank, so sections with such responses do not sum to 56 or 69, respectively.",
      general_title  = "",
      threeparttable = TRUE,
      escape         = FALSE
    ) %>%
    kable_styling(
      full_width    = FALSE,
      position      = "center",
      latex_options = c("scale_down", "hold_position")
    ) %>%
    pack_rows("Gender",
              1,
              nrow(gender_table)) %>%
    pack_rows("Age",
              nrow(gender_table) + 1,
              nrow(gender_table) + nrow(age_table)) %>%
    pack_rows("Region",
              nrow(gender_table) + nrow(age_table) + 1,
              nrow(gender_table) + nrow(age_table) + nrow(region_table)) %>%
    pack_rows("Number of Cows by Region",
              nrow(gender_table) + nrow(age_table) + nrow(region_table) + 1,
              nrow(gender_table) + nrow(age_table) + nrow(region_table) + nrow(cows_region_table)) %>%
    pack_rows("Education",
              nrow(gender_table) + nrow(age_table) + nrow(region_table) + nrow(cows_region_table) + 1,
              nrow(gender_table) + nrow(age_table) + nrow(region_table) + nrow(cows_region_table) + nrow(education_table)) %>%
    pack_rows("Job Functions",
              nrow(gender_table) + nrow(age_table) + nrow(region_table) + nrow(cows_region_table) + nrow(education_table) + 1,
              nrow(gender_table) + nrow(age_table) + nrow(region_table) + nrow(cows_region_table) + nrow(education_table) + nrow(jobfunction_table)),
  file = "tables_figures/summary_table_frequencies.tex"
)