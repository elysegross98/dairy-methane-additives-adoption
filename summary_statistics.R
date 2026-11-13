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
    TRUE ~ "Region not reported"
  )) %>%
  group_by(Region) %>%
  summarise(Count = sum(number.of.cows, na.rm = TRUE), .groups = "drop") %>%
  filter(Count > 0) %>%
  mutate(Percent = round(100 * Count / sum(Count), 1)) %>%
  arrange(Region == "Region not reported", Region) %>%
  rename(Variable = Region)

stopifnot(sum(cows_by_region$Count) == total_cows)   

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

age_midpoints <- c(`1` = 23.5, `2` = 34.5, `3` = 44.5, `4` = 54.5, `5` = 65)
age_valid     <- clean_data$age[clean_data$age %in% 1:5] # Midpoints of age bins
avg_age       <- mean(age_midpoints[as.character(age_valid)]) # Compute weighted mean age

# Age table
age_table <- clean_data %>%
  mutate(Age = case_when(
    age == 1 ~ "18–29 years old",
    age == 2 ~ "30–39 years old",
    age == 3 ~ "40–49 years old",
    age == 4 ~ "50–59 years old",
    age == 5 ~ "60 years or older",
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
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Education)) %>%
  count(Education) %>%
  mutate(Percent = round(100 * n / sum(n), 1)) %>%
  rename(
    Variable = Education,
    Count = n
  ) %>%
  arrange(match(Variable, c("Less than a Bachelor's Degree",
                            "Bachelor's Degree",
                            "Graduate or Professional Degree")))

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

N_resp <- sum(!is.na(clean_data$job.function) & trimws(clean_data$job.function) != "")

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
  mutate(
    Count   = ifelse(is.na(Count),   "", prettyNum(Count, big.mark = ",")),
    Percent = ifelse(is.na(Percent), "", paste0(sprintf("%.1f", Percent), "\\%"))
  )

# ################################################################# 
# 9. SAVE TABLE OUTPUT

save_kable(
  kable(
    Table_summary_freq_clean,
    format    = "latex",
    escape    = FALSE,
    align     = "lrr",
    col.names = c("Variable", "Count", "Percent"),
    caption   = "Summary Statistics",
    label     = "summary_stats",
    booktabs  = TRUE
  ) %>%
    kable_styling(
      full_width    = FALSE,
      position      = "center",
      latex_options = "hold_position"
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
              nrow(gender_table) + nrow(age_table) + nrow(region_table) + nrow(cows_region_table) + nrow(education_table) + nrow(jobfunction_table)) %>%
    footnote(
      general        = "Notes: The number of respondents who completed the Demographic portion of the survey was 56. The number of respondents who completed the Job Function portion was 69. Respondents could select multiple job functions, so percentages in that section do not sum to 100\\\\%. Some respondents left questions blank or selected ``Prefer not to say''; such responses were dropped, so sections with such responses do not sum to 56 or 69, respectively. Number of cows by region includes cows from respondents who did not report a region.",
      general_title  = "",
      threeparttable = TRUE,
      escape         = FALSE
    ),
  file = "tables_figures/summary_table_frequencies.tex"
)