# ################################################################# 
# Perceptions & Attitudes
# ################################################################# 
# Elyse Gross 
# Apr 27, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

rm(list = ls()) 

setwd("~/Desktop/EKGThesis")

# Load libraries
required_packages <- c("multcompView", "dplyr", "tidyr", "ggplot2", "kableExtra", "rstatix", "forcats", "xtable")
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
# 2. Preparing Data for Perceptions

perception_data <- clean_data %>%
  select(ResponseId, X3NOP.familiarity, X3NOP.effective, X3NOP.sell, X3NOP.use, seaweed.familiarity, seaweed.effective, seaweed.sell, seaweed.use, EO.familiarity, EO.effective, EO.sell, EO.use, tannins.familiarity, tannins.effective, tannins.sell, tannins.use)

# Recoding all the scales (they came out of qualtrics with crazy numbers)
perception_data <- perception_data %>%
  mutate(
    X3NOP.sell   = recode(X3NOP.sell,  `13` = 0, `8` = 1),
    X3NOP.use    = recode(X3NOP.use,   `9`  = 0, `8` = 1),
    seaweed.sell = recode(seaweed.sell,`9` = 0, `8` = 1),
    seaweed.use  = recode(seaweed.use, `9` = 0, `8` = 1),
    EO.sell      = recode(EO.sell,     `9` = 0, `8` = 1),
    EO.use       = recode(EO.use,      `21` = 0, `20` = 1),
    tannins.sell = recode(tannins.sell,`9` = 0, `8` = 1),
    tannins.use  = recode(tannins.use, `9` = 0, `8` = 1)
  )

perception_data <- perception_data %>%
  mutate(
    across(
      matches("(familiarity|effective)$"),
      ~ recode(.,
               `8` = 5,
               `9` = 4,
               `10` = 3,
               `11` = 2,
               `12` = 1,
               .default = NA_real_
      )
    )
  )

# Define perception columns (everything except ResponseId)
perception_cols <- perception_data %>%
  select(-ResponseId) %>%
  names()

# Keep ONLY complete respondents
perception_data <- perception_data %>%
  filter(if_all(all_of(perception_cols), ~ !is.na(.) & . != ""))

# Convert to long format
perception_long <- perception_data %>%
  select(ResponseId, ends_with(c("familiarity","effective","sell","use"))) %>%
  pivot_longer(
    cols = -ResponseId,
    names_to = c("Additive","Measure"),
    names_pattern = "(X3NOP|seaweed|EO|tannins)\\.(.*)",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value)) %>%
  mutate(
    Additive = recode(Additive,
                      X3NOP = "3NOP",
                      seaweed = "Seaweed",
                      EO = "Essential Oils",
                      tannins = "Tannins"
    )
  )

# ################################################################# 
# 3. Means & Sums

# Making table with means and sds
perception_summary <- perception_long %>%
  group_by(Additive, Measure) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD   = sd(Value, na.rm = TRUE),
    .groups = "drop"
  )

# ################################################################# 
# 4. Statistical Testing 

cld_results <- perception_long %>%
  group_by(Measure) %>%
  group_modify(~ {
    # Run pairwise t-tests
    pw <- pairwise.t.test(
      x   = .x$Value,
      g   = .x$Additive,
      p.adjust.method = "bonferroni",
      pool.sd = FALSE
    )
    
    # Convert matrix to a named vector
    p_matrix <- pw$p.value
    p_vec <- as.vector(p_matrix)
    
    # Build names from row/col combos
    row_names <- rownames(p_matrix)
    col_names <- colnames(p_matrix)
    pair_names <- outer(row_names, col_names, paste, sep = "-")
    names(p_vec) <- as.vector(pair_names)
    
    # Drop NAs & keep only valid pairs
    p_vec <- p_vec[!is.na(p_vec)]
    
    # Generate CLD letters
    letters <- multcompLetters(p_vec)$Letters
    
    tibble(
      Additive = names(letters),
      Letter   = letters
    )
  }) %>%
  ungroup()


# ################################################################# 
# 5. Making final table

# formatting final table
perception_formatted <- perception_summary %>%
  left_join(cld_results, by = c("Additive", "Measure")) %>%
  mutate(
    Letter = replace_na(Letter, ""),
    Cell   = paste0(round(Mean, 2), " (", round(SD, 2), ")", Letter)
  ) %>%
  select(Measure, Additive, Cell)

perception_table <- perception_formatted %>%
  pivot_wider(names_from = Additive, values_from = Cell) %>%
  mutate(Measure = recode(Measure,
                          "effective"   = "Effectiveness",
                          "familiarity" = "Familiarity",
                          "sell"        = "Sell",
                          "use"         = "Use"
  )) %>%
  arrange(match(Measure, c("Familiarity", "Effectiveness", "Sell", "Use")))

kable(
  perception_table,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  caption   = "Nutritionists' Perceptions of Feed Additive Technologies",
  label     = "perception",
  col.names = c("Measure", "3NOP", "Essential Oils", "Seaweed", "Tannins"),
  align     = c("l", "c", "c", "c", "c")
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width    = FALSE,
    position      = "center"
  ) %>%
  column_spec(1, width = "2.5cm") %>%
  column_spec(2:5, width = "2.8cm") %>%
  footnote(
    general = paste(
      "Notes: Additives sharing a letter within a row are not significantly different",
      "(pairwise $t$-tests with Bonferroni correction, $p < .05$).",
      "Familiarity and Effectiveness: 5 = Strongly Agree, 1 = Strongly Disagree.",
      "Sell and Use: 1 = Yes, 0 = No."
    ),
    general_title  = "",
    threeparttable = TRUE,
    escape         = FALSE
  ) %>%
  save_kable("tables_figures/perception_table.tex")

#################################################################
# 6. Attitudes about climate change & methane

# Keeping only responses for attitude portion
attitude_data <- clean_data %>%
  select(ResponseId, attitude.climate, attitude.methane, attitude.climate.mm)

# Recoding values to correct Likert scale
attitude_data <- attitude_data %>%
  mutate(
    attitude.climate    = recode(as.numeric(attitude.climate),
                                 `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    attitude.methane    = recode(as.numeric(attitude.methane),
                                 `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
    attitude.climate.mm = recode(as.numeric(attitude.climate.mm),
                                 `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
  )

# Keep ONLY complete respondents
attitude_cols <- attitude_data %>%
  select(starts_with("attitude")) %>%
  names()
attitude_data <- attitude_data %>%
  filter(if_all(all_of(attitude_cols), ~ !is.na(.) & . != ""))

attitude_data$attitude.climate    <- as.numeric(attitude_data$attitude.climate)
attitude_data$attitude.methane    <- as.numeric(attitude_data$attitude.methane)
attitude_data$attitude.climate.mm <- as.numeric(attitude_data$attitude.climate.mm)

# Count number of complete responses
n_climate    <- sum(!is.na(attitude_data$attitude.climate))
n_methane    <- sum(!is.na(attitude_data$attitude.methane))
n_climate_mm <- sum(!is.na(attitude_data$attitude.climate.mm))

N = c(n_climate, n_methane, n_climate_mm)

# Test against midpoint = 3 for significance
t_climate    <- t.test(na.omit(attitude_data$attitude.climate),    mu = 3)
t_methane    <- t.test(na.omit(attitude_data$attitude.methane),    mu = 3)
t_climate_mm <- t.test(na.omit(attitude_data$attitude.climate.mm), mu = 3)

# Make Table for Latex
attitude_table <- data.frame(
  Statement = c(
    "Climate change is a serious problem",
    "Methane emissions are a serious problem",
    "Climate change is caused by human activity"
  ),
  Mean_SD = c(
    paste0(round(mean(attitude_data$attitude.climate,    na.rm = TRUE), 2),
           " (", round(sd(attitude_data$attitude.climate,    na.rm = TRUE), 2), ")"),
    paste0(round(mean(attitude_data$attitude.methane,    na.rm = TRUE), 2),
           " (", round(sd(attitude_data$attitude.methane,    na.rm = TRUE), 2), ")"),
    paste0(round(mean(attitude_data$attitude.climate.mm, na.rm = TRUE), 2),
           " (", round(sd(attitude_data$attitude.climate.mm, na.rm = TRUE), 2), ")")
  ),
  t_stat = c(
    round(t_climate$statistic,    2),
    round(t_methane$statistic,    2),
    round(t_climate_mm$statistic, 2)
  ),
  p_value = c(
    ifelse(t_climate$p.value    < .01, "$<$ .01", round(t_climate$p.value,    3)),
    ifelse(t_methane$p.value    < .01, "$<$ .01", round(t_methane$p.value,    3)),
    ifelse(t_climate_mm$p.value < .01, "$<$ .01", round(t_climate_mm$p.value, 3))
  )
)

kable(
  attitude_table,
  format    = "latex",
  booktabs  = TRUE,
  escape    = FALSE,
  caption   = "Nutritionist Attitudes Toward Climate Change and Methane",
  label     = "attitude",
  col.names = c("Statement", "Mean (SD)", "\\textit{t}-statistic", "\\textit{p}-value"),
  align     = c("l", "c", "c", "c")
) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width    = FALSE,
    position      = "center"
  ) %>%
  column_spec(1, width = "8.5cm") %>%
  column_spec(2:4, width = "1.9cm") %>%
  footnote(
    general        = "Notes: Responses measured on a 5-point Likert scale: 5 = Strongly Agree, 1 = Strongly Disagree. \\\\textit{p}-values tested against scale midpoint ($\\mu = 3$).",
    general_title  = "",
    threeparttable = TRUE,
    escape         = FALSE
  ) %>%
  save_kable("tables_figures/attitude_table.tex")