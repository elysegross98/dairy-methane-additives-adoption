# ################################################################# 
# Perceptions & Attitudes
# ################################################################# 
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
