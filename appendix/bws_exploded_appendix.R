# ################################################################# 
# Best Worst - MNL Exploded Logit
#     Robustness Check - Appendix B
# ################################################################# 
# Elyse Gross 
# April 23, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

rm(list = ls())

setwd("~/Desktop/EKGThesis")

# Load pacakges and install if necessary
required_packages <- c("dplyr", "tidyr", "mlogit", "tibble", "boot", "knitr", "kableExtra")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Import Data
clean_data <- read.csv("Data/clean_data.csv")

# ################################################################# 
# 2. Clean Data

# Keep only the columns needed for best worst
bw_data <- clean_data %>%
  select(ResponseId, starts_with("bw.")) %>%
  mutate(
    across(starts_with("bw."), ~ case_when(
      . == 1 ~ 1,
      . == 2 ~ -1,
      is.na(.) ~ 0
    ))
  )

# identify and remove partial respondents (didn't answer both parts of every bws question)
partial_ids <- bw_data %>%
  pivot_longer(-ResponseId, names_to = c("Question", "Local"),
               names_pattern = "bw\\.(\\d+)_(\\d+)", values_to = "Choice") %>%
  group_by(ResponseId, Question) %>%
  summarise(
    complete = any(Choice == 1, na.rm = TRUE) & any(Choice == -1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ResponseId) %>%
  filter(!all(complete)) %>%
  pull(ResponseId) %>%
  unique()

bw_data <- bw_data %>%
  filter(!ResponseId %in% partial_ids) %>%
  mutate(across(starts_with("bw."), ~ replace(., is.na(.), 0)))

# Reshape to long format 
bw_long <- bw_data %>%
  pivot_longer(
    cols = starts_with("bw."),
    names_to = c("Question", "Local"),
    names_pattern = "bw\\.(\\d+)_(\\d+)",
    values_to = "Choice"
  ) 

# ################################################################# 
# 3. Define/map the 7 attributes

attribute_map <- data.frame(
  Question = c(
    rep(1, 3),
    rep(2, 3),
    rep(3, 3),
    rep(4, 3),
    rep(5, 3),
    rep(6, 3),
    rep(7, 3)
  ),
  Local = rep(1:3, 7),
  Attribute = c(
    "Consumer Acceptance", "Rumen Health", "Effect on Milk Components",      # Q1
    "Cost to Farmer", "Effect on Milk Components", "Expected Return to Farmer",          # Q2
    "Expected Return to Farmer", "Feed Intake/Palatability", "Rumen Health",      # Q3
    "Feed Intake/Palatability", "Consumer Acceptance", "Cost to Farmer",          # Q4
    "Rumen Health", "Cost to Farmer", "Effect on Milk Production (yield)",      # Q5
    "Effect on Milk Production (yield)", "Expected Return to Farmer", "Consumer Acceptance",          # Q6
    "Effect on Milk Components", "Effect on Milk Production (yield)", "Feed Intake/Palatability"       # Q7
  )
)

# ################################################################# 
# 4. Prepare Data

# Make all numeric
bw_long <- bw_long %>%
  mutate(
    Question = as.numeric(Question),
    Local = as.numeric(Local)
  )
attribute_map <- attribute_map %>%
  mutate(
    Question = as.numeric(Question),
    Local = as.numeric(Local)
  )

# Merge long BW data with the attribute labels table made above
bw_long_attribute <- bw_long %>%
  left_join(attribute_map, by = c("Question", "Local"))

# Create best worst scores for each using simple summation
bw_long_attribute <- bw_long_attribute %>%
  mutate(Choice = as.numeric(Choice))

# Add respondent index
bw_long_attribute$chid <- as.numeric(as.factor(
  paste(bw_long_attribute$ResponseId, bw_long_attribute$Question)
))


# #################################################################
# 5. Descriptive: Best Minus Worst

bw_scores <- bw_long_attribute %>%
  filter(Choice != 0) %>%
  group_by(Attribute) %>%
  summarise(
    Best     = sum(Choice == 1),
    Worst    = sum(Choice == -1),
    BW_Score = sum(Choice)
  ) %>%
  arrange(desc(BW_Score))

bw_scores %>%
  arrange(desc(BW_Score)) %>%
  rename(
    `Best`      = Best,
    `Worst`     = Worst,
    `B minus W` = BW_Score
  ) %>%
  remove_rownames() %>%
  column_to_rownames("Attribute") %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    caption  = "Best-Worst Count Scores for Feed Additive Attributes",
    label    = "bwscores",
    align    = c("r", "r", "r")
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width    = FALSE,
    position      = "center"
  ) %>%
  footnote(
    general = paste(
      "Notes: Based on 61 respondents completing 7 best-worst choice tasks each.",
      "B minus W = number of times chosen as best minus number of times chosen as worst.",
      "Twenty-one responses had a best choice recorded without a corresponding worst choice",
      "and are included in best and worst counts separately."
    ),
    general_title  = "",
    threeparttable = TRUE,
    escape         = FALSE
  ) %>%
  save_kable("tables_figures/bestworst_scores_table.tex")

# Close the caption gap
tex <- readLines("tables_figures/bestworst_scores_table.tex")
tex <- gsub("(\\\\caption\\{.*\\})", "\\1\n\\\\vspace{-8pt}", tex)
writeLines(tex, "tables_figures/bestworst_scores_table.tex")

# ################################################################# 
# 6. Format for Ranked Order 

# Best task: all 3 attributes, best item is chosen
best_data <- bw_long_attribute %>%
  group_by(ResponseId, Question) %>%
  filter(sum(Choice == 1, na.rm = TRUE) == 1) %>%
  ungroup() %>%
  mutate(
    chosen  = as.integer(Choice == 1 & !is.na(Choice)),
    task_id = paste(ResponseId, Question, "best", sep = "_")
  )

# Middle task: middle chosen over worst (i.e. not chosen for best or worst)
middle_data <- bw_long_attribute %>%
  group_by(ResponseId, Question) %>%
  filter(
    sum(Choice == 1, na.rm = TRUE) == 1,
    sum(Choice == -1, na.rm = TRUE) == 1,
    sum(is.na(Choice)) == 1
  ) %>%
  filter(is.na(Choice) | Choice == -1) %>%  # remove best item
  ungroup() %>%
  mutate(
    chosen  = as.integer(is.na(Choice)),
    task_id = paste(ResponseId, Question, "second", sep = "_")
  )

# Stack best and middle
bw_exploded <- bind_rows(best_data, middle_data) %>%
  arrange(ResponseId, Question, task_id)

# Rename attributes to remove spaces
bw_exploded <- bw_exploded %>%
  mutate(Attribute = recode(Attribute,
                            "Consumer Acceptance"               = "ConsumerAcceptance",
                            "Cost to Farmer"                    = "CostToFarmer",
                            "Effect on Milk Components"         = "EffectMilkComponents",
                            "Effect on Milk Production (yield)" = "EffectMilkProduction",
                            "Expected Return to Farmer"         = "ExpectedReturn",
                            "Feed Intake/Palatability"          = "FeedIntake",
                            "Rumen Health"                      = "RumenHealth"
  ))

# Set reference level
bw_exploded$Attribute <- factor(bw_exploded$Attribute, levels = c(
  "EffectMilkProduction",
  "ConsumerAcceptance",
  "RumenHealth",
  "EffectMilkComponents",
  "CostToFarmer",
  "ExpectedReturn",
  "FeedIntake"
))

bw_exploded$chosen  <- as.integer(bw_exploded$chosen)
bw_exploded <- bw_exploded %>%
  mutate(chid_new = as.numeric(as.factor(task_id)))

# ################################################################# 
# 7. Best-Worst Model (MNL using mlogit)

# Build mlogit data
bw_exploded_data <- mlogit.data(
  bw_exploded,
  choice   = "chosen",
  shape    = "long",
  alt.var  = "Attribute",
  chid.var = "chid_new",
  id.var   = "ResponseId"
)

# MNL model 
bw_mnl_exploded <- mlogit(
  chosen ~ Attribute - 1,
  data = bw_exploded_data
)
summary(bw_mnl_exploded)

# ################################################################# 
# 8. Krinsky-Robb Simulation for Standard Errors

mu         <- coef(bw_mnl_exploded)
var_matrix <- vcov(bw_mnl_exploded)

set.seed(123)
n_sim  <- 1000
T_chol <- t(chol(var_matrix))

sim_matrix <- matrix(NA, nrow = n_sim, ncol = length(mu))
for (j in 1:n_sim) {
  M <- rnorm(length(mu))
  sim_matrix[j, ] <- mu + T_chol %*% M
}

sim_exp   <- exp(sim_matrix)
denom     <- rowSums(sim_exp) + 1
shares    <- sim_exp / denom
ref_share <- 1 / denom

all_shares <- cbind(shares, ref_share)
colnames(all_shares) <- c(
  names(mu),
  "EffectMilkProduction"  # corrected reference label
)

results <- data.frame(
  Attribute = colnames(all_shares),
  Lower95   = apply(all_shares, 2, quantile, 0.025),
  Mean      = apply(all_shares, 2, mean),
  Upper95   = apply(all_shares, 2, quantile, 0.975)
) %>%
  arrange(desc(Mean))

print(results) # results

# #################################################################
# 9. Combined Coefficients + Preference Shares Table (LaTeX)

library(kableExtra)

coef_summary <- summary(bw_mnl_exploded)$CoefTable

name_map <- c(
  "ConsumerAcceptance"   = "Consumer Acceptance",
  "CostToFarmer"         = "Cost to Farmer",
  "RumenHealth"          = "Rumen Health",
  "EffectMilkComponents" = "Effect on Milk Components",
  "ExpectedReturn"       = "Expected Return to Farmer",
  "FeedIntake"           = "Feed Intake/Palatability"
)

share_name_map <- c(
  "AttributeConsumerAcceptance"   = "Consumer Acceptance",
  "AttributeCostToFarmer"         = "Cost to Farmer",
  "AttributeEffectMilkComponents" = "Effect on Milk Components",
  "EffectMilkProduction"          = "Effect on Milk Production (yield)",
  "AttributeExpectedReturn"       = "Expected Return to Farmer",
  "AttributeFeedIntake"           = "Feed Intake/Palatability",
  "AttributeRumenHealth"          = "Rumen Health"
)

# Build coefficient rows (excluding reference)
clean_names <- gsub("^Attribute", "", rownames(coef_summary))

coef_df <- data.frame(
  Attribute = name_map[clean_names],
  Estimate  = coef_summary[, "Estimate"],
  StdError  = coef_summary[, "Std. Error"],
  p.value   = coef_summary[, "Pr(>|z|)"]
) %>%
  mutate(
    Stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    # Format: "0.1234*** \n(0.0567)"
    Coef_Cell = paste0(round(Estimate, 4), Stars, " \\\\ & (", round(StdError, 4), ")")
  )

# Add reference row
ref_row <- data.frame(
  Attribute = "Effect on Milk Production (yield)",
  Estimate  = 0,
  StdError  = NA,
  p.value   = NA,
  Stars     = "",
  Coef_Cell = "0 (reference)"
)

coef_df <- bind_rows(coef_df, ref_row)

# Build preference share rows
results_named <- results %>%
  mutate(Attribute = dplyr::recode(Attribute, !!!share_name_map))

share_df <- results_named %>%
  mutate(
    Share_Mean = round(Mean * 100, 1),
    Share_Low  = round(Lower95 * 100, 1),
    Share_High = round(Upper95 * 100, 1),
    Share_Cell = paste0(Share_Mean, "\\%")
  ) %>%
  select(Attribute, Share_Cell, Share_Low, Share_High)

# Merge
combined <- coef_df %>%
  left_join(share_df, by = "Attribute") %>%
  arrange(desc(Estimate))

# Build a two-row-per-attribute LaTeX table manually

rows <- c()
for (i in seq_len(nrow(combined))) {
  r <- combined[i, ]
  
  attr_label <- r$Attribute
  
  if (!is.na(r$StdError)) {
    coef_line1 <- paste0(round(r$Estimate, 4), r$Stars)
    coef_line2 <- paste0("(", round(r$StdError, 4), ")")
  } else {
    coef_line1 <- r$Coef_Cell  # "0 (reference)"
    coef_line2 <- ""
  }
  
  share_line1 <- r$Share_Cell
  share_line2 <- paste0("[", r$Share_Low, ", ", r$Share_High, "]")
  
  # Row 1
  rows <- c(rows, paste0(
    attr_label, " & ", coef_line1, " & ", share_line1, " \\\\"
  ))
  # Row 2 (std error + CI)
  rows <- c(rows, paste0(
    " & ", coef_line2, " & ", share_line2, " \\\\"
  ))
  # Small space between attributes
  rows <- c(rows, "\\addlinespace[2pt]")
}

# Assemble full LaTeX table
header <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{MNL Coefficients and Preference Shares for Feed Additive Attributes}",
  "\\label{tab:bw_combined}",
  "\\vspace{-8pt}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrr}",
  "\\toprule",
  "Attribute & Coefficient & Preference Share (\\%) \\\\",
  " & & Mean [95\\% CI] \\\\",
  "\\midrule"
)

footer <- c(
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]\\footnotesize",
  paste0("\\item Reference category: Effect on Milk Production (yield). ",
         "Log-Likelihood: ", round(as.numeric(logLik(bw_mnl_exploded)), 2), ". ",
         "N = ", n_distinct(bw_exploded$ResponseId), " respondents. ",
         "Preference shares estimated via Krinsky-Robb simulation (1,000 draws). ",
         "Shares sum to 100\\%. ",
         "*** $p < 0.001$, ** $p < 0.01$, * $p < 0.05$, $\\cdot$ $p < 0.1$."),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

tex_lines <- c(header, rows, footer)
writeLines(tex_lines, "tables_figures/appendix_bw_combined_table.tex")