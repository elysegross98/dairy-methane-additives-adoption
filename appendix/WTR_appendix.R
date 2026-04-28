# ################################################################# 
# Contingent Valuation - WTR
#     Appendix C
# ################################################################# 
# Elyse Gross 
# Apr 27, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

rm(list = ls())

setwd("~/Desktop/EKGThesis")

# Load pacakges and install if necessary
required_packages <- c("dplyr", "tidyr", "ggplot2", "mlogit", "tibble", "knitr", "kableExtra", "survival", "DCchoice")
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

# Selecting only columns needed for CV
CV_clean <- clean_data %>%
  select(ResponseId, Nutritionist.y.n, CV1, CV1info, CV2yes, CV2no, CV2infoyes, CV2infono, BidAmount, FollowUpBidYes, FollowUpBidNo)

# Dropping any (row) respondent who answered none of the CV qs
CV_clean <- CV_clean %>%
  filter(!if_all(starts_with("CV"), is.na)) # This actually drops them

# ################################################################# 
# 3. Preparing for CV

# Creating a dummy indicator for those who had the 'info' question
CV_clean <- CV_clean %>%
  mutate(info_treatment = ifelse(!is.na(CV1info), 1, 0))

# Combining first responses into one column, despite info
CV_clean <- CV_clean %>%
  mutate(
    first_response = ifelse(info_treatment == 1, CV1info, CV1),
    first_yes = ifelse(first_response == 1, 1, 0)
  )

# Now doing this combining with second responses
CV_clean <- CV_clean %>%
  mutate(
    second_response = case_when(
      info_treatment == 0 & first_yes == 1 ~ CV2yes,
      info_treatment == 0 & first_yes == 0 ~ CV2no,
      info_treatment == 1 & first_yes == 1 ~ CV2infoyes,
      info_treatment == 1 & first_yes == 0 ~ CV2infono
    ),
    second_yes = ifelse(second_response == 1, 1, 0)
  )

# Assigning the second bid to one column, inc or dec by 20%, despite yes/no
CV_clean <- CV_clean %>%
  mutate(
    second_bid = ifelse(first_yes == 1, FollowUpBidYes, FollowUpBidNo)
  )

# Assigning four possible response patterns
CV_clean <- CV_clean %>%
  mutate(
    pattern = case_when(
      first_yes == 1 & second_yes == 1 ~ "YY",
      first_yes == 1 & second_yes == 0 ~ "YN",
      first_yes == 0 & second_yes == 1 ~ "NY",
      first_yes == 0 & second_yes == 0 ~ "NN"
    )
  )

# Making sure all datatypes are the same
CV_clean <- CV_clean %>%
  mutate(
    BidAmount = as.numeric(BidAmount),
    FollowUpBidYes = as.numeric(FollowUpBidYes),
    FollowUpBidNo = as.numeric(FollowUpBidNo),
    second_bid = as.numeric(second_bid)
  )

# Making intervals of WTR/WTR
CV_clean <- CV_clean %>%
  mutate(
    lower = case_when(
      pattern == "YY" ~ second_bid,
      pattern == "YN" ~ BidAmount,
      pattern == "NY" ~ second_bid,
      pattern == "NN" ~ 0
    ),
    upper = case_when(
      pattern == "YY" ~ Inf,
      pattern == "YN" ~ second_bid,
      pattern == "NY" ~ BidAmount,
      pattern == "NN" ~ BidAmount
    )
  )

## Couting the amount who said yes and no to each
# CV1
CV_clean %>%
  count(first_yes) %>%
  mutate(Response = ifelse(first_yes == 1, "Yes", "No"))

# CV2
CV_clean %>%
  count(second_yes) %>%
  mutate(Response = ifelse(second_yes == 1, "Yes", "No"))

# ################################################################# 
# 4. Setting up data for package

# Setting up data the way it wants
dc_df <- CV_clean %>%
  mutate(
    R1 = factor(first_yes),
    R2 = factor(second_yes),
    bid1 = BidAmount,
    bid2 = second_bid
  ) %>%
  select(ResponseId, R1, R2, bid1, bid2, info_treatment) %>%
  na.omit()

# This table shows no No->yes
table(dc_df$R1, dc_df$R2)

# Convert to numeric 
dc_df$R2 <- as.numeric(as.character(dc_df$R2))
dc_df$R1 <- as.numeric(as.character(dc_df$R1))

# Groups
dc_pooled <- dc_df
dc_treat <- filter(dc_df, info_treatment == 1)
dc_notreat <- filter(dc_df, info_treatment == 0)

# ################################################################# 
# 5. INFORMATION TREATMENT ####
# Including information treatment dummy

# Run DB model on the treatment group
db_treat <- dbchoice(
  R1 + R2 ~ 1 | bid1 + bid2,
  data = dc_treat,
  dist = "log-logistic"
)
summary(db_treat) ## WTR ESTIMATE!!

# Confidence intervals 
# Using bootCI to get estimates (uses bootstrapping to resample data)
boot_treat_results <- bootCI(db_treat, nboot = 1000, CI = 0.95, individual = NULL)
boot_treat_results ## CONFIDENCE INTERVAL

#Saving these values as ci_treat
ci_treat <- boot_treat_results$out["truncated Mean", c("LB", "UB")]
as.numeric(ci_treat)

# ################################################################# 
# 6. NO TREATMENT:
# Run DB model on the treatment group
db_notreat <- dbchoice(
  R1 + R2 ~ 1 | bid1 + bid2,
  data = dc_notreat,
  dist = "log-logistic"
)
summary(db_notreat) ## WTR ESTIMATE!!

# Confidence intervals 
# Using bootCI to get estimates (uses bootstrapping to resample data)
boot_notreat_results <- bootCI(db_notreat, nboot = 1000, CI = 0.95, individual = NULL)
boot_notreat_results ## CONFIDENCE INTERVAL

#Saving these values as ci_notreat
ci_notreat <- boot_notreat_results$out["truncated Mean", c("LB", "UB")]
as.numeric(ci_notreat)

# ################################################################# 
# 7. DOUBLE BOUNDED, FOR EVERYONE: ###
# Using dc choice
db_pooled <- dbchoice(
  R1 + R2 ~ 1 | bid1 + bid2,
  data = dc_pooled,
  dist = "log-logistic"
)
summary(db_pooled)## WTR ESTIMATE

# Confidence intervals 
# Using bootCI to get estimates (uses bootstrapping to resample data)
boot_results <- bootCI(db_pooled, nboot = 1000, CI = 0.95, individual = NULL)
boot_results ## CONFIDENCE INTERVAL

#Saving these values as ci_treat
ci <- boot_results$out["truncated Mean", c("LB", "UB")]
as.numeric(ci)

# ################################################################# 
# 8. Can these pooled results be reported: log likelihood test:

# Extracting log likelihoods from pooled and individual models
ll_restricted   <- logLik(db_pooled)
ll_unrestricted <- logLik(db_treat) + logLik(db_notreat)

df <- length(coef(db_treat)) + length(coef(db_notreat)) - length(coef(db_pooled))
cat("Degrees of freedom:", df, "\n")

lr_stat <- 2 * (as.numeric(ll_unrestricted) - as.numeric(ll_restricted))
p_value <- pchisq(lr_stat, df = df, lower.tail = FALSE)

cat("====================================\n")
cat("  LIKELIHOOD RATIO TEST: POOLING   \n")
cat("====================================\n")
cat("LL (restricted/pooled):    ", round(as.numeric(ll_restricted), 4), "\n")
cat("LL (unrestricted/separate):", round(as.numeric(ll_unrestricted), 4), "\n")
cat("LR statistic (chi-sq):     ", round(lr_stat, 4), "\n")
cat("Degrees of freedom:        ", df, "\n")
cat("p-value:                   ", round(p_value, 4), "\n")
cat("====================================\n")
if (p_value < 0.05) {
  cat("REJECT pooling (p < 0.05): report groups separately\n")
} else {
  cat("FAIL TO REJECT pooling (p >= 0.05): groups can be pooled\n")
}

# ################################################################# 
# 9. Table

# Extract WTR and CIs from existing info above
WTR_full <- summary(db_pooled)$mean
WTR_treat <- summary(db_treat)$mean
WTR_notreat <- summary(db_notreat)$mean


# Combine into a tibble — now all vectors are length 1 scalars
WTR_table_base <- tibble::tibble(
  Sample = c("Full Sample", "Treatment", "No Treatment"),
  WTR = c(WTR_full, WTR_treat, WTR_notreat),
  CI_Lower = c(ci[1], ci_treat[1], ci_notreat[1]),
  CI_Upper = c(ci[2], ci_treat[2], ci_notreat[2])
)

# statistical significance if CI does not cross 0
WTR_table_base <- WTR_table_base %>%
  mutate(
    Signif = ifelse(CI_Lower > 0 | CI_Upper < 0, "*", ""),
    WTR_formatted = sprintf("%.3f", WTR)
  )

# Build display table
table_WTR_final <- WTR_table_base %>%
  mutate(
    CI = paste0("(",
                sprintf("%.3f", CI_Lower),
                ", ",
                sprintf("%.3f", CI_Upper),
                ")"),
    WTR_display = paste0(WTR_formatted, Signif, " ", CI)
  ) %>%
  select(Sample, WTR_display)

# Render table for export
table_kable <- kable(
  table_WTR_final,
  escape = FALSE,
  col.names = c("Sample", "Mean WTR (95% CI)"),
  caption = "Mean Willingness to Recommend (USD/cow/day) with 95% Confidence Intervals"
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("condensed")
  ) %>%
  footnote(
    general = "Notes: WTR is estimated from the double-bounded dichotomous choice contingent valuation model using maximum likelihood estimation. 95% confidence intervals are obtained using bootstrap methods. * indicates statistical significance at the 5% level (CI does not contain zero).",
    general_title = " "
  )

# Latex
save_kable(
  kable(
    table_WTR_final,
    col.names = c("Sample", "Mean WTR (95\\% CI)"),
    caption   = "Mean Willingness to Recommend (USD/cow/day) with 95\\% Confidence Intervals",
    format    = "latex",
    booktabs  = TRUE
  ),
  file = "tables_figures/WTR_table_final.tex"
)