# ################################################################# 
# Best Worst - Support.BWS (Conditional Logit) 
#         & Mixed Logit Robustness Check
# ################################################################# 
# Elyse Gross 
# Mar 30, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

rm(list = ls())

setwd("~/Desktop/EKGThesis")

required_packages <- c("dplyr", "tidyr", "support.BWS", "survival", "broom")
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

bw_data <- clean_data %>%
  select(ResponseId, starts_with("bw.")) %>%
  mutate(across(starts_with("bw."), ~ as.numeric(.) %>% replace(. == 2, -1))) %>% 
  filter(!if_all(starts_with("bw."), is.na)) 

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

# ################################################################# 
# 3. Define/map the 7 attributes

attribute_names <- c(
  "Consumer Acceptance",              # 1
  "Rumen Health",                     # 2
  "Effect on Milk Components",        # 3
  "Cost to Farmer",                   # 4
  "Expected Return to Farmer",        # 5
  "Feed Intake/Palatability",         # 6
  "Effect on Milk Production (yield)" # 7
)

# ################################################################# 
# 4. Define the design matrix

design <- rbind(
  c(1, 2, 3),
  c(4, 3, 5),
  c(5, 6, 2),
  c(6, 1, 4),
  c(2, 4, 7),
  c(7, 5, 1),
  c(3, 7, 6)
)

nquestions  <- nrow(design)
nattributes <- 3

# ################################################################# 
# 5. Rename BWS response columns to standard b/w format

bws_input <- bw_data %>%
  mutate(ID = row_number())

for(q in 1:7) {
  cols <- paste0("bw.", q, "_", 1:3)
  vals <- bws_input[, cols]
  vals <- apply(vals, 2, as.numeric)
  bws_input[[paste0("b", q)]] <- apply(vals, 1, function(x) which(x == 1)[1])
  bws_input[[paste0("w", q)]] <- apply(vals, 1, function(x) which(x == -1)[1])  # was == 2
}

bws_input <- bws_input %>%
  select(ID, matches("^[bw][0-9]+$"))

# ################################################################# 
# 6. Build the BWS dataset using support.BWS

best_cols     <- paste0("b", 1:7)
worst_cols    <- paste0("w", 1:7)
response_cols <- c(rbind(best_cols, worst_cols))

stopifnot(!anyNA(bws_input))  

bws_ready <- bws.dataset(
  data          = bws_input,  
  response      = response_cols,
  response.type = 1,
  choice.sets   = design,
  design.type   = 2,
  item.names    = attribute_names,
  id            = "ID",
  model         = "maxdiff"
)

# ################################################################# 
# 7. Compute Best Worst counts (descriptive)

bws_counts <- bws.count(bws_ready)
print(bws_counts)

# Create counts table
bw_scores <- bws_counts$aggregate %>%
  rownames_to_column("Attribute") %>%
  select(Attribute, B, W, BW) %>%
  arrange(desc(BW))

bw_scores %>%
  rename(`Best` = B, `Worst` = W, `B minus W` = BW) %>%
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
      "Notes: Based on 54 respondents completing 7 best-worst choice tasks each.",
      "B minus W = number of times chosen as best minus number of times chosen as worst."
    ),
    general_title  = "",
    threeparttable = TRUE,
    escape         = FALSE
  ) %>%
  save_kable("tables_figures/bestworst_scores_table.tex")

# ################################################################# 
# 8. Fit conditional logit model

attr_cols    <- attr(bws_ready, "vnames")
attr_cols_bt <- paste0("`", attr_cols, "`")

bws_formula <- as.formula(
  paste("RES ~", paste(attr_cols_bt, collapse = " + "), "+ strata(STR)")
)

bws_model <- clogit(bws_formula, data = bws_ready, method = "efron")
print(summary(bws_model))

# ################################################################# 
# 9. Krinsky-Robb Simulation for Preference Shares

mu        <- coef(bws_model)
n_sim     <- 10000
mu_clean  <- mu[!is.na(mu)]

var_matrix_clean <- vcov(bws_model)
var_matrix_clean <- var_matrix_clean[!is.na(mu), !is.na(mu)]
var_matrix_clean <- var_matrix_clean + diag(1e-6, nrow(var_matrix_clean))

T_chol <- t(chol(var_matrix_clean))

sim_matrix <- matrix(NA, nrow = n_sim, ncol = length(mu_clean))
for (j in 1:n_sim) {
  M <- rnorm(length(mu_clean))
  sim_matrix[j, ] <- mu_clean + T_chol %*% M
}

sim_exp   <- exp(sim_matrix)
denom     <- rowSums(sim_exp) + 1
shares    <- sim_exp / denom
ref_share <- 1 / denom

all_shares <- cbind(shares, ref_share)
colnames(all_shares) <- c(names(mu_clean), "Effect on Milk Production (yield)")

results_bws <- data.frame(
  Attribute = colnames(all_shares),
  Lower95   = apply(all_shares, 2, quantile, 0.025),
  Mean      = apply(all_shares, 2, mean),
  Upper95   = apply(all_shares, 2, quantile, 0.975)
) %>%
  arrange(desc(Mean))

print(results_bws)

# ################################################################# 
# 10. Make LaTeX Table

# Pull coefficients + SEs from clogit model
coef_df <- data.frame(
  Attribute = names(coef(bws_model)),
  Estimate  = coef(bws_model),
  SE        = sqrt(diag(vcov(bws_model))),
  p.value   = summary(bws_model)$coefficients[, "Pr(>|z|)"],
  row.names = NULL
) %>%
  mutate(
    Stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      TRUE            ~ ""
    )
  )

# Start from results_bws
combined <- results_bws %>%
  left_join(coef_df, by = "Attribute") %>%
  arrange(desc(Mean))

# Build LaTeX rows
rows <- c()
for (i in seq_len(nrow(combined))) {
  r <- combined[i, ]
  
  # Coefficient cell: NA Estimate = reference category
  if (!is.na(r$Estimate)) {
    est_str   <- formatC(r$Estimate, format = "f", digits = 3)
    est_str   <- gsub("^-", "$-$", est_str)
    coef_main <- paste0(est_str, r$Stars)
    coef_sub  <- paste0("(", formatC(r$SE, format = "f", digits = 3), ")")
  } else {
    coef_main <- "0 (reference)"
    coef_sub  <- ""
  }
  
  # Preference share cell
  share_main <- paste0(round(r$Mean * 100, 1), "\\%")
  share_sub  <- paste0("[", round(r$Lower95 * 100, 1), ", ", round(r$Upper95 * 100, 1), "]")
  
  rows <- c(rows,
            paste0(r$Attribute, " & ", coef_main, " & ", share_main, " \\\\"),
            paste0(" & {\\small\\textcolor{black}{", coef_sub, "}} & {\\small\\textcolor{black}{", share_sub, "}} \\\\"),
            "\\addlinespace[4pt]"
  )
}

rows <- rows[-length(rows)]  # remove trailing addlinespace

# Assemble full LaTeX table
n_resp  <- n_distinct(bws_input$ID)
log_lik <- round(as.numeric(logLik(bws_model)), 2)

tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{BWS Coefficients and Preference Shares for Feed Additive Attributes}",
  "\\label{tab:support_bws}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrr}",
  "\\toprule",
  "Attribute & Coefficient & \\multicolumn{1}{r}{Preference Share (\\%)} \\\\",
  "          &             & \\multicolumn{1}{r}{Mean [95\\% CI]} \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item Reference category: Effect on Milk Production (yield). ",
    "Log-Likelihood: $", log_lik, "$. $N = ", n_resp, "$ respondents. ",
    "Preference shares estimated via Krinsky-Robb simulation (10{,}000 draws). ",
    "Shares sum to 100\\%. ",
    "*** $p < 0.001$, ** $p < 0.01$, * $p < 0.05$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex, "tables_figures/bws_support.tex")


# #################################################################
# 11. MXL Robustness Check

# install and load newly necessary packages
required_packages <- c("mlogit")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

bws_ml <- bws_ready %>%
  group_by(STR) %>%
  mutate(ALT = row_number()) %>%
  ungroup()

# Convert to mlogit format
bws_mlogit <- dfidx(
  bws_ml,
  idx     = list(c("STR", "ID"), "ALT"),  # choice set, then alt index
  choice  = "RES"
)

attr_cols    <- attr(bws_ready, "vnames")
attr_cols_bt <- paste0("`", attr_cols[-length(attr_cols)], "`")  # drop reference category

mxl_formula <- as.formula(
  paste("RES ~ 0 +", paste(attr_cols_bt, collapse = " + "))
)

bws_mxl_fixed <- mlogit(
  mxl_formula,
  data = bws_mlogit
)
internal_names <- names(coef(bws_mxl_fixed))

# MXL with normal random parameters
bws_mxl <- mlogit(
  mxl_formula,
  data        = bws_mlogit,
  rpar        = setNames(rep("n", length(internal_names)), internal_names),
  panel       = TRUE,
  correlation = FALSE,
  R           = 500,
  seed        = 42
)

summary(bws_mxl)

## COMPARISON ##

# compare to clogit via log-likelihood
cat("\nConditional Logit LL:", logLik(bws_model))
cat("\nMixed Logit LL:      ", logLik(bws_mxl), "\n")

# coefficient comparison table (means only)
mxl_coefs <- summary(bws_mxl)$CoefTable
mxl_means <- mxl_coefs[!startsWith(rownames(mxl_coefs), "sd."), ]
mxl_sds   <- mxl_coefs[startsWith(rownames(mxl_coefs), "sd."), ]

cat("\nRandom parameter means:\n");  print(round(mxl_means, 3))
cat("\nRandom parameter SDs:\n");    print(round(mxl_sds,   3))

# Are clogit & MXL means consistent
clogit_coefs <- coef(bws_model)
clogit_coefs <- clogit_coefs[!is.na(clogit_coefs)]  # drop reference NA row
mxl_mean_coefs <- mxl_means[, "Estimate"]

comparison <- data.frame(
  Attribute = attr_cols[-length(attr_cols)],
  CLogit    = round(clogit_coefs, 3),
  MXL_Mean  = round(mxl_mean_coefs, 3),
  SD_sig    = ifelse(mxl_sds[, "Pr(>|z|)"] < 0.05, "Yes", "No")
)
print(comparison)

## Make Table w/ side by side comparison
clean_names <- gsub("`", "", attr_cols[-length(attr_cols)])

mxl_se      <- mxl_means[, "Std. Error"]
mxl_pval    <- mxl_means[, "Pr(>|z|)"]
sd_est      <- mxl_sds[,   "Estimate"]
sd_se       <- mxl_sds[,   "Std. Error"]
sd_pval     <- mxl_sds[,   "Pr(>|z|)"]
mxl_log_lik <- round(as.numeric(logLik(bws_mxl)), 2)

star_fn <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    TRUE      ~ ""
  )
}

clogit_pvals <- summary(bws_model)$coefficients[, "Pr(>|z|)"]
clogit_pvals <- clogit_pvals[!is.na(coef(bws_model))]  # drop reference row

mxl_table_df <- data.frame(
  Attribute   = clean_names,
  clogit_est  = as.numeric(clogit_coefs),
  clogit_se   = as.numeric(sqrt(diag(vcov(bws_model)))[!is.na(coef(bws_model))]),
  clogit_pval = as.numeric(clogit_pvals),
  mxl_est     = as.numeric(mxl_mean_coefs),
  mxl_se      = as.numeric(mxl_se),
  mxl_pval    = as.numeric(mxl_pval),
  sd_est      = as.numeric(sd_est),
  sd_se       = as.numeric(sd_se),
  sd_pval     = as.numeric(sd_pval),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(clogit_est))

rows <- c()
ref_inserted <- FALSE

for (i in seq_len(nrow(mxl_table_df))) {
  r <- mxl_table_df[i, ]
  
  # Insert reference row at sign break
  if (!ref_inserted && r$clogit_est < 0) {
    rows <- c(rows,
              "Effect on Milk Production (yield) & 0 (ref.) & & 0 (ref.) & & --- & \\\\",
              "\\addlinespace[4pt]"
    )
    ref_inserted <- TRUE
  }
  
  cl_est <- gsub("^-", "$-$", formatC(r$clogit_est, format = "f", digits = 3))
  cl_se  <- formatC(r$clogit_se,  format = "f", digits = 3)
  mx_est <- gsub("^-", "$-$", formatC(r$mxl_est, format = "f", digits = 3))
  mx_se  <- formatC(r$mxl_se,  format = "f", digits = 3)
  sd_str   <- formatC(abs(r$sd_est), format = "f", digits = 3)
  sdse_str <- formatC(r$sd_se,       format = "f", digits = 3)
  
  rows <- c(rows,
            paste0(r$Attribute, " & ",
                   cl_est, star_fn(r$clogit_pval), " & & ",
                   mx_est, star_fn(r$mxl_pval),    " & & ",
                   sd_str, star_fn(r$sd_pval),     " & \\\\"),
            paste0(" & {\\small (", cl_se, ")} & & ",
                   "{\\small (", mx_se, ")} & & ",
                   "{\\small (", sdse_str, ")} & \\\\"),
            "\\addlinespace[4pt]"
  )
}

rows <- rows[-length(rows)]

tex_mxl <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Robustness Check: Conditional Logit vs.\\ Mixed Logit}",
  "\\label{tab:bws_mxl}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{lrlrlrl}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Conditional Logit} & \\multicolumn{2}{c}{MXL Mean} & \\multicolumn{2}{c}{MXL SD} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  "Attribute & Coef. & & Coef. & & SD & \\\\",
  "\\midrule",
  rows,
  "\\midrule",
  paste0("Log-Likelihood & \\multicolumn{2}{l}{$", log_lik, "$} & \\multicolumn{2}{l}{$", mxl_log_lik, "$} & & \\\\"),
  paste0("$N$ & \\multicolumn{2}{l}{$", n_resp, "$} & \\multicolumn{2}{l}{$", n_resp, "$} & & \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0(
    "\\item Reference category: Effect on Milk Production (yield). ",
    "MXL estimated with 500 Halton draws; random parameters assumed normally distributed and uncorrelated. ",
    "Standard errors in parentheses. ",
    "*** $p < 0.001$, ** $p < 0.01$, * $p < 0.05$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex_mxl, "tables_figures/bws_mxl.tex")