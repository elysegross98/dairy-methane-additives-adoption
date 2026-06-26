# ################################################################# 
# Best Worst - Hausman-McFadden Test for IIA
# ################################################################# 
# Elyse Gross 
# Mar 30, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

rm(list = ls())

setwd("~/Desktop/EKGThesis")

required_packages <- c("dplyr", "tidyr", "support.BWS", "survival", "broom", "tibble", "mlogit", "kableExtra")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Import Data
clean_data <- read.csv("Data/clean_data.csv")

# ################################################################# 
# 2. Clean Data for BWS

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
# 7. Fit conditional logit model

attr_cols    <- attr(bws_ready, "vnames")
attr_cols_bt <- paste0("`", attr_cols, "`")

bws_formula <- as.formula(
  paste("RES ~", paste(attr_cols_bt, collapse = " + "), "+ strata(STR)")
)

bws_model <- clogit(bws_formula, data = bws_ready, method = "efron")
print(summary(bws_model))


# #################################################################
# 8. Build mlogit model for Hausman-McFadden Test

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

# Confirm conditional logit & mlogit models match 
  #(meaning the IIA test on mlogit applies to the support.BWS model)
cbind(
  clogit = coef(bws_model)[1:6],
  mlogit = coef(bws_mxl_fixed)
)

# ################################################################# 
# 9. Hausman-McFadden Test of IIA

attr_to_questions <- list(
  "1" = c(1, 4, 6),  # Consumer Acceptance
  "2" = c(1, 3, 5),  # Rumen Health
  "3" = c(1, 2, 7),  # Effect on Milk Components
  "4" = c(2, 4, 5),  # Cost to Farmer
  "5" = c(2, 3, 6),  # Expected Return
  "6" = c(3, 4, 7),  # Feed Intake
  "7" = c(5, 6, 7)   # Milk Production (your reference — skip)
)

hausman_results <- list()

for (attr_num in names(attr_to_questions)) {
  
  questions_to_drop <- attr_to_questions[[attr_num]]
  
  bws_ml_restricted <- bws_ml %>%
    filter(!( (as.numeric(STR) - 1) %% nquestions + 1 ) %in% questions_to_drop)
  
  bws_mlogit_r <- dfidx(
    bws_ml_restricted,
    idx    = list(c("STR", "ID"), "ALT"),
    choice = "RES"
  )
  
  tryCatch({
    bws_mxl_restricted <- mlogit(
      mxl_formula,
      data = bws_mlogit_r
    )
    
    ht <- hmftest(bws_mxl_fixed, bws_mxl_restricted)
    hausman_results[[paste0("Drop_attr_", attr_num)]] <- ht
    cat("\n--- Dropping attribute", attr_num, "---\n")
    print(ht)
    
  }, error = function(e) {
    cat("\nDropping attribute", attr_num, "failed:", e$message, "\n")
  })
}

# Summary of Hausman results
hausman_summary <- data.frame(
  Attribute = names(hausman_results),
  ChiSq     = sapply(hausman_results, function(x) round(x$statistic, 4)),
  df        = sapply(hausman_results, function(x) x$parameter),
  p_value   = sapply(hausman_results, function(x) round(x$p.value, 4))
)
rownames(hausman_summary) <- NULL
print(hausman_summary)

# ################################################################# 
# 10. LaTeX Table for Hausman-McFadden Test of IIA

hausman_summary %>%
  mutate(
    Attribute = c(
      "Consumer Acceptance",
      "Rumen Health",
      "Effect on Milk Components",
      "Cost to Farmer",
      "Expected Return to Farmer",
      "Feed Intake/Palatability",
      "Effect on Milk Production (yield)"
    )
  ) %>%
  rename(`Attribute Dropped` = Attribute,
         `$\\chi^2$`         = ChiSq,
         `df`                = df,
         `\\textit{p}-value` = p_value) %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    caption  = "Hausman-McFadden Tests for Independence of Irrelevant Alternatives",
    label    = "hausman",
    align    = c("l", "c", "c", "c")
  ) %>%
  kable_styling(
    latex_options = c("hold_position"),
    full_width    = FALSE,
    position      = "center"
  ) %>%
  footnote(
    general = paste(
      "Null hypothesis is that IIA holds.",
      "No test statistic is significant at $p < 0.05$,",
      "providing no evidence of IIA violation.",
      "Tests follow Hausman and McFadden (1984)."
    ),
    general_title  = "",
    threeparttable = TRUE,
    escape         = FALSE
  ) %>%
  save_kable("tables_figures/hausman_iia_table.tex")
