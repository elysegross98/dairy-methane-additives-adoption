# ################################################################# 
# Best Worst - Hausman-McFadden Test for IIA
# ################################################################# 
# Oct 15, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

rm(list = ls())

required_packages <- c("dplyr", "tidyr", "support.BWS", "survival", "broom", "tibble", "mlogit", "kableExtra")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Import Data
clean_data <- read.csv("clean_data.csv")

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
cbind(
  clogit = coef(bws_model)[1:6],
  mlogit = coef(bws_mxl_fixed)
)

# ################################################################# 
# 9. Hausman-McFadden Test of IIA

make_formula <- function(omit) {
  as.formula(paste("RES ~ 0 +",
                   paste0("`", attr_cols[-omit], "`", collapse = " + ")))
}

# Full model with attribute 6 as reference (only needed when dropping attribute 7)
bws_mxl_ref6 <- mlogit(make_formula(6), data = bws_mlogit)

hausman_results <- list()

for (x in 1:7) {
  
  # Reference attribute: 7 normally, 6 when attribute 7 is the one dropped
  ref    <- if (x == 7) 6 else 7
  full_m <- if (ref == 7) bws_mxl_fixed else bws_mxl_ref6
  
  # Drop every choice set in which attribute x appears
  strata_with_x <- bws_ml %>%
    group_by(STR) %>%
    filter(any(.data[[attr_cols[x]]] != 0)) %>%
    pull(STR) %>%
    unique()
  
  bws_ml_restricted <- bws_ml %>% filter(!STR %in% strata_with_x)
  
  # Check: Attribute x must be completely absent now
  stopifnot(all(bws_ml_restricted[[attr_cols[x]]] == 0))
  
  bws_mlogit_r <- dfidx(
    bws_ml_restricted,
    idx    = list(c("STR", "ID"), "ALT"),
    choice = "RES"
  )
  
  # Restricted model omits both the dropped attribute and the reference
  restricted_m <- mlogit(make_formula(c(x, ref)), data = bws_mlogit_r)
  
  ht <- hmftest(full_m, restricted_m)
  hausman_results[[paste0("Drop_attr_", x)]] <- ht
  
  cat("\n--- Dropping attribute", x, "(reference =", ref, ") ---\n")
  cat("Coefficients in restricted model:", length(coef(restricted_m)), "\n")
  print(ht)
}

hausman_summary <- data.frame(
  Attribute = names(hausman_results),
  ChiSq     = sapply(hausman_results, function(x) round(x$statistic, 4)),
  df        = sapply(hausman_results, function(x) x$parameter),
  p_value   = sapply(hausman_results, function(x) round(x$p.value, 4))
)
rownames(hausman_summary) <- NULL
print(hausman_summary)

stopifnot(all(hausman_summary$df == 5))

# ################################################################# 
# 10. LaTeX Table for Hausman-McFadden Test of IIA

hausman_summary %>%
  select(-df) %>%
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
      "All tests have 5 degrees of freedom.",
      "Tests follow Hausman and McFadden (1984).",
      "The test dropping Cost to Farmer rejects at $p < 0.05$;",
      "the remaining six do not."
    ),
    general_title  = "",
    threeparttable = TRUE,
    escape         = FALSE
  ) %>%
  save_kable("tables_figures/hausman_iia_table.tex")
