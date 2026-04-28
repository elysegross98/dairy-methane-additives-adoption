# ################################################################# 
# Best Worst - Robustness Checks
#     Region, Sell & Use, 
# ################################################################# 
# Elyse Gross 
# Mar 30, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

# Clear 
rm(list = ls())

# Set working directory
setwd("~/Desktop/EKGThesis")

# Install and load packages
required_packages <- c("dplyr", "tidyr", "support.BWS", "survival", "broom", "knitr", "kableExtra")
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

# Keep only data needed for bws
bw_data <- clean_data %>%
  select(ResponseId, starts_with("bw."), region, ends_with(".sell"), ends_with(".use"), number.of.cows) %>%
  mutate(across(starts_with("bw."), ~ as.numeric(.) %>% replace(. == 2, -1))) %>% 
  filter(!if_all(starts_with("bw."), is.na)) 

# Identify and remove partial respondents (didn't answer both parts of every bws question)
partial_ids <- bw_data %>%
  pivot_longer(-c(ResponseId, region, number.of.cows, ends_with(".sell"), ends_with(".use")), names_to = c("Question", "Local"),
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

# Recoding use/sell scales
bw_data <- bw_data %>%
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

# #################################################################
# 3. NE versus rest Robustness Check

# Subset data
bw_data_ne   <- bw_data[bw_data$region == "1", ]
bw_data_else <- bw_data[bw_data$region != "1", ]

# Shared setup (identical for both runs)
attribute_names <- c(
  "Consumer Acceptance",              # 1
  "Rumen Health",                     # 2
  "Effect on Milk Components",        # 3
  "Cost to Farmer",                   # 4
  "Expected Return to Farmer",        # 5
  "Feed Intake/Palatability",         # 6
  "Effect on Milk Production (yield)" # 7
)

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
# NE Region (region == "1")

    bws_input_ne <- bw_data_ne %>%
      mutate(ID = row_number())
    
    for(q in 1:7) {
      cols <- paste0("bw.", q, "_", 1:3)
      vals <- bws_input_ne[, cols]
      vals <- apply(vals, 2, as.numeric)
      bws_input_ne[[paste0("b", q)]] <- apply(vals, 1, function(x) which(x == 1)[1])
      bws_input_ne[[paste0("w", q)]] <- apply(vals, 1, function(x) which(x == -1)[1])
    }
    
    bws_input_ne <- bws_input_ne %>%
      select(ID, matches("^[bw][0-9]+$"))
    
    best_cols     <- paste0("b", 1:7)
    worst_cols    <- paste0("w", 1:7)
    response_cols <- c(rbind(best_cols, worst_cols))
    
    stopifnot(!anyNA(bws_input_ne))
    
    bws_ready_ne <- bws.dataset(
      data          = bws_input_ne,
      response      = response_cols,
      response.type = 1,
      choice.sets   = design,
      design.type   = 2,
      item.names    = attribute_names,
      id            = "ID",
      model         = "maxdiff"
    )
    
    bws_counts_ne <- bws.count(bws_ready_ne)
    print(bws_counts_ne)
    
    bw_scores_ne <- bws_counts_ne$aggregate %>%
      rownames_to_column("Attribute") %>%
      select(Attribute, B, W, BW) %>%
      arrange(desc(BW))
    
    attr_cols_ne    <- attr(bws_ready_ne, "vnames")
    attr_cols_bt_ne <- paste0("`", attr_cols_ne, "`")
    
    bws_formula_ne <- as.formula(
      paste("RES ~", paste(attr_cols_bt_ne, collapse = " + "), "+ strata(STR)")
    )
    
    bws_model_ne <- clogit(bws_formula_ne, data = bws_ready_ne, method = "efron")
    print(summary(bws_model_ne))

# #################################################################
# Non-NE Region (region != "1")

    bws_input_else <- bw_data_else %>%
      mutate(ID = row_number())
    
    for(q in 1:7) {
      cols <- paste0("bw.", q, "_", 1:3)
      vals <- bws_input_else[, cols]
      vals <- apply(vals, 2, as.numeric)
      bws_input_else[[paste0("b", q)]] <- apply(vals, 1, function(x) which(x == 1)[1])
      bws_input_else[[paste0("w", q)]] <- apply(vals, 1, function(x) which(x == -1)[1])
    }
    
    bws_input_else <- bws_input_else %>%
      select(ID, matches("^[bw][0-9]+$"))
    
    stopifnot(!anyNA(bws_input_else))
    
    bws_ready_else <- bws.dataset(
      data          = bws_input_else,
      response      = response_cols,
      response.type = 1,
      choice.sets   = design,
      design.type   = 2,
      item.names    = attribute_names,
      id            = "ID",
      model         = "maxdiff"
    )
    
    bws_counts_else <- bws.count(bws_ready_else)
    print(bws_counts_else)
    
    bw_scores_else <- bws_counts_else$aggregate %>%
      rownames_to_column("Attribute") %>%
      select(Attribute, B, W, BW) %>%
      arrange(desc(BW))
    
    attr_cols_else    <- attr(bws_ready_else, "vnames")
    attr_cols_bt_else <- paste0("`", attr_cols_else, "`")
    
    bws_formula_else <- as.formula(
      paste("RES ~", paste(attr_cols_bt_else, collapse = " + "), "+ strata(STR)")
    )
    
    bws_model_else <- clogit(bws_formula_else, data = bws_ready_else, method = "efron")
    print(summary(bws_model_else))
    
    
# Table for NE vs non-NE
    
    format_coef <- function(est, lo, hi) {
      if (is.na(est)) return("(reference)")
      sprintf("%.3f [%.3f, %.3f]", est, lo, hi)
    }
    
    coef_ne <- tidy(bws_model_ne, conf.int = TRUE) %>%
      select(term, estimate, conf.low, conf.high) %>%
      mutate(term = gsub("`", "", term))
    
    coef_else <- tidy(bws_model_else, conf.int = TRUE) %>%
      select(term, estimate, conf.low, conf.high) %>%
      mutate(term = gsub("`", "", term))
    
    # Use attribute_names to set row order, add reference row
    ref_row <- tibble(term = "Effect on Milk Production (yield)",
                      estimate = NA, conf.low = NA, conf.high = NA)
    
    coef_combined <- full_join(coef_ne, coef_else, by = "term", suffix = c("_ne", "_else")) %>%
      mutate(
        term = gsub("`", "", term),
        `NE Region`     = mapply(format_coef, estimate_ne,   conf.low_ne,   conf.high_ne),
        `Non-NE Region` = mapply(format_coef, estimate_else, conf.low_else, conf.high_else)
      ) %>%
      arrange(desc(estimate_ne)) %>%
      select(term, `NE Region`, `Non-NE Region`) %>%
      rename(Attribute = term)
    
    coef_combined %>%
      kable(
        format   = "latex",
        booktabs = TRUE,
        escape   = TRUE,
        caption  = "Conditional Logit Coefficient Estimates by Region",
        label    = "bws_robustness_regional",
        align    = c("l", "l", "l")
      ) %>%
      kable_styling(latex_options = "hold_position", full_width = FALSE, position = "center") %>%
      footnote(
        general = "Coefficients from conditional logit model. 95\\\\% confidence intervals in brackets. Effect on Milk Production (yield) is the reference category.",
        general_title  = "",
        threeparttable = TRUE,
        escape         = FALSE
      ) %>%
      save_kable("tables_figures/bws_robustness_table_regional.tex")
    
# #################################################################
# 4. Sell/Use versus Rest Robustness Check
    
    # Create indicator: answered 1 to ANY column ending in .sell or .use
    sell_use_cols <- grep("\\.sell$|\\.use$", names(bw_data), value = TRUE)
    
    bw_data$any_sell_use <- as.integer(
      rowSums(bw_data[, sell_use_cols] == 1, na.rm = TRUE) > 0
    )
    
    # Subset data
    bw_data_yes  <- bw_data[bw_data$any_sell_use == 1, ]
    bw_data_no   <- bw_data[bw_data$any_sell_use == 0, ]
    
    # Shared setup (identical for both runs)
    attribute_names <- c(
      "Consumer Acceptance",              # 1
      "Rumen Health",                     # 2
      "Effect on Milk Components",        # 3
      "Cost to Farmer",                   # 4
      "Expected Return to Farmer",        # 5
      "Feed Intake/Palatability",         # 6
      "Effect on Milk Production (yield)" # 7
    )
    
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
    # Yes Group (answered 1 to any .sell or .use column)
    
    bws_input_yes <- bw_data_yes %>%
      mutate(ID = row_number())
    
    for(q in 1:7) {
      cols <- paste0("bw.", q, "_", 1:3)
      vals <- bws_input_yes[, cols]
      vals <- apply(vals, 2, as.numeric)
      bws_input_yes[[paste0("b", q)]] <- apply(vals, 1, function(x) which(x == 1)[1])
      bws_input_yes[[paste0("w", q)]] <- apply(vals, 1, function(x) which(x == -1)[1])
    }
    
    bws_input_yes <- bws_input_yes %>%
      select(ID, matches("^[bw][0-9]+$"))
    
    best_cols     <- paste0("b", 1:7)
    worst_cols    <- paste0("w", 1:7)
    response_cols <- c(rbind(best_cols, worst_cols))
    
    stopifnot(!anyNA(bws_input_yes))
    
    bws_ready_yes <- bws.dataset(
      data          = bws_input_yes,
      response      = response_cols,
      response.type = 1,
      choice.sets   = design,
      design.type   = 2,
      item.names    = attribute_names,
      id            = "ID",
      model         = "maxdiff"
    )
    
    bws_counts_yes <- bws.count(bws_ready_yes)
    print(bws_counts_yes)
    
    bw_scores_yes <- bws_counts_yes$aggregate %>%
      rownames_to_column("Attribute") %>%
      select(Attribute, B, W, BW) %>%
      arrange(desc(BW))
    
    attr_cols_yes    <- attr(bws_ready_yes, "vnames")
    attr_cols_bt_yes <- paste0("`", attr_cols_yes, "`")
    
    bws_formula_yes <- as.formula(
      paste("RES ~", paste(attr_cols_bt_yes, collapse = " + "), "+ strata(STR)")
    )
    
    bws_model_yes <- clogit(bws_formula_yes, data = bws_ready_yes, method = "efron")
    print(summary(bws_model_yes))
    
    mu_yes       <- coef(bws_model_yes)
    n_sim        <- 10000
    mu_clean_yes <- mu_yes[!is.na(mu_yes)]
    
    var_matrix_clean_yes <- vcov(bws_model_yes)
    var_matrix_clean_yes <- var_matrix_clean_yes[!is.na(mu_yes), !is.na(mu_yes)]
    var_matrix_clean_yes <- var_matrix_clean_yes + diag(1e-6, nrow(var_matrix_clean_yes))
    
    T_chol_yes <- t(chol(var_matrix_clean_yes))
    
    sim_matrix_yes <- matrix(NA, nrow = n_sim, ncol = length(mu_clean_yes))
    for (j in 1:n_sim) {
      M <- rnorm(length(mu_clean_yes))
      sim_matrix_yes[j, ] <- mu_clean_yes + T_chol_yes %*% M
    }
    
    sim_exp_yes   <- exp(sim_matrix_yes)
    denom_yes     <- rowSums(sim_exp_yes) + 1
    shares_yes    <- sim_exp_yes / denom_yes
    ref_share_yes <- 1 / denom_yes
    
    all_shares_yes <- cbind(shares_yes, ref_share_yes)
    colnames(all_shares_yes) <- c(names(mu_clean_yes), "Effect on Milk Production (yield)")
    
    results_bws_yes <- data.frame(
      Attribute = colnames(all_shares_yes),
      Lower95   = apply(all_shares_yes, 2, quantile, 0.025),
      Mean      = apply(all_shares_yes, 2, mean),
      Upper95   = apply(all_shares_yes, 2, quantile, 0.975)
    ) %>%
      arrange(desc(Mean))
    
    print(results_bws_yes)
    
    # #################################################################
    # No Group (answered 0/NA to all .sell and .use columns)
    
    bws_input_no <- bw_data_no %>%
      mutate(ID = row_number())
    
    for(q in 1:7) {
      cols <- paste0("bw.", q, "_", 1:3)
      vals <- bws_input_no[, cols]
      vals <- apply(vals, 2, as.numeric)
      bws_input_no[[paste0("b", q)]] <- apply(vals, 1, function(x) which(x == 1)[1])
      bws_input_no[[paste0("w", q)]] <- apply(vals, 1, function(x) which(x == -1)[1])
    }
    
    bws_input_no <- bws_input_no %>%
      select(ID, matches("^[bw][0-9]+$"))
    
    stopifnot(!anyNA(bws_input_no))
    
    bws_ready_no <- bws.dataset(
      data          = bws_input_no,
      response      = response_cols,
      response.type = 1,
      choice.sets   = design,
      design.type   = 2,
      item.names    = attribute_names,
      id            = "ID",
      model         = "maxdiff"
    )
    
    bws_counts_no <- bws.count(bws_ready_no)
    print(bws_counts_no)
    
    bw_scores_no <- bws_counts_no$aggregate %>%
      rownames_to_column("Attribute") %>%
      select(Attribute, B, W, BW) %>%
      arrange(desc(BW))
    
    attr_cols_no    <- attr(bws_ready_no, "vnames")
    attr_cols_bt_no <- paste0("`", attr_cols_no, "`")
    
    bws_formula_no <- as.formula(
      paste("RES ~", paste(attr_cols_bt_no, collapse = " + "), "+ strata(STR)")
    )
    
    bws_model_no <- clogit(bws_formula_no, data = bws_ready_no, method = "efron")
    print(summary(bws_model_no))
    
    mu_no       <- coef(bws_model_no)
    mu_clean_no <- mu_no[!is.na(mu_no)]
    
    var_matrix_clean_no <- vcov(bws_model_no)
    var_matrix_clean_no <- var_matrix_clean_no[!is.na(mu_no), !is.na(mu_no)]
    var_matrix_clean_no <- var_matrix_clean_no + diag(1e-6, nrow(var_matrix_clean_no))
    
    T_chol_no <- t(chol(var_matrix_clean_no))
    
    sim_matrix_no <- matrix(NA, nrow = n_sim, ncol = length(mu_clean_no))
    for (j in 1:n_sim) {
      M <- rnorm(length(mu_clean_no))
      sim_matrix_no[j, ] <- mu_clean_no + T_chol_no %*% M
    }
    
    sim_exp_no   <- exp(sim_matrix_no)
    denom_no     <- rowSums(sim_exp_no) + 1
    shares_no    <- sim_exp_no / denom_no
    ref_share_no <- 1 / denom_no
    
    all_shares_no <- cbind(shares_no, ref_share_no)
    colnames(all_shares_no) <- c(names(mu_clean_no), "Effect on Milk Production (yield)")
    
    results_bws_no <- data.frame(
      Attribute = colnames(all_shares_no),
      Lower95   = apply(all_shares_no, 2, quantile, 0.025),
      Mean      = apply(all_shares_no, 2, mean),
      Upper95   = apply(all_shares_no, 2, quantile, 0.975)
    ) %>%
      arrange(desc(Mean))
    
    print(results_bws_no)
    
    
    # #################################################################
    # Table for Yes vs No (sell/use)
    
    format_coef <- function(est, lo, hi) {
      if (is.na(est)) return("(reference)")
      sprintf("%.3f [%.3f, %.3f]", est, lo, hi)
    }
    
    coef_yes <- tidy(bws_model_yes, conf.int = TRUE) %>%
      select(term, estimate, conf.low, conf.high) %>%
      mutate(term = gsub("`", "", term))
    
    coef_no <- tidy(bws_model_no, conf.int = TRUE) %>%
      select(term, estimate, conf.low, conf.high) %>%
      mutate(term = gsub("`", "", term))
    
    ref_row <- tibble(term = "Effect on Milk Production (yield)",
                      estimate = NA, conf.low = NA, conf.high = NA)
    
    coef_combined_selluse <- full_join(coef_yes, coef_no, by = "term", suffix = c("_yes", "_no")) %>%
      mutate(
        term = gsub("`", "", term),
        `Currently Sells/Uses` = mapply(format_coef, estimate_yes, conf.low_yes, conf.high_yes),
        `Does Not Sell/Use`    = mapply(format_coef, estimate_no,  conf.low_no,  conf.high_no)
      ) %>%
      arrange(desc(estimate_yes)) %>%        # <-- sort by yes-group coefficient descending
      select(term, `Currently Sells/Uses`, `Does Not Sell/Use`) %>%
      rename(Attribute = term)
    
    coef_combined_selluse %>%
      kable(
        format   = "latex",
        booktabs = TRUE,
        escape   = TRUE,
        caption  = "Conditional Logit Coefficient Estimates by Sell/Use Status",
        label    = "bws_robustness_selluse",
        align    = c("l", "l", "l")
      ) %>%
      kable_styling(latex_options = "hold_position", full_width = FALSE, position = "center") %>%
      footnote(
        general = "Coefficients from conditional logit model. 95\\\\% confidence intervals in brackets. Effect on Milk Production (yield) is the reference category.",
        general_title  = "",
        threeparttable = TRUE,
        escape         = FALSE
      ) %>%
      save_kable("tables_figures/bws_robustness_table_selluse.tex")
    
    
# #################################################################
# 5. Number of Cows for bws respondents
    
    # Correcting number of cows to be able to count & summing
    bw_data$number.of.cows <- as.numeric(
      gsub(",", "", bw_data$number.of.cows)
    )
    
    total_cows <- sum(bw_data$number.of.cows, na.rm=TRUE)

    print(total_cows)
    
    # By region
    cows_by_region <- bw_data %>%
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
    
    print(cows_by_region)
