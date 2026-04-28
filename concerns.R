# ################################################################# 
# Concerns
# ################################################################# 
# Elyse Gross 
# Mar 30, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

rm(list = ls()) 

setwd("~/Desktop/EKGThesis")

# Load libraries
required_packages <- c("kableExtra", "tidyverse", "showtext")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Import Data
clean_data <- read.csv("Data/clean_data.csv")

# ################################################################# 
# 2. Set up data

# Attribute map
additive_levels <- c("3-NOP", "Essential Oils", "Seaweed", "Tannins")

concern_labels <- c(
  concern_1 = "Cost to Farmer",
  concern_2 = "Expected Return to Farmer",
  concern_3 = "Rumen Health",
  concern_4 = "Palatability/Feed Intake",
  concern_5 = "Milk Production",
  concern_6 = "Components",
  concern_7 = "Milk Safety",
  concern_8 = "Meat/Carcass",
  concern_9 = "Consumer Acceptance"
)
concern_vars <- names(concern_labels)

additive_map <- c("1" = "3-NOP", "2" = "Seaweed", "3" = "Essential Oils", "4" = "Tannins")

concerns_data <- clean_data %>%
  select(all_of(concern_vars))

N_resp <- clean_data %>%
  select(all_of(concern_vars), age, gender, education, region,
         number.of.cows, attitude.climate, attitude.methane) %>%
  filter(if_any(everything(), ~ . != "")) %>%
  nrow()

# Long format with binary selection per respondent x additive x concern
concerns_long_full <- concerns_data %>%
  mutate(respondent_id = row_number()) %>%
  pivot_longer(cols = all_of(concern_vars),
               names_to = "concern", values_to = "additives_raw") %>%
  separate_rows(additives_raw, sep = ",") %>%
  mutate(additives_raw = trimws(additives_raw)) %>%
  filter(additives_raw != "", !is.na(additives_raw)) %>%
  mutate(additive_name = additive_map[additives_raw],
         selected = 1L) %>%
  filter(!is.na(additive_name))

# Full binary matrix (all respondent x concern x additive combinations)
binary_full <- concerns_data %>%
  mutate(respondent_id = row_number()) %>%
  pivot_longer(cols = all_of(concern_vars),
               names_to = "concern", values_to = "additives_raw") %>%
  select(respondent_id, concern) %>%
  distinct() %>%
  cross_join(tibble(additive_name = additive_levels)) %>%
  left_join(concerns_long_full %>% select(respondent_id, concern, additive_name, selected),
            by = c("respondent_id", "concern", "additive_name")) %>%
  mutate(selected = replace_na(selected, 0L))

# ################################################################# 
# 3. Percentages

concern_percents <- concerns_long_full %>%
  count(additive_name, concern) %>%
  mutate(percent = n / N_resp * 100) %>%
  rename(additives = additive_name)

# ################################################################# 
# 4. Statistical testing: Pairwise t-Tests (Between additives)
pairwise_by_concern <- map_dfr(concern_vars, function(cv) {
  df <- binary_full %>% filter(concern == cv)
  pairs <- combn(additive_levels, 2, simplify = FALSE)
  
  map_dfr(pairs, function(pair) {
    x <- df %>% filter(additive_name == pair[1]) %>% pull(selected)
    y <- df %>% filter(additive_name == pair[2]) %>% pull(selected)
    test <- t.test(x, y, paired = TRUE) # LPM approach
    tibble(concern = cv, A = pair[1], B = pair[2], p_raw = test$p.value)
  })
}) %>%
  group_by(concern) %>%
  mutate(p_adj = p.adjust(p_raw, method = "bonferroni"), sig = p_adj < 0.05) %>%
  ungroup()

# ################################################################# 
# 5. Statistical testing: Pairwise t-Tests (Between concerns)
pairwise_by_additive <- map_dfr(additive_levels, function(add) {
  df <- binary_full %>% filter(additive_name == add)
  pairs <- combn(concern_vars, 2, simplify = FALSE)
  
  map_dfr(pairs, function(pair) {
    x <- df %>% filter(concern == pair[1]) %>% pull(selected)
    y <- df %>% filter(concern == pair[2]) %>% pull(selected)
    test <- t.test(x, y, paired = TRUE)
    tibble(additive = add, A = pair[1], B = pair[2], p_raw = test$p.value)
  })
}) %>%
  group_by(additive) %>%
  mutate(p_adj = p.adjust(p_raw, method = "bonferroni"), sig = p_adj < 0.05) %>%
  ungroup()

# ################################################################# 
# 6. Assign Letters For Differences

assign_letters <- function(items, sig_pairs_df, col_A = "A", col_B = "B") {
  
  # If no significant pairs, everyone gets "a" (all the same)
  if (nrow(sig_pairs_df) == 0) {
    return(set_names(rep("a", length(items)), items))
  }
  
  # Build a set of significantly different pairs for easy lookup
  sig_set <- sig_pairs_df %>%
    mutate(pair = map2(get(col_A), get(col_B), ~ sort(c(.x, .y))),
           key  = map_chr(pair, paste, collapse = "|||")) %>%
    pull(key)
  
  is_sig <- function(a, b) {
    paste(sort(c(a, b)), collapse = "|||") %in% sig_set
  }
  
  # Represent assignments as a list of sets (one set of letters per item)
  letter_sets <- set_names(lapply(items, function(x) "a"), items)
  
  all_letters <- c(letters, paste0(letters, "2"))  # extend if needed
  next_letter_idx <- 2  # start issuing new letters from "b"
  
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (i in seq_len(nrow(sig_pairs_df))) {
      a_item <- sig_pairs_df[[col_A]][i]
      b_item <- sig_pairs_df[[col_B]][i]
      
      shared <- intersect(letter_sets[[a_item]], letter_sets[[b_item]])
      
      if (length(shared) > 0) {
        # Remove shared letters from b_item
        letter_sets[[b_item]] <- setdiff(letter_sets[[b_item]], shared)
        
        # If b_item now has no letters, assign a new one
        if (length(letter_sets[[b_item]]) == 0) {
          new_let <- all_letters[next_letter_idx]
          next_letter_idx <- next_letter_idx + 1
          letter_sets[[b_item]] <- new_let
        }
        changed <- TRUE
      }
    }
  }
  
  # Collapse each item's letter set into a single string (e.g. "ab")
  sapply(letter_sets, paste, collapse = "")
}

# Letters: additives within each concern (between-additive comparisons)
letter_df_concern <- map_dfr(concern_vars, function(cv) {
  sig_pairs <- pairwise_by_concern %>%
    filter(concern == cv, sig) %>%
    select(A, B)
  
  lets <- assign_letters(additive_levels, sig_pairs)
  
  tibble(
    concern  = cv,
    additives = additive_levels,
    letter_concern = unname(lets)
  )
}) %>%
  mutate(concern = concern_labels[concern])

# Letters: concerns within each additive (within-additive comparisons)
letter_df_additive <- map_dfr(additive_levels, function(add) {
  sig_pairs <- pairwise_by_additive %>%
    filter(additive == add, sig) %>%
    select(A, B)
  
  lets <- assign_letters(concern_vars, sig_pairs)
  
  tibble(
    additive = add,
    concern  = concern_vars,
    letter_additive = unname(lets)
  )
}) %>%
  mutate(concern = concern_labels[concern])

# ################################################################# 
# 7. Table

# Make the table
concern_table <- concern_percents %>%
  mutate(concern = concern_labels[concern]) %>%
  select(additives, concern, percent) %>%
  pivot_wider(names_from = concern, values_from = percent) %>%
  rename(Additives = additives) %>%
  mutate(across(where(is.numeric), ~ paste0(sprintf("%.1f", .), "%")))

# Save table for latex
save_kable(
  kable(concern_table,
        caption  = "Percentage of Respondents Expressing Each Concern by Feed Additive",
        align    = "lccccccccc",
        format   = "latex",
        booktabs = TRUE) %>%
    kable_styling(
      full_width    = FALSE,
      position      = "center",
      latex_options = c("scale_down")
    ) %>%
    footnote(
      general        = paste0("Note: N = ", N_resp, " respondents. Respondents could select multiple concerns. Percentages use N = ", N_resp, " as denominator."),
      threeparttable = TRUE,
      general_title  = "",
      escape         = FALSE
    ),
  file = "tables_figures/final_concern_table.tex"
)

# ################################################################# 
# 8. Make figure

plot_data <- concern_percents %>%
  mutate(concern = concern_labels[concern]) %>%
  group_by(concern) %>%
  mutate(mean_percent = mean(percent, na.rm = TRUE)) %>%
  ungroup() %>%
  # Sorts the bars from most concerning to least concerning
  mutate(concern = fct_reorder(concern, mean_percent, .desc = TRUE)) %>%
  left_join(letter_df_concern, by = c("concern", "additives")) %>%
  left_join(letter_df_additive, by = c("concern", "additives" = "additive")) %>%
  mutate(
    label_combined = paste0(
      ifelse(is.na(letter_concern) | letter_concern == "", "", letter_concern),
      ifelse(
        (!is.na(letter_concern) & letter_concern != "") & 
          (!is.na(letter_additive) & letter_additive != ""), 
        "/", ""
      ),
      ifelse(is.na(letter_additive) | letter_additive == "", "", letter_additive)
    )
  )

p <- ggplot(plot_data, aes(x = concern, y = percent, fill = additives)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.85), width = 0.75) +
  geom_text(aes(label = label_combined, y = percent + 1.5), 
            position = position_dodge(width = 0.85), vjust = 0, size = 4.5, 
            family = "serif", fontface = "bold") +
  scale_fill_manual(values = c("3-NOP" = "#0072B2", "Essential Oils" = "#D55E00", 
                               "Seaweed" = "#009E73", "Tannins" = "#CC79A7")) +
  labs(
    x = "Concern Category",
    y = "Percentage of Respondents (%)",
    fill = "Feed Additive"
    # ← caption removed entirely
  ) +
  theme_classic(base_size = 14, base_family = "serif") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), legend.position = "top")

ggsave("tables_figures/concerns.pdf", plot = p, width = 10, height = 7)


font_add("Times New Roman", regular = "Times New Roman.ttf")
showtext_auto()

ggsave("tables_figures/concerns.pdf", plot = p, width = 10, height = 7)
ggsave("tables_figures/concerns.pdf", plot = p, width = 15, height = 10)

# ################################################################# 
# 9. Statistical Testing Tables (From t-Tests)

# Appendix Table: Significant Differences Between Additives within each Concern

    # Between-additive pairwise tests (across additives within each concern)
    t_test_between <- pairwise_by_concern %>%
      filter(sig) %>%
      mutate(
        concern_name  = concern_labels[concern],
        p_raw_display = ifelse(p_raw < 0.01, "< 0.01", as.character(round(p_raw, 4))),
        p_adj_display = ifelse(p_adj < 0.01, "< 0.01", as.character(round(p_adj, 4)))
      ) %>%
      select(concern_name, A, B, p_raw_display, p_adj_display) %>%
      rename(
        "Concern Category" = concern_name,
        "Additive A"       = A,
        "Additive B"       = B,
        "p-value (raw)"    = p_raw_display,
        "p-value (Bonferroni)" = p_adj_display
      )

    # Save between-additive table
    save_kable(
      kable(t_test_between,
            caption  = "Significant Pairwise t-Test Results: Between Additives Within Each Concern",
            align    = "lcccc",
            format   = "latex",
            booktabs = TRUE) %>%
        kable_styling(full_width = FALSE, position = "center",
                      latex_options = "scale_down") %>%
        footnote(
          general        = "Note: Only significant pairs shown (Bonferroni-corrected $p < 0.05$). Tests follow a Linear Probability Model approach.",
          threeparttable = TRUE,
          general_title  = "",
          escape         = FALSE
        ),
      file = "tables_figures/appendix_t_tests_by_concern.tex"
    )

# Appendix Table: Significant Differences Between Additives within each Concern

    # Within-additive pairwise tests (across concerns within each additive)
    t_test_within <- pairwise_by_additive %>%
      filter(sig) %>%
      mutate(
        concern_A     = concern_labels[A],
        concern_B     = concern_labels[B],
        p_raw_display = ifelse(p_raw < 0.01, "< 0.01", as.character(round(p_raw, 4))),
        p_adj_display = ifelse(p_adj < 0.01, "< 0.01", as.character(round(p_adj, 4)))
      ) %>%
      select(additive, concern_A, concern_B, p_raw_display, p_adj_display) %>%
      rename(
        "Feed Additive"        = additive,
        "Concern A"            = concern_A,
        "Concern B"            = concern_B,
        "p-value (raw)"        = p_raw_display,
        "p-value (Bonferroni)" = p_adj_display
      )
    

    # Save within-additive table
    save_kable(
      kable(t_test_within,
            caption  = "Significant Pairwise t-Test Results: Between Concerns Within Each Additive",
            align    = "lcccc",
            format   = "latex",
            booktabs = TRUE) %>%
        kable_styling(full_width = FALSE, position = "center",
                      latex_options = "scale_down") %>%
        footnote(
          general        = "Note: Only significant pairs shown (Bonferroni-corrected $p < 0.05$). Tests follow a Linear Probability Model approach.",
          threeparttable = TRUE,
          general_title  = "",
          escape         = FALSE
        ),
      file = "tables_figures/appendix_t_tests_by_additive.tex"
    )