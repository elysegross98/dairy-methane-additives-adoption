# ################################################################# 
# Economic Feasibility Figure
# ################################################################# 
# September 27, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages

rm(list = ls()) 

# Load libraries
required_packages <- c("scales", "ggplot2")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# #################################################################
# 2. Build dataset

carbon_prices <- seq(0, 125, by = 1)

cost_levels <- c("$0.04 (Essential Oils, 10% reduction)", "$0.30 (3-NOP, 30% reduction)")

# Annual costs: $/day * 365
# $0.04 * 365 = $14.60
# $0.30 * 365 = $109.50

# Breakevens: annual_cost / credits_per_cow_per_year
# Essential oils: 10% reduction -> 12.8 kg CH4/cow/yr -> 0.0128 t x GWP 27 = 0.35 credits
#   $14.60 / 0.35 = $41.71
# 3-NOP: 30% reduction -> 38.4 kg CH4/cow/yr -> 0.0384 t x GWP 27 = 1.04 credits
#   $109.50 / 1.04 = $105.28

intercepts <- c("$0.04 (Essential Oils, 10% reduction)" = -14.60,
                "$0.30 (3-NOP, 30% reduction)"         = -109.50)

slopes <- c("$0.04 (Essential Oils, 10% reduction)" = 0.35,
            "$0.30 (3-NOP, 30% reduction)"         = 1.04)

lines_data <- data.frame(
  carbon_price = rep(carbon_prices, 2),
  cost = rep(cost_levels, each = length(carbon_prices))
)
lines_data$cost <- factor(lines_data$cost, levels = cost_levels)
lines_data$margin <- intercepts[as.character(lines_data$cost)] +
  slopes[as.character(lines_data$cost)] * lines_data$carbon_price

# Breakeven points
breakevens <- data.frame(
  x       = c(41.71, 105.28),
  label   = c("Breakeven at $41.71", "Breakeven at $105.28"),
  cost    = factor(cost_levels, levels = cost_levels),
  label_x = c(55, 60),
  label_y = c(60, -80)
)

cost_colors <- c(
  "$0.04 (Essential Oils, 10% reduction)" = "#0072B2",
  "$0.30 (3-NOP, 30% reduction)"         = "#D55E00"
)

# #################################################################
# 3. Plot

ggplot(lines_data, aes(x = carbon_price, y = margin, color = cost)) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.6) +
  
  geom_point(data = breakevens, aes(x = x, y = 0, color = cost), size = 2.5, show.legend = FALSE) +
  
  geom_text(data = breakevens,
            aes(x = label_x, y = label_y, label = label, color = cost),
            fontface = "bold", size = 3.5, hjust = 0, show.legend = FALSE) +
  
  geom_segment(data = breakevens,
               aes(x = label_x, xend = x,
                   y = label_y * 0.85, yend = 2,
                   color = cost),
               arrow = arrow(length = unit(0.2, "cm")), linewidth = 0.5, show.legend = FALSE) +
  
  scale_color_manual(
    values = cost_colors,
    name = "Feed additive cost, $/head/d",
    guide = guide_legend(override.aes = list(linetype = 1, shape = 15, size = 5))
  ) +
  scale_x_continuous(
    breaks = seq(0, 125, by = 25),
    labels = dollar_format(prefix = "$"),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-150, 150, by = 50),
    labels = function(x) ifelse(x < 0, paste0("(", dollar(abs(x)), ")"), dollar(x)),
    limits = c(-150, 150)
  ) +
  labs(
    x = "Carbon price received by dairy farmer, $ per metric ton CO2e",
    y = "Difference from baseline scenario margin,\n$/cow/year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.border     = element_rect(fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    axis.title.y     = element_text(face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 10),
    plot.margin      = margin(10, 15, 10, 10)
  )
ggsave("tables_figures/economic_feasibility.pdf", width = 8, height = 5, units = "in")
