# ################################################################# 
# Economic Feasibility Figure
# ################################################################# 
# Elyse Gross 
# April 19, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages

rm(list = ls()) 

setwd("~/Desktop/EKGThesis")

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

# #################################################################
# 2. Build dataset

carbon_prices <- seq(0, 125, by = 1)

cost_levels <- c("$0.04 (Essential Oils)", "$0.30 (3-NOP)")

# Annual costs: $/day * 365
# $0.04 * 365 = $14.60
# $0.30 * 365 = $109.50

# Breakevens: annual_cost / 1.15 credits
# $14.60 / 1.15 = $12.70
# $109.50 / 1.15 = $95.22

intercepts <- c("$0.04 (Essential Oils)" = -14.60,
                "$0.30 (3-NOP)"         = -109.50)

slopes <- c("$0.04 (Essential Oils)" = 1.15,
            "$0.30 (3-NOP)"         = 1.15)

lines_data <- data.frame(
  carbon_price = rep(carbon_prices, 2),
  cost = rep(cost_levels, each = length(carbon_prices))
)
lines_data$cost <- factor(lines_data$cost, levels = cost_levels)
lines_data$margin <- intercepts[as.character(lines_data$cost)] +
  slopes[as.character(lines_data$cost)] * lines_data$carbon_price

# Breakeven points
breakevens <- data.frame(
  x       = c(12.70, 95.22),
  label   = c("Breakeven at $12.70", "Breakeven at $95.22"),
  cost    = factor(cost_levels, levels = cost_levels),
  label_x = c(20, 60),
  label_y = c(60, -80)
)

cost_colors <- c(
  "$0.04 (Essential Oils)" = "#0072B2",
  "$0.30 (3-NOP)"         = "#D55E00"
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
    panel.border     = element_rect(color = "#0072B2", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    axis.title.y     = element_text(color = "#D55E00", face = "bold"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 10),
    plot.margin      = margin(10, 15, 10, 10)
  )
ggsave("tables_figures/economic_feasibility.pdf", width = 8, height = 5, units = "in")