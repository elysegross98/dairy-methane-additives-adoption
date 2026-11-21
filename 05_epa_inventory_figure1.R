# ################################################################# 
# Introductory/Background Figures
# ################################################################# 
# Apr 27, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

rm(list = ls())


# Load pacakges and install if necessary
required_packages <- c("tidyverse", "tidyr", "dplyr", "readxl", "ggplot2")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


# ################################################################# 
# Figure 1: Enteric Emissions of Dairy Cattle over time

fig1_data <- read_excel("epa_inventory/enteric_emissions_ag_species.xlsx")

colnames(fig1_data)[1] <- "Species"

# Convert columns to numeric
fig1_data <- fig1_data %>%
  mutate(across(-Species, as.numeric))

# Filter to Dairy Cattle and reshape to long format
dairy_long <- fig1_data %>%
  filter(Species == "Dairy Cattle") %>%
  pivot_longer(
    cols = -Species,
    names_to = "Year",
    values_to = "Emissions"
  ) %>%
  mutate(Year = as.numeric(Year))

# Plot
p1 <- ggplot(dairy_long, aes(x = Year, y = Emissions)) +
  geom_line(color = "#2E6D8E", linewidth = 0.8) +
  scale_x_continuous(
    breaks = seq(1990, 2020, by = 5),
    limits = c(1990, 2022)
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 10),
    limits = c(0, 52)
  ) +
  labs(
    title = "U.S. Methane Emissions, Enteric Fermentation from Dairy Cattle",
    x = "Year",
    y = "MMT CO2 Eq."
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(color = "grey70"),
    plot.title       = element_text(hjust = 0.5, size = 11, family = "serif", color = "black"),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 9, family = "serif", color = "black"),
    legend.key.width = unit(1.5, "cm"),
    axis.text        = element_text(size = 9, family = "serif", color = "black"),
    axis.title       = element_text(size = 10, family = "serif", color = "black"),
    plot.margin      = margin(10, 15, 10, 10)
  )

# Save as PDF for LaTeX
ggsave("tables_figures/figure1_enteric.pdf", plot = p4, width = 6, height = 4, units = "in")
