# ################################################################# 
# Introductory/Background Figures
# ################################################################# 
# Elyse Gross 
# Apr 27, 2026
# R version 4.5.0

# ################################################################# 
# 1. Load packages & raw data

rm(list = ls())

# Set working directory
setwd("~/Desktop/EKGThesis")

# Load pacakges and install if necessary
required_packages <- c("tidyverse", "tidyr", "dplyr", "readxl", "ggplot2")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# ################################################################# 
# 1. Ag Sector Emissions

# Important Data
fig1_data <- read_excel("epa_inventory/emissions_ag_sector.xlsx")

# Reshape to long format
df <- fig1_data %>%
  filter(`...1` %in% c("Crop cultivation", "Livestock", "Fuel combustion")) %>%
  rename(category = `...1`) %>%
  pivot_longer(-category, names_to = "year", values_to = "emissions") %>%
  mutate(year = as.integer(year),
         category = factor(category, levels = c("Crop cultivation", "Livestock", "Fuel combustion")))

# Colors matching the figure
fill_colors <- c(
  "Crop cultivation" = "#2E6A9E",   # dark blue
  "Livestock"        = "#E07B3A",   # orange
  "Fuel combustion"  = "#2D5A1B"    # dark green
)

# Plot
p1 <- ggplot(df, aes(x = year, y = emissions, fill = category)) +
  geom_area(position = "stack") +
  scale_fill_manual(values = fill_colors) +
  scale_x_continuous(
    breaks = seq(1990, 2020, by = 5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 800, by = 100),
    limits = c(0, 800),
    expand = c(0, 0)
  ) +
  labs(
    title = "U.S. Emissions, Agricultural Sector",
    x = "Year",
    y = "MMT CO2 Eq.",
    fill = NULL
  ) +
  theme_bw() +
  theme(
    plot.title         = element_text(hjust = 0.5, size = 11, family = "serif", color = "black"),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.text        = element_text(size = 9, family = "serif", color = "black"),
    legend.key.size    = unit(0.4, "cm"),
    legend.margin      = margin(t = -5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(size = 9, family = "serif", color = "black"),
    axis.title         = element_text(size = 10, family = "serif", color = "black"),
    plot.margin        = margin(10, 15, 5, 10)
  ) +
  guides(fill = guide_legend(nrow = 1))

# Save as PDF for LaTeX
ggsave("tables_figures/figure1_ag_emissions.pdf", plot = p1, width = 6, height = 4, units = "in")

# ################################################################# 
# 2. Ag Emissions by Gas

fig2_data <- read_excel("epa_inventory/emissions_ag_bygas.xlsx")

colnames(fig2_data)[1] <- "Gas"

# make sure all are numeric
fig2_data <- fig2_data %>%
  mutate(across(-Gas, as.numeric))

# Reshape to long format
fig2_long <- fig2_data %>%
  pivot_longer(
    cols = -Gas,
    names_to = "Year",
    values_to = "Emissions"
  ) %>%
  mutate(Year = as.numeric(Year))%>%
  filter(Gas != "Total")

# Colors matching the figure
gas_colors <- c(
  "Nitrous oxide" = "#003366",  # dark navy blue
  "Methane"  = "#CC6600",  # orange
  "Carbon dioxide"        = "#336600"  # dark green
)

# Plot
p2 <- ggplot(fig2_long, aes(x = Year, y = Emissions, color = Gas)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = gas_colors) +
  scale_x_continuous(breaks = seq(1990, 2022, by = 5)) +
  scale_y_continuous(breaks = seq(0, 400, by = 50), limits = c(0, 400)) +
  labs(
    title = "U.S. Emissions, Agricultural Activities by Gas",
    x = "Year",
    y = "MMT CO2 Eq.",
    color = NULL
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
ggsave("tables_figures/figure2_gas.pdf", plot = p2, width = 6, height = 4, units = "in")


# ################################################################# 
# 3. Ag Methane Emissions by Species

fig3_data <- read_excel("epa_inventory/emissions_ag_livestockspecies.xlsx")

colnames(fig3_data)[1] <- "Species"

# make sure all are numeric
fig3_data <- fig3_data %>%
  mutate(across(-Species, as.numeric))

# Reshape to long format
fig3_long <- fig3_data %>%
  pivot_longer(
    cols = -Species,
    names_to = "Year",
    values_to = "Emissions"
  ) %>%
  mutate(Year = as.numeric(Year))

# Colors matching the figure
species_colors <- c(
  "Dairy Cattle" = "#003366",  # dark navy blue
  "Beef Cattle"  = "#CC6600",  # orange
  "Swine"        = "#336600",  # dark green
  "Other"        = "#0099CC"   # light blue
)

# Plot
p3 <- ggplot(fig3_long, aes(x = Year, y = Emissions, color = Species)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = species_colors) +
  scale_x_continuous(breaks = seq(1990, 2022, by = 5)) +
  scale_y_continuous(breaks = seq(0, 180, by = 20), limits = c(0, 180)) +
  labs(
    title = "U.S. Livestock Methane Emissions by Species",
    x = "Year",
    y = "MMT CO2 Eq.",
    color = NULL
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
ggsave("tables_figures/figure3_species.pdf", plot = p3, width = 6, height = 4, units = "in")

# ################################################################# 
# 4. Enteric Emissions of Dairy Cattle over time

fig4_data <- read_excel("epa_inventory/enteric_emissions_ag_species.xlsx")

colnames(fig4_data)[1] <- "Species"

# Convert columns to numeric
fig4_data <- fig4_data %>%
  mutate(across(-Species, as.numeric))

# Filter to Dairy Cattle and reshape to long format
dairy_long <- fig4_data %>%
  filter(Species == "Dairy Cattle") %>%
  pivot_longer(
    cols = -Species,
    names_to = "Year",
    values_to = "Emissions"
  ) %>%
  mutate(Year = as.numeric(Year))

# Plot
p4 <- ggplot(dairy_long, aes(x = Year, y = Emissions)) +
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
ggsave("tables_figures/figure4_enteric.pdf", plot = p4, width = 6, height = 4, units = "in")