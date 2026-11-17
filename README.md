# dairy-methane-additives-adoption

This repository contains files that are necessary to replicate the results of the paper. Upon publication, the paper will be included here as well.

## Data
- `data/clean_data.csv` contains already cleaned and fully anonymous data.
- The data that support the findings of this study were collected via an online Qualtrics survey of U.S. dairy nutritionists conducted from November 7, 2025, until February 18, 2026. The data are not publicly available due to the confidentiality protections required by the IRB protocol governing this research (Cornell University IRB Protocol \#0150135). Anonymized data may be available from the corresponding author upon reasonable request.

## How to replicate
Run the scripts in order from the repository root:

1. `01_clean.R` – cleans raw survey data. This is not useful unless raw data is aquired.
2. `02_perceptions_attitudes.R` – Table 4
3. `03_concerns.R` – Tables 2 and 5
4. `epa_inventory/04_epa_inventory_figures.R` – Figure 1
5. `05_economic_feasibility.R` – Figure 3
6. 
7. `04_robustness.R` – Appendix Tables 6–9

## Output
Tables and figures are saved to `tables_figures/`
