# Load required packages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(broom)
  library(ggplot2)
})

# Paths
normalized_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/panel_prea_alleged_normalized.csv"
compliance_path_excel <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"
if (!dir.exists(output_data_dir)) dir.create(output_data_dir, recursive = TRUE)
if (!dir.exists(output_fig_dir)) dir.create(output_fig_dir, recursive = TRUE)

# Helper
normalize_state <- function(x) {
  x <- str_trim(as.character(x))
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  x <- str_to_title(x)
  x <- str_replace_all(x, "Washington,? D\\.?C\\.?", "District of Columbia")
  x <- str_replace_all(x, "D\\.?c\\.?", "District of Columbia")
  x
}

# Read z-score panel
cat("Reading normalized panel...\n")
panel_norm <- read_csv(normalized_path, show_col_types = FALSE) %>%
  mutate(state = normalize_state(state))

# Aggregate z-score per state
state_zscores <- panel_norm %>%
  group_by(state) %>%
  summarise(
    avg_z_score = mean(z_score, na.rm = TRUE),
    .groups = "drop"
  )

cat("States with z-scores:", nrow(state_zscores), "\n")

# Read PREA compliance data (years 2015-2018)
cat("Reading PREA compliance worksheet...\n")
library(readxl)
prea_compliance <- read_excel(compliance_path_excel, sheet = "PREA_Certification_vs_Assurance")
prea_compliance$State <- normalize_state(prea_compliance$State)

# dynamically select compliance years 2015-2024 if present
year_cols <- names(prea_compliance)[str_detect(names(prea_compliance), "^20\\d{2}$")]
year_cols_num <- suppressWarnings(as.numeric(year_cols))
compliance_years <- year_cols[!is.na(year_cols_num) & year_cols_num >= 2015 & year_cols_num <= 2024]
if (length(compliance_years) == 0) {
  stop("No compliance year columns found between 2015 and 2024.")
}
cat("Using compliance year columns:", paste(compliance_years, collapse = ", "), "\n")

prea_compliance_summary <- prea_compliance %>%
  select(State, all_of(compliance_years)) %>%
  mutate(
    years_compliance = rowSums(select(., all_of(compliance_years)) == "1", na.rm = TRUE)
  ) %>%
  select(state = State, years_compliance) %>%
  filter(!is.na(state), state != "")

cat("States with compliance info:", nrow(prea_compliance_summary), "\n")

# Merge datasets
merged <- state_zscores %>%
  inner_join(prea_compliance_summary, by = "state")

cat("Merged states:", nrow(merged), "\n")

# Regression
model <- lm(avg_z_score ~ years_compliance, data = merged)
cat("\n=== Regression: avg_z_score ~ years_compliance ===\n")
print(summary(model))

# Save tidy output
tidy_path <- file.path(output_data_dir, "regression_zscore_on_compliance.csv")
write_csv(tidy(model), tidy_path)
cat("Tidy regression saved to:", tidy_path, "\n")

# Scatter plot
p <- ggplot(merged, aes(x = years_compliance, y = avg_z_score)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#2C7FB8") +
  labs(
    title = "Average Alleged Z-score vs PREA Compliance Years",
    subtitle = paste0("Adj. R² = ", round(summary(model)$adj.r.squared, 3)),
    x = "Years of PREA Compliance (2015-2018)",
    y = "Average z-score (normalized alleged rate)",
    caption = "Source: PREA panel (2012-2018), compliance worksheet"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

fig_path <- file.path(output_fig_dir, "regression_zscore_on_compliance.png")
ggsave(fig_path, p, width = 9, height = 6, dpi = 300)
cat("Plot saved to:", fig_path, "\n")

# Summary table
cat("\nTop states by avg z-score:\n")
print(merged %>% arrange(desc(avg_z_score)) %>% select(state, avg_z_score, years_compliance) %>% head(10))

cat("\nBottom states by avg z-score:\n")
print(merged %>% arrange(avg_z_score) %>% select(state, avg_z_score, years_compliance) %>% head(10))

cat("\nScript complete.\n")

