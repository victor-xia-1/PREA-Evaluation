# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(broom)
})

# Paths
excel_path <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
data_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/all_states_alleged_substantiated_per_1000_2012_2018_long.csv"
output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"

# Ensure output directories exist
if (!dir.exists(output_data_dir)) dir.create(output_data_dir, recursive = TRUE)
if (!dir.exists(output_fig_dir)) dir.create(output_fig_dir, recursive = TRUE)

# Helper: normalize state names
normalize_state <- function(x) {
  x <- str_trim(as.character(x))
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  x <- str_to_title(x)
  x <- str_replace_all(x, "Washington,? D\\.?C\\.?", "District of Columbia")
  x <- str_replace_all(x, "D\\.?c\\.?", "District of Columbia")
  x
}

# Read PREA compliance data
cat("Reading PREA compliance data...\n")
prea_compliance <- read_excel(excel_path, sheet = "PREA_Certification_vs_Assurance")

# Clean state names
prea_compliance$State <- normalize_state(prea_compliance$State)

# Filter to years 2015-2024 if available
year_cols <- names(prea_compliance)[str_detect(names(prea_compliance), "^20\\d{2}$")]
year_nums <- suppressWarnings(as.numeric(year_cols))
compliance_years <- year_cols[!is.na(year_nums) & year_nums >= 2015 & year_nums <= 2024]
if (length(compliance_years) == 0) stop("No compliance columns between 2015 and 2024 found.")
cat("Using compliance columns:", paste(compliance_years, collapse = ", "), "\n")

# Calculate years of compliance
prea_compliance_summary <- prea_compliance %>%
  select(State, all_of(compliance_years)) %>%
  mutate(
    years_compliance = rowSums(
      select(., all_of(compliance_years)) == "1", 
      na.rm = TRUE
    )
  ) %>%
  select(State, years_compliance) %>%
  filter(!is.na(State), State != "")

cat("\nPREA Compliance Summary:\n")
print(summary(prea_compliance_summary$years_compliance))
cat("\nStates with compliance data:", nrow(prea_compliance_summary), "\n")

# Read alleged_per_1000 data
cat("\nReading alleged_per_1000 data...\n")
alleged_data <- read_csv(data_path, show_col_types = FALSE)

# Calculate average alleged_per_1000 for each state (across all years 2012-2018)
state_alleged_avg <- alleged_data %>%
  filter(!is.na(alleged_per_1000)) %>%
  group_by(state) %>%
  summarise(
    avg_alleged_per_1000 = mean(alleged_per_1000, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  ) %>%
  mutate(state = normalize_state(state))

cat("\nAlleged per 1000 Summary:\n")
print(summary(state_alleged_avg$avg_alleged_per_1000))
cat("\nStates with alleged data:", nrow(state_alleged_avg), "\n")

# Merge compliance and alleged data
merged_data <- state_alleged_avg %>%
  inner_join(prea_compliance_summary, by = c("state" = "State"))

cat("\nMerged data - States in both datasets:", nrow(merged_data), "\n")

# Run regression: alleged_per_1000 ~ years_compliance
cat("\nRunning regression: avg_alleged_per_1000 ~ years_compliance\n")
model <- lm(avg_alleged_per_1000 ~ years_compliance, data = merged_data)

# Display regression results
cat("\n=== REGRESSION RESULTS ===\n")
print(summary(model))

# Get tidy regression output
tidy_results <- tidy(model)
cat("\nTidy regression results:\n")
print(tidy_results)

# Save regression results
regression_output_path <- file.path(output_data_dir, "regression_alleged_on_prea_compliance.csv")
write_csv(tidy_results, regression_output_path)
cat("\nRegression results saved to:", regression_output_path, "\n")

# Save merged data for inspection
merged_output_path <- file.path(output_data_dir, "merged_alleged_prea_compliance.csv")
write_csv(merged_data, merged_output_path)
cat("Merged data saved to:", merged_output_path, "\n")

# Create scatter plot with regression line
library(ggplot2)

p <- ggplot(merged_data, aes(x = years_compliance, y = avg_alleged_per_1000)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#2C7FB8") +
  labs(
    title = "Regression: Average Alleged per 1000 vs. Years of PREA Compliance (2015-2018)",
    subtitle = paste0("R² = ", round(summary(model)$r.squared, 3), 
                     ", p-value = ", format.pval(coef(summary(model))[2,4], digits = 3)),
    x = "Years of PREA Compliance (2015-2018)",
    y = "Average Alleged per 1000 (2012-2018)",
    caption = "Source: PREA data workbook"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# Save plot
fig_path <- file.path(output_fig_dir, "regression_alleged_on_prea_compliance.png")
ggsave(filename = fig_path, plot = p, width = 10, height = 7, dpi = 300)
cat("\nPlot saved to:", fig_path, "\n")

# Additional analysis: correlation
correlation <- cor(merged_data$years_compliance, merged_data$avg_alleged_per_1000, use = "complete.obs")
cat("\nCorrelation coefficient:", round(correlation, 4), "\n")

# Show top and bottom states
cat("\nTop 5 states by average alleged per 1000:\n")
print(merged_data %>% 
  arrange(desc(avg_alleged_per_1000)) %>% 
  select(state, avg_alleged_per_1000, years_compliance) %>% 
  head(5))

cat("\nBottom 5 states by average alleged per 1000:\n")
print(merged_data %>% 
  arrange(avg_alleged_per_1000) %>% 
  select(state, avg_alleged_per_1000, years_compliance) %>% 
  head(5))

cat("\nStates with most years of compliance:\n")
print(merged_data %>% 
  arrange(desc(years_compliance)) %>% 
  select(state, years_compliance, avg_alleged_per_1000) %>% 
  head(10))

cat("\nStates with least years of compliance:\n")
print(merged_data %>% 
  arrange(years_compliance) %>% 
  select(state, years_compliance, avg_alleged_per_1000) %>% 
  head(10))

