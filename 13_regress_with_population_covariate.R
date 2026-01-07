# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(broom)
  library(ggplot2)
  library(purrr)
})

# Paths
excel_path <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
panel_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/all_states_alleged_substantiated_per_1000_2012_2018_long.csv"
staff_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/state_correctional_officer_employment_avg.csv"
output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"

if (!dir.exists(output_data_dir)) dir.create(output_data_dir, recursive = TRUE)
if (!dir.exists(output_fig_dir)) dir.create(output_fig_dir, recursive = TRUE)

normalize_state <- function(x) {
  x <- str_trim(as.character(x))
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  x <- str_to_title(x)
  x <- str_replace_all(x, "Washington,? D\\.?C\\.?", "District of Columbia")
  x <- str_replace_all(x, "D\\.?c\\.?", "District of Columbia")
  x
}

# Population table (2020 Census)
state_populations <- data.frame(
  state = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia",
            "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
            "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina",
            "North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
            "Virginia","Washington","West Virginia","Wisconsin","Wyoming"),
  population_2020 = c(5024,733,7151,3012,39538,5774,
                      3606,990,689,21538,10712,
                      1452,1838,12821,6786,3190,2938,4506,
                      4658,1362,6177,7029,10037,5706,
                      2961,6155,1084,1962,3101,1378,
                      9289,2118,20201,10439,779,
                      11799,3959,4237,13003,1097,
                      5118,886,6911,29146,3272,643,
                      8630,7705,1794,5894,577),
  stringsAsFactors = FALSE
) %>%
  mutate(
    population_actual = population_2020 * 1000,
    population_millions = population_2020 / 1000
  )

cat("State population data loaded.\n")

# Compliance data
cat("\nReading PREA compliance data...\n")
prea_compliance <- read_excel(excel_path, sheet = "PREA_Certification_vs_Assurance")
prea_compliance$State <- normalize_state(prea_compliance$State)
year_cols <- names(prea_compliance)[str_detect(names(prea_compliance), "^20\\d{2}$")]
year_nums <- suppressWarnings(as.numeric(year_cols))
compliance_years <- year_cols[!is.na(year_nums) & year_nums >= 2015 & year_nums <= 2024]
if (length(compliance_years) == 0) stop("No compliance columns between 2015 and 2024 found.")
cat("Using compliance columns:", paste(compliance_years, collapse = ", "), "\n")
prea_compliance_summary <- prea_compliance %>%
  select(State, all_of(compliance_years)) %>%
  mutate(
    years_compliance = rowSums(select(., all_of(compliance_years)) == "1", na.rm = TRUE)
  ) %>%
  select(State, years_compliance) %>%
  filter(!is.na(State), State != "")

# PREA outcome data
cat("\nReading alleged/substantiated data...\n")
panel_data <- read_csv(panel_path, show_col_types = FALSE)
state_avg <- panel_data %>%
  group_by(state) %>%
  summarise(
    avg_alleged_per_1000 = mean(alleged_per_1000, na.rm = TRUE),
    avg_substantiated_per_1000 = mean(substantiated_per_1000, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  ) %>%
  mutate(state = normalize_state(state))

# Incarceration counts from PREA workbook
cat("\nExtracting incarceration counts from PREA workbook...\n")
sheet_names <- sort(readxl::excel_sheets(excel_path))
year_sheets <- sheet_names[str_detect(sheet_names, "^20(1[2-8])$")]
extract_incarceration <- function(sheet_name) {
  df <- read_excel(excel_path, sheet = sheet_name, col_names = FALSE)
  header_row <- NA_integer_
  prisoners_col <- NA_integer_
  for (i in 1:25) {
    for (j in 1:ncol(df)) {
      cell <- as.character(df[[j]][i])
      if (!is.na(cell) && str_detect(tolower(cell), "prisoners in custody")) {
        header_row <- i
        prisoners_col <- j
        break
      }
    }
    if (!is.na(header_row)) break
  }
  if (is.na(prisoners_col)) return(tibble())
  rows <- list()
  for (r in (header_row + 1):nrow(df)) {
    st <- normalize_state(df[[2]][r])
    prisoners <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(df[[prisoners_col]][r]))))
    if (!is.na(st) && !is.na(prisoners)) {
      rows[[length(rows)+1]] <- tibble(state = st, year = as.integer(sheet_name), prisoners = prisoners)
    }
  }
  bind_rows(rows)
}
incarceration_counts <- map_dfr(year_sheets, extract_incarceration)
state_incarceration <- incarceration_counts %>%
  group_by(state) %>%
  summarise(avg_prisoners = mean(prisoners, na.rm = TRUE), .groups = "drop") %>%
  inner_join(state_populations, by = "state") %>%
  mutate(incarceration_rate_per_100k = (avg_prisoners / population_actual) * 100000) %>%
  select(state, avg_prisoners, incarceration_rate_per_100k, population_millions)
cat("Computed incarceration metrics for", nrow(state_incarceration), "states.\n")

# Staffing averages
staffing_avg <- read_csv(staff_path, show_col_types = FALSE) %>%
  mutate(state = normalize_state(state),
         avg_correctional_staff = as.numeric(avg_correctional_staff))
cat("Loaded staffing averages for", nrow(staffing_avg), "states.\n")

# Merge everything
merged_data <- state_avg %>%
  inner_join(prea_compliance_summary, by = c("state" = "State")) %>%
  inner_join(state_incarceration, by = "state") %>%
  inner_join(staffing_avg %>% select(state, avg_correctional_staff), by = "state") %>%
  mutate(
    staff_per_1000_inmates = ifelse(avg_prisoners > 0,
                                    (avg_correctional_staff / avg_prisoners) * 1000,
                                    NA_real_)
  )

cat("\nMerged data - states:", nrow(merged_data), "\n")

# Regression 1: Alleged
cat("\n", strrep("=", 60), "\nREGRESSION 1: ALLEGED PER 1000\n", strrep("=", 60), "\n", sep = "")
model1_alleged <- lm(avg_alleged_per_1000 ~ years_compliance, data = merged_data)
model2_alleged <- lm(avg_alleged_per_1000 ~ years_compliance + population_millions + staff_per_1000_inmates,
                     data = merged_data)
cat("\nModel 1 (baseline):\n"); print(summary(model1_alleged))
cat("\nModel 2 (population + staffing):\n"); print(summary(model2_alleged))
cat("\nModel Comparison:\n")
cat("Model 1 R²:", round(summary(model1_alleged)$r.squared, 4), "\n")
cat("Model 2 R²:", round(summary(model2_alleged)$r.squared, 4), "\n")
cat("R² improvement:", round(summary(model2_alleged)$r.squared - summary(model1_alleged)$r.squared, 4), "\n")
write_csv(tidy(model2_alleged), file.path(output_data_dir, "regression_alleged_with_population.csv"))

# Regression 2: Substantiated
cat("\n", strrep("=", 60), "\nREGRESSION 2: SUBSTANTIATED PER 1000\n", strrep("=", 60), "\n", sep = "")
model1_substantiated <- lm(avg_substantiated_per_1000 ~ years_compliance, data = merged_data)
model2_substantiated <- lm(avg_substantiated_per_1000 ~ years_compliance + population_millions + staff_per_1000_inmates,
                           data = merged_data)
cat("\nModel 1 (baseline):\n"); print(summary(model1_substantiated))
cat("\nModel 2 (population + staffing):\n"); print(summary(model2_substantiated))
cat("\nModel Comparison:\n")
cat("Model 1 R²:", round(summary(model1_substantiated)$r.squared, 4), "\n")
cat("Model 2 R²:", round(summary(model2_substantiated)$r.squared, 4), "\n")
cat("R² improvement:", round(summary(model2_substantiated)$r.squared - summary(model1_substantiated)$r.squared, 4), "\n")
write_csv(tidy(model2_substantiated), file.path(output_data_dir, "regression_substantiated_with_population.csv"))

# Save merged data
write_csv(merged_data, file.path(output_data_dir, "merged_data_with_population.csv"))
cat("\nMerged dataset with population & staffing saved.\n")

# Visualizations
p1 <- ggplot(merged_data, aes(x = years_compliance, y = avg_alleged_per_1000,
                              size = staff_per_1000_inmates, color = population_millions)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#2C7FB8") +
  scale_size_continuous(name = "Staff per 1,000 inmates", range = c(2, 10)) +
  scale_color_gradient(name = "Population (millions)", low = "#E8E8E8", high = "#2C7FB8") +
  labs(
    title = "Alleged per 1000 vs PREA Compliance",
    subtitle = paste0("Model 2 R² = ", round(summary(model2_alleged)$r.squared, 3)),
    x = "Years of PREA compliance (2015-2024)",
    y = "Average alleged per 1000 (2012-2018)",
    caption = "Sources: PREA workbook, US Census 2020, BLS OES"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_fig_dir, "regression_alleged_with_population.png"), p1,
       width = 10, height = 7, dpi = 300)

p2 <- ggplot(merged_data, aes(x = years_compliance, y = avg_substantiated_per_1000,
                              size = staff_per_1000_inmates, color = population_millions)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "#E31A1C") +
  scale_size_continuous(name = "Staff per 1,000 inmates", range = c(2, 10)) +
  scale_color_gradient(name = "Population (millions)", low = "#FEE0D2", high = "#E31A1C") +
  labs(
    title = "Substantiated per 1000 vs PREA Compliance",
    subtitle = paste0("Model 2 R² = ", round(summary(model2_substantiated)$r.squared, 3)),
    x = "Years of PREA compliance (2015-2024)",
    y = "Average substantiated per 1000 (2012-2018)",
    caption = "Sources: PREA workbook, US Census 2020, BLS OES"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_fig_dir, "regression_substantiated_with_population.png"), p2,
       width = 10, height = 7, dpi = 300)

cat("\nVisualizations saved.\n")

# Summary statistics
cat("\n", strrep("=", 60), "\nSUMMARY STATISTICS\n", strrep("=", 60), "\n", sep = "")
cat("\nPopulation statistics:\n"); print(summary(merged_data$population_millions))
cat("\nStaff per 1,000 inmates statistics:\n"); print(summary(merged_data$staff_per_1000_inmates))

cat("\nCorrelation matrix:\n")
cor_matrix <- cor(merged_data[, c("years_compliance", "population_millions",
                                  "staff_per_1000_inmates",
                                  "avg_alleged_per_1000", "avg_substantiated_per_1000")],
                  use = "complete.obs")
print(round(cor_matrix, 3))
# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(broom)
  library(ggplot2)
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

# State population data (2020 Census estimates, in thousands)
# Source: US Census Bureau, 2020 Census
state_populations <- data.frame(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
            "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", 
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
            "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
            "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
            "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  population_2020 = c(5024, 733, 7151, 3012, 39538, 5774, 3606, 990, 689, 21538, 
                      10712, 1452, 1838, 12821, 6786, 3190, 2938, 4506, 4658, 1362, 
                      6177, 7029, 10037, 5706, 2961, 6155, 1084, 1962, 3101, 1378, 
                      9289, 2118, 20201, 10439, 779, 11799, 3959, 4237, 13003, 1097, 
                      5118, 886, 6911, 29146, 3272, 643, 8630, 7705, 1794, 5894, 577),
  stringsAsFactors = FALSE
)

# Convert to millions for easier interpretation
state_populations$population_millions <- state_populations$population_2020 / 1000

cat("State population data loaded (2020 Census)\n")
cat("Population range:", min(state_populations$population_millions), "to", 
    max(state_populations$population_millions), "million\n")

# Read PREA compliance data
cat("\nReading PREA compliance data...\n")
prea_compliance <- read_excel(excel_path, sheet = "PREA_Certification_vs_Assurance")

# Clean state names
prea_compliance$State <- normalize_state(prea_compliance$State)

# Automatically select available compliance years from 2015-2024
year_cols <- names(prea_compliance)[str_detect(names(prea_compliance), "^20\\d{2}$")]
year_nums <- suppressWarnings(as.numeric(year_cols))
compliance_years <- year_cols[!is.na(year_nums) & year_nums >= 2015 & year_nums <= 2024]
if (length(compliance_years) == 0) stop("No compliance columns between 2015 and 2024 found.")
cat("Using compliance columns:", paste(compliance_years, collapse = ", "), "\n")

# Calculate years of compliance for each state (2015-2018)
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

# Read alleged and substantiated data
cat("\nReading alleged and substantiated data...\n")
all_data <- read_csv(data_path, show_col_types = FALSE)

# Calculate averages for each state
state_avg <- all_data %>%
  group_by(state) %>%
  summarise(
    avg_alleged_per_1000 = mean(alleged_per_1000, na.rm = TRUE),
    avg_substantiated_per_1000 = mean(substantiated_per_1000, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  ) %>%
  mutate(state = normalize_state(state))

# Merge all data
merged_data <- state_avg %>%
  inner_join(prea_compliance_summary, by = c("state" = "State")) %>%
  inner_join(state_populations, by = "state")

cat("\nMerged data - States in all datasets:", nrow(merged_data), "\n")

# ===== REGRESSION 1: ALLEGED PER 1000 =====
cat("\n" , strrep("=", 60), "\n")
cat("REGRESSION 1: ALLEGED PER 1000\n")
cat(strrep("=", 60), "\n")

# Model 1: Without population
model1_alleged <- lm(avg_alleged_per_1000 ~ years_compliance, data = merged_data)

# Model 2: With population
model2_alleged <- lm(avg_alleged_per_1000 ~ years_compliance + population_millions, data = merged_data)

cat("\nModel 1 (without population):\n")
print(summary(model1_alleged))

cat("\nModel 2 (with population):\n")
print(summary(model2_alleged))

# Compare models
cat("\nModel Comparison:\n")
cat("Model 1 R²:", round(summary(model1_alleged)$r.squared, 4), "\n")
cat("Model 2 R²:", round(summary(model2_alleged)$r.squared, 4), "\n")
cat("R² improvement:", round(summary(model2_alleged)$r.squared - summary(model1_alleged)$r.squared, 4), "\n")

# Save results
tidy_results_alleged <- tidy(model2_alleged)
write_csv(tidy_results_alleged, file.path(output_data_dir, "regression_alleged_with_population.csv"))

# ===== REGRESSION 2: SUBSTANTIATED PER 1000 =====
cat("\n" , strrep("=", 60), "\n")
cat("REGRESSION 2: SUBSTANTIATED PER 1000\n")
cat(strrep("=", 60), "\n")

# Model 1: Without population
model1_substantiated <- lm(avg_substantiated_per_1000 ~ years_compliance, data = merged_data)

# Model 2: With population
model2_substantiated <- lm(avg_substantiated_per_1000 ~ years_compliance + population_millions, data = merged_data)

cat("\nModel 1 (without population):\n")
print(summary(model1_substantiated))

cat("\nModel 2 (with population):\n")
print(summary(model2_substantiated))

# Compare models
cat("\nModel Comparison:\n")
cat("Model 1 R²:", round(summary(model1_substantiated)$r.squared, 4), "\n")
cat("Model 2 R²:", round(summary(model2_substantiated)$r.squared, 4), "\n")
cat("R² improvement:", round(summary(model2_substantiated)$r.squared - summary(model1_substantiated)$r.squared, 4), "\n")

# Save results
tidy_results_substantiated <- tidy(model2_substantiated)
write_csv(tidy_results_substantiated, file.path(output_data_dir, "regression_substantiated_with_population.csv"))

# Save merged dataset
write_csv(merged_data, file.path(output_data_dir, "merged_data_with_population.csv"))
cat("\nMerged dataset with population saved.\n")

# Create visualizations
# 1. Alleged with population
p1 <- ggplot(merged_data, aes(x = years_compliance, y = avg_alleged_per_1000, 
                              size = population_millions, color = population_millions)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#2C7FB8", size = 1) +
  scale_size_continuous(name = "Population\n(millions)", range = c(2, 8)) +
  scale_color_gradient(name = "Population\n(millions)", low = "#E8E8E8", high = "#2C7FB8") +
  labs(
    title = "Alleged per 1000 vs. PREA Compliance (with Population Covariate)",
    subtitle = paste0("R² = ", round(summary(model2_alleged)$r.squared, 3)),
    x = "Years of PREA Compliance (2015-2018)",
    y = "Average Alleged per 1000 (2012-2018)",
    caption = "Source: PREA data workbook, US Census Bureau 2020"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(output_fig_dir, "regression_alleged_with_population.png"), 
       plot = p1, width = 10, height = 7, dpi = 300)

# 2. Substantiated with population
p2 <- ggplot(merged_data, aes(x = years_compliance, y = avg_substantiated_per_1000, 
                              size = population_millions, color = population_millions)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#E31A1C", size = 1) +
  scale_size_continuous(name = "Population\n(millions)", range = c(2, 8)) +
  scale_color_gradient(name = "Population\n(millions)", low = "#E8E8E8", high = "#E31A1C") +
  labs(
    title = "Substantiated per 1000 vs. PREA Compliance (with Population Covariate)",
    subtitle = paste0("R² = ", round(summary(model2_substantiated)$r.squared, 3)),
    x = "Years of PREA Compliance (2015-2018)",
    y = "Average Substantiated per 1000 (2012-2018)",
    caption = "Source: PREA data workbook, US Census Bureau 2020"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(output_fig_dir, "regression_substantiated_with_population.png"), 
       plot = p2, width = 10, height = 7, dpi = 300)

cat("\nVisualizations saved.\n")

# Summary statistics
cat("\n" , strrep("=", 60), "\n")
cat("SUMMARY STATISTICS\n")
cat(strrep("=", 60), "\n")
cat("\nPopulation statistics:\n")
print(summary(merged_data$population_millions))

cat("\nCorrelation matrix:\n")
cor_matrix <- cor(merged_data[, c("years_compliance", "population_millions", 
                                  "avg_alleged_per_1000", "avg_substantiated_per_1000")], 
                  use = "complete.obs")
print(round(cor_matrix, 3))

