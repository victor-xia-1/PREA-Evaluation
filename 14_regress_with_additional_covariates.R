# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(broom)
  library(tibble)  # For rownames_to_column
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(lmtest)  # For clustered standard errors
  library(sandwich)  # For robust/clustered standard errors
})

# Paths
excel_path <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
data_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/all_states_alleged_substantiated_per_1000_2012_2020_long.csv"
staff_path_long <- "/Users/victorxia/Documents/Honors Thesis/data/clean/state_correctional_officer_employment_long.csv"
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

# State population data (2020 Census estimates, thousands of persons)
# Source: US Census Bureau, 2020 Census redistricting data
state_populations <- data.frame(
  state = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
            "Connecticut","Delaware","District of Columbia","Florida","Georgia",
            "Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
            "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
            "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire",
            "New Jersey","New Mexico","New York","North Carolina","North Dakota",
            "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
            "South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
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
)
state_populations <- state_populations %>%
  mutate(population_millions = population_2020 / 1000,
         population_actual = population_2020 * 1000)

cat("Loaded 2020 Census population data for", nrow(state_populations), "states.\n")

# Read PREA compliance data - need year-by-year compliance status
cat("\nReading PREA compliance data (year-by-year)...\n")
prea_compliance <- read_excel(excel_path, sheet = "PREA_Certification_vs_Assurance")
prea_compliance$State <- normalize_state(prea_compliance$State)

# Get all year columns (2012-2020 for our analysis period)
year_cols <- names(prea_compliance)[str_detect(names(prea_compliance), "^20\\d{2}$")]
year_nums <- suppressWarnings(as.numeric(year_cols))
analysis_years <- year_cols[!is.na(year_nums) & year_nums >= 2012 & year_nums <= 2020]

# Reshape to long format: state-year observations
prea_compliance_long <- prea_compliance %>%
  select(State, all_of(analysis_years)) %>%
  filter(!is.na(State), State != "") %>%
  pivot_longer(cols = all_of(analysis_years), 
               names_to = "year", 
               values_to = "prea_compliance_status") %>%
  mutate(
    year = as.integer(year),
    prea_compliant = case_when(
      prea_compliance_status == "1" ~ 1L,
      prea_compliance_status == "0" ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(State, year, prea_compliant, prea_compliance_status)

cat("Created panel of PREA compliance status for", nrow(prea_compliance_long), "state-year observations.\n")

# Read alleged/substantiated data (already in panel format)
cat("\nReading alleged/substantiated panel data...\n")
panel_data <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(state = normalize_state(state))

cat("Panel data contains", nrow(panel_data), "state-year observations.\n")

# Extract year-by-year incarceration counts
cat("\nExtracting incarceration counts from Excel sheets...\n")
all_sheets <- readxl::excel_sheets(excel_path)
year_sheets <- all_sheets[str_detect(all_sheets, "^20(1[2-9]|20)$")]
year_sheets <- sort(year_sheets)

extract_incarceration <- function(sheet_name) {
  df_raw <- readxl::read_excel(excel_path, sheet = sheet_name, col_names = FALSE)
  header_row <- NULL
  prisoners_col <- NULL
  for (i in 1:25) {
    for (j in 1:ncol(df_raw)) {
      cell <- as.character(df_raw[[j]][i])
      if (!is.na(cell) && str_detect(tolower(cell), "prisoners in custody")) {
        header_row <- i
        prisoners_col <- j
        break
      }
    }
    if (!is.null(header_row)) break
  }
  if (is.null(prisoners_col)) {
    warning("Could not find prisoners column for sheet ", sheet_name)
    return(tibble())
  }
  results <- list()
  for (r in (header_row + 1):nrow(df_raw)) {
    state_raw <- df_raw[[2]][r]
    if (!is.na(state_raw)) {
      state_name <- normalize_state(state_raw)
      if (!is.na(state_name) && state_name %in% state_populations$state) {
        prisoners <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(df_raw[[prisoners_col]][r]))))
        if (!is.na(prisoners)) {
          results[[length(results)+1]] <- tibble(
            state = state_name,
            year = as.integer(sheet_name),
            prisoners = prisoners
          )
        }
      }
    }
  }
  bind_rows(results)
}

incarceration_counts <- purrr::map_dfr(year_sheets, extract_incarceration)
incarceration_panel <- incarceration_counts %>%
  inner_join(state_populations, by = "state") %>%
  mutate(
    incarceration_rate_per_100k = (prisoners / population_actual) * 100000
  ) %>%
  select(state, year, prisoners, incarceration_rate_per_100k, population_millions)

cat("Computed incarceration rates for", nrow(incarceration_panel), "state-year observations.\n")

# Read year-by-year staffing data
cat("\nReading year-by-year staffing data...\n")
if (!file.exists(staff_path_long)) {
  cat("Warning: Long format staffing data not found. Using average staffing.\n")
  staff_path_avg <- "/Users/victorxia/Documents/Honors Thesis/data/clean/state_correctional_officer_employment_avg.csv"
  staffing_avg <- read_csv(staff_path_avg, show_col_types = FALSE) %>%
    mutate(state = normalize_state(state))
  
  # Merge with panel data to get year-by-year
  staffing_panel <- panel_data %>%
    select(state, year) %>%
    left_join(staffing_avg %>% select(state, avg_correctional_staff), by = "state") %>%
    rename(tot_emp = avg_correctional_staff)
} else {
  staffing_panel <- read_csv(staff_path_long, show_col_types = FALSE) %>%
    mutate(
      state = normalize_state(state),
      year = as.integer(year),
      tot_emp = as.numeric(tot_emp)
    ) %>%
    select(state, year, tot_emp)
}

cat("Loaded staffing data for", nrow(staffing_panel), "state-year observations.\n")

# Violent crime data using USArrests (FBI Uniform Crime Report, 1973)
# Note: This is time-invariant, but we'll include it as a control
data("USArrests")
violent_crime <- USArrests %>%
  as_tibble(rownames = "state") %>%
  mutate(
    state = normalize_state(state),
    violent_crime_rate_per_100k = Murder + Assault + Rape
  ) %>%
  select(state, violent_crime_rate_per_100k)

cat("USArrests violent crime data available for", nrow(violent_crime), "states.\n")

# Merge all data into panel format
cat("\n=== Merge Diagnostics ===\n")
cat("Starting with panel_data:", nrow(panel_data), "observations\n")

after_prea <- panel_data %>%
  inner_join(prea_compliance_long, by = c("state" = "State", "year" = "year"))
cat("After PREA compliance join:", nrow(after_prea), "observations\n")
cat("  Years:", paste(sort(unique(after_prea$year)), collapse = ", "), "\n")

after_incarceration <- after_prea %>%
  inner_join(incarceration_panel, by = c("state", "year"))
cat("After incarceration join:", nrow(after_incarceration), "observations\n")

after_staffing <- after_incarceration %>%
  left_join(staffing_panel, by = c("state", "year"))
cat("After staffing join:", nrow(after_staffing), "observations\n")

merged_panel <- after_staffing %>%
  left_join(violent_crime, by = "state") %>%
  mutate(
    staff_per_1000_inmates = ifelse(prisoners > 0 & !is.na(tot_emp),
                                    (tot_emp / prisoners) * 1000,
                                    NA_real_)
  ) %>%
  filter(!is.na(alleged_per_1000) | !is.na(substantiated_per_1000)) %>%
  filter(!is.na(prea_compliant))  # Keep only observations with known compliance status

cat("After filters:", nrow(merged_panel), "observations\n")
cat("\nMerged panel dataset size:", nrow(merged_panel), "state-year observations.\n")
cat("States:", length(unique(merged_panel$state)), "\n")
cat("Years:", paste(sort(unique(merged_panel$year)), collapse = ", "), "\n")

# Check for missing values in covariates
cat("\nMissing values in covariates:\n")
cat("  staff_per_1000_inmates:", sum(is.na(merged_panel$staff_per_1000_inmates)), "\n")
cat("  population_millions:", sum(is.na(merged_panel$population_millions)), "\n")
cat("  incarceration_rate_per_100k:", sum(is.na(merged_panel$incarceration_rate_per_100k)), "\n")
cat("  violent_crime_rate_per_100k:", sum(is.na(merged_panel$violent_crime_rate_per_100k)), "\n")

# Create cumulative years of PREA compliance (scalar)
# This counts how many years a state has been compliant up to and including the current year
# This captures the cumulative effect of compliance over time
merged_panel <- merged_panel %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(
    cumulative_years_compliance = cumsum(ifelse(is.na(prea_compliant), 0, prea_compliant)),
    # Ever treated by year t: 1 once a state has ever been compliant up to t
    EverTreated = as.integer(cummax(ifelse(is.na(prea_compliant), 0L, prea_compliant)) > 0L),
    # Lag outcomes (t-1)
    alleged_per_1000_tminus1 = dplyr::lag(alleged_per_1000, n = 1),
    substantiated_per_1000_tminus1 = dplyr::lag(substantiated_per_1000, n = 1),
    # Lag outcomes (t-2)
    alleged_per_1000_tminus2 = dplyr::lag(alleged_per_1000, n = 2),
    substantiated_per_1000_tminus2 = dplyr::lag(substantiated_per_1000, n = 2),
    # Lead outcomes (t+1)
    alleged_per_1000_tplus1 = dplyr::lead(alleged_per_1000, n = 1),
    substantiated_per_1000_tplus1 = dplyr::lead(substantiated_per_1000, n = 1)
  ) %>%
  ungroup()

cat("\nCreated cumulative years of PREA compliance variable.\n")
cat("Created EverTreated indicator (ever compliant up to year t).\n")
cat("Cumulative years of compliance summary:\n")
print(summary(merged_panel$cumulative_years_compliance))
cat("\nCumulative years distribution:\n")
print(table(merged_panel$cumulative_years_compliance, useNA = "ifany"))
cat("\nEverTreated distribution:\n")
print(table(merged_panel$EverTreated, useNA = "ifany"))

# ============================================================================
# PANEL REGRESSION MODELS WITH TWO-WAY FIXED EFFECTS
# ============================================================================
# Best practices for causal inference with panel data:
# 1. Two-way fixed effects (state + year) to control for:
#    - Unobserved time-invariant state characteristics
#    - Common time trends affecting all states
# 2. Clustered standard errors at the state level to account for:
#    - Serial correlation within states
#    - Heteroskedasticity
# 3. Time-varying treatment (prea_compliant indicator) to capture
#    the effect of compliance in each period
# ============================================================================

cat("\n=== Running Panel Regressions with Two-Way Fixed Effects ===\n")
cat("Model specification:\n")
cat("  - State fixed effects: Yes (controls for time-invariant state characteristics)\n")
cat("  - Year fixed effects: Yes (controls for common time trends)\n")
cat("  - Clustered standard errors: Yes (clustered at state level)\n")
cat("  - Treatment variable: cumulative_years_compliance (cumulative years of compliance)\n")
cat("  - Controls: population_millions, incarceration_rate_per_100k,\n")
cat("              staff_per_1000_inmates, violent_crime_rate_per_100k\n\n")

# Model 1: Alleged per 1000
# Using lm with factor variables for two-way fixed effects
# factor(state) and factor(year) create dummy variables for each state and year
# Using cumulative_years_compliance to capture cumulative effect of compliance over time
# Note: lm() automatically drops rows with missing values in any variable
# For staffing, we'll create a missing indicator and set missing values to 0 (or use mean imputation)
cat("\n=== Model Estimation ===\n")
cat("Observations before model estimation:", nrow(merged_panel), "\n")

# Note: We will use actual staffing data only - no imputation
# Observations with missing staffing will be excluded from the regression
cat("Observations with missing staffing data:", sum(is.na(merged_panel$staff_per_1000_inmates)), "\n")

cat("Rows with complete data for alleged model:", sum(complete.cases(merged_panel %>% 
  select(alleged_per_1000, cumulative_years_compliance, population_millions,
         incarceration_rate_per_100k, staff_per_1000_inmates, violent_crime_rate_per_100k))), "\n")

model_alleged <- lm(
  alleged_per_1000 ~ cumulative_years_compliance + population_millions + 
    incarceration_rate_per_100k + staff_per_1000_inmates + 
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = merged_panel
)
cat("Observations in alleged model:", nrow(model_alleged$model), "\n")

# Model 2: Substantiated per 1000
cat("Rows with complete data for substantiated model:", sum(complete.cases(merged_panel %>% 
  select(substantiated_per_1000, cumulative_years_compliance, population_millions,
         incarceration_rate_per_100k, staff_per_1000_inmates, violent_crime_rate_per_100k))), "\n")

model_substantiated <- lm(
  substantiated_per_1000 ~ cumulative_years_compliance + population_millions + 
    incarceration_rate_per_100k + staff_per_1000_inmates + 
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = merged_panel
)
cat("Observations in substantiated model:", nrow(model_substantiated$model), "\n")

# Calculate clustered standard errors at state level
# Using vcovCL from sandwich package
vcov_alleged <- vcovCL(model_alleged, cluster = ~state, type = "HC1")
vcov_substantiated <- vcovCL(model_substantiated, cluster = ~state, type = "HC1")

cat("\n=== Regression: Alleged per 1000 (with cumulative years of PREA compliance) ===\n")
# Use coeftest to get clustered standard errors
coeftest_alleged <- coeftest(model_alleged, vcov = vcov_alleged)
print(coeftest_alleged)

cat("\n=== Regression: Substantiated per 1000 (with cumulative years of PREA compliance) ===\n")
coeftest_substantiated <- coeftest(model_substantiated, vcov = vcov_substantiated)
print(coeftest_substantiated)

# Extract results for saving (excluding fixed effects dummies)
# coeftest returns a matrix, need to convert properly
results_alleged <- data.frame(
  term = rownames(coeftest_alleged),
  estimate = coeftest_alleged[, "Estimate"],
  std.error = coeftest_alleged[, "Std. Error"],
  statistic = coeftest_alleged[, "t value"],
  p.value = coeftest_alleged[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
)
# Filter out fixed effects
results_alleged <- results_alleged[
  !str_detect(results_alleged$term, "^factor\\(state\\)") & 
  !str_detect(results_alleged$term, "^factor\\(year\\)"),
]

results_substantiated <- data.frame(
  term = rownames(coeftest_substantiated),
  estimate = coeftest_substantiated[, "Estimate"],
  std.error = coeftest_substantiated[, "Std. Error"],
  statistic = coeftest_substantiated[, "t value"],
  p.value = coeftest_substantiated[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
)
# Filter out fixed effects
results_substantiated <- results_substantiated[
  !str_detect(results_substantiated$term, "^factor\\(state\\)") & 
  !str_detect(results_substantiated$term, "^factor\\(year\\)"),
]

# Save tidy outputs
write_csv(results_alleged, file.path(output_data_dir, "regression_alleged_panel_twfe_cumulative_2012_2020.csv"))
write_csv(results_substantiated, file.path(output_data_dir, "regression_substantiated_panel_twfe_cumulative_2012_2020.csv"))
write_csv(merged_panel, file.path(output_data_dir, "merged_panel_data_twfe_cumulative_2012_2020.csv"))

# Create comparison table manually
comparison_table <- data.frame(
  Variable = results_alleged$term,
  Alleged_Coef = results_alleged$estimate,
  Alleged_SE = results_alleged$std.error,
  Alleged_P = results_alleged$p.value,
  Substantiated_Coef = results_substantiated$estimate,
  Substantiated_SE = results_substantiated$std.error,
  Substantiated_P = results_substantiated$p.value
)

cat("\n=== Comparison Table ===\n")
print(comparison_table)

# Save comparison table
write_csv(comparison_table, file.path(output_data_dir, "regression_comparison_twfe_cumulative_2012_2020.csv"))

# ============================================================================
# TWFE WITH LAGGED OUTCOMES (outcome in t-1)
# ============================================================================
cat("\n=== TWFE with lagged outcomes (outcome in t-1) ===\n")

twfe_lag_data <- merged_panel %>%
  filter(!is.na(alleged_per_1000_tminus1) | !is.na(substantiated_per_1000_tminus1))

model_alleged_tminus1 <- lm(
  alleged_per_1000_tminus1 ~ cumulative_years_compliance + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = twfe_lag_data
)

model_substantiated_tminus1 <- lm(
  substantiated_per_1000_tminus1 ~ cumulative_years_compliance + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = twfe_lag_data
)

vcov_alleged_tminus1 <- vcovCL(model_alleged_tminus1, cluster = ~state, type = "HC1")
vcov_substantiated_tminus1 <- vcovCL(model_substantiated_tminus1, cluster = ~state, type = "HC1")

coeftest_alleged_tminus1 <- coeftest(model_alleged_tminus1, vcov = vcov_alleged_tminus1)
coeftest_substantiated_tminus1 <- coeftest(model_substantiated_tminus1, vcov = vcov_substantiated_tminus1)

cat("\nTWFE (t-1) Alleged per 1000:\n")
print(coeftest_alleged_tminus1)
cat("\nTWFE (t-1) Substantiated per 1000:\n")
print(coeftest_substantiated_tminus1)

results_alleged_tminus1 <- data.frame(
  term = rownames(coeftest_alleged_tminus1),
  estimate = coeftest_alleged_tminus1[, "Estimate"],
  std.error = coeftest_alleged_tminus1[, "Std. Error"],
  statistic = coeftest_alleged_tminus1[, "t value"],
  p.value = coeftest_alleged_tminus1[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(!str_detect(term, "^factor\\(state\\)"),
         !str_detect(term, "^factor\\(year\\)"))

results_substantiated_tminus1 <- data.frame(
  term = rownames(coeftest_substantiated_tminus1),
  estimate = coeftest_substantiated_tminus1[, "Estimate"],
  std.error = coeftest_substantiated_tminus1[, "Std. Error"],
  statistic = coeftest_substantiated_tminus1[, "t value"],
  p.value = coeftest_substantiated_tminus1[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(!str_detect(term, "^factor\\(state\\)"),
         !str_detect(term, "^factor\\(year\\)"))

write_csv(results_alleged_tminus1, file.path(output_data_dir, "regression_alleged_panel_twfe_cumulative_tminus1_2012_2020.csv"))
write_csv(results_substantiated_tminus1, file.path(output_data_dir, "regression_substantiated_panel_twfe_cumulative_tminus1_2012_2020.csv"))

comparison_table_tminus1 <- results_alleged_tminus1 %>%
  rename(
    Alleged_tminus1_Coef = estimate,
    Alleged_tminus1_SE = std.error,
    Alleged_tminus1_P = p.value
  ) %>%
  inner_join(
    results_substantiated_tminus1 %>%
      rename(
        Substantiated_tminus1_Coef = estimate,
        Substantiated_tminus1_SE = std.error,
        Substantiated_tminus1_P = p.value
      ),
    by = "term"
  ) %>%
  rename(Variable = term)

write_csv(comparison_table_tminus1, file.path(output_data_dir, "regression_comparison_twfe_cumulative_tminus1_2012_2020.csv"))
cat("Saved TWFE t-1 outputs to data/clean.\n")

# ============================================================================
# TWFE WITH LAGGED OUTCOMES (outcome in t-2)
# ============================================================================
cat("\n=== TWFE with lagged outcomes (outcome in t-2) ===\n")

twfe_lag2_data <- merged_panel %>%
  filter(!is.na(alleged_per_1000_tminus2) | !is.na(substantiated_per_1000_tminus2))

model_alleged_tminus2 <- lm(
  alleged_per_1000_tminus2 ~ cumulative_years_compliance + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = twfe_lag2_data
)

model_substantiated_tminus2 <- lm(
  substantiated_per_1000_tminus2 ~ cumulative_years_compliance + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = twfe_lag2_data
)

vcov_alleged_tminus2 <- vcovCL(model_alleged_tminus2, cluster = ~state, type = "HC1")
vcov_substantiated_tminus2 <- vcovCL(model_substantiated_tminus2, cluster = ~state, type = "HC1")

coeftest_alleged_tminus2 <- coeftest(model_alleged_tminus2, vcov = vcov_alleged_tminus2)
coeftest_substantiated_tminus2 <- coeftest(model_substantiated_tminus2, vcov = vcov_substantiated_tminus2)

cat("\nTWFE (t-2) Alleged per 1000:\n")
print(coeftest_alleged_tminus2)
cat("\nTWFE (t-2) Substantiated per 1000:\n")
print(coeftest_substantiated_tminus2)

results_alleged_tminus2 <- data.frame(
  term = rownames(coeftest_alleged_tminus2),
  estimate = coeftest_alleged_tminus2[, "Estimate"],
  std.error = coeftest_alleged_tminus2[, "Std. Error"],
  statistic = coeftest_alleged_tminus2[, "t value"],
  p.value = coeftest_alleged_tminus2[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(!str_detect(term, "^factor\\(state\\)"),
         !str_detect(term, "^factor\\(year\\)"))

results_substantiated_tminus2 <- data.frame(
  term = rownames(coeftest_substantiated_tminus2),
  estimate = coeftest_substantiated_tminus2[, "Estimate"],
  std.error = coeftest_substantiated_tminus2[, "Std. Error"],
  statistic = coeftest_substantiated_tminus2[, "t value"],
  p.value = coeftest_substantiated_tminus2[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(!str_detect(term, "^factor\\(state\\)"),
         !str_detect(term, "^factor\\(year\\)"))

write_csv(results_alleged_tminus2, file.path(output_data_dir, "regression_alleged_panel_twfe_cumulative_tminus2_2012_2020.csv"))
write_csv(results_substantiated_tminus2, file.path(output_data_dir, "regression_substantiated_panel_twfe_cumulative_tminus2_2012_2020.csv"))

comparison_table_tminus2 <- results_alleged_tminus2 %>%
  rename(
    Alleged_tminus2_Coef = estimate,
    Alleged_tminus2_SE = std.error,
    Alleged_tminus2_P = p.value
  ) %>%
  inner_join(
    results_substantiated_tminus2 %>%
      rename(
        Substantiated_tminus2_Coef = estimate,
        Substantiated_tminus2_SE = std.error,
        Substantiated_tminus2_P = p.value
      ),
    by = "term"
  ) %>%
  rename(Variable = term)

write_csv(comparison_table_tminus2, file.path(output_data_dir, "regression_comparison_twfe_cumulative_tminus2_2012_2020.csv"))
cat("Saved TWFE t-2 outputs to data/clean.\n")

# Helper for weak-IV robust Anderson-Rubin inference (single instrument)
ar_test_grid <- function(data, y_var, x_var, z_var, beta_grid) {
  pvals <- sapply(beta_grid, function(b0) {
    fml <- as.formula(paste0(
      "I(", y_var, " - (", b0, ")*", x_var, ") ~ ",
      z_var, " + population_millions + incarceration_rate_per_100k + ",
      "staff_per_1000_inmates + violent_crime_rate_per_100k + factor(state) + factor(year)"
    ))
    m <- lm(fml, data = data)
    v <- vcovCL(m, cluster = ~state, type = "HC1")
    ct <- coeftest(m, vcov = v)
    as.numeric(ct[z_var, "Pr(>|t|)"])
  })
  data.frame(beta0 = beta_grid, ar_p_value = pvals, stringsAsFactors = FALSE)
}

ar_intervals <- function(ar_df, alpha = 0.05) {
  accepted <- ar_df$ar_p_value > alpha
  if (!any(accepted)) return(data.frame(lower = NA_real_, upper = NA_real_))
  idx <- which(accepted)
  splits <- c(0, which(diff(idx) > 1), length(idx))
  out <- vector("list", length(splits) - 1)
  for (k in seq_len(length(splits) - 1)) {
    seg_idx <- idx[(splits[k] + 1):splits[k + 1]]
    out[[k]] <- data.frame(
      lower = min(ar_df$beta0[seg_idx]),
      upper = max(ar_df$beta0[seg_idx])
    )
  }
  bind_rows(out)
}

# ============================================================================
# IV APPROACH:
# Instrument cumulative_years_compliance with EverTreated (up to year t)
# ============================================================================
cat("\n=== IV Models (2SLS): cumulative_years_compliance instrumented by EverTreated ===\n")

# Use a consistent complete-case dataset for IV stages
iv_data <- merged_panel %>%
  filter(complete.cases(
    cumulative_years_compliance, EverTreated, population_millions,
    incarceration_rate_per_100k, staff_per_1000_inmates, violent_crime_rate_per_100k,
    state, year
  ))

cat("IV complete-case observations:", nrow(iv_data), "\n")

# First stage: effect of EverTreated on cumulative years compliance
first_stage <- lm(
  cumulative_years_compliance ~ EverTreated + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = iv_data
)
vcov_first_stage <- vcovCL(first_stage, cluster = ~state, type = "HC1")
coeftest_first_stage <- coeftest(first_stage, vcov = vcov_first_stage)

cat("\nFirst stage (dependent variable: cumulative_years_compliance):\n")
print(coeftest_first_stage["EverTreated", , drop = FALSE])
fs_fstat <- as.numeric(coeftest_first_stage["EverTreated", "t value"])^2
cat("Approx. first-stage F-stat (EverTreated, clustered):", round(fs_fstat, 3), "\n")
if (is.finite(fs_fstat) && fs_fstat < 10) {
  cat("WARNING: Potential weak instrument in baseline IV (F < 10).\n")
}

# Manual 2SLS implementation (dependency-free):
# Stage 1 already estimated above; use fitted values as instrumented regressor.
merged_panel_iv <- iv_data %>%
  mutate(cumulative_years_hat = fitted(first_stage))

# Second stage (2SLS) for alleged and substantiated outcomes
iv_model_alleged <- lm(
  alleged_per_1000 ~ cumulative_years_hat + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = merged_panel_iv
)

iv_model_substantiated <- lm(
  substantiated_per_1000 ~ cumulative_years_hat + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = merged_panel_iv
)

vcov_iv_alleged <- vcovCL(iv_model_alleged, cluster = ~state, type = "HC1")
vcov_iv_substantiated <- vcovCL(iv_model_substantiated, cluster = ~state, type = "HC1")

coeftest_iv_alleged <- coeftest(iv_model_alleged, vcov = vcov_iv_alleged)
coeftest_iv_substantiated <- coeftest(iv_model_substantiated, vcov = vcov_iv_substantiated)

cat("\nIV regression: Alleged per 1000\n")
print(coeftest_iv_alleged[c("cumulative_years_hat",
                            "population_millions",
                            "incarceration_rate_per_100k",
                            "staff_per_1000_inmates",
                            "violent_crime_rate_per_100k"), , drop = FALSE])

cat("\nIV regression: Substantiated per 1000\n")
print(coeftest_iv_substantiated[c("cumulative_years_hat",
                                  "population_millions",
                                  "incarceration_rate_per_100k",
                                  "staff_per_1000_inmates",
                                  "violent_crime_rate_per_100k"), , drop = FALSE])

# Save IV results
results_first_stage <- data.frame(
  term = rownames(coeftest_first_stage),
  estimate = coeftest_first_stage[, "Estimate"],
  std.error = coeftest_first_stage[, "Std. Error"],
  statistic = coeftest_first_stage[, "t value"],
  p.value = coeftest_first_stage[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(term == "EverTreated")

results_iv_alleged <- data.frame(
  term = rownames(coeftest_iv_alleged),
  estimate = coeftest_iv_alleged[, "Estimate"],
  std.error = coeftest_iv_alleged[, "Std. Error"],
  statistic = coeftest_iv_alleged[, "t value"],
  p.value = coeftest_iv_alleged[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(term %in% c("cumulative_years_hat",
                     "population_millions",
                     "incarceration_rate_per_100k",
                     "staff_per_1000_inmates",
                     "violent_crime_rate_per_100k"))

results_iv_substantiated <- data.frame(
  term = rownames(coeftest_iv_substantiated),
  estimate = coeftest_iv_substantiated[, "Estimate"],
  std.error = coeftest_iv_substantiated[, "Std. Error"],
  statistic = coeftest_iv_substantiated[, "t value"],
  p.value = coeftest_iv_substantiated[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(term %in% c("cumulative_years_hat",
                     "population_millions",
                     "incarceration_rate_per_100k",
                     "staff_per_1000_inmates",
                     "violent_crime_rate_per_100k"))

write_csv(results_first_stage, file.path(output_data_dir, "first_stage_ever_treated_to_cumulative_2012_2020.csv"))
write_csv(results_iv_alleged, file.path(output_data_dir, "iv_alleged_cumulative_on_ever_treated_2012_2020.csv"))
write_csv(results_iv_substantiated, file.path(output_data_dir, "iv_substantiated_cumulative_on_ever_treated_2012_2020.csv"))

iv_comparison <- results_iv_alleged %>%
  rename(
    Alleged_Coef = estimate,
    Alleged_SE = std.error,
    Alleged_t = statistic,
    Alleged_P = p.value
  ) %>%
  inner_join(
    results_iv_substantiated %>%
      rename(
        Substantiated_Coef = estimate,
        Substantiated_SE = std.error,
        Substantiated_t = statistic,
        Substantiated_P = p.value
      ),
    by = "term"
  ) %>%
  rename(Variable = term)

write_csv(iv_comparison, file.path(output_data_dir, "iv_comparison_cumulative_on_ever_treated_2012_2020.csv"))
cat("\nSaved IV outputs to data/clean.\n")

# ============================================================================
# IV APPROACH WITH LEAD OUTCOMES (outcome in t+1)
# ============================================================================
cat("\n=== IV Models with lead outcomes (outcome in t+1) ===\n")

iv_data_lead <- merged_panel %>%
  filter(complete.cases(
    cumulative_years_compliance, EverTreated, population_millions,
    incarceration_rate_per_100k, staff_per_1000_inmates, violent_crime_rate_per_100k,
    alleged_per_1000_tplus1, substantiated_per_1000_tplus1, state, year
  ))

cat("IV lead-outcome complete-case observations:", nrow(iv_data_lead), "\n")

first_stage_lead <- lm(
  cumulative_years_compliance ~ EverTreated + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = iv_data_lead
)
vcov_first_stage_lead <- vcovCL(first_stage_lead, cluster = ~state, type = "HC1")
coeftest_first_stage_lead <- coeftest(first_stage_lead, vcov = vcov_first_stage_lead)

cat("\nFirst stage on lead sample (dependent variable: cumulative_years_compliance):\n")
print(coeftest_first_stage_lead["EverTreated", , drop = FALSE])
fs_fstat_lead <- as.numeric(coeftest_first_stage_lead["EverTreated", "t value"])^2
cat("Approx. first-stage F-stat (EverTreated, clustered, lead sample):", round(fs_fstat_lead, 3), "\n")
if (is.finite(fs_fstat_lead) && fs_fstat_lead < 10) {
  cat("WARNING: Potential weak instrument in lead-outcome IV (F < 10).\n")
}

iv_data_lead <- iv_data_lead %>%
  mutate(cumulative_years_hat = fitted(first_stage_lead))

iv_model_alleged_lead <- lm(
  alleged_per_1000_tplus1 ~ cumulative_years_hat + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = iv_data_lead
)

iv_model_substantiated_lead <- lm(
  substantiated_per_1000_tplus1 ~ cumulative_years_hat + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = iv_data_lead
)

vcov_iv_alleged_lead <- vcovCL(iv_model_alleged_lead, cluster = ~state, type = "HC1")
vcov_iv_substantiated_lead <- vcovCL(iv_model_substantiated_lead, cluster = ~state, type = "HC1")

coeftest_iv_alleged_lead <- coeftest(iv_model_alleged_lead, vcov = vcov_iv_alleged_lead)
coeftest_iv_substantiated_lead <- coeftest(iv_model_substantiated_lead, vcov = vcov_iv_substantiated_lead)

cat("\nIV regression with outcome in t+1: Alleged per 1000\n")
print(coeftest_iv_alleged_lead[c("cumulative_years_hat",
                                 "population_millions",
                                 "incarceration_rate_per_100k",
                                 "staff_per_1000_inmates",
                                 "violent_crime_rate_per_100k"), , drop = FALSE])

cat("\nIV regression with outcome in t+1: Substantiated per 1000\n")
print(coeftest_iv_substantiated_lead[c("cumulative_years_hat",
                                       "population_millions",
                                       "incarceration_rate_per_100k",
                                       "staff_per_1000_inmates",
                                       "violent_crime_rate_per_100k"), , drop = FALSE])

results_first_stage_lead <- data.frame(
  term = rownames(coeftest_first_stage_lead),
  estimate = coeftest_first_stage_lead[, "Estimate"],
  std.error = coeftest_first_stage_lead[, "Std. Error"],
  statistic = coeftest_first_stage_lead[, "t value"],
  p.value = coeftest_first_stage_lead[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(term == "EverTreated")

results_iv_alleged_lead <- data.frame(
  term = rownames(coeftest_iv_alleged_lead),
  estimate = coeftest_iv_alleged_lead[, "Estimate"],
  std.error = coeftest_iv_alleged_lead[, "Std. Error"],
  statistic = coeftest_iv_alleged_lead[, "t value"],
  p.value = coeftest_iv_alleged_lead[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(term %in% c("cumulative_years_hat",
                     "population_millions",
                     "incarceration_rate_per_100k",
                     "staff_per_1000_inmates",
                     "violent_crime_rate_per_100k"))

results_iv_substantiated_lead <- data.frame(
  term = rownames(coeftest_iv_substantiated_lead),
  estimate = coeftest_iv_substantiated_lead[, "Estimate"],
  std.error = coeftest_iv_substantiated_lead[, "Std. Error"],
  statistic = coeftest_iv_substantiated_lead[, "t value"],
  p.value = coeftest_iv_substantiated_lead[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(term %in% c("cumulative_years_hat",
                     "population_millions",
                     "incarceration_rate_per_100k",
                     "staff_per_1000_inmates",
                     "violent_crime_rate_per_100k"))

write_csv(results_first_stage_lead, file.path(output_data_dir, "first_stage_ever_treated_to_cumulative_lead_outcome_2012_2020.csv"))
write_csv(results_iv_alleged_lead, file.path(output_data_dir, "iv_alleged_tplus1_cumulative_on_ever_treated_2012_2020.csv"))
write_csv(results_iv_substantiated_lead, file.path(output_data_dir, "iv_substantiated_tplus1_cumulative_on_ever_treated_2012_2020.csv"))

iv_comparison_lead <- results_iv_alleged_lead %>%
  rename(
    Alleged_tplus1_Coef = estimate,
    Alleged_tplus1_SE = std.error,
    Alleged_tplus1_t = statistic,
    Alleged_tplus1_P = p.value
  ) %>%
  inner_join(
    results_iv_substantiated_lead %>%
      rename(
        Substantiated_tplus1_Coef = estimate,
        Substantiated_tplus1_SE = std.error,
        Substantiated_tplus1_t = statistic,
        Substantiated_tplus1_P = p.value
      ),
    by = "term"
  ) %>%
  rename(Variable = term)

write_csv(iv_comparison_lead, file.path(output_data_dir, "iv_comparison_tplus1_cumulative_on_ever_treated_2012_2020.csv"))
cat("Saved IV lead-outcome outputs to data/clean.\n")

# ============================================================================
# IV APPROACH WITH LAG OUTCOMES (outcome in t-1)
# ============================================================================
cat("\n=== IV Models with lag outcomes (outcome in t-1) ===\n")

iv_data_lag <- merged_panel %>%
  filter(complete.cases(
    cumulative_years_compliance, EverTreated, population_millions,
    incarceration_rate_per_100k, staff_per_1000_inmates, violent_crime_rate_per_100k,
    alleged_per_1000_tminus1, substantiated_per_1000_tminus1, state, year
  ))

cat("IV lag-outcome complete-case observations:", nrow(iv_data_lag), "\n")

first_stage_lag <- lm(
  cumulative_years_compliance ~ EverTreated + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = iv_data_lag
)
vcov_first_stage_lag <- vcovCL(first_stage_lag, cluster = ~state, type = "HC1")
coeftest_first_stage_lag <- coeftest(first_stage_lag, vcov = vcov_first_stage_lag)

cat("\nFirst stage on lag sample (dependent variable: cumulative_years_compliance):\n")
print(coeftest_first_stage_lag["EverTreated", , drop = FALSE])
fs_fstat_lag <- as.numeric(coeftest_first_stage_lag["EverTreated", "t value"])^2
cat("Approx. first-stage F-stat (EverTreated, clustered, lag sample):", round(fs_fstat_lag, 3), "\n")
if (is.finite(fs_fstat_lag) && fs_fstat_lag < 10) {
  cat("WARNING: Potential weak instrument in lag-outcome IV (F < 10).\n")
}

iv_data_lag <- iv_data_lag %>%
  mutate(cumulative_years_hat = fitted(first_stage_lag))

iv_model_alleged_lag <- lm(
  alleged_per_1000_tminus1 ~ cumulative_years_hat + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = iv_data_lag
)

iv_model_substantiated_lag <- lm(
  substantiated_per_1000_tminus1 ~ cumulative_years_hat + population_millions +
    incarceration_rate_per_100k + staff_per_1000_inmates +
    violent_crime_rate_per_100k + factor(state) + factor(year),
  data = iv_data_lag
)

vcov_iv_alleged_lag <- vcovCL(iv_model_alleged_lag, cluster = ~state, type = "HC1")
vcov_iv_substantiated_lag <- vcovCL(iv_model_substantiated_lag, cluster = ~state, type = "HC1")

coeftest_iv_alleged_lag <- coeftest(iv_model_alleged_lag, vcov = vcov_iv_alleged_lag)
coeftest_iv_substantiated_lag <- coeftest(iv_model_substantiated_lag, vcov = vcov_iv_substantiated_lag)

cat("\nIV regression with outcome in t-1: Alleged per 1000\n")
print(coeftest_iv_alleged_lag[c("cumulative_years_hat",
                                "population_millions",
                                "incarceration_rate_per_100k",
                                "staff_per_1000_inmates",
                                "violent_crime_rate_per_100k"), , drop = FALSE])

cat("\nIV regression with outcome in t-1: Substantiated per 1000\n")
print(coeftest_iv_substantiated_lag[c("cumulative_years_hat",
                                      "population_millions",
                                      "incarceration_rate_per_100k",
                                      "staff_per_1000_inmates",
                                      "violent_crime_rate_per_100k"), , drop = FALSE])

results_first_stage_lag <- data.frame(
  term = rownames(coeftest_first_stage_lag),
  estimate = coeftest_first_stage_lag[, "Estimate"],
  std.error = coeftest_first_stage_lag[, "Std. Error"],
  statistic = coeftest_first_stage_lag[, "t value"],
  p.value = coeftest_first_stage_lag[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(term == "EverTreated")

results_iv_alleged_lag <- data.frame(
  term = rownames(coeftest_iv_alleged_lag),
  estimate = coeftest_iv_alleged_lag[, "Estimate"],
  std.error = coeftest_iv_alleged_lag[, "Std. Error"],
  statistic = coeftest_iv_alleged_lag[, "t value"],
  p.value = coeftest_iv_alleged_lag[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(term %in% c("cumulative_years_hat",
                     "population_millions",
                     "incarceration_rate_per_100k",
                     "staff_per_1000_inmates",
                     "violent_crime_rate_per_100k"))

results_iv_substantiated_lag <- data.frame(
  term = rownames(coeftest_iv_substantiated_lag),
  estimate = coeftest_iv_substantiated_lag[, "Estimate"],
  std.error = coeftest_iv_substantiated_lag[, "Std. Error"],
  statistic = coeftest_iv_substantiated_lag[, "t value"],
  p.value = coeftest_iv_substantiated_lag[, "Pr(>|t|)"],
  stringsAsFactors = FALSE
) %>%
  filter(term %in% c("cumulative_years_hat",
                     "population_millions",
                     "incarceration_rate_per_100k",
                     "staff_per_1000_inmates",
                     "violent_crime_rate_per_100k"))

write_csv(results_first_stage_lag, file.path(output_data_dir, "first_stage_ever_treated_to_cumulative_lag_outcome_2012_2020.csv"))
write_csv(results_iv_alleged_lag, file.path(output_data_dir, "iv_alleged_tminus1_cumulative_on_ever_treated_2012_2020.csv"))
write_csv(results_iv_substantiated_lag, file.path(output_data_dir, "iv_substantiated_tminus1_cumulative_on_ever_treated_2012_2020.csv"))

iv_comparison_lag <- results_iv_alleged_lag %>%
  rename(
    Alleged_tminus1_Coef = estimate,
    Alleged_tminus1_SE = std.error,
    Alleged_tminus1_t = statistic,
    Alleged_tminus1_P = p.value
  ) %>%
  inner_join(
    results_iv_substantiated_lag %>%
      rename(
        Substantiated_tminus1_Coef = estimate,
        Substantiated_tminus1_SE = std.error,
        Substantiated_tminus1_t = statistic,
        Substantiated_tminus1_P = p.value
      ),
    by = "term"
  ) %>%
  rename(Variable = term)

write_csv(iv_comparison_lag, file.path(output_data_dir, "iv_comparison_tminus1_cumulative_on_ever_treated_2012_2020.csv"))
cat("Saved IV lag-outcome outputs to data/clean.\n")

# Weak-IV robust inference for lag-outcome IV via Anderson-Rubin test
cat("\n=== Weak-IV robust inference (Anderson-Rubin) for lag-outcome IV ===\n")
beta_grid <- seq(-50, 50, by = 0.1)

ar_alleged_lag <- ar_test_grid(
  data = iv_data_lag,
  y_var = "alleged_per_1000_tminus1",
  x_var = "cumulative_years_compliance",
  z_var = "EverTreated",
  beta_grid = beta_grid
)
ar_substantiated_lag <- ar_test_grid(
  data = iv_data_lag,
  y_var = "substantiated_per_1000_tminus1",
  x_var = "cumulative_years_compliance",
  z_var = "EverTreated",
  beta_grid = beta_grid
)

ar_alleged_ci <- ar_intervals(ar_alleged_lag, alpha = 0.05)
ar_substantiated_ci <- ar_intervals(ar_substantiated_lag, alpha = 0.05)

cat("AR 95% confidence set(s), alleged t-1:\n")
print(ar_alleged_ci)
cat("AR 95% confidence set(s), substantiated t-1:\n")
print(ar_substantiated_ci)

write_csv(ar_alleged_lag, file.path(output_data_dir, "ar_grid_alleged_tminus1_2012_2020.csv"))
write_csv(ar_substantiated_lag, file.path(output_data_dir, "ar_grid_substantiated_tminus1_2012_2020.csv"))
write_csv(ar_alleged_ci, file.path(output_data_dir, "ar_ci_alleged_tminus1_2012_2020.csv"))
write_csv(ar_substantiated_ci, file.path(output_data_dir, "ar_ci_substantiated_tminus1_2012_2020.csv"))

ar_summary <- bind_rows(
  ar_alleged_ci %>% mutate(outcome = "alleged_tminus1"),
  ar_substantiated_ci %>% mutate(outcome = "substantiated_tminus1")
) %>% select(outcome, lower, upper)

write_csv(ar_summary, file.path(output_data_dir, "ar_ci_summary_tminus1_2012_2020.csv"))
cat("Saved AR weak-IV robust outputs to data/clean.\n")

# Visualizations
# Create scatter plots with regression lines
p_alleged <- ggplot(merged_panel %>% filter(!is.na(cumulative_years_compliance)), 
                    aes(x = cumulative_years_compliance, y = alleged_per_1000)) +
  geom_point(alpha = 0.5, color = "#2C7FB8") +
  geom_smooth(method = "lm", se = TRUE, color = "#2C7FB8", fill = "#2C7FB8", alpha = 0.2) +
  labs(
    title = "Alleged per 1000 vs Cumulative Years of PREA Compliance",
    subtitle = paste0("Panel regression with state & year FE | N = ", nrow(model_alleged$model), 
                     " | R² = ", round(summary(model_alleged)$r.squared, 3)),
    x = "Cumulative Years of PREA Compliance",
    y = "Alleged per 1000",
    caption = "Sources: PREA data (2012-2020), US Census 2020, USArrests, BLS OES\nTwo-way fixed effects (state + year), clustered SEs at state level"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_fig_dir, "regression_alleged_panel_twfe_cumulative_2012_2020.png"),
       p_alleged, width = 11, height = 7, dpi = 300)

p_substantiated <- ggplot(merged_panel %>% filter(!is.na(cumulative_years_compliance)), 
                          aes(x = cumulative_years_compliance, y = substantiated_per_1000)) +
  geom_point(alpha = 0.5, color = "#E31A1C") +
  geom_smooth(method = "lm", se = TRUE, color = "#E31A1C", fill = "#E31A1C", alpha = 0.2) +
  labs(
    title = "Substantiated per 1000 vs Cumulative Years of PREA Compliance",
    subtitle = paste0("Panel regression with state & year FE | N = ", nrow(model_substantiated$model),
                     " | R² = ", round(summary(model_substantiated)$r.squared, 3)),
    x = "Cumulative Years of PREA Compliance",
    y = "Substantiated per 1000",
    caption = "Sources: PREA data (2012-2020), US Census 2020, USArrests, BLS OES\nTwo-way fixed effects (state + year), clustered SEs at state level"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_fig_dir, "regression_substantiated_panel_twfe_cumulative_2012_2020.png"),
       p_substantiated, width = 11, height = 7, dpi = 300)

# Summary stats
cat("\n=== Summary Statistics ===\n")
cat("\nPREA Compliance Status (current year):\n")
print(table(merged_panel$prea_compliant, useNA = "ifany"))
cat("\nCumulative Years of PREA Compliance:\n")
print(summary(merged_panel$cumulative_years_compliance))

cat("\nStaff per 1,000 inmates stats:\n")
print(summary(merged_panel$staff_per_1000_inmates))

cat("\nCorrelation matrix (selected variables):\n")
print(round(cor(merged_panel %>%
                  select(cumulative_years_compliance, population_millions,
                         incarceration_rate_per_100k, violent_crime_rate_per_100k,
                         staff_per_1000_inmates,
                         alleged_per_1000, substantiated_per_1000),
                use = "complete.obs"), 3))

cat("\n=== Model Diagnostics ===\n")
summary_alleged_base <- summary(model_alleged)
summary_substantiated_base <- summary(model_substantiated)

cat("Alleged model:\n")
cat("  - Observations:", nrow(model_alleged$model), "\n")
cat("  - R-squared:", round(summary_alleged_base$r.squared, 4), "\n")
cat("  - Adjusted R-squared:", round(summary_alleged_base$adj.r.squared, 4), "\n")
cat("  - Number of states:", length(unique(merged_panel$state)), "\n")
cat("  - Number of years:", length(unique(merged_panel$year)), "\n")

cat("\nSubstantiated model:\n")
cat("  - Observations:", nrow(model_substantiated$model), "\n")
cat("  - R-squared:", round(summary_substantiated_base$r.squared, 4), "\n")
cat("  - Adjusted R-squared:", round(summary_substantiated_base$adj.r.squared, 4), "\n")

