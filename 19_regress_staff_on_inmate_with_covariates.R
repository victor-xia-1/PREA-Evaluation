# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(broom)
  library(ggplot2)
  library(tidyr)
  library(purrr)
})

# Paths
excel_path <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
data_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/all_states_staff_on_inmate_alleged_substantiated_per_1000_2012_2018_long.csv"
staffing_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/state_correctional_officer_employment_avg.csv"
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

# State population data (2020 Census, in millions)
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
  population_millions = c(4.903,0.731,7.279,3.018,39.512,5.759,
                          3.565,0.973,0.706,21.477,10.617,
                          1.416,1.787,12.671,6.732,3.155,2.913,4.468,
                          4.649,1.344,6.046,6.893,9.987,5.611,
                          2.976,6.137,1.069,1.934,3.080,1.360,
                          8.882,2.096,19.454,10.488,0.762,
                          11.689,3.956,4.219,12.802,1.059,
                          5.149,0.884,6.829,28.996,3.205,0.624,
                          8.536,7.615,1.792,5.822,0.578),
  stringsAsFactors = FALSE
)

cat("Loaded 2020 Census population data for", nrow(state_populations), "states.\n")

# Read PREA compliance data
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
    years_compliance = rowSums(
      select(., all_of(compliance_years)) == "1",
      na.rm = TRUE
    )
  ) %>%
  select(State, years_compliance) %>%
  filter(!is.na(State), State != "")

cat("Compliance stats:\n")
print(summary(prea_compliance_summary$years_compliance))

# Read staff-on-inmate data
cat("\nReading staff-on-inmate datasets...\n")
panel_data <- read_csv(data_path, show_col_types = FALSE)
state_avg <- panel_data %>%
  group_by(state) %>%
  summarise(
    avg_alleged_per_1000 = mean(alleged_per_1000, na.rm = TRUE),
    avg_substantiated_per_1000 = mean(substantiated_per_1000, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  ) %>%
  mutate(state = normalize_state(state))

# Derive incarceration rate per 100k using prisoners in custody data
cat("\nExtracting incarceration counts from Excel sheets...\n")
all_sheets <- readxl::excel_sheets(excel_path)
year_sheets <- all_sheets[str_detect(all_sheets, "^20(1[2-8])$")]
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
state_incarceration <- incarceration_counts %>%
  group_by(state) %>%
  summarise(avg_prisoners = mean(prisoners, na.rm = TRUE), .groups = "drop") %>%
  inner_join(state_populations, by = "state") %>%
  mutate(
    population_actual = population_millions * 1000000,
    incarceration_rate_per_100k = (avg_prisoners / population_actual) * 100000
  ) %>%
  select(state, avg_prisoners, incarceration_rate_per_100k, population_millions)

cat("Computed incarceration rates for", nrow(state_incarceration), "states.\n")

# Read staffing data
cat("\nReading staffing data...\n")
staffing_data <- read_csv(staffing_path, show_col_types = FALSE) %>%
  mutate(state = normalize_state(state))

# Calculate staff per 1000 inmates
state_staffing <- state_incarceration %>%
  inner_join(staffing_data, by = "state") %>%
  mutate(staff_per_1000_inmates = (avg_correctional_staff / avg_prisoners) * 1000) %>%
  select(state, staff_per_1000_inmates)

cat("Loaded staffing ratios for", nrow(state_staffing), "states.\n")

# Violent crime data using USArrests (FBI Uniform Crime Report, 1973)
data("USArrests")
violent_crime <- USArrests %>%
  as_tibble(rownames = "state") %>%
  mutate(
    state = normalize_state(state),
    violent_crime_rate_per_100k = Murder + Assault + Rape
  ) %>%
  select(state, violent_crime_rate_per_100k)

cat("USArrests violent crime data available for", nrow(violent_crime), "states.\n")

# Merge all covariates
merged_data <- state_avg %>%
  inner_join(prea_compliance_summary, by = c("state" = "State")) %>%
  inner_join(state_populations %>% select(state, population_millions), by = "state") %>%
  inner_join(state_incarceration %>% select(state, incarceration_rate_per_100k), by = "state") %>%
  inner_join(state_staffing, by = "state") %>%
  inner_join(violent_crime, by = "state")

cat("\nMerged dataset size:", nrow(merged_data), "states.\n")

# Regression formulas
formula_alleged <- avg_alleged_per_1000 ~ years_compliance + population_millions +
  staff_per_1000_inmates + violent_crime_rate_per_100k
formula_substantiated <- avg_substantiated_per_1000 ~ years_compliance + population_millions +
  staff_per_1000_inmates + violent_crime_rate_per_100k

model_alleged <- lm(formula_alleged, data = merged_data)
model_substantiated <- lm(formula_substantiated, data = merged_data)

cat("\n=== Regression: Staff-on-Inmate ALLEGED per 1000 ===\n")
print(summary(model_alleged))
cat("\n=== Regression: Staff-on-Inmate SUBSTANTIATED per 1000 ===\n")
print(summary(model_substantiated))

# Save tidy outputs
write_csv(tidy(model_alleged), file.path(output_data_dir, "regression_staff_on_inmate_alleged_with_covariates.csv"))
write_csv(tidy(model_substantiated), file.path(output_data_dir, "regression_staff_on_inmate_substantiated_with_covariates.csv"))
write_csv(merged_data, file.path(output_data_dir, "merged_data_staff_on_inmate_with_covariates.csv"))

# Visualizations
p_alleged <- ggplot(merged_data, aes(x = years_compliance, y = avg_alleged_per_1000,
                                     size = staff_per_1000_inmates, color = violent_crime_rate_per_100k)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#2C7FB8") +
  scale_size_continuous(name = "Staff per 1,000 inmates", range = c(2,8)) +
  scale_color_gradient(name = "Violent crime\n(per 100k)", low = "#FDE725", high = "#440154") +
  labs(
    title = "Staff-on-Inmate Alleged per 1000 vs PREA Compliance",
    subtitle = paste0("Adj. R² = ", round(summary(model_alleged)$adj.r.squared, 3), 
                     " | Predictors: compliance, population, staffing, violent crime"),
    x = "Years of PREA Compliance (2015-2024)",
    y = "Average Staff-on-Inmate Alleged per 1000 (2012-2018)",
    caption = "Sources: PREA data (2012-2018), US Census 2020, USArrests, BLS OES"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_fig_dir, "regression_staff_on_inmate_alleged_with_covariates.png"),
       p_alleged, width = 11, height = 7, dpi = 300)

p_substantiated <- ggplot(merged_data, aes(x = years_compliance, y = avg_substantiated_per_1000,
                                           size = staff_per_1000_inmates,
                                           color = violent_crime_rate_per_100k)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "#E31A1C") +
  scale_size_continuous(name = "Staff per 1,000 inmates", range = c(2,8)) +
  scale_color_gradient(name = "Violent crime\n(per 100k)", low = "#FDE725", high = "#440154") +
  labs(
    title = "Staff-on-Inmate Substantiated per 1000 vs PREA Compliance",
    subtitle = paste0("Adj. R² = ", round(summary(model_substantiated)$adj.r.squared, 3),
                     " | Predictors: compliance, population, staffing, violent crime"),
    x = "Years of PREA Compliance (2015-2024)",
    y = "Average Staff-on-Inmate Substantiated per 1000 (2012-2018)",
    caption = "Sources: PREA data (2012-2018), US Census 2020, USArrests, BLS OES"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
ggsave(file.path(output_fig_dir, "regression_staff_on_inmate_substantiated_with_covariates.png"),
       p_substantiated, width = 11, height = 7, dpi = 300)

# Summary stats
cat("\nStaff per 1,000 inmates stats:\n")
print(summary(merged_data$staff_per_1000_inmates))

cat("\nCorrelation matrix (selected variables):\n")
print(round(cor(merged_data %>%
                  select(years_compliance, population_millions,
                         staff_per_1000_inmates, violent_crime_rate_per_100k,
                         avg_alleged_per_1000, avg_substantiated_per_1000),
                use = "complete.obs"), 3))

cat("\nScript completed. Outputs saved in data/clean and figures directories.\n")

