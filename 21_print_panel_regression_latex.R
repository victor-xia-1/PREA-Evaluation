# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(broom)
  library(tidyr)
})

# Paths
excel_path <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
data_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/all_states_alleged_substantiated_per_1000_2012_2018_long.csv"
staff_path_long <- "/Users/victorxia/Documents/Honors Thesis/data/clean/state_correctional_officer_employment_long.csv"
output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"

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

# Get all year columns (2012-2018 for our analysis period)
year_cols <- names(prea_compliance)[str_detect(names(prea_compliance), "^20\\d{2}$")]
year_nums <- suppressWarnings(as.numeric(year_cols))
analysis_years <- year_cols[!is.na(year_nums) & year_nums >= 2012 & year_nums <= 2018]

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

# Calculate cumulative compliance years (2015-2024) for each state
compliance_years_range <- year_cols[!is.na(suppressWarnings(as.numeric(year_cols))) & 
                                     suppressWarnings(as.numeric(year_cols)) >= 2015 & 
                                     suppressWarnings(as.numeric(year_cols)) <= 2024]

if (length(compliance_years_range) > 0) {
  prea_compliance_cumulative <- prea_compliance %>%
    select(State, all_of(compliance_years_range)) %>%
    filter(!is.na(State), State != "") %>%
    mutate(
      years_compliance_total = rowSums(
        select(., all_of(compliance_years_range)) == "1",
        na.rm = TRUE
      )
    ) %>%
    select(State, years_compliance_total)
  
  prea_compliance_long <- prea_compliance_long %>%
    left_join(prea_compliance_cumulative, by = "State")
} else {
  prea_compliance_long$years_compliance_total <- 0L
}

# Read alleged/substantiated data (already in panel format)
panel_data <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(state = normalize_state(state))

# Extract year-by-year incarceration counts
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
  if (is.null(prisoners_col)) return(tibble())
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

# Read year-by-year staffing data
staffing_panel <- read_csv(staff_path_long, show_col_types = FALSE) %>%
  mutate(
    state = normalize_state(state),
    year = as.integer(year),
    tot_emp = as.numeric(tot_emp)
  ) %>%
  select(state, year, tot_emp)

# Violent crime data using FBI UCR (Uniform Crime Report) - year-by-year
ucr_path <- file.path(output_data_dir, "fbi_ucr_violent_crime_rates_panel.csv")
if (!file.exists(ucr_path)) {
  stop("FBI UCR data not found. Please run 22_extract_fbi_ucr_violent_crime.R first.")
}

violent_crime_panel <- read_csv(ucr_path, show_col_types = FALSE) %>%
  mutate(state = normalize_state(state)) %>%
  select(state, year, violent_crime_rate_per_100k)

# Merge all data into panel format
merged_panel <- panel_data %>%
  inner_join(prea_compliance_long, by = c("state" = "State", "year" = "year")) %>%
  inner_join(incarceration_panel, by = c("state", "year")) %>%
  inner_join(staffing_panel, by = c("state", "year")) %>%
  inner_join(violent_crime_panel, by = c("state", "year")) %>%
  mutate(
    staff_per_1000_inmates = ifelse(prisoners > 0,
                                    (tot_emp / prisoners) * 1000,
                                    NA_real_)
  ) %>%
  filter(!is.na(alleged_per_1000) | !is.na(substantiated_per_1000))

# Regression formulas - using year-by-year compliance status
formula_alleged <- alleged_per_1000 ~ prea_compliant + population_millions +
  incarceration_rate_per_100k + violent_crime_rate_per_100k + staff_per_1000_inmates +
  factor(year)

formula_substantiated <- substantiated_per_1000 ~ prea_compliant + population_millions +
  incarceration_rate_per_100k + violent_crime_rate_per_100k + staff_per_1000_inmates +
  factor(year)

# Run regressions
cat("\n=== Running Panel Regressions ===\n")
model_alleged <- lm(formula_alleged, data = merged_panel)
model_substantiated <- lm(formula_substantiated, data = merged_panel)

# Get tidy results
results_alleged <- tidy(model_alleged)
results_substantiated <- tidy(model_substantiated)
summary_alleged <- summary(model_alleged)
summary_substantiated <- summary(model_substantiated)

# Function to format coefficient with significance stars (economics convention)
format_coef <- function(coef, pval) {
  stars <- ifelse(pval < 0.01, "***",
           ifelse(pval < 0.05, "**",
           ifelse(pval < 0.1, "*", "")))
  sprintf("%.3f%s", coef, stars)
}

# Function to format coefficient and standard error together (standard economics format)
format_coef_se <- function(coef, se, pval) {
  stars <- ifelse(pval < 0.01, "***",
           ifelse(pval < 0.05, "**",
           ifelse(pval < 0.1, "*", "")))
  sprintf("%.3f%s \\\\\n & (%.3f)", coef, stars, se)
}

# Get variable labels
get_var_label <- function(var_name) {
  case_when(
    var_name == "(Intercept)" ~ "Constant",
    var_name == "prea_compliant" ~ "PREA Compliant",
    var_name == "population_millions" ~ "State Population (millions)",
    var_name == "incarceration_rate_per_100k" ~ "Incarceration Rate (per 100,000)",
    var_name == "violent_crime_rate_per_100k" ~ "Violent Crime Rate (per 100,000)",
    var_name == "staff_per_1000_inmates" ~ "Staff per 1,000 Inmates",
    str_detect(var_name, "^factor\\(year\\)") ~ str_replace(var_name, "^factor\\(year\\)", "Year "),
    TRUE ~ var_name
  )
}

# Filter out year fixed effects for main table (we'll note them separately)
main_vars <- c("(Intercept)", "prea_compliant", "population_millions", 
               "incarceration_rate_per_100k", "violent_crime_rate_per_100k", 
               "staff_per_1000_inmates")

# Create LaTeX table
latex_output_path <- file.path(output_data_dir, "panel_regression_table.tex")

cat("\n=== Creating LaTeX Table ===\n")

# Write LaTeX table
cat("\\begin{table}[htbp]\n", file = latex_output_path)
cat("\\centering\n", file = latex_output_path, append = TRUE)
cat("\\caption{Panel Regression Results: PREA Compliance and Sexual Victimization Rates}\n", file = latex_output_path, append = TRUE)
cat("\\label{tab:panel_regression}\n", file = latex_output_path, append = TRUE)
cat("\\begin{tabular}{lcc}\n", file = latex_output_path, append = TRUE)
cat("\\toprule\n", file = latex_output_path, append = TRUE)
cat(" & (1) & (2) \\\\\n", file = latex_output_path, append = TRUE)
cat(" & Alleged & Substantiated \\\\\n", file = latex_output_path, append = TRUE)
cat(" & per 1,000 & per 1,000 \\\\\n", file = latex_output_path, append = TRUE)
cat("\\midrule\n", file = latex_output_path, append = TRUE)

# Add main variables
for (var in main_vars) {
  # Alleged model
  row_alleged <- results_alleged %>% filter(term == var)
  if (nrow(row_alleged) > 0) {
    coef_alleged <- format_coef(row_alleged$estimate[1], row_alleged$p.value[1])
    se_alleged <- sprintf("(%.3f)", row_alleged$std.error[1])
  } else {
    coef_alleged <- ""
    se_alleged <- ""
  }
  
  # Substantiated model
  row_substantiated <- results_substantiated %>% filter(term == var)
  if (nrow(row_substantiated) > 0) {
    coef_substantiated <- format_coef(row_substantiated$estimate[1], row_substantiated$p.value[1])
    se_substantiated <- sprintf("(%.3f)", row_substantiated$std.error[1])
  } else {
    coef_substantiated <- ""
    se_substantiated <- ""
  }
  
  var_label <- get_var_label(var)
  
  # Write coefficient and standard error on same line (standard economics format)
  cat(sprintf("%s & %s & %s \\\\\n", var_label, coef_alleged, coef_substantiated), 
      file = latex_output_path, append = TRUE)
  cat(sprintf(" & %s & %s \\\\\n", se_alleged, se_substantiated), 
      file = latex_output_path, append = TRUE)
}

cat("\\midrule\n", file = latex_output_path, append = TRUE)
cat("Year Fixed Effects & Yes & Yes \\\\\n", file = latex_output_path, append = TRUE)
cat("\\midrule\n", file = latex_output_path, append = TRUE)

# Model statistics
n_obs_alleged <- nrow(model_alleged$model)
n_obs_substantiated <- nrow(model_substantiated$model)
r2_alleged <- summary_alleged$r.squared
r2_substantiated <- summary_substantiated$r.squared
adj_r2_alleged <- summary_alleged$adj.r.squared
adj_r2_substantiated <- summary_substantiated$adj.r.squared

cat(sprintf("Observations & %d & %d \\\\\n", n_obs_alleged, n_obs_substantiated), 
    file = latex_output_path, append = TRUE)
cat(sprintf("R-squared & %.3f & %.3f \\\\\n", r2_alleged, r2_substantiated), 
    file = latex_output_path, append = TRUE)
cat(sprintf("Adjusted R-squared & %.3f & %.3f \\\\\n", adj_r2_alleged, adj_r2_substantiated), 
    file = latex_output_path, append = TRUE)

cat("\\bottomrule\n", file = latex_output_path, append = TRUE)
cat("\\multicolumn{3}{l}{\\footnotesize{Standard errors in parentheses.}} \\\\\n", file = latex_output_path, append = TRUE)
cat("\\multicolumn{3}{l}{\\footnotesize{$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$}} \\\\\n", file = latex_output_path, append = TRUE)
years_range <- paste0(min(merged_panel$year, na.rm = TRUE), "--", max(merged_panel$year, na.rm = TRUE))
cat(sprintf("\\multicolumn{3}{l}{\\footnotesize{Panel data: state-year observations (%s).}} \\\\\n", years_range), file = latex_output_path, append = TRUE)
base_year <- min(merged_panel$year, na.rm = TRUE)
cat(sprintf("\\multicolumn{3}{l}{\\footnotesize{Year fixed effects included. Base year: %d.}} \\\\\n", base_year), file = latex_output_path, append = TRUE)
cat("\\end{tabular}\n", file = latex_output_path, append = TRUE)
cat("\\end{table}\n", file = latex_output_path, append = TRUE)

cat("LaTeX table saved to:", latex_output_path, "\n")

# Also create a standalone LaTeX document
latex_doc_path <- file.path(output_data_dir, "panel_regression_table_standalone.tex")

cat("\\documentclass[11pt]{article}\n", file = latex_doc_path)
cat("\\usepackage{booktabs}\n", file = latex_doc_path, append = TRUE)
cat("\\usepackage{geometry}\n", file = latex_doc_path, append = TRUE)
cat("\\geometry{letterpaper, margin=1in}\n", file = latex_doc_path, append = TRUE)
cat("\\begin{document}\n\n", file = latex_doc_path, append = TRUE)

# Read and append the table
table_content <- readLines(latex_output_path)
cat(paste(table_content, collapse = "\n"), "\n", file = latex_doc_path, append = TRUE)

cat("\n\\end{document}\n", file = latex_doc_path, append = TRUE)

cat("Standalone LaTeX document saved to:", latex_doc_path, "\n")
cat("\n=== LaTeX Table Generation Complete ===\n")

