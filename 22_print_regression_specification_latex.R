# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# Paths
excel_path <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
data_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/all_states_alleged_substantiated_per_1000_2012_2018_long.csv"
staff_path_long <- "/Users/victorxia/Documents/Honors Thesis/data/clean/state_correctional_officer_employment_long.csv"
output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"

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

# Read PREA compliance data
prea_compliance <- read_excel(excel_path, sheet = "PREA_Certification_vs_Assurance")
prea_compliance$State <- normalize_state(prea_compliance$State)

year_cols <- names(prea_compliance)[str_detect(names(prea_compliance), "^20\\d{2}$")]
year_nums <- suppressWarnings(as.numeric(year_cols))
analysis_years <- year_cols[!is.na(year_nums) & year_nums >= 2012 & year_nums <= 2018]

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

# Read panel data
panel_data <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(state = normalize_state(state))

# Extract incarceration counts
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

# Read staffing data
staffing_panel <- read_csv(staff_path_long, show_col_types = FALSE) %>%
  mutate(
    state = normalize_state(state),
    year = as.integer(year),
    tot_emp = as.numeric(tot_emp)
  ) %>%
  select(state, year, tot_emp)

# Violent crime data
ucr_path <- file.path(output_data_dir, "fbi_ucr_violent_crime_rates_panel.csv")
if (!file.exists(ucr_path)) {
  stop("FBI UCR data not found. Please run 22_extract_fbi_ucr_violent_crime.R first.")
}

violent_crime_panel <- read_csv(ucr_path, show_col_types = FALSE) %>%
  mutate(state = normalize_state(state)) %>%
  select(state, year, violent_crime_rate_per_100k)

# Merge panel data
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

# Get year range
years_range <- paste0(min(merged_panel$year, na.rm = TRUE), "--", max(merged_panel$year, na.rm = TRUE))
base_year <- min(merged_panel$year, na.rm = TRUE)

# Create LaTeX specification document
latex_output_path <- file.path(output_data_dir, "regression_specification.tex")

cat("\n=== Creating LaTeX Regression Specification ===\n")

cat("\\documentclass[11pt]{article}\n", file = latex_output_path)
cat("\\usepackage{amsmath}\n", file = latex_output_path, append = TRUE)
cat("\\usepackage{booktabs}\n", file = latex_output_path, append = TRUE)
cat("\\usepackage{geometry}\n", file = latex_output_path, append = TRUE)
cat("\\geometry{letterpaper, margin=1in}\n", file = latex_output_path, append = TRUE)
cat("\\begin{document}\n\n", file = latex_output_path, append = TRUE)

cat("\\section{Regression Specification}\n\n", file = latex_output_path, append = TRUE)

cat("The following panel regression models are estimated using state-year observations:\n\n", file = latex_output_path, append = TRUE)

# Specification 1: Alleged with year-by-year compliance
cat("\\subsection{Alleged Sexual Victimization Rate}\n\n", file = latex_output_path, append = TRUE)
cat("\\begin{equation}\n", file = latex_output_path, append = TRUE)
cat("\\begin{split}\n", file = latex_output_path, append = TRUE)
cat("\\text{Alleged}_{st} &= \\beta_0 + \\beta_1 \\text{PREA Compliant}_{st} + \\beta_2 \\text{Population}_s \\\\\n", file = latex_output_path, append = TRUE)
cat("&\\quad + \\beta_3 \\text{Incarceration Rate}_{st} + \\beta_4 \\text{Violent Crime Rate}_{st} \\\\\n", file = latex_output_path, append = TRUE)
cat("&\\quad + \\beta_5 \\text{Staff per 1,000 Inmates}_{st} + \\sum_{t=2015}^{2018} \\gamma_t \\mathbf{1}[\\text{Year} = t] + \\varepsilon_{st}\n", file = latex_output_path, append = TRUE)
cat("\\end{split}\n", file = latex_output_path, append = TRUE)
cat("\\end{equation}\n\n", file = latex_output_path, append = TRUE)

cat("where $\\text{Alleged}_{st}$ is the number of alleged incidents of sexual victimization per 1,000 prisoners in state $s$ and year $t$.\n\n", file = latex_output_path, append = TRUE)

# Specification 2: Substantiated with year-by-year compliance
cat("\\subsection{Substantiated Sexual Victimization Rate}\n\n", file = latex_output_path, append = TRUE)
cat("\\begin{equation}\n", file = latex_output_path, append = TRUE)
cat("\\begin{split}\n", file = latex_output_path, append = TRUE)
cat("\\text{Substantiated}_{st} &= \\beta_0 + \\beta_1 \\text{PREA Compliant}_{st} + \\beta_2 \\text{Population}_s \\\\\n", file = latex_output_path, append = TRUE)
cat("&\\quad + \\beta_3 \\text{Incarceration Rate}_{st} + \\beta_4 \\text{Violent Crime Rate}_{st} \\\\\n", file = latex_output_path, append = TRUE)
cat("&\\quad + \\beta_5 \\text{Staff per 1,000 Inmates}_{st} + \\sum_{t=2015}^{2018} \\gamma_t \\mathbf{1}[\\text{Year} = t] + \\varepsilon_{st}\n", file = latex_output_path, append = TRUE)
cat("\\end{split}\n", file = latex_output_path, append = TRUE)
cat("\\end{equation}\n\n", file = latex_output_path, append = TRUE)

cat("where $\\text{Substantiated}_{st}$ is the number of substantiated incidents of sexual victimization per 1,000 prisoners in state $s$ and year $t$.\n\n", file = latex_output_path, append = TRUE)

# Variable definitions
cat("\\subsection{Variable Definitions}\n\n", file = latex_output_path, append = TRUE)
cat("\\begin{itemize}\n", file = latex_output_path, append = TRUE)
cat("\\item $\\text{PREA Compliant}_{st}$: Binary indicator equal to 1 if state $s$ was PREA compliant in year $t$, 0 otherwise.\n", file = latex_output_path, append = TRUE)
cat("\\item $\\text{Population}_s$: State population in millions (2020 Census).\n", file = latex_output_path, append = TRUE)
cat("\\item $\\text{Incarceration Rate}_{st}$: Number of prisoners per 100,000 state population in state $s$ and year $t$.\n", file = latex_output_path, append = TRUE)
cat("\\item $\\text{Violent Crime Rate}_{st}$: Violent crime rate per 100,000 population in state $s$ and year $t$ (FBI UCR data).\n", file = latex_output_path, append = TRUE)
cat("\\item $\\text{Staff per 1,000 Inmates}_{st}$: Number of correctional staff per 1,000 prisoners in state $s$ and year $t$.\n", file = latex_output_path, append = TRUE)
cat("\\item $\\gamma_t$: Year fixed effects for years 2015--2018 (base year: ", base_year, ").\n", file = latex_output_path, append = TRUE)
cat("\\item $\\varepsilon_{st}$: Error term.\n", file = latex_output_path, append = TRUE)
cat("\\end{itemize}\n\n", file = latex_output_path, append = TRUE)

# Data description
cat("\\subsection{Data}\n\n", file = latex_output_path, append = TRUE)
cat("The panel dataset consists of state-year observations from ", years_range, ". ", file = latex_output_path, append = TRUE)
cat("The analysis includes all 50 U.S. states and the District of Columbia. ", file = latex_output_path, append = TRUE)
cat("Year fixed effects are included to control for time-varying factors common across all states.\n\n", file = latex_output_path, append = TRUE)

cat("\\end{document}\n", file = latex_output_path, append = TRUE)

cat("LaTeX specification document saved to:", latex_output_path, "\n")
cat("\n=== LaTeX Specification Generation Complete ===\n")



