# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(readr)
  library(purrr)
})

# Paths
base_dir <- "/Users/victorxia/Documents/Honors Thesis"
states_data_dir <- file.path(base_dir, "States Occupational Data")
output_dir <- file.path(base_dir, "data", "clean")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

normalize_state <- function(x) {
  x <- str_trim(as.character(x))
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  x <- str_to_title(x)
  x <- str_replace_all(x, "Washington,? D\\.?C\\.?", "District of Columbia")
  x <- str_replace_all(x, "D\\.?c\\.?", "District of Columbia")
  x
}

read_staff_file <- function(year) {
  folder <- sprintf("oesm%02dst", year %% 100)
  file_base <- sprintf("state_M%04d_dl", year)
  ext <- if (year <= 2013) ".xls" else ".xlsx"
  file_path <- file.path(states_data_dir, folder, paste0(file_base, ext))
  if (!file.exists(file_path)) {
    warning("File missing for year ", year, ": ", file_path)
    return(tibble())
  }
  cat("Reading", file_path, "\n")
  df <- read_excel(file_path)
  
  # Handle different column name formats across years
  if (year == 2019) {
    # 2019: lowercase column names, uses area_title
    df_filtered <- df %>%
      filter(occ_code == "33-3012") %>%  # Correctional Officers and Jailers
      transmute(
        year = year,
        state = normalize_state(area_title),
        tot_emp_raw = tot_emp
      )
  } else if (year == 2020) {
    # 2020: uppercase column names, uses AREA_TITLE (not STATE)
    df_filtered <- df %>%
      filter(OCC_CODE == "33-3012") %>%  # Correctional Officers and Jailers
      transmute(
        year = year,
        state = normalize_state(AREA_TITLE),
        tot_emp_raw = TOT_EMP
      )
  } else {
    # 2012-2018: uppercase column names, uses STATE
    df_filtered <- df %>%
      filter(OCC_CODE == "33-3012") %>%  # Correctional Officers and Jailers
      transmute(
        year = year,
        state = normalize_state(STATE),
        tot_emp_raw = TOT_EMP
      )
  }
  
  df_filtered %>%
    mutate(
      tot_emp = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(tot_emp_raw))))
    ) %>%
    select(year, state, tot_emp)
}

years <- 2012:2020
staff_long <- map_dfr(years, read_staff_file) %>%
  filter(!is.na(state), !is.na(tot_emp))

cat("Records extracted:", nrow(staff_long), "\n")

staff_agg <- staff_long %>%
  group_by(state) %>%
  summarise(
    avg_correctional_staff = mean(tot_emp, na.rm = TRUE),
    median_correctional_staff = median(tot_emp, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

long_path <- file.path(output_dir, "state_correctional_officer_employment_long.csv")
agg_path <- file.path(output_dir, "state_correctional_officer_employment_avg.csv")
write_csv(staff_long, long_path)
write_csv(staff_agg, agg_path)

cat("Long dataset saved to:", long_path, "\n")
cat("Aggregated dataset saved to:", agg_path, "\n")

