# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(readr)
})

# Paths
excel_path <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"

# Ensure output directory exists
if (!dir.exists(output_data_dir)) dir.create(output_data_dir, recursive = TRUE)

# Canonical list of 50 states + DC
states_50_dc <- c(
  "Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware",
  "District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa",
  "Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota",
  "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
  "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
  "South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington",
  "West Virginia","Wisconsin","Wyoming"
)

# Helper: normalize state names
normalize_state <- function(x) {
  x <- str_trim(as.character(x))
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  x <- str_to_title(x)
  x <- str_replace_all(x, "Washington,? D\\.?C\\.?", "District of Columbia")
  x <- str_replace_all(x, "D\\.?c\\.?", "District of Columbia")
  x
}

# Helper: coerce numeric safely
as_numeric_safely <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

# Get year sheets (2012-2018)
all_sheets <- readxl::excel_sheets(excel_path)
year_sheets <- all_sheets[str_detect(all_sheets, "^20(1[2-8])$")]
year_sheets <- sort(year_sheets)

cat("Found year sheets:", paste(year_sheets, collapse = ", "), "\n")

# Function to extract all four types of data from a year sheet
extract_year_data <- function(sheet_name) {
  cat("Processing sheet:", sheet_name, "\n")
  
  # Read the sheet without skipping rows first to see structure
  df_raw <- readxl::read_excel(excel_path, sheet = sheet_name, col_names = FALSE)
  
  # Find the header row that contains "Alleged" and "Substantiated"
  # For inmate-on-inmate: typically columns 4-7
  # For staff-on-inmate: typically columns 8-11
  header_row <- NULL
  inmate_alleged_col <- NULL
  inmate_substantiated_col <- NULL
  staff_alleged_col <- NULL
  staff_substantiated_col <- NULL
  
  for (i in 1:20) {
    if (i <= nrow(df_raw)) {
      # Check for inmate-on-inmate section (around columns 4-7)
      for (j in 1:7) {
        cell_value <- as.character(df_raw[[j]][i])
        if (!is.na(cell_value)) {
          cell_lower <- tolower(cell_value)
          if (str_detect(cell_lower, "^alleged$") || str_detect(cell_lower, "^alleg")) {
            # Check if this is in the inmate-on-inmate section
            if (j >= 4 && j <= 7) {
              header_row <- i
              inmate_alleged_col <- j
            }
          }
          if (str_detect(cell_lower, "^substantiated$") || str_detect(cell_lower, "^sub")) {
            if (j >= 4 && j <= 7 && is.null(inmate_substantiated_col)) {
              if (is.null(header_row)) header_row <- i
              inmate_substantiated_col <- j
            }
          }
        }
      }
      
      # Check for staff-on-inmate section (around columns 8-11)
      for (j in 8:min(15, ncol(df_raw))) {
        cell_value <- as.character(df_raw[[j]][i])
        if (!is.na(cell_value)) {
          cell_lower <- tolower(cell_value)
          if (str_detect(cell_lower, "^alleged$") || str_detect(cell_lower, "^alleg")) {
            if (is.null(staff_alleged_col)) {
              if (is.null(header_row)) header_row <- i
              staff_alleged_col <- j
            }
          }
          if (str_detect(cell_lower, "^substantiated$") || str_detect(cell_lower, "^sub")) {
            if (is.null(staff_substantiated_col)) {
              if (is.null(header_row)) header_row <- i
              staff_substantiated_col <- j
            }
          }
        }
      }
      
      if (!is.null(header_row) && !is.null(inmate_alleged_col) && !is.null(inmate_substantiated_col) &&
          !is.null(staff_alleged_col) && !is.null(staff_substantiated_col)) {
        break
      }
    }
  }
  
  if (is.null(inmate_alleged_col) || is.null(inmate_substantiated_col) ||
      is.null(staff_alleged_col) || is.null(staff_substantiated_col)) {
    cat("Could not find all required headers in", sheet_name, "\n")
    cat("Found: inmate_alleged=", inmate_alleged_col, ", inmate_sub=", inmate_substantiated_col,
        ", staff_alleged=", staff_alleged_col, ", staff_sub=", staff_substantiated_col, "\n")
    return(data.frame(year = integer(), state = character(), 
                     inmate_alleged = double(), inmate_substantiated = double(),
                     staff_alleged = double(), staff_substantiated = double()))
  }
  
  cat("Found headers at row", header_row, ":\n")
  cat("  Inmate Alleged: column", LETTERS[inmate_alleged_col], "\n")
  cat("  Inmate Substantiated: column", LETTERS[inmate_substantiated_col], "\n")
  cat("  Staff Alleged: column", LETTERS[staff_alleged_col], "\n")
  cat("  Staff Substantiated: column", LETTERS[staff_substantiated_col], "\n")
  
  # Extract all state data
  results <- list()
  
  for (i in (header_row + 1):nrow(df_raw)) {
    # Check if this row has a state name in column 2
    state_name_raw <- df_raw[[2]][i]
    
    if (!is.na(state_name_raw)) {
      state_name <- normalize_state(state_name_raw)
      
      # Only process if it's one of our 50 states + DC
      if (state_name %in% states_50_dc) {
        inmate_alleged_value <- as_numeric_safely(df_raw[[inmate_alleged_col]][i])
        inmate_substantiated_value <- as_numeric_safely(df_raw[[inmate_substantiated_col]][i])
        staff_alleged_value <- as_numeric_safely(df_raw[[staff_alleged_col]][i])
        staff_substantiated_value <- as_numeric_safely(df_raw[[staff_substantiated_col]][i])
        
        results[[length(results) + 1]] <- data.frame(
          year = as.integer(sheet_name),
          state = state_name,
          inmate_alleged = inmate_alleged_value,
          inmate_substantiated = inmate_substantiated_value,
          staff_alleged = staff_alleged_value,
          staff_substantiated = staff_substantiated_value,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(results) > 0) {
    year_data <- do.call(rbind, results)
    cat("Extracted", nrow(year_data), "state records from", sheet_name, "\n")
    return(year_data)
  } else {
    cat("No state data found in", sheet_name, "\n")
    return(data.frame(year = integer(), state = character(), 
                     inmate_alleged = double(), inmate_substantiated = double(),
                     staff_alleged = double(), staff_substantiated = double()))
  }
}

# Extract data for all years
all_data <- purrr::map_dfr(year_sheets, extract_year_data)

# Remove any rows with all NA values
all_data <- all_data %>%
  filter(!is.na(year), !is.na(state))

# Save long format
long_output_path <- file.path(output_data_dir, "annual_state_counts_all_types_2012_2018_long.csv")
write_csv(all_data, long_output_path, na = "")

# Create wide format tables for each type
# 1. Inmate Alleged
inmate_alleged_wide <- all_data %>%
  select(year, state, inmate_alleged) %>%
  pivot_wider(names_from = year, values_from = inmate_alleged, values_fill = NA) %>%
  arrange(state)

# 2. Inmate Substantiated
inmate_substantiated_wide <- all_data %>%
  select(year, state, inmate_substantiated) %>%
  pivot_wider(names_from = year, values_from = inmate_substantiated, values_fill = NA) %>%
  arrange(state)

# 3. Staff Alleged
staff_alleged_wide <- all_data %>%
  select(year, state, staff_alleged) %>%
  pivot_wider(names_from = year, values_from = staff_alleged, values_fill = NA) %>%
  arrange(state)

# 4. Staff Substantiated
staff_substantiated_wide <- all_data %>%
  select(year, state, staff_substantiated) %>%
  pivot_wider(names_from = year, values_from = staff_substantiated, values_fill = NA) %>%
  arrange(state)

# Save wide format tables
write_csv(inmate_alleged_wide, file.path(output_data_dir, "annual_state_counts_inmate_alleged_wide.csv"), na = "")
write_csv(inmate_substantiated_wide, file.path(output_data_dir, "annual_state_counts_inmate_substantiated_wide.csv"), na = "")
write_csv(staff_alleged_wide, file.path(output_data_dir, "annual_state_counts_staff_alleged_wide.csv"), na = "")
write_csv(staff_substantiated_wide, file.path(output_data_dir, "annual_state_counts_staff_substantiated_wide.csv"), na = "")

# Create a combined summary table
summary_table <- all_data %>%
  group_by(year) %>%
  summarise(
    inmate_alleged_total = sum(inmate_alleged, na.rm = TRUE),
    inmate_substantiated_total = sum(inmate_substantiated, na.rm = TRUE),
    staff_alleged_total = sum(staff_alleged, na.rm = TRUE),
    staff_substantiated_total = sum(staff_substantiated, na.rm = TRUE),
    n_states_inmate_alleged = sum(!is.na(inmate_alleged)),
    n_states_inmate_substantiated = sum(!is.na(inmate_substantiated)),
    n_states_staff_alleged = sum(!is.na(staff_alleged)),
    n_states_staff_substantiated = sum(!is.na(staff_substantiated)),
    .groups = "drop"
  )

write_csv(summary_table, file.path(output_data_dir, "annual_totals_summary_2012_2018.csv"), na = "")

cat("\n=== Data Summary ===\n")
cat("Total records:", nrow(all_data), "\n")
cat("Years covered:", paste(sort(unique(all_data$year)), collapse = ", "), "\n")
cat("States covered:", length(unique(all_data$state)), "\n")

cat("\n=== Annual Totals Summary ===\n")
print(summary_table)

cat("\n=== Sample of Long Format Data (first 20 rows) ===\n")
print(head(all_data, 20))

cat("\n=== Sample of Inmate Alleged Wide Format (first 10 states) ===\n")
print(head(inmate_alleged_wide, 10))

cat("\nFiles saved:\n")
cat("  - Long format:", long_output_path, "\n")
cat("  - Inmate Alleged (wide):", file.path(output_data_dir, "annual_state_counts_inmate_alleged_wide.csv"), "\n")
cat("  - Inmate Substantiated (wide):", file.path(output_data_dir, "annual_state_counts_inmate_substantiated_wide.csv"), "\n")
cat("  - Staff Alleged (wide):", file.path(output_data_dir, "annual_state_counts_staff_alleged_wide.csv"), "\n")
cat("  - Staff Substantiated (wide):", file.path(output_data_dir, "annual_state_counts_staff_substantiated_wide.csv"), "\n")
cat("  - Annual Totals Summary:", file.path(output_data_dir, "annual_totals_summary_2012_2018.csv"), "\n")

