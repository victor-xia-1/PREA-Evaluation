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

# Function to extract all states data from a year sheet for staff-on-inmate
extract_year_data <- function(sheet_name) {
  cat("Processing sheet:", sheet_name, "\n")
  
  # Read the sheet without skipping rows first to see structure
  df_raw <- readxl::read_excel(excel_path, sheet = sheet_name, col_names = FALSE)
  
  # Find the header row that contains "Alleg per 1000" and "Sub per 1000"
  header_row <- NULL
  alleged_per_1000_col <- NULL
  substantiated_per_1000_col <- NULL
  
  for (i in 1:20) {
    if (i <= nrow(df_raw)) {
      # Check all columns in this row for the headers
      for (j in 1:ncol(df_raw)) {
        cell_value <- as.character(df_raw[[j]][i])
        if (!is.na(cell_value)) {
          cell_lower <- tolower(cell_value)
          # Look for staff-on-inmate alleged per 1000 (should be in column 9 area)
          if (str_detect(cell_lower, "alleg.*per.*1000") || str_detect(cell_lower, "alleged.*per.*1000")) {
            # Check if this is in the staff-on-inmate section (column 8+)
            if (j >= 8) {
              header_row <- i
              alleged_per_1000_col <- j
            }
          }
          # Look for staff-on-inmate substantiated per 1000
          if (str_detect(cell_lower, "sub.*per.*1000") || str_detect(cell_lower, "substantiated.*per.*1000")) {
            if (j >= 8 && is.null(substantiated_per_1000_col)) {
              if (is.null(header_row)) header_row <- i
              substantiated_per_1000_col <- j
            }
          }
        }
      }
      if (!is.null(header_row) && !is.null(alleged_per_1000_col) && !is.null(substantiated_per_1000_col)) {
        break
      }
    }
  }
  
  if (is.null(alleged_per_1000_col) || is.null(substantiated_per_1000_col)) {
    cat("Could not find required headers in", sheet_name, "\n")
    return(data.frame(year = integer(), state = character(), alleged_per_1000 = double(), substantiated_per_1000 = double()))
  }
  
  cat("Found 'Alleg per 1000' in column", LETTERS[alleged_per_1000_col], "and 'Sub per 1000' in column", LETTERS[substantiated_per_1000_col], "at row", header_row, "\n")
  
  # Extract all state data
  results <- list()
  
  for (i in (header_row + 1):nrow(df_raw)) {
    # Check if this row has a state name in column 2
    state_name_raw <- df_raw[[2]][i]
    
    if (!is.na(state_name_raw)) {
      state_name <- normalize_state(state_name_raw)
      
      # Only process if it's one of our 50 states + DC
      if (state_name %in% states_50_dc) {
        alleged_value <- as_numeric_safely(df_raw[[alleged_per_1000_col]][i])
        substantiated_value <- as_numeric_safely(df_raw[[substantiated_per_1000_col]][i])
        
        results[[length(results) + 1]] <- data.frame(
          year = as.integer(sheet_name),
          state = state_name,
          alleged_per_1000 = alleged_value,
          substantiated_per_1000 = substantiated_value,
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
    return(data.frame(year = integer(), state = character(), alleged_per_1000 = double(), substantiated_per_1000 = double()))
  }
}

# Extract data for all years
all_data <- purrr::map_dfr(year_sheets, extract_year_data)

# Remove any rows with NA values for the per_1000 columns
all_data <- all_data %>%
  filter(!is.na(year), !is.na(state), 
         !is.na(alleged_per_1000) | !is.na(substantiated_per_1000))

# Save long format
long_output_path <- file.path(output_data_dir, "all_states_staff_on_inmate_alleged_substantiated_per_1000_2012_2018_long.csv")
write_csv(all_data, long_output_path, na = "")

cat("\nData Summary:\n")
cat("Total records:", nrow(all_data), "\n")
cat("Years covered:", paste(sort(unique(all_data$year)), collapse = ", "), "\n")
cat("States covered:", length(unique(all_data$state)), "\n")
cat("\nLong format CSV saved to:", long_output_path, "\n")

# Show sample
cat("\nSample of data (first 10 rows):\n")
print(head(all_data, 10))

