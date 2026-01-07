# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
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

# Helper: normalize state names (trim, title case, special cases)
normalize_state <- function(x) {
  x <- str_trim(as.character(x))
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  # Title case
  x <- str_to_title(x)
  # Handle Washington, D.C. variants
  x <- str_replace_all(x, "Washington,? D\\.?C\\.?", "District of Columbia")
  x <- str_replace_all(x, "D\\.?c\\.?", "District of Columbia")
  x
}

# Helper: coerce numeric safely
as_numeric_safely <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

# Get year sheets (2012-2020)
all_sheets <- readxl::excel_sheets(excel_path)
year_sheets <- all_sheets[str_detect(all_sheets, "^20(1[2-9]|20)$")]
year_sheets <- sort(year_sheets)

cat("Found year sheets:", paste(year_sheets, collapse = ", "), "\n")

# Function to extract all states data from a year sheet
extract_year_data <- function(sheet_name) {
  cat("Processing sheet:", sheet_name, "\n")
  
  # Read the sheet without skipping rows first to see structure
  df_raw <- readxl::read_excel(excel_path, sheet = sheet_name, col_names = FALSE)
  
  # Check if this is the new format (2019-2020) which has counts instead of per 1000
  is_new_format <- as.integer(sheet_name) >= 2019
  
  if (is_new_format) {
    # New format: Find "Alleged" and "Substantiated" in row 12, columns 5-6 (Nonconsensual sexual acts)
    # Also need prisoners count in column 3
    header_row <- 12
    alleged_col <- 5  # Nonconsensual sexual acts - Alleged
    substantiated_col <- 6  # Nonconsensual sexual acts - Substantiated
    prisoners_col <- 3
    
    cat("Using new format (2019-2020): Alleged in column", LETTERS[alleged_col], 
        ", Substantiated in column", LETTERS[substantiated_col], 
        ", Prisoners in column", LETTERS[prisoners_col], "\n")
  } else {
    # Old format: Find "Alleg per 1000" and "Sub per 1000"
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
            if (str_detect(cell_lower, "alleg.*per.*1000") || str_detect(cell_lower, "alleged.*per.*1000")) {
              header_row <- i
              alleged_per_1000_col <- j
            }
            if (str_detect(cell_lower, "sub.*per.*1000") || str_detect(cell_lower, "substantiated.*per.*1000")) {
              if (is.null(header_row)) header_row <- i
              substantiated_per_1000_col <- j
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
    
    alleged_col <- alleged_per_1000_col
    substantiated_col <- substantiated_per_1000_col
    prisoners_col <- NULL  # Not needed for old format
    
    cat("Found 'Alleg per 1000' in column", LETTERS[alleged_col], 
        "and 'Sub per 1000' in column", LETTERS[substantiated_col], "at row", header_row, "\n")
  }
  
  # Extract all state data
  results <- list()
  
  for (i in (header_row + 1):nrow(df_raw)) {
    # Check if this row has a state name in column 2
    state_name_raw <- df_raw[[2]][i]
    
    if (!is.na(state_name_raw)) {
      state_name <- normalize_state(state_name_raw)
      
      # Only process if it's one of our 50 states + DC
      if (state_name %in% states_50_dc) {
        if (is_new_format) {
          # Calculate per 1000 from counts
          alleged_count <- as_numeric_safely(df_raw[[alleged_col]][i])
          substantiated_count <- as_numeric_safely(df_raw[[substantiated_col]][i])
          prisoners <- as_numeric_safely(df_raw[[prisoners_col]][i])
          
          alleged_per_1000 <- ifelse(!is.na(prisoners) && prisoners > 0, 
                                    (alleged_count / prisoners) * 1000, NA_real_)
          substantiated_per_1000 <- ifelse(!is.na(prisoners) && prisoners > 0, 
                                          (substantiated_count / prisoners) * 1000, NA_real_)
        } else {
          # Already in per 1000 format
          alleged_per_1000 <- as_numeric_safely(df_raw[[alleged_col]][i])
          substantiated_per_1000 <- as_numeric_safely(df_raw[[substantiated_col]][i])
        }
        
        results[[length(results) + 1]] <- data.frame(
          year = as.integer(sheet_name),
          state = state_name,
          alleged_per_1000 = alleged_per_1000,
          substantiated_per_1000 = substantiated_per_1000,
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

# Create wide format: rows are years, columns are states with two columns each (Alleged and Substantiated)
wide_data <- all_data %>%
  pivot_wider(
    id_cols = year,
    names_from = state,
    values_from = c(alleged_per_1000, substantiated_per_1000),
    names_sep = "_"
  )

# Reorder columns to have Alleged and Substantiated together for each state
# Get unique states
states_in_data <- sort(unique(all_data$state))

# Create column order: Year, then for each state: State_Alleged, State_Substantiated
col_order <- c("year")
for (state in states_in_data) {
  col_order <- c(col_order, paste0("alleged_per_1000_", state), paste0("substantiated_per_1000_", state))
}

# Reorder columns (only include columns that exist)
col_order <- col_order[col_order %in% names(wide_data)]
wide_data <- wide_data[, col_order]

# Sort by year
wide_data <- wide_data %>% arrange(year)

# Save to CSV
output_path <- file.path(output_data_dir, "all_states_alleged_substantiated_per_1000_2012_2020.csv")
write_csv(wide_data, output_path, na = "")

cat("\nData Summary:\n")
cat("Total records:", nrow(all_data), "\n")
cat("Years covered:", paste(sort(unique(all_data$year)), collapse = ", "), "\n")
cat("States covered:", length(unique(all_data$state)), "\n")
cat("Wide format:", nrow(wide_data), "rows (years) x", ncol(wide_data), "columns\n")
cat("\nCSV file saved to:", output_path, "\n")

# Also create a long format version for easier analysis
long_output_path <- file.path(output_data_dir, "all_states_alleged_substantiated_per_1000_2012_2020_long.csv")
write_csv(all_data, long_output_path, na = "")

cat("Long format CSV also saved to:", long_output_path, "\n")

# Show sample of the data
cat("\nSample of wide format (first 5 rows, first 10 columns):\n")
print(wide_data[1:min(5, nrow(wide_data)), 1:min(10, ncol(wide_data))])

