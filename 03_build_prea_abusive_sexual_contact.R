# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
  library(ggplot2)
  library(purrr)
})

# Paths
excel_path <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"
output_fig_dir  <- "/Users/victorxia/Documents/Honors Thesis/figures"

# Ensure output directories exist
if (!dir.exists(output_data_dir)) dir.create(output_data_dir, recursive = TRUE)
if (!dir.exists(output_fig_dir)) dir.create(output_fig_dir, recursive = TRUE)

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

# Get year sheets (2012-2018)
all_sheets <- readxl::excel_sheets(excel_path)
year_sheets <- all_sheets[str_detect(all_sheets, "^20(1[2-8])$")]
year_sheets <- sort(year_sheets)

cat("Found year sheets:", paste(year_sheets, collapse = ", "), "\n")

# Function to read and tidy a single year sheet for abusive sexual contact
read_and_tidy_year_sheet <- function(sheet_name) {
  cat("Processing sheet:", sheet_name, "\n")
  
  # Read the sheet, skip metadata rows
  df <- readxl::read_excel(excel_path, sheet = sheet_name, skip = 10) %>%
    janitor::remove_empty(which = c("rows","cols"))

  # Clean names
  df <- janitor::clean_names(df)

  # Find the jurisdiction/state column (usually first column with state names)
  state_col <- names(df)[1]
  
  # Look for the specific columns we want
  # Based on the structure, we want "Inmate-on-inmate" -> "Substantiated" column (abusive sexual contact)
  inmate_cols <- names(df)[str_detect(names(df), "inmate.*inmate|inmate_on_inmate")]
  
  if (length(inmate_cols) == 0) {
    cat("No inmate-on-inmate columns found in", sheet_name, "\n")
    return(data.frame(state = character(), year = integer(), subtype = character(), value = double()))
  }
  
  # Find the "Substantiated" row (should be row 2 based on structure)
  substantiated_row <- which(str_detect(df[[1]], "Substantiated|substantiated"))
  
  if (length(substantiated_row) == 0) {
    # Try looking in other columns
    for (i in 1:ncol(df)) {
      substantiated_row <- which(str_detect(df[[i]], "Substantiated|substantiated"))
      if (length(substantiated_row) > 0) break
    }
  }
  
  if (length(substantiated_row) == 0) {
    cat("No 'Substantiated' row found in", sheet_name, "\n")
    return(data.frame(state = character(), year = integer(), subtype = character(), value = double()))
  }
  
  # States start after the header rows - find where actual state data begins
  # Look for the first row that has a state name in column 2
  state_start <- NULL
  for (i in substantiated_row:nrow(df)) {
    if (!is.na(df[[2]][i]) && df[[2]][i] != "" && df[[2]][i] != "Total" && 
        !str_detect(df[[2]][i], "Federal|State|Jurisdiction")) {
      state_start <- i
      break
    }
  }
  
  if (is.null(state_start)) {
    cat("Could not find state data start in", sheet_name, "\n")
    return(data.frame(state = character(), year = integer(), subtype = character(), value = double()))
  }
  
  state_data <- df[state_start:nrow(df), ]
  
  # Get state names (second column based on structure)
  states <- state_data[[2]]  # State names are in column 2
  states <- states[!is.na(states) & states != "" & states != "Total"]
  
  # Get the inmate-on-inmate substantiated values (column 6 based on structure)
  inmate_col <- names(df)[str_detect(names(df), "inmate.*inmate|inmate_on_inmate")]
  if (length(inmate_col) == 0) {
    cat("No inmate-on-inmate column found in", sheet_name, "\n")
    return(data.frame(state = character(), year = integer(), subtype = character(), value = double()))
  }
  
  # Extract values from the inmate-on-inmate substantiated column (column 5)
  # Based on structure: column 5 contains the substantiated values
  substantiated_col_idx <- which(names(state_data) == inmate_col) + 1  # Skip to substantiated column
  if (substantiated_col_idx > ncol(state_data)) {
    cat("Could not find substantiated column in", sheet_name, "\n")
    return(data.frame(state = character(), year = integer(), subtype = character(), value = double()))
  }
  
  values <- state_data[[substantiated_col_idx]][1:length(states)]
  values <- as_numeric_safely(values)
  
  # Create result dataframe
  result <- data.frame(
    state = normalize_state(states),
    year = as.integer(sheet_name),
    subtype = "inmate-on-inmate abusive sexual contact, substantiated",
    value = values,
    stringsAsFactors = FALSE
  )
  
  # Filter to valid states and remove NAs
  result <- result %>%
    filter(!is.na(state), !is.na(value), state %in% states_50_dc)
  
  cat("Extracted", nrow(result), "records from", sheet_name, "\n")
  return(result)
}

# Read all year sheets and bind
prea_long <- purrr::map_dfr(year_sheets, read_and_tidy_year_sheet)

# Clean subtype labels
prea_long <- prea_long %>%
  mutate(
    subtype = str_squish(str_to_lower(subtype))
  )

# Save combined tidy dataset
output_data_path <- file.path(output_data_dir, "panel_prea_abusive_sexual_contact_2012_2018.csv")
readr::write_csv(prea_long, output_data_path, na = "")

cat("Tidy dataset saved to:", output_data_path, "\n")
cat("Total records:", nrow(prea_long), "\n")
cat("Years covered:", paste(sort(unique(prea_long$year)), collapse = ", "), "\n")
cat("States covered:", length(unique(prea_long$state)), "\n")

# Create plot
if (nrow(prea_long) > 0) {
  plot_df <- prea_long %>% filter(subtype == "inmate-on-inmate abusive sexual contact, substantiated")
  
  if (nrow(plot_df) > 0) {
    # Plot: annual trends by state, faceted
    p <- ggplot(plot_df, aes(x = year, y = value, group = state)) +
      geom_line(color = "#E31A1C", linewidth = 0.4, alpha = 0.9) +
      geom_point(color = "#E31A1C", size = 0.8, alpha = 0.9) +
      facet_wrap(~ state, ncol = 8, scales = "free_y") +
      labs(
        title = "Inmate-on-inmate Abusive Sexual Contact (Substantiated), 2012–2018",
        subtitle = "Annual counts by state; facets show 50 states + DC",
        x = "Year",
        y = "Count",
        caption = "Source: PREA data workbook"
      ) +
      scale_x_continuous(breaks = 2012:2018) +
      theme_minimal(base_size = 10) +
      theme(
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        plot.title = element_text(face = "bold")
      )
    
    # Save figure
    fig_path <- file.path(output_fig_dir, "annual_inmate_on_inmate_abusive_sexual_contact_substantiated_2012_2018.png")
    ggsave(filename = fig_path, plot = p, width = 14, height = 10, dpi = 300)
    
    cat("Figure saved to:", fig_path, "\n")
  } else {
    cat("No data found for plotting\n")
  }
} else {
  cat("No data extracted from any sheets\n")
}
