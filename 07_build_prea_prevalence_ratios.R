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

# Function to read and tidy a single year sheet for all four subtypes
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
  inmate_cols <- names(df)[str_detect(names(df), "inmate.*inmate|inmate_on_inmate")]
  
  if (length(inmate_cols) == 0) {
    cat("No inmate-on-inmate columns found in", sheet_name, "\n")
    return(data.frame(state = character(), year = integer(), subtype = character(), 
                     incidents = double(), prisoners = double(), prevalence_ratio = double()))
  }
  
  # Find the "Alleged" and "Substantiated" rows
  alleged_row <- which(str_detect(df[[1]], "Alleged|alleged"))
  substantiated_row <- which(str_detect(df[[1]], "Substantiated|substantiated"))
  
  if (length(alleged_row) == 0) {
    # Try looking in other columns
    for (i in 1:ncol(df)) {
      alleged_row <- which(str_detect(df[[i]], "Alleged|alleged"))
      if (length(alleged_row) > 0) break
    }
  }
  
  if (length(substantiated_row) == 0) {
    # Try looking in other columns
    for (i in 1:ncol(df)) {
      substantiated_row <- which(str_detect(df[[i]], "Substantiated|substantiated"))
      if (length(substantiated_row) > 0) break
    }
  }
  
  if (length(alleged_row) == 0 || length(substantiated_row) == 0) {
    cat("Could not find Alleged or Substantiated rows in", sheet_name, "\n")
    return(data.frame(state = character(), year = integer(), subtype = character(), 
                     incidents = double(), prisoners = double(), prevalence_ratio = double()))
  }
  
  # States start after the header rows - find where actual state data begins
  state_start <- NULL
  for (i in min(alleged_row, substantiated_row):nrow(df)) {
    if (!is.na(df[[2]][i]) && df[[2]][i] != "" && df[[2]][i] != "Total" && 
        !str_detect(df[[2]][i], "Federal|State|Jurisdiction")) {
      state_start <- i
      break
    }
  }
  
  if (is.null(state_start)) {
    cat("Could not find state data start in", sheet_name, "\n")
    return(data.frame(state = character(), year = integer(), subtype = character(), 
                     incidents = double(), prisoners = double(), prevalence_ratio = double()))
  }
  
  state_data <- df[state_start:nrow(df), ]
  
  # Get state names (second column based on structure)
  states <- state_data[[2]]  # State names are in column 2
  states <- states[!is.na(states) & states != "" & states != "Total"]
  
  # Get prisoner counts (third column based on structure)
  prisoners <- state_data[[3]][1:length(states)]
  prisoners <- as_numeric_safely(prisoners)
  
  # Get the inmate-on-inmate columns
  inmate_col <- names(df)[str_detect(names(df), "inmate.*inmate|inmate_on_inmate")]
  if (length(inmate_col) == 0) {
    cat("No inmate-on-inmate column found in", sheet_name, "\n")
    return(data.frame(state = character(), year = integer(), subtype = character(), 
                     incidents = double(), prisoners = double(), prevalence_ratio = double()))
  }
  
  # Extract values for all four subtypes
  # Column structure: Inmate-on-inmate, then Alleged, then Substantiated
  inmate_col_idx <- which(names(state_data) == inmate_col)
  
  # 1. Nonconsensual Sexual Acts, Alleged (column 4)
  nonconsensual_alleged_idx <- inmate_col_idx
  nonconsensual_alleged_values <- state_data[[nonconsensual_alleged_idx]][1:length(states)]
  nonconsensual_alleged_values <- as_numeric_safely(nonconsensual_alleged_values)
  
  # 2. Nonconsensual Sexual Acts, Substantiated (column 5)
  nonconsensual_substantiated_idx <- inmate_col_idx + 1
  if (nonconsensual_substantiated_idx <= ncol(state_data)) {
    nonconsensual_substantiated_values <- state_data[[nonconsensual_substantiated_idx]][1:length(states)]
    nonconsensual_substantiated_values <- as_numeric_safely(nonconsensual_substantiated_values)
  } else {
    nonconsensual_substantiated_values <- rep(0, length(states))
  }
  
  # 3. Abusive Sexual Contact, Alleged (column 6)
  abusive_alleged_idx <- inmate_col_idx + 2
  if (abusive_alleged_idx <= ncol(state_data)) {
    abusive_alleged_values <- state_data[[abusive_alleged_idx]][1:length(states)]
    abusive_alleged_values <- as_numeric_safely(abusive_alleged_values)
  } else {
    abusive_alleged_values <- rep(0, length(states))
  }
  
  # 4. Abusive Sexual Contact, Substantiated (column 7)
  abusive_substantiated_idx <- inmate_col_idx + 3
  if (abusive_substantiated_idx <= ncol(state_data)) {
    abusive_substantiated_values <- state_data[[abusive_substantiated_idx]][1:length(states)]
    abusive_substantiated_values <- as_numeric_safely(abusive_substantiated_values)
  } else {
    abusive_substantiated_values <- rep(0, length(states))
  }
  
  # Create result dataframes for each subtype
  subtypes <- c(
    "inmate-on-inmate nonconsensual sexual acts, alleged",
    "inmate-on-inmate nonconsensual sexual acts, substantiated", 
    "inmate-on-inmate abusive sexual contact, alleged",
    "inmate-on-inmate abusive sexual contact, substantiated"
  )
  
  values_list <- list(
    nonconsensual_alleged_values,
    nonconsensual_substantiated_values,
    abusive_alleged_values,
    abusive_substantiated_values
  )
  
  all_results <- list()
  
  for (i in 1:length(subtypes)) {
    result <- data.frame(
      state = normalize_state(states),
      year = as.integer(sheet_name),
      subtype = subtypes[i],
      incidents = values_list[[i]],
      prisoners = prisoners,
      prevalence_ratio = ifelse(prisoners > 0, (values_list[[i]] / prisoners) * 100, 0),
      stringsAsFactors = FALSE
    )
    
    # Filter to valid states and remove NAs
    result <- result %>%
      filter(!is.na(state), !is.na(incidents), !is.na(prisoners), state %in% states_50_dc)
    
    all_results[[i]] <- result
  }
  
  # Combine all results
  combined_result <- do.call(rbind, all_results)
  
  cat("Extracted", nrow(combined_result), "records from", sheet_name, "\n")
  return(combined_result)
}

# Read all year sheets and bind
prea_long <- purrr::map_dfr(year_sheets, read_and_tidy_year_sheet)

# Clean subtype labels
prea_long <- prea_long %>%
  mutate(
    subtype = str_squish(str_to_lower(subtype))
  )

# Save combined tidy dataset
output_data_path <- file.path(output_data_dir, "panel_prea_prevalence_ratios_2012_2018.csv")
readr::write_csv(prea_long, output_data_path, na = "")

cat("Tidy dataset saved to:", output_data_path, "\n")
cat("Total records:", nrow(prea_long), "\n")
cat("Years covered:", paste(sort(unique(prea_long$year)), collapse = ", "), "\n")
cat("States covered:", length(unique(prea_long$state)), "\n")

# Create plots for each subtype
subtypes <- unique(prea_long$subtype)

for (subtype in subtypes) {
  cat("\nCreating plot for:", subtype, "\n")
  
  plot_df <- prea_long %>% filter(subtype == !!subtype)
  
  if (nrow(plot_df) > 0) {
    # Clean subtype name for filename
    clean_subtype <- str_replace_all(subtype, "[^a-zA-Z0-9]", "_")
    
    # Plot: annual trends by state, faceted
    p <- ggplot(plot_df, aes(x = year, y = prevalence_ratio, group = state)) +
      geom_line(color = "#2C7FB8", linewidth = 0.4, alpha = 0.9) +
      geom_point(color = "#2C7FB8", size = 0.8, alpha = 0.9) +
      facet_wrap(~ state, ncol = 8, scales = "free_y") +
      labs(
        title = paste0(str_to_title(str_replace_all(subtype, "_", " ")), " - Prevalence Ratios, 2012–2018"),
        subtitle = "Incidents per 100 prisoners by state; facets show 50 states + DC",
        x = "Year",
        y = "Prevalence Ratio (per 100 prisoners)",
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
    fig_path <- file.path(output_fig_dir, paste0("prevalence_ratios_", clean_subtype, "_2012_2018.png"))
    ggsave(filename = fig_path, plot = p, width = 14, height = 10, dpi = 300)
    
    cat("Figure saved to:", fig_path, "\n")
  } else {
    cat("No data found for plotting", subtype, "\n")
  }
}

# Display summary statistics
cat("\nData Summary by Subtype:\n")
summary_stats <- prea_long %>%
  group_by(subtype) %>%
  summarise(
    records = n(),
    years = length(unique(year)),
    states = length(unique(state)),
    min_ratio = min(prevalence_ratio, na.rm = TRUE),
    max_ratio = max(prevalence_ratio, na.rm = TRUE),
    mean_ratio = mean(prevalence_ratio, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)


