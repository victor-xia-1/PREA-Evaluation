# Load required packages
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(htmlwidgets)
  library(stringr)
})

# Paths
excel_path <- "/Users/victorxia/Documents/Honors Thesis/PREA Data 102125.xlsx"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"

# Ensure output directory exists
if (!dir.exists(output_fig_dir)) dir.create(output_fig_dir, recursive = TRUE)

# Get year sheets (2012-2018)
all_sheets <- readxl::excel_sheets(excel_path)
year_sheets <- all_sheets[str_detect(all_sheets, "^20(1[2-8])$")]
year_sheets <- sort(year_sheets)

cat("Found year sheets:", paste(year_sheets, collapse = ", "), "\n")

# Helper: coerce numeric safely
as_numeric_safely <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

# Function to extract California data from a year sheet
extract_california_data <- function(sheet_name) {
  cat("Processing sheet:", sheet_name, "\n")
  
  # Read the sheet without skipping rows first to see structure
  df_raw <- readxl::read_excel(excel_path, sheet = sheet_name, col_names = FALSE)
  
  # Find the header row that contains "Alleg per 1000" or "Alleged per 1000"
  header_row <- NULL
  alleged_per_1000_col <- NULL
  
  for (i in 1:20) {
    if (i <= nrow(df_raw)) {
      # Check all columns in this row for the header
      for (j in 1:ncol(df_raw)) {
        cell_value <- as.character(df_raw[[j]][i])
        if (!is.na(cell_value) && 
            (str_detect(tolower(cell_value), "alleg.*per.*1000") || 
             str_detect(tolower(cell_value), "alleged.*per.*1000"))) {
          header_row <- i
          alleged_per_1000_col <- j
          break
        }
      }
      if (!is.null(header_row)) break
    }
  }
  
  if (is.null(alleged_per_1000_col)) {
    cat("Could not find 'Alleg per 1000' header in", sheet_name, "\n")
    return(data.frame(year = integer(), alleged_per_1000 = double()))
  }
  
  cat("Found 'Alleg per 1000' in column", LETTERS[alleged_per_1000_col], "(column", alleged_per_1000_col, ") at row", header_row, "\n")
  
  # Try to find where the data starts - look for "California"
  california_row <- NULL
  for (i in (header_row + 1):nrow(df_raw)) {
    if (!is.na(df_raw[[2]][i]) && str_detect(tolower(df_raw[[2]][i]), "california")) {
      california_row <- i
      break
    }
  }
  
  if (is.null(california_row)) {
    cat("Could not find California in", sheet_name, "\n")
    return(data.frame(year = integer(), alleged_per_1000 = double()))
  }
  
  # Get the value from the correct column
  alleged_per_1000 <- as_numeric_safely(df_raw[[alleged_per_1000_col]][california_row])
  
  result <- data.frame(
    year = as.integer(sheet_name),
    alleged_per_1000 = alleged_per_1000,
    stringsAsFactors = FALSE
  )
  
  cat("Extracted California data for", sheet_name, ": ", alleged_per_1000, " per 1000\n")
  return(result)
}

# Extract California data for all years
california_data <- purrr::map_dfr(year_sheets, extract_california_data)

# Remove any rows with NA values
california_data <- california_data %>%
  filter(!is.na(year), !is.na(alleged_per_1000))

cat("\nCalifornia Data Summary:\n")
print(california_data)

# Create static plot
if (nrow(california_data) > 0) {
  p_static <- ggplot(california_data, aes(x = year, y = alleged_per_1000)) +
    geom_line(color = "#2C7FB8", linewidth = 1.2) +
    geom_point(color = "#2C7FB8", size = 3) +
    labs(
      title = "California: Inmate-on-Inmate Alleged Incidents per 1000 Prisoners, 2012–2018",
      subtitle = "Prevalence rate over time",
      x = "Year",
      y = "Alleged per 1000 Prisoners",
      caption = "Source: PREA data workbook"
    ) +
    scale_x_continuous(breaks = 2012:2018) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11)
    )
  
  # Save static figure
  fig_path_static <- file.path(output_fig_dir, "california_alleged_per_1000_2012_2018.png")
  ggsave(filename = fig_path_static, plot = p_static, width = 10, height = 6, dpi = 300)
  cat("\nStatic figure saved to:", fig_path_static, "\n")
  
  # Create interactive plot
  p_interactive <- plot_ly(california_data, x = ~year, y = ~alleged_per_1000,
                          type = 'scatter', mode = 'lines+markers',
                          line = list(color = '#2C7FB8', width = 2),
                          marker = list(color = '#2C7FB8', size = 8),
                          hovertemplate = paste(
                            "<b>California</b><br>",
                            "Year: %{x}<br>",
                            "Alleged per 1000: %{y:.3f}<br>",
                            "<extra></extra>"
                          )) %>%
    layout(
      title = list(
        text = "California: Inmate-on-Inmate Alleged Incidents per 1000 Prisoners, 2012–2018",
        font = list(size = 16)
      ),
      xaxis = list(
        title = "Year",
        tickmode = "linear",
        tick0 = 2012,
        dtick = 1,
        range = c(2011.5, 2018.5)
      ),
      yaxis = list(
        title = "Alleged per 1000 Prisoners",
        zeroline = FALSE
      ),
      hovermode = "closest"
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d"),
      displaylogo = FALSE
    )
  
  # Save interactive figure
  fig_path_interactive <- file.path(output_fig_dir, "california_alleged_per_1000_2012_2018.html")
  htmlwidgets::saveWidget(p_interactive, fig_path_interactive, selfcontained = FALSE)
  cat("Interactive figure saved to:", fig_path_interactive, "\n")
  
} else {
  cat("\nNo data found for California\n")
}

