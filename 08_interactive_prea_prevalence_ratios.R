# Load required packages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(plotly)
  library(htmlwidgets)
  library(stringr)
})

# Paths
data_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/panel_prea_prevalence_ratios_2012_2018.csv"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"

# Read the clean dataset
prea_data <- read_csv(data_path, show_col_types = FALSE)

# Create interactive plot with plotly for prevalence ratios
create_interactive_plot <- function(data, subtype_name) {
  # Create the base plot
  p <- plot_ly(data, x = ~year, y = ~prevalence_ratio, 
               color = ~state, colors = "Set3",
               type = 'scatter', mode = 'lines+markers',
               hovertemplate = paste(
                 "<b>%{fullData.name}</b><br>",
                 "Year: %{x}<br>",
                 "Prevalence Ratio: %{y:.3f}%<br>",
                 "Incidents: %{customdata}<br>",
                 "<extra></extra>"
               ),
               customdata = ~incidents) %>%
    layout(
      title = list(
        text = paste0(str_to_title(str_replace_all(subtype_name, "_", " ")), " - Prevalence Ratios, 2012–2018"),
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
        title = "Prevalence Ratio (per 100 prisoners)",
        zeroline = FALSE
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 1,
        font = list(size = 10)
      ),
      margin = list(r = 200),
      hovermode = "closest"
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d"),
      displaylogo = FALSE
    )
  
  return(p)
}

# Create a heatmap version for pattern recognition
create_heatmap_plot <- function(data, subtype_name) {
  # Prepare data for heatmap - manually reshape
  years <- sort(unique(data$year))
  states <- sort(unique(data$state))
  
  # Create matrix manually
  year_matrix <- matrix(0, nrow = length(states), ncol = length(years))
  rownames(year_matrix) <- states
  colnames(year_matrix) <- years
  
  # Fill matrix
  for (i in 1:nrow(data)) {
    state_idx <- which(rownames(year_matrix) == data$state[i])
    year_idx <- which(colnames(year_matrix) == data$year[i])
    if (length(state_idx) > 0 && length(year_idx) > 0) {
      year_matrix[state_idx, year_idx] <- data$prevalence_ratio[i]
    }
  }
  
  p <- plot_ly(
    z = year_matrix,
    x = colnames(year_matrix),
    y = rownames(year_matrix),
    type = "heatmap",
    colorscale = "Reds",
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Year: %{x}<br>",
      "Prevalence Ratio: %{z:.3f}%<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(
    title = list(
      text = paste0(str_to_title(str_replace_all(subtype_name, "_", " ")), " - Prevalence Ratios Heatmap, 2012–2018"),
      font = list(size = 16)
    ),
    xaxis = list(title = "Year"),
    yaxis = list(title = "State", categoryorder = "array", categoryarray = rev(states))
  ) %>%
  config(
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d"),
    displaylogo = FALSE
  )
  
  return(p)
}

# Create a state comparison plot
create_state_comparison_plot <- function(data, subtype_name) {
  # Get top 10 states by total prevalence ratio
  top_states <- data %>%
    group_by(state) %>%
    summarise(avg_prevalence = mean(prevalence_ratio, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(avg_prevalence)) %>%
    head(10) %>%
    pull(state)
  
  # Filter data to top states
  top_data <- data %>% filter(state %in% top_states)
  
  p <- plot_ly(top_data, x = ~year, y = ~prevalence_ratio, 
               color = ~state, colors = "Set3",
               type = 'scatter', mode = 'lines+markers',
               hovertemplate = paste(
                 "<b>%{fullData.name}</b><br>",
                 "Year: %{x}<br>",
                 "Prevalence Ratio: %{y:.3f}%<br>",
                 "Incidents: %{customdata}<br>",
                 "<extra></extra>"
               ),
               customdata = ~incidents) %>%
    layout(
      title = list(
        text = paste0("Top 10 States: ", str_to_title(str_replace_all(subtype_name, "_", " ")), " - Prevalence Ratios, 2012–2018"),
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
        title = "Prevalence Ratio (per 100 prisoners)",
        zeroline = FALSE
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 1,
        font = list(size = 12)
      ),
      margin = list(r = 200),
      hovermode = "closest"
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d"),
      displaylogo = FALSE
    )
  
  return(p)
}

# Get all subtypes
subtypes <- unique(prea_data$subtype)

cat("Creating interactive visualizations for prevalence ratios...\n")

# Create visualizations for each subtype
for (subtype in subtypes) {
  cat("\nProcessing:", subtype, "\n")
  
  # Clean subtype name for filenames
  clean_subtype <- str_replace_all(subtype, "[^a-zA-Z0-9]", "_")
  
  # Filter data for this subtype
  subtype_data <- prea_data %>% filter(subtype == !!subtype)
  
  if (nrow(subtype_data) > 0) {
    # 1. Overlaid lines plot (all states)
    p1 <- create_interactive_plot(subtype_data, clean_subtype)
    htmlwidgets::saveWidget(p1, file.path(output_fig_dir, paste0("interactive_prevalence_", clean_subtype, "_overlay.html")), selfcontained = FALSE)
    
    # 2. Heatmap
    p2 <- create_heatmap_plot(subtype_data, clean_subtype)
    htmlwidgets::saveWidget(p2, file.path(output_fig_dir, paste0("interactive_prevalence_", clean_subtype, "_heatmap.html")), selfcontained = FALSE)
    
    # 3. Top 10 states comparison
    p3 <- create_state_comparison_plot(subtype_data, clean_subtype)
    htmlwidgets::saveWidget(p3, file.path(output_fig_dir, paste0("interactive_prevalence_", clean_subtype, "_top10.html")), selfcontained = FALSE)
    
    cat("Created 3 interactive plots for", subtype)
  } else {
    cat("No data found for", subtype)
  }
}

cat("\n\nInteractive visualizations saved to figures directory\n")

# Display summary statistics
cat("\nData Summary by Subtype:\n")
summary_stats <- prea_data %>%
  group_by(subtype) %>%
  summarise(
    records = n(),
    years = length(unique(year)),
    states = length(unique(state)),
    min_ratio = min(prevalence_ratio, na.rm = TRUE),
    max_ratio = max(prevalence_ratio, na.rm = TRUE),
    mean_ratio = mean(prevalence_ratio, na.rm = TRUE),
    median_ratio = median(prevalence_ratio, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

# Show top 5 states by average prevalence for each subtype
cat("\nTop 5 states by average prevalence ratio for each subtype:\n")
for (subtype in subtypes) {
  cat("\n", subtype, ":\n")
  top_states <- prea_data %>%
    filter(subtype == !!subtype) %>%
    group_by(state) %>%
    summarise(avg_prevalence = mean(prevalence_ratio, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(avg_prevalence)) %>%
    head(5)
  print(top_states)
}

cat("\nTo view the interactive plots, open the HTML files in your web browser.\n")
cat("The plots show prevalence ratios (incidents per 100 prisoners) and allow you to:\n")
cat("- Hover over data points for details including raw counts\n")
cat("- Zoom in/out using mouse wheel or toolbar\n")
cat("- Pan around the plot\n")
cat("- Toggle states on/off by clicking legend items\n")
cat("- Use the toolbar to download images or reset view\n")
