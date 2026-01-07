# Load required packages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(plotly)
  library(htmlwidgets)
})

# Paths
data_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/panel_prea_abusive_sexual_contact_2012_2018.csv"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"

# Read the clean dataset
prea_data <- read_csv(data_path, show_col_types = FALSE)

# Create interactive plot with plotly
create_interactive_plot <- function(data) {
  # Create the base plot
  p <- plot_ly(data, x = ~year, y = ~value, 
               color = ~state, colors = "Set3",
               type = 'scatter', mode = 'lines+markers',
               hovertemplate = paste(
                 "<b>%{fullData.name}</b><br>",
                 "Year: %{x}<br>",
                 "Count: %{y}<br>",
                 "<extra></extra>"
               )) %>%
    layout(
      title = list(
        text = "Inmate-on-inmate Abusive Sexual Contact (Substantiated), 2012–2018",
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
        title = "Count",
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
create_heatmap_plot <- function(data) {
  # Prepare data for heatmap - manually reshape since pivot_wider might not be available
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
      year_matrix[state_idx, year_idx] <- data$value[i]
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
      "Count: %{z}<br>",
      "<extra></extra>"
    )
  ) %>%
  layout(
    title = list(
      text = "Inmate-on-inmate Abusive Sexual Contact (Substantiated) Heatmap, 2012–2018",
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
create_state_comparison_plot <- function(data) {
  # Get top 10 states by total incidents
  top_states <- data %>%
    group_by(state) %>%
    summarise(total_incidents = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_incidents)) %>%
    head(10) %>%
    pull(state)
  
  # Filter data to top states
  top_data <- data %>% filter(state %in% top_states)
  
  p <- plot_ly(top_data, x = ~year, y = ~value, 
               color = ~state, colors = "Set3",
               type = 'scatter', mode = 'lines+markers',
               hovertemplate = paste(
                 "<b>%{fullData.name}</b><br>",
                 "Year: %{x}<br>",
                 "Count: %{y}<br>",
                 "<extra></extra>"
               )) %>%
    layout(
      title = list(
        text = "Top 10 States: Inmate-on-inmate Abusive Sexual Contact (Substantiated), 2012–2018",
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
        title = "Count",
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

# Create all visualizations
cat("Creating interactive visualizations for abusive sexual contact...\n")

# 1. Overlaid lines plot (all states)
p1 <- create_interactive_plot(prea_data)
htmlwidgets::saveWidget(p1, file.path(output_fig_dir, "interactive_prea_abusive_overlay.html"), selfcontained = FALSE)

# 2. Heatmap
p2 <- create_heatmap_plot(prea_data)
htmlwidgets::saveWidget(p2, file.path(output_fig_dir, "interactive_prea_abusive_heatmap.html"), selfcontained = FALSE)

# 3. Top 10 states comparison
p3 <- create_state_comparison_plot(prea_data)
htmlwidgets::saveWidget(p3, file.path(output_fig_dir, "interactive_prea_abusive_top10.html"), selfcontained = FALSE)

cat("Interactive visualizations saved:\n")
cat("1. Overlay plot (all states):", file.path(output_fig_dir, "interactive_prea_abusive_overlay.html"), "\n")
cat("2. Heatmap:", file.path(output_fig_dir, "interactive_prea_abusive_heatmap.html"), "\n")
cat("3. Top 10 states:", file.path(output_fig_dir, "interactive_prea_abusive_top10.html"), "\n")

# Display summary statistics
cat("\nData Summary:\n")
cat("Total records:", nrow(prea_data), "\n")
cat("Years covered:", paste(sort(unique(prea_data$year)), collapse = ", "), "\n")
cat("States covered:", length(unique(prea_data$state)), "\n")
cat("Value range:", min(prea_data$value, na.rm = TRUE), "to", max(prea_data$value, na.rm = TRUE), "\n")

# Show top 10 states by total incidents
top_states <- prea_data %>%
  group_by(state) %>%
  summarise(total_incidents = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_incidents)) %>%
  head(10)

cat("\nTop 10 states by total incidents (2012-2018):\n")
print(top_states)

cat("\nTo view the interactive plots, open the HTML files in your web browser.\n")
cat("The plots are zoomable and allow you to:\n")
cat("- Hover over data points for details\n")
cat("- Zoom in/out using mouse wheel or toolbar\n")
cat("- Pan around the plot\n")
cat("- Toggle states on/off by clicking legend items\n")
cat("- Use the toolbar to download images or reset view\n")

