# Load required packages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(tidyr)
  library(stringr)
  library(htmlwidgets)
})

# Paths
data_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/all_states_alleged_substantiated_per_1000_2012_2020_long.csv"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"

# Ensure output directory exists
if (!dir.exists(output_fig_dir)) dir.create(output_fig_dir, recursive = TRUE)

# Read the data
cat("Reading alleged/substantiated data...\n")
panel_data <- read_csv(data_path, show_col_types = FALSE)

# Filter for alleged inmate-on-inmate data (this is the alleged_per_1000 column)
alleged_data <- panel_data %>%
  select(year, state, alleged_per_1000) %>%
  filter(!is.na(alleged_per_1000), !is.na(state), !is.na(year)) %>%
  filter(year >= 2012 & year <= 2020)

cat("Data loaded:", nrow(alleged_data), "state-year observations\n")
cat("Years:", paste(sort(unique(alleged_data$year)), collapse = ", "), "\n")
cat("States:", length(unique(alleged_data$state)), "\n")

# Create static heatmap using ggplot2
cat("\nCreating static heatmap...\n")
p_static <- ggplot(alleged_data, aes(x = factor(year), y = reorder(state, -alleged_per_1000, mean), 
                                     fill = alleged_per_1000)) +
  geom_tile(color = "white", linewidth = 0.1) +
  scale_fill_gradient(low = "#fff5f0", high = "#67000d", 
                     name = "Alleged\nper 1,000",
                     na.value = "grey90") +
  labs(
    title = "Inmate-on-Inmate Alleged Sexual Victimization Rates per 1,000",
    subtitle = "All 50 States + DC, 2012-2020",
    x = "Year",
    y = "State",
    caption = "Source: PREA Data (2012-2020)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank()
  )

ggsave(file.path(output_fig_dir, "heatmap_alleged_inmate_on_inmate_2012_2020.png"),
       p_static, width = 12, height = 14, dpi = 300)

cat("Static heatmap saved to:", file.path(output_fig_dir, "heatmap_alleged_inmate_on_inmate_2012_2020.png"), "\n")

# Create interactive heatmap using plotly
cat("\nCreating interactive heatmap...\n")

# Prepare data for heatmap - create matrix
years <- sort(unique(alleged_data$year))
states <- sort(unique(alleged_data$state))

# Create matrix
year_matrix <- matrix(NA, nrow = length(states), ncol = length(years))
rownames(year_matrix) <- states
colnames(year_matrix) <- years

# Fill matrix
for (i in 1:nrow(alleged_data)) {
  state_idx <- which(rownames(year_matrix) == alleged_data$state[i])
  year_idx <- which(colnames(year_matrix) == as.character(alleged_data$year[i]))
  if (length(state_idx) > 0 && length(year_idx) > 0) {
    year_matrix[state_idx, year_idx] <- alleged_data$alleged_per_1000[i]
  }
}

# Create interactive heatmap
p_interactive <- plot_ly(
  z = year_matrix,
  x = colnames(year_matrix),
  y = rownames(year_matrix),
  type = "heatmap",
  colorscale = list(
    c(0, "#fff5f0"),
    c(0.2, "#fee0d2"),
    c(0.4, "#fc9272"),
    c(0.6, "#ef3b2c"),
    c(0.8, "#cb181d"),
    c(1, "#67000d")
  ),
  hovertemplate = paste(
    "<b>%{y}</b><br>",
    "Year: %{x}<br>",
    "Alleged per 1,000: %{z:.2f}<br>",
    "<extra></extra>"
  ),
  colorbar = list(title = "Alleged per 1,000")
) %>%
layout(
  title = list(
    text = "Inmate-on-Inmate Alleged Sexual Victimization Rates per 1,000<br>All 50 States + DC, 2012-2020",
    font = list(size = 16)
  ),
  xaxis = list(
    title = "Year",
    tickmode = "linear",
    tick0 = 2012,
    dtick = 1
  ),
  yaxis = list(
    title = "State",
    categoryorder = "array",
    categoryarray = rev(states)
  ),
  margin = list(l = 120, r = 50, t = 80, b = 50)
) %>%
config(
  displayModeBar = TRUE,
  modeBarButtonsToRemove = c("pan2d", "lasso2d", "select2d"),
  displaylogo = FALSE
)

# Save interactive heatmap
htmlwidgets::saveWidget(p_interactive, 
                       file.path(output_fig_dir, "heatmap_alleged_inmate_on_inmate_2012_2020.html"),
                       selfcontained = FALSE)

cat("Interactive heatmap saved to:", file.path(output_fig_dir, "heatmap_alleged_inmate_on_inmate_2012_2020.html"), "\n")

# Summary statistics
cat("\n=== Summary Statistics ===\n")
cat("Alleged per 1,000 by year:\n")
print(alleged_data %>%
  group_by(year) %>%
  summarise(
    mean = mean(alleged_per_1000, na.rm = TRUE),
    median = median(alleged_per_1000, na.rm = TRUE),
    min = min(alleged_per_1000, na.rm = TRUE),
    max = max(alleged_per_1000, na.rm = TRUE),
    n_states = n(),
    .groups = "drop"
  ))

cat("\nTop 10 states by average alleged per 1,000 (2012-2020):\n")
top_states <- alleged_data %>%
  group_by(state) %>%
  summarise(avg_alleged = mean(alleged_per_1000, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_alleged)) %>%
  head(10)
print(top_states)

cat("\nHeatmap creation complete!\n")

