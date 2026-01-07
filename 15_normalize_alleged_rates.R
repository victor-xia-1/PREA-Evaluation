# Load required packages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(scales)
})

# Paths
panel_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/panel_prea_2012_2018.csv"
output_data_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/panel_prea_alleged_normalized.csv"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"
if (!dir.exists(dirname(output_data_path))) dir.create(dirname(output_data_path), recursive = TRUE)
if (!dir.exists(output_fig_dir)) dir.create(output_fig_dir, recursive = TRUE)

# Read panel data
cat("Reading tidy panel dataset...\n")
panel <- read_csv(panel_path, show_col_types = FALSE)

# Filter to inmate-on-inmate alleged subtype (already normalized earlier)
target_subtype <- "inmate-on-inmate nonconsensual sexual acts, alleged"
panel_filtered <- panel %>%
  filter(subtype == target_subtype) %>%
  rename(alleged_per_1000 = value)

cat("Observations for target subtype:", nrow(panel_filtered), "\n")

# Normalize within each year to make states comparable
panel_normalized <- panel_filtered %>%
  group_by(year) %>%
  mutate(
    year_mean = mean(alleged_per_1000, na.rm = TRUE),
    year_sd = sd(alleged_per_1000, na.rm = TRUE),
    z_score = ifelse(year_sd > 0, (alleged_per_1000 - year_mean) / year_sd, 0),
    min_val = min(alleged_per_1000, na.rm = TRUE),
    max_val = max(alleged_per_1000, na.rm = TRUE),
    minmax_normalized = ifelse(max_val > min_val,
                               (alleged_per_1000 - min_val) / (max_val - min_val),
                               0.5),
    percentile_rank = percent_rank(alleged_per_1000)
  ) %>%
  ungroup() %>%
  select(state, year, alleged_per_1000, z_score, minmax_normalized, percentile_rank)

# Save normalized dataset
write_csv(panel_normalized, output_data_path, na = "")
cat("Normalized panel saved to:", output_data_path, "\n")

# Create visualization: z-scores by state across years
top_states <- panel_normalized %>%
  group_by(state) %>%
  summarise(avg_z = mean(z_score, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_z)) %>%
  slice_head(n = 15) %>%
  pull(state)

p <- panel_normalized %>%
  filter(state %in% top_states) %>%
  ggplot(aes(x = year, y = z_score, color = state)) +
  geom_line() +
  geom_point(size = 1.5) +
  labs(
    title = "Normalized Alleged Rates (z-score) for Top 15 States",
    subtitle = "Inmate-on-inmate nonconsensual sexual acts, alleged (higher = more above annual average)",
    x = "Year",
    y = "Z-score (standardized within year)",
    caption = "Source: PREA panel (2012–2018)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

fig_path <- file.path(output_fig_dir, "normalized_alleged_rates_zscore_top15.png")
ggsave(fig_path, p, width = 12, height = 7, dpi = 300)
cat("Visualization saved to:", fig_path, "\n")

# Summary statistics
cat("\nSummary of normalized metrics:\n")
print(panel_normalized %>%
        summarise(
          z_mean = mean(z_score, na.rm = TRUE),
          z_sd = sd(z_score, na.rm = TRUE),
          minmax_mean = mean(minmax_normalized, na.rm = TRUE),
          percentile_mean = mean(percentile_rank, na.rm = TRUE)
        ))

cat("\nStates with highest average z-score:\n")
print(
  panel_normalized %>%
    group_by(state) %>%
    summarise(avg_z = mean(z_score, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(avg_z)) %>%
    slice_head(n = 10)
)

cat("\nStates with lowest average z-score:\n")
print(
  panel_normalized %>%
    group_by(state) %>%
    summarise(avg_z = mean(z_score, na.rm = TRUE), .groups = "drop") %>%
    arrange(avg_z) %>%
    slice_head(n = 10)
)

cat("\nNormalization complete.\n")

