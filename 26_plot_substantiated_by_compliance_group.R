## Plot substantiated per 1000 over time by cumulative PREA compliance groups
##
## This script:
## - Reads the merged panel dataset with cumulative_years_compliance
##   created in 14_regress_with_additional_covariates.R
## - Defines two compliance groups based on cumulative years of compliance
##   by 2020:
##     * "<3 years" : cumulative_years_2020 < 3
##     * ">3 years" : cumulative_years_2020 >= 3
## - Computes the average substantiated_per_1000 for each group-year
## - Plots a time series with:
##     * x-axis: year
##     * y-axis: mean substantiated_per_1000
##     * color: compliance group

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

# Paths
merged_panel_path <- "/Users/victorxia/Documents/Honors Thesis/data/clean/merged_panel_data_twfe_cumulative_2012_2020.csv"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"

# Ensure output directory exists
if (!dir.exists(output_fig_dir)) dir.create(output_fig_dir, recursive = TRUE)

cat("Reading merged panel data from:\n  ", merged_panel_path, "\n")

if (!file.exists(merged_panel_path)) {
  stop("Merged panel data not found. Please run 14_regress_with_additional_covariates.R first to create this file.")
}

panel <- read_csv(merged_panel_path, show_col_types = FALSE)

cat("Merged panel has", nrow(panel), "state-year observations.\n")
cat("Years in data:", paste(sort(unique(panel$year)), collapse = ", "), "\n")

# Check required columns
required_cols <- c("state", "year", "substantiated_per_1000", "cumulative_years_compliance")
missing_cols <- setdiff(required_cols, names(panel))
if (length(missing_cols) > 0) {
  stop("The merged panel data is missing required columns: ",
       paste(missing_cols, collapse = ", "))
}

# Compute cumulative years of compliance by 2020 for each state
# This uses the maximum cumulative_years_compliance up to and including 2020
state_cumulative_2020 <- panel %>%
  filter(year <= 2020) %>%
  group_by(state) %>%
  summarise(
    cumulative_years_2020 = suppressWarnings(
      max(cumulative_years_compliance, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Handle cases where all values were NA (max() with na.rm=TRUE would give -Inf)
state_cumulative_2020 <- state_cumulative_2020 %>%
  mutate(
    cumulative_years_2020 = ifelse(is.infinite(cumulative_years_2020),
                                   NA_real_, cumulative_years_2020)
  )

cat("Summary of cumulative years of compliance by 2020:\n")
print(summary(state_cumulative_2020$cumulative_years_2020))

# Define compliance groups based on cumulative years by 2020
# Two groups: <3 years vs >3 years
state_cumulative_2020 <- state_cumulative_2020 %>%
  mutate(
    compliance_group = case_when(
      !is.na(cumulative_years_2020) & cumulative_years_2020 < 3 ~ "<3 years",
      !is.na(cumulative_years_2020) & cumulative_years_2020 >= 3 ~ ">3 years",
      TRUE ~ NA_character_
    )
  )

cat("\nDistribution of compliance groups (by 2020):\n")
print(table(state_cumulative_2020$compliance_group, useNA = "ifany"))

# Join group assignment back to the panel data
panel_grouped <- panel %>%
  left_join(state_cumulative_2020 %>% select(state, cumulative_years_2020, compliance_group),
            by = "state") %>%
  filter(!is.na(compliance_group),
         !is.na(substantiated_per_1000))

cat("\nPanel observations with valid group and substantiated_per_1000:",
    nrow(panel_grouped), "\n")
cat("Years in grouped data:", paste(sort(unique(panel_grouped$year)), collapse = ", "), "\n")

# Compute average substantiated_per_1000 by year and compliance group
group_year_summary <- panel_grouped %>%
  group_by(compliance_group, year) %>%
  summarise(
    mean_substantiated_per_1000 = mean(substantiated_per_1000, na.rm = TRUE),
    n_states = n_distinct(state),
    .groups = "drop"
  )

cat("\nNumber of states per group-year (first few rows):\n")
print(head(group_year_summary))

# Order groups for plotting
group_year_summary <- group_year_summary %>%
  mutate(
    compliance_group = factor(
      compliance_group,
      levels = c("<3 years", ">3 years")
    )
  )

# Create the plot
p_substantiated_groups <- ggplot(group_year_summary,
                                 aes(x = year,
                                     y = mean_substantiated_per_1000,
                                     color = compliance_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Substantiated inmate-on-inmate incidents per 1,000 prisoners",
    subtitle = "Grouped by cumulative years of PREA compliance by 2020",
    x = "Year",
    y = "Mean substantiated incidents per 1,000 prisoners",
    color = "Compliance group\n(by 2020)",
    caption = "Data: PREA 2012–2020; groups defined by cumulative years of compliance as of 2020"
  ) +
  scale_color_manual(
    values = c(
      "<3 years" = "#E31A1C",
      ">3 years" = "#1F78B4"
    )
  ) +
  scale_x_continuous(breaks = sort(unique(group_year_summary$year))) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(p_substantiated_groups)

output_path <- file.path(output_fig_dir,
                         "substantiated_per_1000_by_cumulative_compliance_group_2012_2020.png")
ggsave(output_path, p_substantiated_groups, width = 11, height = 7, dpi = 300)

cat("\nFigure saved to:\n  ", output_path, "\n")

