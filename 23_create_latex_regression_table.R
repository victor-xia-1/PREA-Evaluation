# Load required packages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

# Paths
output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"
output_fig_dir <- "/Users/victorxia/Documents/Honors Thesis/figures"

# Read regression results
results_alleged <- read_csv(file.path(output_data_dir, "regression_alleged_panel_twfe_cumulative_2012_2020.csv"), show_col_types = FALSE)
results_substantiated <- read_csv(file.path(output_data_dir, "regression_substantiated_panel_twfe_cumulative_2012_2020.csv"), show_col_types = FALSE)

# Function to format coefficient with significance stars
format_coef <- function(coef, pval) {
  stars <- ifelse(pval < 0.01, "***",
           ifelse(pval < 0.05, "**",
           ifelse(pval < 0.1, "*", "")))
  sprintf("%.3f%s", coef, stars)
}

# Function to format standard error
format_se <- function(se) {
  sprintf("(%.3f)", se)
}

# Function to format p-value
format_pval <- function(pval) {
  if (is.na(pval)) return("")
  if (pval < 0.001) {
    return("$<$0.001")
  } else if (pval < 0.01) {
    return(sprintf("%.3f", pval))
  } else {
    return(sprintf("%.3f", pval))
  }
}

# Function to get variable label
get_var_label <- function(var_name) {
  case_when(
    var_name == "(Intercept)" ~ "Constant",
    var_name == "cumulative_years_compliance" ~ "Cumulative Years of PREA Compliance",
    var_name == "population_millions" ~ "State Population (millions)",
    var_name == "incarceration_rate_per_100k" ~ "Incarceration Rate (per 100,000)",
    var_name == "staff_per_1000_inmates" ~ "Staff per 1,000 Inmates",
    var_name == "violent_crime_rate_per_100k" ~ "Violent Crime Rate (per 100,000)",
    TRUE ~ var_name
  )
}

# Create LaTeX table
latex_output_path <- file.path(output_data_dir, "regression_table_cumulative_2012_2020.tex")

# Merge results for easier table creation
merged_results <- results_alleged %>%
  select(term, estimate_alleged = estimate, se_alleged = std.error, p_alleged = p.value) %>%
  full_join(
    results_substantiated %>%
      select(term, estimate_sub = estimate, se_sub = std.error, p_sub = p.value),
    by = "term"
  ) %>%
  filter(term != "(Intercept)") %>%  # We'll add intercept separately if needed
  arrange(match(term, c("cumulative_years_compliance", "population_millions", 
                       "incarceration_rate_per_100k", "staff_per_1000_inmates",
                       "violent_crime_rate_per_100k")))

# Write LaTeX table
cat("\\begin{table}[htbp]\n", file = latex_output_path)
cat("\\centering\n", file = latex_output_path, append = TRUE)
cat("\\caption{Panel Regression Results: Cumulative Years of PREA Compliance and Sexual Victimization Rates (2012-2020)}\n", file = latex_output_path, append = TRUE)
cat("\\label{tab:panel_regression_cumulative_2012_2020}\n", file = latex_output_path, append = TRUE)
cat("\\begin{tabular}{lcc}\n", file = latex_output_path, append = TRUE)
cat("\\toprule\n", file = latex_output_path, append = TRUE)
cat(" & (1) & (2) \\\\\n", file = latex_output_path, append = TRUE)
cat(" & Alleged & Substantiated \\\\\n", file = latex_output_path, append = TRUE)
cat(" & per 1,000 & per 1,000 \\\\\n", file = latex_output_path, append = TRUE)
cat("\\midrule\n", file = latex_output_path, append = TRUE)

# Add main variables
for (i in 1:nrow(merged_results)) {
  var <- merged_results$term[i]
  var_label <- get_var_label(var)
  
  # Alleged model
  coef_alleged <- format_coef(merged_results$estimate_alleged[i], merged_results$p_alleged[i])
  se_alleged <- format_se(merged_results$se_alleged[i])
  pval_alleged <- format_pval(merged_results$p_alleged[i])
  
  # Substantiated model
  coef_sub <- format_coef(merged_results$estimate_sub[i], merged_results$p_sub[i])
  se_sub <- format_se(merged_results$se_sub[i])
  pval_sub <- format_pval(merged_results$p_sub[i])
  
  # Write coefficient, standard error, and p-value (three rows per variable)
  cat(sprintf("%s & %s & %s \\\\\n", var_label, coef_alleged, coef_sub), 
      file = latex_output_path, append = TRUE)
  cat(sprintf(" & %s & %s \\\\\n", se_alleged, se_sub), 
      file = latex_output_path, append = TRUE)
  cat(sprintf(" & $p=%s$ & $p=%s$ \\\\\n", pval_alleged, pval_sub), 
      file = latex_output_path, append = TRUE)
}

cat("\\midrule\n", file = latex_output_path, append = TRUE)
cat("State Fixed Effects & Yes & Yes \\\\\n", file = latex_output_path, append = TRUE)
cat("Year Fixed Effects & Yes & Yes \\\\\n", file = latex_output_path, append = TRUE)
cat("Clustered Standard Errors & Yes (State) & Yes (State) \\\\\n", file = latex_output_path, append = TRUE)
cat("\\midrule\n", file = latex_output_path, append = TRUE)

# Model statistics - updated with latest regression results (271 observations)
cat("Observations & 271 & 271 \\\\\n", file = latex_output_path, append = TRUE)
cat("R-squared & 0.692 & 0.627 \\\\\n", file = latex_output_path, append = TRUE)
cat("Adjusted R-squared & 0.614 & 0.532 \\\\\n", file = latex_output_path, append = TRUE)

cat("\\bottomrule\n", file = latex_output_path, append = TRUE)
cat("\\multicolumn{3}{l}{\\footnotesize{Standard errors in parentheses. P-values reported below standard errors.}} \\\\\n", file = latex_output_path, append = TRUE)
cat("\\multicolumn{3}{l}{\\footnotesize{$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$}} \\\\\n", file = latex_output_path, append = TRUE)
cat("\\multicolumn{3}{l}{\\footnotesize{Panel data: state-year observations (2012-2020).}} \\\\\n", file = latex_output_path, append = TRUE)
cat("\\multicolumn{3}{l}{\\footnotesize{Two-way fixed effects (state + year) included.}} \\\\\n", file = latex_output_path, append = TRUE)
cat("\\multicolumn{3}{l}{\\footnotesize{Standard errors clustered at the state level.}} \\\\\n", file = latex_output_path, append = TRUE)
cat("\\end{tabular}\n", file = latex_output_path, append = TRUE)
cat("\\end{table}\n", file = latex_output_path, append = TRUE)

cat("\nLaTeX table saved to:", latex_output_path, "\n")

# Also create a standalone LaTeX document
latex_doc_path <- file.path(output_data_dir, "regression_table_cumulative_2012_2020_standalone.tex")

cat("\\documentclass[11pt]{article}\n", file = latex_doc_path)
cat("\\usepackage{booktabs}\n", file = latex_doc_path, append = TRUE)
cat("\\usepackage{geometry}\n", file = latex_doc_path, append = TRUE)
cat("\\geometry{letterpaper, margin=1in}\n", file = latex_doc_path, append = TRUE)
cat("\\begin{document}\n\n", file = latex_doc_path, append = TRUE)

# Read and append the table
table_content <- readLines(latex_output_path)
cat(paste(table_content, collapse = "\n"), "\n", file = latex_doc_path, append = TRUE)

cat("\n\\end{document}\n", file = latex_doc_path, append = TRUE)

cat("Standalone LaTeX document saved to:", latex_doc_path, "\n")
cat("\n=== LaTeX Table Generation Complete ===\n")

# Print preview
cat("\n=== Preview of LaTeX Table ===\n")
cat(paste(readLines(latex_output_path), collapse = "\n"), "\n")

# Create document with suggestions for increasing statistical power
power_suggestions_path <- file.path(output_data_dir, "suggestions_increase_statistical_power.md")

cat("# Suggestions for Increasing Statistical Power\n\n", file = power_suggestions_path)
cat("## Current Analysis Constraints\n\n", file = power_suggestions_path, append = TRUE)
cat("- Sample size: 193 state-year observations (48 states, 5 years: 2014-2018)\n", file = power_suggestions_path, append = TRUE)
cat("- Treatment variation: Cumulative years of PREA compliance (0-5 years)\n", file = power_suggestions_path, append = TRUE)
cat("- Outcome variables: Alleged and substantiated sexual victimization rates per 1,000\n\n", file = power_suggestions_path, append = TRUE)

cat("## Strategies to Increase Statistical Power\n\n", file = power_suggestions_path, append = TRUE)

cat("### 1. Increase Sample Size\n\n", file = power_suggestions_path, append = TRUE)
cat("- **Extend time period**: Include additional years of data (e.g., 2012-2018 or beyond)\n", file = power_suggestions_path, append = TRUE)
cat("  - Current: 2014-2018 (5 years)\n", file = power_suggestions_path, append = TRUE)
cat("  - Potential: 2012-2024 (13 years) would nearly triple observations\n", file = power_suggestions_path, append = TRUE)
cat("- **Include more units**: If possible, include county-level or facility-level data\n", file = power_suggestions_path, append = TRUE)
cat("  - Note: This may require different identification strategy\n\n", file = power_suggestions_path, append = TRUE)

cat("### 2. Increase Treatment Variation\n\n", file = power_suggestions_path, append = TRUE)
cat("- **Longer compliance periods**: States with longer compliance histories provide more variation\n", file = power_suggestions_path, append = TRUE)
cat("- **Compliance intensity**: Consider measures beyond binary compliance (e.g., audit scores, implementation quality)\n", file = power_suggestions_path, append = TRUE)
cat("- **Timing variation**: Exploit variation in when states became compliant (event study design)\n\n", file = power_suggestions_path, append = TRUE)

cat("### 3. Improve Outcome Measurement\n\n", file = power_suggestions_path, append = TRUE)
cat("- **More precise outcomes**: Use raw counts instead of rates if appropriate\n", file = power_suggestions_path, append = TRUE)
cat("- **Alternative specifications**: Consider log transformations or other functional forms\n", file = power_suggestions_path, append = TRUE)
cat("- **Composite measures**: Create indices combining multiple related outcomes\n\n", file = power_suggestions_path, append = TRUE)

cat("### 4. Reduce Error Variance\n\n", file = power_suggestions_path, append = TRUE)
cat("- **Additional controls**: Include more time-varying covariates that predict outcomes\n", file = power_suggestions_path, append = TRUE)
cat("  - State-level economic conditions (unemployment, GDP)\n", file = power_suggestions_path, append = TRUE)
cat("  - Prison system characteristics (facility age, capacity utilization)\n", file = power_suggestions_path, append = TRUE)
cat("  - Policy environment (other criminal justice reforms)\n", file = power_suggestions_path, append = TRUE)
cat("- **Remove outliers**: Identify and potentially winsorize or remove extreme observations\n", file = power_suggestions_path, append = TRUE)
cat("- **Stratified analysis**: Analyze subgroups separately (e.g., by region, prison size)\n\n", file = power_suggestions_path, append = TRUE)

cat("### 5. Alternative Estimation Strategies\n\n", file = power_suggestions_path, append = TRUE)
cat("- **Difference-in-differences**: If there's clear variation in treatment timing\n", file = power_suggestions_path, append = TRUE)
cat("- **Event study**: Estimate dynamic effects around compliance adoption\n", file = power_suggestions_path, append = TRUE)
cat("- **Instrumental variables**: If there's a valid instrument for compliance\n", file = power_suggestions_path, append = TRUE)
cat("- **Synthetic control**: For states with unique compliance patterns\n\n", file = power_suggestions_path, append = TRUE)

cat("### 6. Power Calculations\n\n", file = power_suggestions_path, append = TRUE)
cat("To determine minimum detectable effect (MDE) with current sample:\n\n", file = power_suggestions_path, append = TRUE)
cat("```r\n", file = power_suggestions_path, append = TRUE)
cat("# Example power calculation\n", file = power_suggestions_path, append = TRUE)
cat("# library(pwr)\n", file = power_suggestions_path, append = TRUE)
cat("# pwr.f2.test(u = 5, v = 193 - 5 - 48 - 5 - 1, f2 = NULL, sig.level = 0.05, power = 0.80)\n", file = power_suggestions_path, append = TRUE)
cat("```\n\n", file = power_suggestions_path, append = TRUE)

cat("### 7. Practical Recommendations (Prioritized)\n\n", file = power_suggestions_path, append = TRUE)
cat("**High Priority (Easiest to implement):**\n", file = power_suggestions_path, append = TRUE)
cat("1. Extend time period to include 2012-2013 data (already available)\n", file = power_suggestions_path, append = TRUE)
cat("2. Add more time-varying controls (economic conditions, other policies)\n", file = power_suggestions_path, append = TRUE)
cat("3. Consider log transformation of outcome variables\n\n", file = power_suggestions_path, append = TRUE)

cat("**Medium Priority (Moderate effort):**\n", file = power_suggestions_path, append = TRUE)
cat("1. Implement event study design to exploit timing variation\n", file = power_suggestions_path, append = TRUE)
cat("2. Analyze substantiated rates separately (may have more signal)\n", file = power_suggestions_path, append = TRUE)
cat("3. Stratified analysis by state characteristics (size, region)\n\n", file = power_suggestions_path, append = TRUE)

cat("**Lower Priority (More complex):**\n", file = power_suggestions_path, append = TRUE)
cat("1. Obtain facility-level data for more granular analysis\n", file = power_suggestions_path, append = TRUE)
cat("2. Develop compliance intensity measures beyond binary\n", file = power_suggestions_path, append = TRUE)
cat("3. Explore instrumental variable strategies\n\n", file = power_suggestions_path, append = TRUE)

cat("### 8. Notes on Current Results\n\n", file = power_suggestions_path, append = TRUE)
cat("The current analysis shows:\n", file = power_suggestions_path, append = TRUE)
cat("- Cumulative years of compliance: Not statistically significant (p = 0.453 for alleged, p = 0.154 for substantiated)\n", file = power_suggestions_path, append = TRUE)
cat("- Effect sizes: -1.161 (alleged) and -0.247 (substantiated) per additional year of compliance\n", file = power_suggestions_path, append = TRUE)
cat("- These effects may be meaningful but lack statistical power to detect\n", file = power_suggestions_path, append = TRUE)
cat("- The negative coefficients suggest potential beneficial effects of compliance\n\n", file = power_suggestions_path, append = TRUE)

cat("### 9. Sample Size Requirements\n\n", file = power_suggestions_path, append = TRUE)
cat("To detect a small effect (Cohen's d = 0.2) with 80% power at α = 0.05:\n", file = power_suggestions_path, append = TRUE)
cat("- Would need approximately 400-500 observations\n", file = power_suggestions_path, append = TRUE)
cat("- Current sample: 193 observations\n", file = power_suggestions_path, append = TRUE)
cat("- Extending to 2012-2018 (7 years) would yield ~270 observations\n", file = power_suggestions_path, append = TRUE)
cat("- Extending to 2012-2024 (13 years) would yield ~624 observations\n\n", file = power_suggestions_path, append = TRUE)

cat("---\n\n", file = power_suggestions_path, append = TRUE)
cat("*Generated: ", Sys.Date(), "*\n", file = power_suggestions_path, append = TRUE)

cat("\nSuggestions document saved to:", power_suggestions_path, "\n")

