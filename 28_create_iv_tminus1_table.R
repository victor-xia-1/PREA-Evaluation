## Create IV table for t-1 lag outcomes only
## Rationale: PREA certification status in year t reflects prior fiscal-year compliance

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"

first_stage_path <- file.path(output_data_dir, "first_stage_ever_treated_to_cumulative_lag_outcome_2012_2020.csv")
alleged_path <- file.path(output_data_dir, "iv_alleged_tminus1_cumulative_on_ever_treated_2012_2020.csv")
substantiated_path <- file.path(output_data_dir, "iv_substantiated_tminus1_cumulative_on_ever_treated_2012_2020.csv")

required_files <- c(first_stage_path, alleged_path, substantiated_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required lag-IV files. Run 14_regress_with_additional_covariates.R first.\nMissing:\n",
       paste(missing_files, collapse = "\n"))
}

first_stage <- read_csv(first_stage_path, show_col_types = FALSE)
alleged <- read_csv(alleged_path, show_col_types = FALSE)
substantiated <- read_csv(substantiated_path, show_col_types = FALSE)

fmt_coef <- function(est, p) {
  stars <- ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**",
           ifelse(p < 0.1, "*", "")))
  sprintf("%.3f%s", est, stars)
}

fmt_se <- function(se) sprintf("(%.3f)", se)
fmt_p <- function(p) sprintf("p=%.3f", p)

term_labels <- c(
  "cumulative_years_hat" = "Instrumented Cumulative Compliance Years",
  "population_millions" = "State Population (millions)",
  "incarceration_rate_per_100k" = "Incarceration Rate (per 100,000)",
  "staff_per_1000_inmates" = "Staff per 1,000 Inmates",
  "violent_crime_rate_per_100k" = "Violent Crime Rate (per 100,000)"
)

key_terms <- names(term_labels)

table_df <- data.frame(term = key_terms, stringsAsFactors = FALSE) %>%
  left_join(
    alleged %>%
      filter(term %in% key_terms) %>%
      transmute(
        term,
        alleged_coef = fmt_coef(estimate, p.value),
        alleged_se = fmt_se(std.error),
        alleged_p = fmt_p(p.value)
      ),
    by = "term"
  ) %>%
  left_join(
    substantiated %>%
      filter(term %in% key_terms) %>%
      transmute(
        term,
        substantiated_coef = fmt_coef(estimate, p.value),
        substantiated_se = fmt_se(std.error),
        substantiated_p = fmt_p(p.value)
      ),
    by = "term"
  ) %>%
  mutate(variable = unname(term_labels[term])) %>%
  select(variable, alleged_coef, alleged_se, alleged_p, substantiated_coef, substantiated_se, substantiated_p)

csv_out <- file.path(output_data_dir, "iv_tminus1_table.csv")
write_csv(table_df, csv_out)

fs_row <- first_stage %>% filter(term == "EverTreated")
first_stage_note <- if (nrow(fs_row) == 1) {
  fs_f <- (fs_row$statistic)^2
  sprintf(
    "First stage (EverTreated -> cumulative compliance years): coef=%.3f, SE=%.3f, p=%.3f, approx. F=%.3f.",
    fs_row$estimate, fs_row$std.error, fs_row$p.value, fs_f
  )
} else {
  "First-stage EverTreated coefficient not found."
}
weak_iv_note <- if (nrow(fs_row) == 1 && is.finite((fs_row$statistic)^2) && (fs_row$statistic)^2 < 10) {
  "Warning: First-stage F-statistic is below 10; instrument may be weak."
} else {
  "First-stage F-statistic is at or above 10."
}

tex_out <- file.path(output_data_dir, "iv_tminus1_table.tex")
tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{IV Estimates Using Lagged Outcomes ($t-1$)}",
  "\\label{tab:iv_tminus1}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "Variable & Alleged per 1,000 ($t-1$) & Substantiated per 1,000 ($t-1$) \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(table_df))) {
  r <- table_df[i, ]
  tex_lines <- c(
    tex_lines,
    sprintf("%s & %s & %s \\\\", r$variable, r$alleged_coef, r$substantiated_coef),
    sprintf(" & %s & %s \\\\", r$alleged_se, r$substantiated_se),
    sprintf(" & %s & %s \\\\", r$alleged_p, r$substantiated_p)
  )
}

tex_lines <- c(
  tex_lines,
  "\\midrule",
  sprintf("\\multicolumn{3}{l}{\\footnotesize{%s}} \\\\", first_stage_note),
  sprintf("\\multicolumn{3}{l}{\\footnotesize{%s}} \\\\", weak_iv_note),
  "\\multicolumn{3}{l}{\\footnotesize{Rationale: PREA certification in year $t$ reflects compliance in the prior fiscal year.}} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize{All models include state and year fixed effects, with state-clustered standard errors.}} \\\\",
  "\\multicolumn{3}{l}{\\footnotesize{$^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$}} \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(tex_lines, tex_out)

cat("Saved t-1 IV CSV table to:\n  ", csv_out, "\n")
cat("Saved t-1 IV LaTeX table to:\n  ", tex_out, "\n")

