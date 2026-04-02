## Create combined IV results table:
## Baseline outcome (t), lead outcome (t+1), and lag outcome (t-1)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

output_data_dir <- "/Users/victorxia/Documents/Honors Thesis/data/clean"

# Input files from script 14
base_alleged_path <- file.path(output_data_dir, "iv_alleged_cumulative_on_ever_treated_2012_2020.csv")
base_sub_path <- file.path(output_data_dir, "iv_substantiated_cumulative_on_ever_treated_2012_2020.csv")
lead_alleged_path <- file.path(output_data_dir, "iv_alleged_tplus1_cumulative_on_ever_treated_2012_2020.csv")
lead_sub_path <- file.path(output_data_dir, "iv_substantiated_tplus1_cumulative_on_ever_treated_2012_2020.csv")
lag_alleged_path <- file.path(output_data_dir, "iv_alleged_tminus1_cumulative_on_ever_treated_2012_2020.csv")
lag_sub_path <- file.path(output_data_dir, "iv_substantiated_tminus1_cumulative_on_ever_treated_2012_2020.csv")

required_files <- c(
  base_alleged_path, base_sub_path,
  lead_alleged_path, lead_sub_path,
  lag_alleged_path, lag_sub_path
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing IV result files. Run 14_regress_with_additional_covariates.R first.\nMissing:\n",
       paste(missing_files, collapse = "\n"))
}

base_alleged <- read_csv(base_alleged_path, show_col_types = FALSE)
base_sub <- read_csv(base_sub_path, show_col_types = FALSE)
lead_alleged <- read_csv(lead_alleged_path, show_col_types = FALSE)
lead_sub <- read_csv(lead_sub_path, show_col_types = FALSE)
lag_alleged <- read_csv(lag_alleged_path, show_col_types = FALSE)
lag_sub <- read_csv(lag_sub_path, show_col_types = FALSE)

key_terms <- c(
  "cumulative_years_hat",
  "population_millions",
  "incarceration_rate_per_100k",
  "staff_per_1000_inmates",
  "violent_crime_rate_per_100k"
)

fmt_coef <- function(est, p) {
  stars <- ifelse(p < 0.01, "***",
           ifelse(p < 0.05, "**",
           ifelse(p < 0.1, "*", "")))
  sprintf("%.3f%s", est, stars)
}

fmt_se <- function(se) sprintf("(%.3f)", se)

mk_side <- function(df, prefix) {
  df %>%
    filter(term %in% key_terms) %>%
    transmute(
      term,
      !!paste0(prefix, "_coef") := fmt_coef(estimate, p.value),
      !!paste0(prefix, "_se") := fmt_se(std.error),
      !!paste0(prefix, "_p") := sprintf("p=%.3f", p.value)
    )
}

tbl <- data.frame(term = key_terms, stringsAsFactors = FALSE) %>%
  left_join(mk_side(base_alleged, "alleged_t"), by = "term") %>%
  left_join(mk_side(base_sub, "substantiated_t"), by = "term") %>%
  left_join(mk_side(lead_alleged, "alleged_tplus1"), by = "term") %>%
  left_join(mk_side(lead_sub, "substantiated_tplus1"), by = "term") %>%
  left_join(mk_side(lag_alleged, "alleged_tminus1"), by = "term") %>%
  left_join(mk_side(lag_sub, "substantiated_tminus1"), by = "term")

label_term <- function(x) {
  case_when(
    x == "cumulative_years_hat" ~ "Instrumented Cumulative Compliance Years",
    x == "population_millions" ~ "State Population (millions)",
    x == "incarceration_rate_per_100k" ~ "Incarceration Rate (per 100,000)",
    x == "staff_per_1000_inmates" ~ "Staff per 1,000 Inmates",
    x == "violent_crime_rate_per_100k" ~ "Violent Crime Rate (per 100,000)",
    TRUE ~ x
  )
}

tbl <- tbl %>% mutate(term_label = label_term(term))

csv_out <- file.path(output_data_dir, "iv_combined_table_t_tplus1_tminus1.csv")
write_csv(tbl, csv_out)

# Build compact LaTeX table
tex_out <- file.path(output_data_dir, "iv_combined_table_t_tplus1_tminus1.tex")

tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{IV Estimates Across Outcome Timing: $t$, $t+1$, and $t-1$}",
  "\\label{tab:iv_timing_comparison}",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{$t$ outcome} & \\multicolumn{2}{c}{$t+1$ outcome} & \\multicolumn{2}{c}{$t-1$ outcome} \\\\",
  "Variable & Alleged & Substantiated & Alleged & Substantiated & Alleged & Substantiated \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(tbl))) {
  r <- tbl[i, ]
  tex_lines <- c(
    tex_lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s & %s \\\\",
      r$term_label,
      r$alleged_t_coef, r$substantiated_t_coef,
      r$alleged_tplus1_coef, r$substantiated_tplus1_coef,
      r$alleged_tminus1_coef, r$substantiated_tminus1_coef
    ),
    sprintf(
      " & %s & %s & %s & %s & %s & %s \\\\",
      r$alleged_t_se, r$substantiated_t_se,
      r$alleged_tplus1_se, r$substantiated_tplus1_se,
      r$alleged_tminus1_se, r$substantiated_tminus1_se
    ),
    sprintf(
      " & %s & %s & %s & %s & %s & %s \\\\",
      r$alleged_t_p, r$substantiated_t_p,
      r$alleged_tplus1_p, r$substantiated_tplus1_p,
      r$alleged_tminus1_p, r$substantiated_tminus1_p
    )
  )
}

tex_lines <- c(
  tex_lines,
  "\\midrule",
  "\\multicolumn{7}{l}{\\footnotesize{All models use IV with EverTreated as instrument for cumulative compliance years.}} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize{Two-way fixed effects (state and year) and state-clustered standard errors.}} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize{$^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$}} \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(tex_lines, tex_out)

cat("Saved combined IV CSV to:\n  ", csv_out, "\n")
cat("Saved combined IV LaTeX table to:\n  ", tex_out, "\n")

