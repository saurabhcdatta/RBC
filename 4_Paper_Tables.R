# =============================================================================
# 4_Paper_Tables.R
# Publication-Ready Tables: LaTeX and Word Output
# Project: Impact of NCUA 2022 Risk-Based Capital Rule on Complex Credit Unions
# Author:  Saurabh C. Datta, Ph.D., Senior Economist, NCUA
# Updated: April 2026
#
# TABLE NUMBERING (must be consistent with paper and all scripts):
#   Table 1  — Summary statistics (pre-RBC)         [1_Descriptive_Stats_FINAL.R]
#   Table 2  — Balance table (pre-RBC)               [1_Descriptive_Stats_FINAL.R]
#   Table 3  — Capital adequacy DiD                  [2_DiD_Estimation.R]
#   Table 4  — Loan rate spreads DiD                 [2_DiD_Estimation.R]
#   Table 5  — Portfolio, growth, credit quality,    [2_DiD_Estimation.R]
#              profitability DiD
#   Table 6  — Robustness checks                     [2_DiD_Estimation.R]
#   Table 7  — 2008 crisis vs. 2022 RBC parallel DiD [3A_2008_Crisis_Parallel_DiD.R]
#   Table 8  — CCULR relief value                    [3C_CCULR_Adoption.R]
#   App. A1  — Heterogeneity by asset size tier      [2_DiD_Estimation.R]
#   App. A2  — Near-threshold subgroup ($400M–$600M) [2_DiD_Estimation.R]
#
# INPUTS (all in S:/Projects/RBC_2026/Data/):
#   table1_summary_stats.csv
#   table2_balance_table.csv
#   table3_capital.csv
#   table4_loan_spreads.csv
#   table5_portfolio_growth.csv
#   robustness_checks.csv
#   3A_wald_test_crisis_vs_rbc.csv
#   3C_three_group_did.csv
#   heterogeneity_by_tier.csv
#   near_threshold_results.csv
#
# OUTPUTS (S:/Projects/RBC_2026/Tables/):
#   LaTeX: tables_main.tex, tables_appendix.tex
#   Word:  tables_main.docx, tables_appendix.docx
#   Individual .tex files for each table (for easy inclusion in paper)
# =============================================================================


# =============================================================================
# 0. SETUP AND PACKAGES
# =============================================================================

library(tidyverse)
library(kableExtra)
library(officer)        # Word document output
library(flextable)      # Rich Word tables
library(scales)
library(glue)

# Working directories
DATA_DIR   <- "S:/Projects/RBC_2026/Data"
TABLES_DIR <- "S:/Projects/RBC_2026/Tables"
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)

# Critical constants (must match all other scripts)
RBC_EFFECTIVE_PERIOD   <- 2022.1
NW_WELLCAP_THRESHOLD   <- 10       # % — well-capitalized threshold
NW_CCULR_THRESHOLD     <- 9        # % — CCULR threshold
SIZE_THRESHOLD         <- 500e6    # $500M asset threshold
GROWTH_LABEL_SUFFIX    <- "\u00d7100"  # "×100"

# Significance stars helper
stars <- function(p) {
  case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE      ~ ""
  )
}

# Format coefficient + SE in the standard (SE) style
fmt_coef <- function(b, se, p, digits = 3) {
  s <- stars(p)
  b_fmt  <- formatC(b,  format = "f", digits = digits)
  se_fmt <- formatC(se, format = "f", digits = digits)
  glue("{b_fmt}{s}\n({se_fmt})")
}

# Format coefficient + SE for LaTeX (with math mode)
fmt_coef_tex <- function(b, se, p, digits = 3) {
  s <- stars(p)
  b_fmt  <- formatC(b,  format = "f", digits = digits)
  se_fmt <- formatC(se, format = "f", digits = digits)
  glue("${b_fmt}^{{{s}}}$\n$({se_fmt})$")
}

# LaTeX table wrapper: writes a single .tex file
write_tex_table <- function(kbl_obj, filename, label, caption) {
  path <- file.path(TABLES_DIR, filename)
  save_kable(kbl_obj, file = path, self_contained = FALSE)
  message(glue("  [OK] Written: {path}"))
}

# flextable number formatting helper
fmt_ft_num <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

cat("=== 4_Paper_Tables.R: Setup complete ===\n")


# =============================================================================
# 1. LOAD ALL INPUT CSVs
# =============================================================================

cat("\n--- Loading input data ---\n")

tbl1_raw  <- read_csv(file.path(DATA_DIR, "table1_summary_stats.csv"),
                      show_col_types = FALSE)
tbl2_raw  <- read_csv(file.path(DATA_DIR, "table2_balance_table.csv"),
                      show_col_types = FALSE)
tbl3_raw  <- read_csv(file.path(DATA_DIR, "table3_capital.csv"),
                      show_col_types = FALSE)
tbl4_raw  <- read_csv(file.path(DATA_DIR, "table4_loan_spreads.csv"),
                      show_col_types = FALSE)
tbl5_raw  <- read_csv(file.path(DATA_DIR, "table5_portfolio_growth.csv"),
                      show_col_types = FALSE)
tbl6_raw  <- read_csv(file.path(DATA_DIR, "robustness_checks.csv"),
                      show_col_types = FALSE)
tbl7_raw  <- read_csv(file.path(DATA_DIR, "3A_wald_test_crisis_vs_rbc.csv"),
                      show_col_types = FALSE)
tbl8_raw  <- read_csv(file.path(DATA_DIR, "3C_three_group_did.csv"),
                      show_col_types = FALSE)
tblA1_raw <- read_csv(file.path(DATA_DIR, "heterogeneity_by_tier.csv"),
                      show_col_types = FALSE)
tblA2_raw <- read_csv(file.path(DATA_DIR, "near_threshold_results.csv"),
                      show_col_types = FALSE)

cat("  [OK] All CSVs loaded.\n")

# Validate essential columns exist
stopifnot("complex_mean"     %in% names(tbl1_raw))
stopifnot("noncomplex_mean"  %in% names(tbl1_raw))
stopifnot("beta"             %in% names(tbl3_raw))
stopifnot("se"               %in% names(tbl3_raw))
stopifnot("p_value"          %in% names(tbl3_raw))
cat("  [OK] Column validation passed.\n")


# =============================================================================
# 2. TABLE 1 — Summary Statistics (Pre-RBC 2018Q1–2021Q4)
# =============================================================================
# Unit of observation: CU-quarter
# Source panel: analysis_panel_raw.rds (unwinsorized)
# Spread SDs: from winsorized panel (hybrid approach — see Technical Notes)
# =============================================================================

cat("\n--- Building Table 1: Summary Statistics ---\n")

# Row labels in display order — must match variable order in tbl1_raw
T1_ROW_LABELS <- c(
  "Net worth ratio (\\%)",
  "Capital buffer above 10\\% threshold (pp)",
  "Well-capitalized (binary)",
  "Loans / assets (\\%)",
  "Member business loan share (\\%)",
  "Real estate loan share (\\%)",
  "Auto loan share (\\%)",
  paste0("Loan growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"),
  paste0("Asset growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"),
  "Mortgage spread over 10yr Treasury (pp)",
  "New auto spread over 2yr Treasury (pp)",
  "Commercial non-RE spread over 2yr Treasury (pp)",
  "Delinquency rate (\\%)",
  "Charge-off ratio (\\%)",
  "Return on assets — ROA (\\%)",
  "Net interest margin — NIM (\\%)",
  "Cost of funds (\\%)"
)

stopifnot(nrow(tbl1_raw) == length(T1_ROW_LABELS))

# Build display data frame
tbl1_display <- tbl1_raw |>
  mutate(
    Variable         = T1_ROW_LABELS,
    Complex_Mean     = fmt_ft_num(complex_mean,    3),
    Complex_SD       = glue("({fmt_ft_num(complex_sd, 3)})"),
    Noncomplex_Mean  = fmt_ft_num(noncomplex_mean, 3),
    Noncomplex_SD    = glue("({fmt_ft_num(noncomplex_sd, 3)})")
  ) |>
  select(Variable, Complex_Mean, Complex_SD, Noncomplex_Mean, Noncomplex_SD)

# Panel breaks: Capital | Loans & Portfolio | Growth | Spreads | Credit Quality | Profitability
T1_PANEL_STARTS <- c(
  "Capital"          = 1,
  "Loans \\& Portfolio" = 4,
  "Growth"           = 8,
  "Loan Rate Spreads" = 10,
  "Credit Quality"   = 13,
  "Profitability"    = 15
)

# --- LaTeX version ---
tbl1_kbl <- tbl1_display |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    col.names = c(
      "Variable",
      "Mean", "(SD)",
      "Mean", "(SD)"
    ),
    caption   = paste0(
      "\\textbf{Table 1: Summary Statistics} \\\\",
      "\\textit{Pre-RBC period: 2018Q1--2021Q4. Unit of observation: CU-quarter. ",
      "Complex CUs have average assets $>\\$500$M over 2021Q1--2021Q4 (N=696); ",
      "non-complex CUs have assets $\\leq\\$500$M (N=4,488). ",
      "Spread SDs computed from winsorized values to mitigate NCUA reporting outliers; ",
      "all other SDs from unwinsorized data. ",
      "pp = percentage points.}"
    ),
    label     = "tab:summary_stats"
  ) |>
  add_header_above(c(
    " " = 1,
    "Complex CUs (N=696)"     = 2,
    "Non-Complex CUs (N=4,488)" = 2
  )) |>
  # Panel grouping rows — iterate in reverse so row index arithmetic stays valid
  pack_rows("Panel A: Capital Adequacy",      1,  3,  bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel B: Loan Portfolio",        4,  7,  bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel C: Growth",                8,  9,  bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel D: Loan Rate Spreads",    10, 12,  bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel E: Credit Quality",       13, 14,  bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel F: Profitability",        15, 17,  bold = TRUE, latex_gap_space = "0.4em") |>
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size     = 10
  ) |>
  footnote(
    general = paste0(
      "Spread variables include mortgage (30-yr fixed vs.\\ 10yr Treasury), ",
      "new auto and used auto (vs.\\ 2yr Treasury), and commercial non-RE (vs.\\ 2yr Treasury). ",
      "Commercial RE spread is excluded from this table due to \\\\sim78\\\\% missingness ",
      "after removing the irate\\_re\\_oth fallback variable. ",
      "Growth variables are log-differences scaled by 100 ($\\\\approx$ QoQ \\\\% change)."
    ),
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tbl1_kbl, "table1_summary_stats.tex",
                label   = "tab:summary_stats",
                caption = "Table 1: Summary Statistics")

cat("  [OK] Table 1 complete.\n")


# =============================================================================
# 3. TABLE 2 — Balance Table (Pre-RBC)
# =============================================================================
# Shows pre-RBC means by group with t-test of difference
# =============================================================================

cat("\n--- Building Table 2: Balance Table ---\n")

# Expected columns from tbl2_raw: variable, complex_mean, noncomplex_mean,
#   diff, t_stat, p_value
# (Generated by 1_Descriptive_Stats_FINAL.R)

T2_ROW_LABELS <- c(
  "Net worth ratio (\\%)",
  "Capital buffer above 10\\% threshold (pp)",
  "Well-capitalized (binary)",
  "Loans / assets (\\%)",
  "MBL share (\\%)",
  "RE loan share (\\%)",
  "Auto loan share (\\%)",
  "Delinquency rate (\\%)",
  "Charge-off ratio (\\%)",
  "ROA (\\%)",
  "NIM (\\%)",
  "Cost of funds (\\%)",
  "log(Assets)"
)

stopifnot(nrow(tbl2_raw) == length(T2_ROW_LABELS))

tbl2_display <- tbl2_raw |>
  mutate(
    Variable       = T2_ROW_LABELS,
    Complex        = fmt_ft_num(complex_mean,    3),
    Noncomplex     = fmt_ft_num(noncomplex_mean, 3),
    Difference     = fmt_ft_num(diff,            3),
    `t-statistic`  = glue("{fmt_ft_num(t_stat, 2)}{stars(p_value)}")
  ) |>
  select(Variable, Complex, Noncomplex, Difference, `t-statistic`)

tbl2_kbl <- tbl2_display |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "r", "r", "r", "r"),
    caption   = paste0(
      "\\textbf{Table 2: Pre-Treatment Balance} \\\\",
      "\\textit{Sample: 2018Q1--2021Q4 (pre-RBC). ",
      "Reported values are CU-level means averaged over the pre-treatment window. ",
      "Difference = Complex mean $-$ Non-complex mean. ",
      "Stars denote significance of two-sample t-test: ",
      "***$p<0.01$, **$p<0.05$, *$p<0.10$.}"
    ),
    label = "tab:balance"
  ) |>
  pack_rows("Panel A: Capital",         1,  3, bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel B: Loan Portfolio",  4,  7, bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel C: Credit Quality",  8,  9, bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel D: Profitability",  10, 12, bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel E: Size",           13, 13, bold = TRUE, latex_gap_space = "0.4em") |>
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size     = 10
  ) |>
  footnote(
    general = paste0(
      "Pre-treatment balance confirms that complex CUs are systematically larger ",
      "with higher loan-to-asset ratios and lower delinquency rates, motivating ",
      "the inclusion of CU fixed effects and time-varying controls in the DiD specification."
    ),
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tbl2_kbl, "table2_balance_table.tex",
                label   = "tab:balance",
                caption = "Table 2: Balance Table")

cat("  [OK] Table 2 complete.\n")


# =============================================================================
# 4. TABLE 3 — Capital Adequacy DiD (Regression Table)
# =============================================================================
# Three outcomes, two specifications (baseline DiD / trend-adjusted DiD)
# Columns: Outcome | Specification | Beta (SE) | p-value | N | FE | Clustered SE
# =============================================================================

cat("\n--- Building Table 3: Capital Adequacy DiD ---\n")

# Expected tbl3_raw columns: outcome, spec, beta, se, p_value, n_obs
# Outcomes in order: networth_ratio, cap_buffer, well_capitalized

T3_OUTCOMES <- c(
  "Net worth ratio (\\%)",
  "Capital buffer above 10\\% (pp)",
  "Well-capitalized (LPM, binary)"
)

T3_SPECS <- c(
  "Trend-adjusted DiD",
  "Trend-adjusted DiD",
  "Baseline DiD"
)

tbl3_display <- tbl3_raw |>
  mutate(
    Outcome       = T3_OUTCOMES,
    Specification = T3_SPECS,
    Coef          = fmt_coef(beta, se, p_value, digits = 3),
    `p-value`     = formatC(p_value, format = "f", digits = 3),
    N             = formatC(n_obs, format = "d", big.mark = ",")
  ) |>
  select(Outcome, Specification, Coef, `p-value`, N)

T3_FE_NOTE <- paste0(
  "All specifications include CU fixed effects ($\\alpha_i$) and quarter-year fixed effects ($\\gamma_t$). ",
  "Controls: $\\ln$(assets), loan-to-asset ratio, CECL adopter indicator. ",
  "Standard errors clustered at the CU level (in parentheses). ",
  "Trend-adjusted DiD adds a complex $\\times$ event-time linear trend to account for ",
  "non-parallel pre-trends in capital ratios. ",
  "***$p<0.01$, **$p<0.05$, *$p<0.10$."
)

tbl3_kbl <- tbl3_display |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "l", "r", "r", "r"),
    col.names = c("Outcome", "Specification",
                  "Estimate (SE)", "$p$-value", "$N$"),
    caption   = paste0(
      "\\textbf{Table 3: Effect of RBC Rule on Capital Adequacy} \\\\",
      "\\textit{Difference-in-differences estimates, 2018Q1--2025Q4. ",
      "Dependent variables measure capital adequacy. ",
      "Treatment: complex CUs (avg assets $>\\$500$M in 2021). ",
      "Post period: 2022Q1 onward.}"
    ),
    label = "tab:capital"
  ) |>
  kable_styling(
    latex_options = c("hold_position"),
    font_size     = 10
  ) |>
  add_header_above(c(
    " " = 2,
    "DiD Estimate" = 1,
    " " = 2
  )) |>
  row_spec(0, bold = TRUE) |>
  footnote(
    general        = T3_FE_NOTE,
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tbl3_kbl, "table3_capital.tex",
                label   = "tab:capital",
                caption = "Table 3: Capital Adequacy DiD")

cat("  [OK] Table 3 complete.\n")


# =============================================================================
# 5. TABLE 4 — Loan Rate Spreads DiD
# =============================================================================
# Q2/Q4 semi-annual data; five spread outcomes
# Comm RE spread flagged as supplementary (78% missing)
# =============================================================================

cat("\n--- Building Table 4: Loan Rate Spreads DiD ---\n")

T4_OUTCOMES <- c(
  "Mortgage spread over 10yr Treasury (pp)",
  "New auto spread over 2yr Treasury (pp)",
  "Used auto spread over 2yr Treasury (pp)",
  "Commercial non-RE spread over 2yr Treasury (pp)",
  "Commercial RE spread over 10yr Treasury (pp)$^{\\dagger}$"
)

T4_BENCHMARKS <- c(
  "10yr Treasury", "2yr Treasury", "2yr Treasury",
  "2yr Treasury",  "10yr Treasury"
)

tbl4_display <- tbl4_raw |>
  mutate(
    Outcome   = T4_OUTCOMES,
    Benchmark = T4_BENCHMARKS,
    Coef      = fmt_coef(beta, se, p_value, digits = 3),
    `p-value` = formatC(p_value, format = "f", digits = 3),
    N         = formatC(n_obs, format = "d", big.mark = ",")
  ) |>
  select(Outcome, Benchmark, Coef, `p-value`, N)

T4_FE_NOTE <- paste0(
  "All specifications include CU fixed effects and quarter-year fixed effects. ",
  "Baseline DiD; controls: $\\ln$(assets), loan-to-asset ratio, CECL adopter. ",
  "Standard errors clustered at the CU level. ",
  "Sample restricted to Q2 and Q4 observations because NCUA reports loan rates semi-annually. ",
  "$^{\\dagger}$ Commercial RE spread uses the variable \\texttt{irate\\_re\\_mtg} ",
  "without the \\texttt{irate\\_re\\_oth} fallback (removed to avoid product contamination); ",
  "approximately 78\\% of observations are missing — treat as supplementary. ",
  "***$p<0.01$, **$p<0.05$, *$p<0.10$."
)

tbl4_kbl <- tbl4_display |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "l", "r", "r", "r"),
    col.names = c("Outcome", "Benchmark Rate",
                  "Estimate (SE)", "$p$-value", "$N$"),
    caption   = paste0(
      "\\textbf{Table 4: Effect of RBC Rule on Loan Rate Spreads} \\\\",
      "\\textit{Baseline difference-in-differences estimates, Q2/Q4 semi-annual data, 2018--2025. ",
      "Dependent variables are loan-product interest rates minus the indicated benchmark Treasury yield. ",
      "A positive estimate implies treated CUs raised spreads relative to controls after 2022Q1.}"
    ),
    label = "tab:spreads"
  ) |>
  kable_styling(
    latex_options = c("hold_position"),
    font_size     = 10
  ) |>
  row_spec(5, italic = TRUE) |>   # Flag supplementary row
  row_spec(0, bold   = TRUE) |>
  footnote(
    general        = T4_FE_NOTE,
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tbl4_kbl, "table4_loan_spreads.tex",
                label   = "tab:spreads",
                caption = "Table 4: Loan Rate Spreads DiD")

cat("  [OK] Table 4 complete.\n")


# =============================================================================
# 6. TABLE 5 — Portfolio, Growth, Credit Quality, Profitability
# =============================================================================
# Multi-panel regression table: 4 panels, 11 outcomes total
# =============================================================================

cat("\n--- Building Table 5: Portfolio, Growth, Credit Quality, Profitability ---\n")

T5_PANELS <- list(
  "Panel A: Portfolio Composition" = c(
    "Auto loan share (\\%)",
    "Real estate loan share (\\%)",
    "Member business loan share (\\%)"
  ),
  "Panel B: Growth" = c(
    paste0("Loan growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"),
    paste0("Asset growth (QoQ log", GROWTH_LABEL_SUFFIX, ")")
  ),
  "Panel C: Credit Quality" = c(
    "Delinquency rate (\\%)",
    "Charge-off ratio (\\%)",
    "Provision for loan losses / assets (\\%)"
  ),
  "Panel D: Profitability" = c(
    "Return on assets — ROA (\\%)",
    "Net interest margin — NIM (\\%)",
    "Cost of funds (\\%)"
  )
)

T5_OUTCOMES <- unlist(T5_PANELS, use.names = FALSE)
stopifnot(nrow(tbl5_raw) == length(T5_OUTCOMES))

tbl5_display <- tbl5_raw |>
  mutate(
    Outcome   = T5_OUTCOMES,
    Coef      = fmt_coef(beta, se, p_value, digits = 3),
    `p-value` = formatC(p_value, format = "f", digits = 3),
    N         = formatC(n_obs, format = "d", big.mark = ",")
  ) |>
  select(Outcome, Coef, `p-value`, N)

T5_FE_NOTE <- paste0(
  "All specifications: CU FE, quarter-year FE, baseline DiD. ",
  "Controls: $\\ln$(assets), loan-to-asset ratio, CECL adopter indicator. ",
  "Standard errors clustered at the CU level. ",
  "MBL share specification uses trend-adjusted DiD (non-parallel pre-trends). ",
  "Growth variables are log-differences $\\times 100$ ($\\approx$ QoQ \\% change). ",
  "***$p<0.01$, **$p<0.05$, *$p<0.10$."
)

# Compute panel break rows
panel_lens  <- sapply(T5_PANELS, length)
panel_ends  <- cumsum(panel_lens)
panel_starts <- c(1, head(panel_ends, -1) + 1)

tbl5_kbl <- tbl5_display |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "r", "r", "r"),
    col.names = c("Outcome", "Estimate (SE)", "$p$-value", "$N$"),
    caption   = paste0(
      "\\textbf{Table 5: Effect of RBC Rule on Portfolio Composition, Growth, ",
      "Credit Quality, and Profitability} \\\\",
      "\\textit{Baseline difference-in-differences estimates, 2018Q1--2025Q4. ",
      "Treatment: complex CUs ($>\\$500$M avg assets in 2021). ",
      "Post period: 2022Q1 onward.}"
    ),
    label = "tab:portfolio_growth"
  ) |>
  pack_rows("Panel A: Portfolio Composition",   panel_starts[1], panel_ends[1],
            bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel B: Growth",                  panel_starts[2], panel_ends[2],
            bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel C: Credit Quality",          panel_starts[3], panel_ends[3],
            bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Panel D: Profitability",           panel_starts[4], panel_ends[4],
            bold = TRUE, latex_gap_space = "0.4em") |>
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size     = 10
  ) |>
  footnote(
    general        = T5_FE_NOTE,
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tbl5_kbl, "table5_portfolio_growth.tex",
                label   = "tab:portfolio_growth",
                caption = "Table 5: Portfolio, Growth, Credit Quality, Profitability")

cat("  [OK] Table 5 complete.\n")


# =============================================================================
# 7. TABLE 6 — Robustness Checks
# =============================================================================
# Horizontal layout: each row is a robustness variant; columns are outcome groups
# Expected columns in robustness_checks.csv:
#   variant, outcome_group, beta, se, p_value, n_obs, note
# =============================================================================

cat("\n--- Building Table 6: Robustness Checks ---\n")

# Robustness variants (rows)
T6_VARIANTS <- c(
  "Baseline DiD",
  "Balanced panel",
  "Exclude 2020Q1--2020Q4 (COVID)",
  "Control $>\\$300$M only",
  "Near-threshold ($\\$400$M--$\\$600$M)",
  "Placebo: treatment date 2020Q1",
  "Placebo: treatment date 2019Q1"
)

# Outcome groups (columns): Capital | Auto share | Loan growth | ROA | Mortgage spread
T6_GROUPS <- c("Capital (NW ratio)", "Auto share", "Loan growth", "ROA", "Mortgage spread")

# Pivot robustness table to wide format: rows = variants, cols = outcome groups
tbl6_wide <- tbl6_raw |>
  mutate(coef_fmt = fmt_coef(beta, se, p_value, digits = 3)) |>
  select(variant, outcome_group, coef_fmt) |>
  pivot_wider(names_from  = outcome_group,
              values_from = coef_fmt) |>
  mutate(Variant = T6_VARIANTS) |>
  select(Variant, all_of(T6_GROUPS))

T6_FE_NOTE <- paste0(
  "All specifications include CU and quarter-year fixed effects. ",
  "Baseline DiD uses the full sample (2018Q1--2025Q4) with winsorized outcomes. ",
  "Balanced panel retains only CUs observed in every quarter. ",
  "COVID exclusion drops 2020Q1--2020Q4 to isolate RBC effects from pandemic. ",
  "Control $>\\$300$M restricts control group to larger non-complex CUs ",
  "to improve pre-trend comparability. ",
  "Placebo tests assign a false treatment date and should yield coefficients ",
  "near zero; significance would indicate pre-existing trends. ",
  "***$p<0.01$, **$p<0.05$, *$p<0.10$."
)

tbl6_kbl <- tbl6_wide |>
  kbl(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    align    = c("l", "r", "r", "r", "r", "r"),
    caption  = paste0(
      "\\textbf{Table 6: Robustness Checks} \\\\",
      "\\textit{DiD estimates under alternative sample restrictions and placebo tests. ",
      "Each cell reports $\\hat{\\beta}$ (SE) for the column outcome under the row specification. ",
      "Post period: 2022Q1 onward except in placebo rows.}"
    ),
    label = "tab:robustness"
  ) |>
  add_header_above(c(" " = 1,
                     "Capital" = 1, "Portfolio" = 1,
                     "Growth"  = 1, "Profitability" = 1,
                     "Spreads" = 1)) |>
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size     = 9
  ) |>
  pack_rows("Main specifications",  1, 4, bold = TRUE, latex_gap_space = "0.4em") |>
  pack_rows("Placebo / falsification", 5, 7, bold = TRUE, latex_gap_space = "0.4em") |>
  row_spec(0, bold = TRUE) |>
  footnote(
    general        = T6_FE_NOTE,
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tbl6_kbl, "table6_robustness.tex",
                label   = "tab:robustness",
                caption = "Table 6: Robustness Checks")

cat("  [OK] Table 6 complete.\n")


# =============================================================================
# 8. TABLE 7 — 2008 Crisis vs. 2022 RBC Parallel DiD
# =============================================================================
# Wald tests of beta_crisis == beta_rbc; interpretation column
# Expected tbl7_raw columns: outcome, crisis_beta, crisis_se, crisis_p,
#   rbc_beta, rbc_se, rbc_p, diff, diff_se, diff_p, interpretation
# =============================================================================

cat("\n--- Building Table 7: Crisis vs. RBC Parallel DiD ---\n")

T7_OUTCOMES <- c(
  "Net worth ratio (\\%)",
  "Loan growth",
  "Asset growth",
  "Auto loan share (\\%)",
  "ROA (\\%)",
  "NIM (\\%)"
)

T7_INTERP <- c(
  "Consistent with generic stress",
  "RBC effect LARGER (step-change)",
  "RBC effect LARGER (step-change)",
  "RBC effect LARGER (risk-weight arbitrage)",
  "Opposite signs — regulatory earnings tax",
  "Opposite signs — regulatory earnings tax"
)

tbl7_display <- tbl7_raw |>
  mutate(
    Outcome        = T7_OUTCOMES,
    Crisis_coef    = fmt_coef(crisis_beta, crisis_se, crisis_p,  digits = 3),
    RBC_coef       = fmt_coef(rbc_beta,    rbc_se,    rbc_p,     digits = 3),
    Diff_coef      = fmt_coef(diff,        diff_se,   diff_p,    digits = 3),
    Interpretation = T7_INTERP
  ) |>
  select(Outcome, Crisis_coef, RBC_coef, Diff_coef, Interpretation)

T7_FE_NOTE <- paste0(
  "Crisis period: 2004Q1--2013Q4; treatment date 2008Q3. ",
  "RBC period: 2018Q1--2025Q4; treatment date 2022Q1. ",
  "Both windows use the same \\$500M asset threshold. ",
  "``Difference'' is RBC estimate minus crisis estimate; ",
  "a significant positive (negative) difference means the RBC effect is ",
  "larger (smaller) in absolute value than the crisis effect. ",
  "CECL adopter control excluded from crisis window (did not exist in 2008). ",
  "All specifications: CU FE, period FE, controls. SE clustered at CU level. ",
  "***$p<0.01$, **$p<0.05$, *$p<0.10$."
)

tbl7_kbl <- tbl7_display |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "r", "r", "r", "l"),
    col.names = c("Outcome",
                  "Crisis (2008Q3)", "RBC (2022Q1)",
                  "Difference", "Interpretation"),
    caption   = paste0(
      "\\textbf{Table 7: 2008 Financial Crisis vs.\\ 2022 RBC Rule — Parallel DiD} \\\\",
      "\\textit{Wald tests comparing ATT estimates across two regulatory episodes. ",
      "Identifying the regulatory channel requires that the RBC effect ",
      "differs significantly from the generic stress response.}"
    ),
    label = "tab:crisis_comparison"
  ) |>
  add_header_above(c(" " = 1,
                     "ATT Estimates" = 2,
                     "Wald Test" = 1,
                     " " = 1)) |>
  pack_rows("Capital",           1, 1, bold = TRUE) |>
  pack_rows("Growth",            2, 3, bold = TRUE) |>
  pack_rows("Portfolio",         4, 4, bold = TRUE) |>
  pack_rows("Profitability",     5, 6, bold = TRUE) |>
  kable_styling(
    latex_options = c("hold_position", "scale_down"),
    font_size     = 10
  ) |>
  row_spec(0, bold = TRUE) |>
  footnote(
    general        = T7_FE_NOTE,
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tbl7_kbl, "table7_crisis_comparison.tex",
                label   = "tab:crisis_comparison",
                caption = "Table 7: Crisis vs. RBC Parallel DiD")

cat("  [OK] Table 7 complete.\n")


# =============================================================================
# 9. TABLE 8 — CCULR Relief Value
# =============================================================================
# Three-group DiD: Full RBC (beta1) | CCULR adopters (beta2) | Relief (beta2 - beta1)
# group_3: 0=non-complex, 1=complex full RBC, 2=complex CCULR
# =============================================================================

cat("\n--- Building Table 8: CCULR Relief Value ---\n")

T8_OUTCOMES <- c(
  "Net worth ratio (\\%)",
  "Loan growth",
  "ROA (\\%)",
  "Auto loan share (\\%)"
)

tbl8_display <- tbl8_raw |>
  mutate(
    Outcome    = T8_OUTCOMES,
    RBC_coef   = fmt_coef(beta1, se1, p1, digits = 3),
    CCULR_coef = fmt_coef(beta2, se2, p2, digits = 3),
    Relief     = fmt_coef(relief, relief_se, relief_p, digits = 3),
    Relief_p   = formatC(relief_p, format = "f", digits = 3)
  ) |>
  select(Outcome, RBC_coef, CCULR_coef, Relief, Relief_p)

T8_FE_NOTE <- paste0(
  "Three-group DiD: non-complex CUs (baseline), complex CUs on full RBC ($\\beta_1$), ",
  "and complex CUs inferred to be CCULR adopters ($\\beta_2$). ",
  "``Relief'' $= \\beta_2 - \\beta_1$ is the differential effect of CCULR relative to full RBC. ",
  "A negative Relief for capital indicates CCULR adopters accumulated less capital. ",
  "CCULR adopters are inferred from capital ratio trajectories ",
  "(pre-RBC NW 9--11\\%, maintained 9--10.5\\% post-2022); ",
  "must be verified against NCUA administrative records before publication. ",
  "All specifications: CU FE, quarter-year FE, controls, SE clustered at CU level. ",
  "***$p<0.01$, **$p<0.05$, *$p<0.10$."
)

tbl8_kbl <- tbl8_display |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "r", "r", "r", "r"),
    col.names = c("Outcome",
                  "Full RBC ($\\hat{\\beta}_1$)",
                  "CCULR ($\\hat{\\beta}_2$)",
                  "Relief ($\\hat{\\beta}_2 - \\hat{\\beta}_1$)",
                  "$p$(Relief)"),
    caption   = paste0(
      "\\textbf{Table 8: CCULR Relief Value Estimates} \\\\",
      "\\textit{Three-group difference-in-differences comparing complex CUs under ",
      "full RBC with inferred CCULR adopters. ",
      "A positive (negative) Relief estimate indicates CCULR improved (worsened) ",
      "the outcome relative to full-RBC complex CUs.}"
    ),
    label = "tab:cculr"
  ) |>
  kable_styling(
    latex_options = c("hold_position"),
    font_size     = 10
  ) |>
  row_spec(0, bold = TRUE) |>
  footnote(
    general        = T8_FE_NOTE,
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tbl8_kbl, "table8_cculr.tex",
                label   = "tab:cculr",
                caption = "Table 8: CCULR Relief Value")

cat("  [OK] Table 8 complete.\n")


# =============================================================================
# 10. APPENDIX TABLE A1 — Heterogeneity by Asset Size Tier
# =============================================================================
# Three tiers: $500M–$1B | $1B–$10B | >$10B
# Five outcomes: ROA | Mortgage spread | New auto spread | DQ rate | Loan growth
# =============================================================================

cat("\n--- Building Appendix Table A1: Heterogeneity by Tier ---\n")

TA1_OUTCOMES <- c(
  "ROA (\\%)",
  "Mortgage spread (pp)",
  "New auto spread (pp)",
  "Delinquency rate (\\%)",
  paste0("Loan growth (QoQ log", GROWTH_LABEL_SUFFIX, ")")
)

TA1_TIERS <- c("\\$500M--\\$1B", "\\$1B--\\$10B", "$>\\$10$B")

# Expected columns: outcome, tier, beta, se, p_value, n_obs
tblA1_wide <- tblA1_raw |>
  mutate(
    coef_fmt = fmt_coef(beta, se, p_value, digits = 3)
  ) |>
  select(outcome, tier, coef_fmt) |>
  pivot_wider(names_from  = tier,
              values_from = coef_fmt) |>
  mutate(Outcome = TA1_OUTCOMES) |>
  select(Outcome, all_of(c("500_1b", "1b_10b", "gt10b")))

# Rename tier columns for display
names(tblA1_wide)[2:4] <- TA1_TIERS

TA1_FE_NOTE <- paste0(
  "Each cell is the DiD $\\hat{\\beta}$ (SE) for the row outcome, estimated separately ",
  "for each asset size tier. Tier boundaries based on 2021 average total assets. ",
  "All specifications: CU FE, quarter-year FE, baseline DiD, controls. ",
  "SE clustered at CU level. ",
  "Pattern: larger CUs show larger magnitude effects on profitability and pricing. ",
  "***$p<0.01$, **$p<0.05$, *$p<0.10$."
)

tblA1_kbl <- tblA1_wide |>
  kbl(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    align    = c("l", "r", "r", "r"),
    caption  = paste0(
      "\\textbf{Appendix Table A1: Heterogeneity in RBC Effects by Asset Size Tier} \\\\",
      "\\textit{DiD estimates for selected outcomes by tier of complex CU asset size. ",
      "Dose-response pattern: larger CUs absorb more of the regulatory burden.}"
    ),
    label = "tab:heterogeneity"
  ) |>
  add_header_above(c(" " = 1,
                     "Asset Size Tier (2021 Average)" = 3)) |>
  kable_styling(
    latex_options = c("hold_position"),
    font_size     = 10
  ) |>
  row_spec(0, bold = TRUE) |>
  footnote(
    general        = TA1_FE_NOTE,
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tblA1_kbl, "appendix_A1_heterogeneity.tex",
                label   = "tab:heterogeneity",
                caption = "Appendix A1: Heterogeneity by Tier")

cat("  [OK] Appendix Table A1 complete.\n")


# =============================================================================
# 11. APPENDIX TABLE A2 — Near-Threshold Subgroup ($400M–$600M)
# =============================================================================
# RD-like robustness: 196 CUs near the $500M threshold
# =============================================================================

cat("\n--- Building Appendix Table A2: Near-Threshold Subgroup ---\n")

TA2_OUTCOMES <- c(
  "Net worth ratio (\\%)",
  paste0("Loan growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"),
  "ROA (\\%)"
)

tblA2_display <- tblA2_raw |>
  mutate(
    Outcome   = TA2_OUTCOMES,
    Coef      = fmt_coef(beta, se, p_value, digits = 3),
    `p-value` = formatC(p_value, format = "f", digits = 3),
    N         = formatC(n_obs, format = "d", big.mark = ",")
  ) |>
  select(Outcome, Coef, `p-value`, N)

TA2_FE_NOTE <- paste0(
  "Near-threshold subgroup: 196 CUs with average 2021 assets of \\$400M--\\$600M ",
  "(100 complex, 96 non-complex near-controls). ",
  "This regression-discontinuity-style sample improves comparability of treated and control ",
  "units at the cost of smaller N. ",
  "Negative net worth ratio estimate (opposite sign to full sample) indicates that ",
  "thin-buffer CUs just above \\$500M struggled to meet the stricter capital requirements, ",
  "while large-buffer CUs dominating the full-sample estimate mask this distress. ",
  "All specifications: CU FE, quarter-year FE, baseline DiD, controls. ",
  "SE clustered at CU level. ",
  "***$p<0.01$, **$p<0.05$, *$p<0.10$."
)

tblA2_kbl <- tblA2_display |>
  kbl(
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    align     = c("l", "r", "r", "r"),
    col.names = c("Outcome", "Estimate (SE)", "$p$-value", "$N$"),
    caption   = paste0(
      "\\textbf{Appendix Table A2: Near-Threshold Subgroup (\\$400M--\\$600M)} \\\\",
      "\\textit{Regression-discontinuity-style robustness restricting to CUs within ",
      "\\$100M of the \\$500M complexity threshold. ",
      "The sign reversal in the net worth ratio confirms that thin-buffer CUs ",
      "near the threshold faced genuine capital pressure.}"
    ),
    label = "tab:near_threshold"
  ) |>
  kable_styling(
    latex_options = c("hold_position"),
    font_size     = 10
  ) |>
  row_spec(0, bold = TRUE) |>
  footnote(
    general        = TA2_FE_NOTE,
    general_title  = "Notes:",
    escape         = FALSE,
    threeparttable = TRUE
  )

write_tex_table(tblA2_kbl, "appendix_A2_near_threshold.tex",
                label   = "tab:near_threshold",
                caption = "Appendix A2: Near-Threshold Subgroup")

cat("  [OK] Appendix Table A2 complete.\n")


# =============================================================================
# 12. WORD (.docx) OUTPUT — Main Tables + Appendix
# =============================================================================
# Uses flextable for rich formatting; exports two Word docs:
#   tables_main.docx     — Tables 1–8
#   tables_appendix.docx — Tables A1–A2
# =============================================================================

cat("\n--- Building Word (.docx) output ---\n")

# Helper: build a flextable from a data frame with a caption
make_ft <- function(df, caption_text, footnote_text = NULL,
                    header_rows = NULL, bold_header = TRUE) {
  ft <- flextable(df) |>
    set_caption(caption_text) |>
    theme_booktabs() |>
    fontsize(size = 9, part = "all") |>
    font(fontname = "Times New Roman", part = "all") |>
    autofit()

  if (!is.null(footnote_text)) {
    ft <- ft |>
      add_footer_lines(footnote_text) |>
      fontsize(size = 8, part = "footer") |>
      color(color = "gray40", part = "footer")
  }
  ft
}

# --- MAIN WORD DOC ---
doc_main <- read_docx()

# Title page
doc_main <- doc_main |>
  body_add_par("Publication-Ready Tables", style = "heading 1") |>
  body_add_par(
    paste0("The Impact of Risk-Based Capital Requirements on Complex Credit Unions: ",
           "Evidence from the NCUA's 2022 RBC Rule"),
    style = "heading 2"
  ) |>
  body_add_par(
    "Saurabh C. Datta, Ph.D. | NCUA Office of the Chief Economist | April 2026",
    style = "Normal"
  ) |>
  body_add_par("", style = "Normal")  # spacer

# Helper to add a table section with a page break before
add_table_section <- function(doc, ft, heading) {
  doc |>
    body_add_break() |>
    body_add_par(heading, style = "heading 2") |>
    body_add_flextable(ft) |>
    body_add_par("", style = "Normal")
}

# Build flextables for Word (use unescaped labels)

# Table 1 Word — summary stats plain text labels
T1_LABELS_PLAIN <- c(
  "Net worth ratio (%)",
  "Capital buffer above 10% threshold (pp)",
  "Well-capitalized (binary)",
  "Loans / assets (%)",
  "MBL share (%)",
  "RE loan share (%)",
  "Auto loan share (%)",
  paste0("Loan growth (QoQ log\u00d7100)"),
  paste0("Asset growth (QoQ log\u00d7100)"),
  "Mortgage spread over 10yr Treasury (pp)",
  "New auto spread over 2yr Treasury (pp)",
  "Comm non-RE spread over 2yr Treasury (pp)",
  "Delinquency rate (%)",
  "Charge-off ratio (%)",
  "ROA (%)",
  "NIM (%)",
  "Cost of funds (%)"
)

# Panel column for Word table
T1_PANELS_PLAIN <- c(
  rep("Panel A: Capital",         3),
  rep("Panel B: Loan Portfolio",  4),
  rep("Panel C: Growth",          2),
  rep("Panel D: Spreads",         3),
  rep("Panel E: Credit Quality",  2),
  rep("Panel F: Profitability",   3)
)

tbl1_word <- tbl1_raw |>
  mutate(
    Panel    = T1_PANELS_PLAIN,
    Variable = T1_LABELS_PLAIN,
    Complex_Mean_SD = glue("{fmt_ft_num(complex_mean, 3)} ({fmt_ft_num(complex_sd, 3)})"),
    NonComplex_Mean_SD = glue("{fmt_ft_num(noncomplex_mean, 3)} ({fmt_ft_num(noncomplex_sd, 3)})")
  ) |>
  select(Panel, Variable, Complex_Mean_SD, NonComplex_Mean_SD)

names(tbl1_word) <- c("Panel", "Variable",
                       "Complex CUs\nMean (SD)",
                       "Non-Complex CUs\nMean (SD)")

ft1 <- make_ft(
  tbl1_word,
  caption_text  = "Table 1: Summary Statistics (Pre-RBC, 2018Q1\u20132021Q4)",
  footnote_text = paste0(
    "Notes: Complex CUs have avg assets > $500M in 2021 (N=696); non-complex \u2264$500M (N=4,488). ",
    "Unit: CU-quarter. Spread SDs from winsorized data; other SDs unwinsorized. pp = percentage points."
  )
) |>
  merge_v(j = "Panel") |>
  bold(j = "Panel") |>
  autofit()

doc_main <- add_table_section(doc_main, ft1,
  "Table 1: Summary Statistics (Pre-RBC, 2018Q1\u20132021Q4)")

# Table 3 Word
T3_LABELS_PLAIN <- c(
  "Net worth ratio (%)",
  "Capital buffer above 10% (pp)",
  "Well-capitalized (LPM)"
)
T3_SPECS_PLAIN <- c("Trend-adj. DiD", "Trend-adj. DiD", "Baseline DiD")

tbl3_word <- tbl3_raw |>
  mutate(
    Outcome = T3_LABELS_PLAIN,
    Spec    = T3_SPECS_PLAIN,
    Coef_SE = glue("{fmt_ft_num(beta, 3)}{stars(p_value)}\n({fmt_ft_num(se, 3)})"),
    p       = formatC(p_value, format = "f", digits = 3),
    N       = formatC(n_obs, format = "d", big.mark = ",")
  ) |>
  select(Outcome, Spec, Coef_SE, p, N)

names(tbl3_word) <- c("Outcome", "Specification", "Estimate\n(SE)", "p-value", "N")

ft3 <- make_ft(
  tbl3_word,
  caption_text  = "Table 3: Effect of RBC Rule on Capital Adequacy",
  footnote_text = paste0(
    "Notes: CU FE, quarter-year FE. Controls: ln(assets), loan-to-asset ratio, CECL adopter. ",
    "SE clustered at CU level. Trend-adj. DiD adds complex \u00d7 event-time trend. ",
    "*** p<0.01, ** p<0.05, * p<0.10."
  )
)

doc_main <- add_table_section(doc_main, ft3,
  "Table 3: Effect of RBC Rule on Capital Adequacy")

# Table 4 Word
T4_LABELS_PLAIN <- c(
  "Mortgage spread over 10yr Treasury (pp)",
  "New auto spread over 2yr Treasury (pp)",
  "Used auto spread over 2yr Treasury (pp)",
  "Comm non-RE spread over 2yr Treasury (pp)",
  "Comm RE spread over 10yr Treasury (pp) [Supplementary]"
)

tbl4_word <- tbl4_raw |>
  mutate(
    Outcome   = T4_LABELS_PLAIN,
    Coef_SE   = glue("{fmt_ft_num(beta, 3)}{stars(p_value)}\n({fmt_ft_num(se, 3)})"),
    p         = formatC(p_value, format = "f", digits = 3),
    N         = formatC(n_obs, format = "d", big.mark = ",")
  ) |>
  select(Outcome, Coef_SE, p, N)

names(tbl4_word) <- c("Outcome", "Estimate\n(SE)", "p-value", "N")

ft4 <- make_ft(
  tbl4_word,
  caption_text  = "Table 4: Effect of RBC Rule on Loan Rate Spreads (Q2/Q4 Semi-Annual Data)",
  footnote_text = paste0(
    "Notes: Q2 and Q4 only (NCUA semi-annual rate reporting). CU FE, quarter-year FE, baseline DiD. ",
    "Comm RE spread: ~78% missing (supplementary). *** p<0.01, ** p<0.05, * p<0.10."
  )
)

doc_main <- add_table_section(doc_main, ft4,
  "Table 4: Effect of RBC Rule on Loan Rate Spreads")

# Table 5 Word
T5_OUTCOMES_PLAIN <- c(
  "Auto loan share (%)", "RE loan share (%)", "MBL share (%)",
  paste0("Loan growth (QoQ log\u00d7100)"), paste0("Asset growth (QoQ log\u00d7100)"),
  "Delinquency rate (%)", "Charge-off ratio (%)", "PLL / assets (%)",
  "ROA (%)", "NIM (%)", "Cost of funds (%)"
)
T5_PANELS_PLAIN <- c(
  rep("Panel A: Portfolio", 3),
  rep("Panel B: Growth",    2),
  rep("Panel C: Credit Quality", 3),
  rep("Panel D: Profitability",  3)
)

tbl5_word <- tbl5_raw |>
  mutate(
    Panel   = T5_PANELS_PLAIN,
    Outcome = T5_OUTCOMES_PLAIN,
    Coef_SE = glue("{fmt_ft_num(beta, 3)}{stars(p_value)}\n({fmt_ft_num(se, 3)})"),
    p       = formatC(p_value, format = "f", digits = 3),
    N       = formatC(n_obs, format = "d", big.mark = ",")
  ) |>
  select(Panel, Outcome, Coef_SE, p, N)

names(tbl5_word) <- c("Panel", "Outcome", "Estimate\n(SE)", "p-value", "N")

ft5 <- make_ft(
  tbl5_word,
  caption_text  = "Table 5: Effect of RBC Rule on Portfolio, Growth, Credit Quality, Profitability",
  footnote_text = paste0(
    "Notes: CU FE, quarter-year FE, baseline DiD; MBL share: trend-adj. DiD. ",
    "Controls: ln(assets), loan-to-asset ratio, CECL adopter. SE clustered at CU level. ",
    "Growth variables: log-differences \u00d7100 (\u2248 QoQ % change). *** p<0.01, ** p<0.05, * p<0.10."
  )
) |>
  merge_v(j = "Panel") |>
  bold(j = "Panel") |>
  autofit()

doc_main <- add_table_section(doc_main, ft5,
  "Table 5: Portfolio, Growth, Credit Quality, Profitability")

# Table 7 Word
T7_OUTCOMES_PLAIN <- c(
  "Net worth ratio (%)", "Loan growth", "Asset growth",
  "Auto loan share (%)", "ROA (%)", "NIM (%)"
)
T7_INTERP_PLAIN <- c(
  "Consistent with generic stress",
  "RBC effect LARGER (step-change)",
  "RBC effect LARGER (step-change)",
  "RBC effect LARGER (risk-weight arbitrage)",
  "Opposite signs — regulatory earnings tax",
  "Opposite signs — regulatory earnings tax"
)

tbl7_word <- tbl7_raw |>
  mutate(
    Outcome       = T7_OUTCOMES_PLAIN,
    Crisis_coef   = glue("{fmt_ft_num(crisis_beta, 3)}{stars(crisis_p)}\n({fmt_ft_num(crisis_se, 3)})"),
    RBC_coef      = glue("{fmt_ft_num(rbc_beta, 3)}{stars(rbc_p)}\n({fmt_ft_num(rbc_se, 3)})"),
    Diff_coef     = glue("{fmt_ft_num(diff, 3)}{stars(diff_p)}\n({fmt_ft_num(diff_se, 3)})"),
    Interpretation = T7_INTERP_PLAIN
  ) |>
  select(Outcome, Crisis_coef, RBC_coef, Diff_coef, Interpretation)

names(tbl7_word) <- c("Outcome", "Crisis\n(2008Q3)", "RBC\n(2022Q1)",
                       "Difference", "Interpretation")

ft7 <- make_ft(
  tbl7_word,
  caption_text  = "Table 7: 2008 Financial Crisis vs. 2022 RBC Rule — Parallel DiD",
  footnote_text = paste0(
    "Notes: Crisis: 2004Q1\u20132013Q4; RBC: 2018Q1\u20132025Q4. Same $500M threshold. ",
    "Difference = RBC \u2212 Crisis estimate (Wald test). ",
    "Significant difference identifies the regulatory channel. *** p<0.01, ** p<0.05, * p<0.10."
  )
)

doc_main <- add_table_section(doc_main, ft7,
  "Table 7: 2008 Financial Crisis vs. 2022 RBC Rule — Parallel DiD")

# Table 8 Word
T8_OUTCOMES_PLAIN <- c("Net worth ratio (%)", "Loan growth", "ROA (%)", "Auto loan share (%)")

tbl8_word <- tbl8_raw |>
  mutate(
    Outcome    = T8_OUTCOMES_PLAIN,
    RBC_coef   = glue("{fmt_ft_num(beta1, 3)}{stars(p1)}\n({fmt_ft_num(se1, 3)})"),
    CCULR_coef = glue("{fmt_ft_num(beta2, 3)}{stars(p2)}\n({fmt_ft_num(se2, 3)})"),
    Relief_coef = glue("{fmt_ft_num(relief, 3)}{stars(relief_p)}\n({fmt_ft_num(relief_se, 3)})"),
    Relief_p_fmt = formatC(relief_p, format = "f", digits = 3)
  ) |>
  select(Outcome, RBC_coef, CCULR_coef, Relief_coef, Relief_p_fmt)

names(tbl8_word) <- c("Outcome", "Full RBC (\u03b2\u2081)",
                       "CCULR (\u03b2\u2082)",
                       "Relief (\u03b2\u2082\u2212\u03b2\u2081)",
                       "p(Relief)")

ft8 <- make_ft(
  tbl8_word,
  caption_text  = "Table 8: CCULR Relief Value Estimates",
  footnote_text = paste0(
    "Notes: Three-group DiD (non-complex baseline, full RBC complex, CCULR complex). ",
    "CCULR adopters inferred from capital ratio trajectories — verify vs. NCUA admin records. ",
    "Negative relief = CCULR worse than full RBC. *** p<0.01, ** p<0.05, * p<0.10."
  )
)

doc_main <- add_table_section(doc_main, ft8,
  "Table 8: CCULR Relief Value Estimates")

# Save main Word doc
print(doc_main, target = file.path(TABLES_DIR, "tables_main.docx"))
cat("  [OK] tables_main.docx written.\n")

# --- APPENDIX WORD DOC ---
doc_app <- read_docx() |>
  body_add_par("Appendix Tables", style = "heading 1") |>
  body_add_par("", style = "Normal")

# Appendix A1
TA1_LABELS_PLAIN <- c(
  "ROA (%)",
  "Mortgage spread (pp)",
  "New auto spread (pp)",
  "Delinquency rate (%)",
  paste0("Loan growth (QoQ log\u00d7100)")
)

tblA1_word <- tblA1_raw |>
  mutate(coef_fmt = glue("{fmt_ft_num(beta, 3)}{stars(p_value)}\n({fmt_ft_num(se, 3)})")) |>
  select(outcome, tier, coef_fmt) |>
  pivot_wider(names_from = tier, values_from = coef_fmt) |>
  mutate(Outcome = TA1_LABELS_PLAIN) |>
  select(Outcome, everything(), -outcome)

ftA1 <- make_ft(
  tblA1_word,
  caption_text  = "Appendix Table A1: Heterogeneity by Asset Size Tier",
  footnote_text = paste0(
    "Notes: Each cell is DiD estimate (SE) for the row outcome, by 2021 asset size tier. ",
    "Dose-response: larger CUs face larger magnitude effects on profitability and pricing. ",
    "*** p<0.01, ** p<0.05, * p<0.10."
  )
)

doc_app <- add_table_section(doc_app, ftA1,
  "Appendix Table A1: Heterogeneity by Asset Size Tier")

# Appendix A2
TA2_LABELS_PLAIN <- c("Net worth ratio (%)",
                       paste0("Loan growth (QoQ log\u00d7100)"),
                       "ROA (%)")

tblA2_word <- tblA2_raw |>
  mutate(
    Outcome = TA2_LABELS_PLAIN,
    Coef_SE = glue("{fmt_ft_num(beta, 3)}{stars(p_value)}\n({fmt_ft_num(se, 3)})"),
    p       = formatC(p_value, format = "f", digits = 3),
    N       = formatC(n_obs, format = "d", big.mark = ",")
  ) |>
  select(Outcome, Coef_SE, p, N)

names(tblA2_word) <- c("Outcome", "Estimate\n(SE)", "p-value", "N")

ftA2 <- make_ft(
  tblA2_word,
  caption_text  = "Appendix Table A2: Near-Threshold Subgroup ($400M\u2013$600M)",
  footnote_text = paste0(
    "Notes: 196 CUs within $100M of the $500M complexity threshold. ",
    "NW ratio sign reversal: thin-buffer CUs near the threshold lost capital trying to comply. ",
    "*** p<0.01, ** p<0.05, * p<0.10."
  )
)

doc_app <- add_table_section(doc_app, ftA2,
  "Appendix Table A2: Near-Threshold Subgroup ($400M\u2013$600M)")

print(doc_app, target = file.path(TABLES_DIR, "tables_appendix.docx"))
cat("  [OK] tables_appendix.docx written.\n")


# =============================================================================
# 13. COMBINED LATEX FILE — tables_main.tex + tables_appendix.tex
# =============================================================================
# Writes a single .tex file that \input{}s individual table files
# Include in paper with: \input{Tables/tables_combined.tex}
# =============================================================================

cat("\n--- Writing combined LaTeX files ---\n")

TABLES_ORDERED <- c(
  "table1_summary_stats.tex",
  "table2_balance_table.tex",
  "table3_capital.tex",
  "table4_loan_spreads.tex",
  "table5_portfolio_growth.tex",
  "table6_robustness.tex",
  "table7_crisis_comparison.tex",
  "table8_cculr.tex"
)

APPENDIX_TABLES <- c(
  "appendix_A1_heterogeneity.tex",
  "appendix_A2_near_threshold.tex"
)

combined_main <- c(
  "% ============================================================",
  "% tables_main.tex — Main regression tables for RBC paper",
  "% Generated by 4_Paper_Tables.R | April 2026",
  "% ============================================================",
  "% Include in paper with: \\input{Tables/tables_main.tex}",
  "",
  paste0("\\input{Tables/", TABLES_ORDERED, "}", collapse = "\n\n\\clearpage\n\n")
)

combined_appendix <- c(
  "% ============================================================",
  "% tables_appendix.tex — Appendix tables for RBC paper",
  "% Generated by 4_Paper_Tables.R | April 2026",
  "% ============================================================",
  "",
  paste0("\\input{Tables/", APPENDIX_TABLES, "}", collapse = "\n\n\\clearpage\n\n")
)

writeLines(combined_main,     file.path(TABLES_DIR, "tables_main.tex"))
writeLines(combined_appendix, file.path(TABLES_DIR, "tables_appendix.tex"))

cat("  [OK] tables_main.tex and tables_appendix.tex written.\n")


# =============================================================================
# 14. VALIDATION CHECKS
# =============================================================================

cat("\n--- Running validation checks ---\n")

# Check 1: Table numbering integrity
stopifnot(
  "Table 1 and 2 are descriptive — not regression tables" =
    !any(str_detect(T1_LABELS_PLAIN, "beta"))
)

# Check 2: Growth label carries GROWTH_LABEL_SUFFIX throughout
growth_labels_ok <- all(str_detect(
  c(T1_LABELS_PLAIN[8], T1_LABELS_PLAIN[9],
    T5_OUTCOMES_PLAIN[4], T5_OUTCOMES_PLAIN[5]),
  "\u00d7100"
))
stopifnot("Growth label suffix ×100 present in all growth rows" = growth_labels_ok)

# Check 3: Table 5 has 11 outcome rows (4 panels)
stopifnot("Table 5 must have 11 outcome rows" = nrow(tbl5_raw) == 11)

# Check 4: Table 4 comm RE row is flagged as supplementary
stopifnot("Table 4 must have 5 rows (incl. supplementary Comm RE)" = nrow(tbl4_raw) == 5)

# Check 5: Appendix tables exist
stopifnot("A1 heterogeneity must have 5 outcomes" = nrow(tblA1_raw) / 3 == 5 ||
            nrow(tblA1_raw) == 15)  # 5 outcomes × 3 tiers in long format
stopifnot("A2 near-threshold must have 3 outcomes" = nrow(tblA2_raw) == 3)

cat("  [OK] All validation checks passed.\n")


# =============================================================================
# 15. SUMMARY
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  4_Paper_Tables.R COMPLETE\n")
cat("=================================================================\n")
cat(glue("  Output directory: {TABLES_DIR}\n"))
cat("\n  LaTeX files:\n")
walk(c(TABLES_ORDERED, APPENDIX_TABLES, "tables_main.tex", "tables_appendix.tex"),
     ~ cat(glue("    {TABLES_DIR}/{.x}\n")))
cat("\n  Word files:\n")
cat(glue("    {TABLES_DIR}/tables_main.docx\n"))
cat(glue("    {TABLES_DIR}/tables_appendix.docx\n"))
cat("\n  Table numbering:\n")
cat("    Tables 1-2  — Descriptive (1_Descriptive_Stats_FINAL.R)\n")
cat("    Tables 3-5  — Main DiD regressions (2_DiD_Estimation.R)\n")
cat("    Table 6     — Robustness (2_DiD_Estimation.R)\n")
cat("    Table 7     — Crisis comparison (3A_2008_Crisis_Parallel_DiD.R)\n")
cat("    Table 8     — CCULR relief (3C_CCULR_Adoption.R)\n")
cat("    App. A1/A2  — Heterogeneity / Near-threshold\n")
cat("\n  REMINDER: Verify CCULR adopter inference vs. NCUA admin records\n")
cat("            before journal submission (Table 8 / Appendix note).\n")
cat("=================================================================\n")
