# =============================================================================
# 4_Paper_Tables.R
# Publication-Ready Tables: LaTeX and Word Output
# Project: Impact of NCUA 2022 Risk-Based Capital Rule on Complex Credit Unions
# Author:  Saurabh C. Datta, Ph.D., Senior Economist, NCUA
# Updated: April 2026
#
# PACKAGES REQUIRED:
#   tidyverse, kableExtra, scales, glue
#   NO officer, NO flextable — Word output is HTML-based (open in Word directly)
#
# TABLE NUMBERING (must be consistent with paper and all scripts):
#   Table 1  — Summary statistics (pre-RBC)          [1_Descriptive_Stats_FINAL.R]
#   Table 2  — Balance table (pre-RBC)                [1_Descriptive_Stats_FINAL.R]
#   Table 3  — Capital adequacy DiD                   [2_DiD_Estimation.R]
#   Table 4  — Loan rate spreads DiD                  [2_DiD_Estimation.R]
#   Table 5  — Portfolio, growth, credit quality,     [2_DiD_Estimation.R]
#              profitability DiD
#   Table 6  — Robustness checks                      [2_DiD_Estimation.R]
#   Table 7  — 2008 crisis vs. 2022 RBC parallel DiD  [3A_2008_Crisis_Parallel_DiD.R]
#   Table 8  — CCULR relief value                     [3C_CCULR_Adoption.R]
#   App. A1  — Heterogeneity by asset size tier       [2_DiD_Estimation.R]
#   App. A2  — Near-threshold subgroup ($400M-$600M)  [2_DiD_Estimation.R]
#
# INPUTS  (S:/Projects/RBC_2026/Data/):
#   table1_summary_stats.csv      columns: complex_mean, complex_sd,
#                                          noncomplex_mean, noncomplex_sd
#   table2_balance_table.csv      columns: complex_mean, noncomplex_mean,
#                                          diff, t_stat, p_value
#   table3_capital.csv            columns: beta, se, p_value, n_obs
#   table4_loan_spreads.csv       columns: beta, se, p_value, n_obs
#   table5_portfolio_growth.csv   columns: beta, se, p_value, n_obs
#   robustness_checks.csv         columns: variant, outcome_group,
#                                          beta, se, p_value
#   3A_wald_test_crisis_vs_rbc.csv columns: crisis_beta, crisis_se, crisis_p,
#                                            rbc_beta, rbc_se, rbc_p,
#                                            diff, diff_se, diff_p
#   3C_three_group_did.csv        columns: beta1, se1, p1,
#                                          beta2, se2, p2,
#                                          relief, relief_se, relief_p
#   heterogeneity_by_tier.csv     columns: outcome, tier, beta, se, p_value
#   near_threshold_results.csv    columns: beta, se, p_value, n_obs
#
# OUTPUTS (S:/Projects/RBC_2026/Tables/):
#   LaTeX (.tex): individual table files + tables_main.tex + tables_appendix.tex
#   Word-compatible HTML: tables_main_word.html, tables_appendix_word.html
#     -> Open either .html file in Microsoft Word, then File > Save As > .docx
# =============================================================================


# =============================================================================
# 0. SETUP AND PACKAGES
# =============================================================================

library(tidyverse)
library(kableExtra)   # kbl(), kable_styling(), pack_rows(), save_kable()
library(scales)
library(glue)

# Directories
DATA_DIR   <- "S:/Projects/RBC_2026/Data"
TABLES_DIR <- "S:/Projects/RBC_2026/Tables"
dir.create(TABLES_DIR, showWarnings = FALSE, recursive = TRUE)

# Critical constants (must match all other scripts)
RBC_EFFECTIVE_PERIOD <- 2022.1
NW_WELLCAP_THRESHOLD <- 10
NW_CCULR_THRESHOLD   <- 9
SIZE_THRESHOLD       <- 500e6
GROWTH_LABEL_SUFFIX  <- "\u00d7100"   # ×100

# Significance stars
stars <- function(p) {
  dplyr::case_when(
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE      ~ ""
  )
}

# Format "b***(SE)" — sep=" " for HTML, sep="\n" for two-line LaTeX cells
fmt_coef <- function(b, se, p, digits = 3, sep = " ") {
  paste0(formatC(b, format="f", digits=digits), stars(p),
         sep, "(", formatC(se, format="f", digits=digits), ")")
}

# Plain number
fmt_num <- function(x, digits = 3) formatC(x, format="f", digits=digits)

# Write LaTeX kable to .tex file
write_tex <- function(kbl_obj, filename) {
  path <- file.path(TABLES_DIR, filename)
  save_kable(kbl_obj, file=path, self_contained=FALSE)
  message(glue("  [TEX] {filename}"))
  invisible(path)
}

# Fail-fast column check
check_cols <- function(df, required, csv_name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0)
    stop(glue("{csv_name}: missing columns: {paste(missing, collapse=', ')}"))
}

cat("=== 4_Paper_Tables.R: Setup complete ===\n")
cat(glue("    Data dir:   {DATA_DIR}\n"))
cat(glue("    Tables dir: {TABLES_DIR}\n\n"))


# =============================================================================
# 1. LOAD ALL INPUT CSVs
# =============================================================================

cat("--- Loading input CSVs ---\n")

tbl1_raw  <- read_csv(file.path(DATA_DIR, "table1_summary_stats.csv"),      show_col_types=FALSE)
tbl2_raw  <- read_csv(file.path(DATA_DIR, "table2_balance_table.csv"),      show_col_types=FALSE)
tbl3_raw  <- read_csv(file.path(DATA_DIR, "table3_capital.csv"),            show_col_types=FALSE)
tbl4_raw  <- read_csv(file.path(DATA_DIR, "table4_loan_spreads.csv"),       show_col_types=FALSE)
tbl5_raw  <- read_csv(file.path(DATA_DIR, "table5_portfolio_growth.csv"),   show_col_types=FALSE)
tbl6_raw  <- read_csv(file.path(DATA_DIR, "robustness_checks.csv"),         show_col_types=FALSE)
tbl7_raw  <- read_csv(file.path(DATA_DIR, "3A_wald_test_crisis_vs_rbc.csv"),show_col_types=FALSE)
tbl8_raw  <- read_csv(file.path(DATA_DIR, "3C_three_group_did.csv"),        show_col_types=FALSE)
tblA1_raw <- read_csv(file.path(DATA_DIR, "heterogeneity_by_tier.csv"),     show_col_types=FALSE)
tblA2_raw <- read_csv(file.path(DATA_DIR, "near_threshold_results.csv"),    show_col_types=FALSE)

cat("  [OK] All CSVs loaded\n")

check_cols(tbl1_raw,  c("complex_mean","complex_sd","noncomplex_mean","noncomplex_sd"),
           "table1_summary_stats.csv")
check_cols(tbl2_raw,  c("complex_mean","noncomplex_mean","diff","t_stat","p_value"),
           "table2_balance_table.csv")
check_cols(tbl3_raw,  c("beta","se","p_value","n_obs"), "table3_capital.csv")
check_cols(tbl4_raw,  c("beta","se","p_value","n_obs"), "table4_loan_spreads.csv")
check_cols(tbl5_raw,  c("beta","se","p_value","n_obs"), "table5_portfolio_growth.csv")
check_cols(tbl6_raw,  c("variant","outcome_group","beta","se","p_value"),
           "robustness_checks.csv")
check_cols(tbl7_raw,  c("crisis_beta","crisis_se","crisis_p",
                         "rbc_beta","rbc_se","rbc_p","diff","diff_se","diff_p"),
           "3A_wald_test_crisis_vs_rbc.csv")
check_cols(tbl8_raw,  c("beta1","se1","p1","beta2","se2","p2",
                         "relief","relief_se","relief_p"),
           "3C_three_group_did.csv")
check_cols(tblA1_raw, c("outcome","tier","beta","se","p_value"),
           "heterogeneity_by_tier.csv")
check_cols(tblA2_raw, c("beta","se","p_value","n_obs"), "near_threshold_results.csv")

cat("  [OK] Column validation passed\n\n")


# =============================================================================
# 2. ROW-LABEL VECTORS
# =============================================================================
# All label vectors defined once here and shared across LaTeX + HTML builders.
# Edit here if outcomes change; stopifnot() checks below will catch mismatches.

GL <- GROWTH_LABEL_SUFFIX   # shorthand for ×100

T1_LABELS <- c(
  "Net worth ratio (%)",
  "Capital buffer above 10% threshold (pp)",
  "Well-capitalized (binary)",
  "Loans / assets (%)",
  "Member business loan share (%)",
  "Real estate loan share (%)",
  "Auto loan share (%)",
  paste0("Loan growth (QoQ log", GL, ")"),
  paste0("Asset growth (QoQ log", GL, ")"),
  "Mortgage spread over 10yr Treasury (pp)",
  "New auto spread over 2yr Treasury (pp)",
  "Commercial non-RE spread over 2yr Treasury (pp)",
  "Delinquency rate (%)",
  "Charge-off ratio (%)",
  "Return on assets (ROA, %)",
  "Net interest margin (NIM, %)",
  "Cost of funds (%)"
)
T1_PANELS <- c(rep("Panel A: Capital",           3),
               rep("Panel B: Loan Portfolio",     4),
               rep("Panel C: Growth",             2),
               rep("Panel D: Loan Rate Spreads",  3),
               rep("Panel E: Credit Quality",     2),
               rep("Panel F: Profitability",      3))
stopifnot("T1 rows match CSV" = nrow(tbl1_raw) == length(T1_LABELS))

T2_LABELS <- c(
  "Net worth ratio (%)", "Capital buffer above 10% (pp)", "Well-capitalized (binary)",
  "Loans / assets (%)", "MBL share (%)", "RE loan share (%)", "Auto loan share (%)",
  "Delinquency rate (%)", "Charge-off ratio (%)",
  "ROA (%)", "NIM (%)", "Cost of funds (%)",
  "log(Assets)"
)
T2_PANELS <- c(rep("Panel A: Capital",        3),
               rep("Panel B: Loan Portfolio",  4),
               rep("Panel C: Credit Quality",  2),
               rep("Panel D: Profitability",   3),
               rep("Panel E: Size",            1))
stopifnot("T2 rows match CSV" = nrow(tbl2_raw) == length(T2_LABELS))

T3_OUTCOMES <- c("Net worth ratio (%)",
                  "Capital buffer above 10% (pp)",
                  "Well-capitalized (LPM, binary)")
T3_SPECS    <- c("Trend-adjusted DiD","Trend-adjusted DiD","Baseline DiD")
stopifnot("T3 = 3 rows" = nrow(tbl3_raw) == 3)

T4_OUTCOMES   <- c("Mortgage spread over 10yr Treasury (pp)",
                    "New auto spread over 2yr Treasury (pp)",
                    "Used auto spread over 2yr Treasury (pp)",
                    "Commercial non-RE spread over 2yr Treasury (pp)",
                    "Commercial RE spread over 10yr Treasury (pp) \u2020")
T4_BENCHMARKS <- c("10yr Treasury","2yr Treasury","2yr Treasury","2yr Treasury","10yr Treasury")
stopifnot("T4 = 5 rows" = nrow(tbl4_raw) == 5)

T5_OUTCOMES <- c(
  "Auto loan share (%)", "Real estate loan share (%)", "Member business loan share (%)",
  paste0("Loan growth (QoQ log", GL, ")"), paste0("Asset growth (QoQ log", GL, ")"),
  "Delinquency rate (%)", "Charge-off ratio (%)", "Provision for loan losses / assets (%)",
  "Return on assets (ROA, %)", "Net interest margin (NIM, %)", "Cost of funds (%)"
)
T5_PANELS <- c(rep("Panel A: Portfolio Composition", 3),
               rep("Panel B: Growth",                 2),
               rep("Panel C: Credit Quality",         3),
               rep("Panel D: Profitability",          3))
stopifnot("T5 = 11 rows" = nrow(tbl5_raw) == 11)

T6_VARIANTS <- c(
  "Baseline DiD",
  "Balanced panel",
  "Exclude 2020Q1-2020Q4 (COVID)",
  "Control >$300M only",
  "Near-threshold ($400M-$600M)",
  "Placebo: treatment date 2020Q1",
  "Placebo: treatment date 2019Q1"
)
T6_GROUPS <- c("Capital (NW ratio)","Auto share","Loan growth","ROA","Mortgage spread")

T7_OUTCOMES <- c("Net worth ratio (%)","Loan growth","Asset growth",
                  "Auto loan share (%)","ROA (%)","NIM (%)")
T7_INTERP   <- c("Consistent with generic stress",
                  "RBC effect LARGER — step-change",
                  "RBC effect LARGER — step-change",
                  "RBC effect LARGER — risk-weight arbitrage",
                  "Opposite signs — regulatory earnings tax",
                  "Opposite signs — regulatory earnings tax")
stopifnot("T7 = 6 rows" = nrow(tbl7_raw) == 6)

T8_OUTCOMES <- c("Net worth ratio (%)","Loan growth","ROA (%)","Auto loan share (%)")
stopifnot("T8 = 4 rows" = nrow(tbl8_raw) == 4)

TA1_OUTCOMES <- c("ROA (%)","Mortgage spread (pp)","New auto spread (pp)",
                   "Delinquency rate (%)", paste0("Loan growth (QoQ log", GL, ")"))
stopifnot("A1 = 15 rows (5 outcomes x 3 tiers)" = nrow(tblA1_raw) == 15)

TA2_OUTCOMES <- c("Net worth ratio (%)",
                   paste0("Loan growth (QoQ log", GL, ")"),
                   "ROA (%)")
stopifnot("A2 = 3 rows" = nrow(tblA2_raw) == 3)

cat("--- Row-label vectors validated ---\n\n")


# =============================================================================
# 3-12. BUILD TABLES
# =============================================================================
# Each table is built twice: once as LaTeX (write_tex), once as HTML
# (stored in html_tables / html_tables_app lists for assembly at Section 13).

html_tables     <- list()   # Tables 1-8
html_tables_app <- list()   # Appendix A1-A2

# Shared footnote notes
NOTE_FE   <- "CU FE + quarter-year FE. Controls: ln(assets), loan-to-asset ratio, CECL adopter. SE clustered at CU."
NOTE_STARS <- "***p<0.01, **p<0.05, *p<0.10."


# ── TABLE 1 ──────────────────────────────────────────────────────────────────
cat("--- Table 1 ---\n")
T1_NOTE <- paste0(
  NOTE_FE, " ",
  "Spread SDs from winsorized data; other SDs unwinsorized. ",
  "Commercial RE spread excluded (~78% missing). ",
  "Growth vars: log-differences x100 (approx. QoQ % change). ",
  NOTE_STARS
)

tbl1_disp <- tbl1_raw |>
  mutate(
    Variable  = T1_LABELS,
    Panel     = T1_PANELS,
    Cmplx_M   = fmt_num(complex_mean,    3),
    Cmplx_SD  = paste0("(", fmt_num(complex_sd,    3), ")"),
    NonCmx_M  = fmt_num(noncomplex_mean, 3),
    NonCmx_SD = paste0("(", fmt_num(noncomplex_sd, 3), ")")
  ) |>
  select(Panel, Variable, Cmplx_M, Cmplx_SD, NonCmx_M, NonCmx_SD)

# LaTeX
tbl1_tex <- tbl1_disp |> select(-Panel) |>
  mutate(Variable = str_replace_all(Variable, c("%" = "\\\\%", "\u00d7" = "$\\\\times$"))) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      col.names = c("Variable","Mean","(SD)","Mean","(SD)"),
      caption = paste0("\\textbf{Table 1: Summary Statistics} \\\\ ",
        "\\textit{Pre-RBC 2018Q1--2021Q4. Unit: CU-quarter. Complex N=696; non-complex N=4,488.}"),
      label = "summary_stats") |>
  add_header_above(c(" "=1,"Complex CUs (N=696)"=2,"Non-Complex CUs (N=4,488)"=2)) |>
  pack_rows("Panel A: Capital",           1,  3, bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Panel B: Loan Portfolio",    4,  7, bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Panel C: Growth",            8,  9, bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Panel D: Loan Rate Spreads",10, 12, bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Panel E: Credit Quality",   13, 14, bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Panel F: Profitability",    15, 17, bold=TRUE, latex_gap_space="0.4em") |>
  kable_styling(latex_options=c("hold_position","striped"), font_size=10) |>
  footnote(general=T1_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tbl1_tex, "table1_summary_stats.tex")

# HTML
tbl1_html <- tbl1_disp |> select(-Panel) |>
  kbl(format="html", escape=TRUE,
      col.names = c("Variable","Mean","(SD)","Mean","(SD)"),
      caption = "<b>Table 1: Summary Statistics</b> (Pre-RBC 2018Q1&ndash;2021Q4)") |>
  add_header_above(c(" "=1,"Complex CUs (N=696)"=2,"Non-Complex CUs (N=4,488)"=2)) |>
  pack_rows("Panel A: Capital",           1,  3) |>
  pack_rows("Panel B: Loan Portfolio",    4,  7) |>
  pack_rows("Panel C: Growth",            8,  9) |>
  pack_rows("Panel D: Loan Rate Spreads",10, 12) |>
  pack_rows("Panel E: Credit Quality",   13, 14) |>
  pack_rows("Panel F: Profitability",    15, 17) |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=12) |>
  footnote(general=T1_NOTE, general_title="Notes:")
html_tables[["table1"]] <- tbl1_html
cat("  [OK]\n")


# ── TABLE 2 ──────────────────────────────────────────────────────────────────
cat("--- Table 2 ---\n")
T2_NOTE <- paste0("CU-level means averaged over 2018Q1-2021Q4. ",
                   "Difference = Complex minus Non-complex. ",
                   "Stars: two-sample t-test. ", NOTE_STARS)

tbl2_disp <- tbl2_raw |>
  mutate(
    Panel     = T2_PANELS,
    Variable  = T2_LABELS,
    Complex   = fmt_num(complex_mean,    3),
    Noncomplex = fmt_num(noncomplex_mean, 3),
    Diff      = fmt_num(diff, 3),
    tstat     = paste0(fmt_num(t_stat, 2), stars(p_value))
  ) |>
  select(Panel, Variable, Complex, Noncomplex, Diff, tstat)

tbl2_tex <- tbl2_disp |> select(-Panel) |>
  mutate(Variable = str_replace_all(Variable, "%", "\\\\%")) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      col.names=c("Variable","Complex","Non-complex","Difference","t-stat"),
      caption=paste0("\\textbf{Table 2: Pre-Treatment Balance} \\\\ ",
        "\\textit{Pre-RBC period 2018Q1--2021Q4. ***$p<0.01$, **$p<0.05$, *$p<0.10$.}"),
      label="balance") |>
  pack_rows("Panel A: Capital",        1, 3,  bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Panel B: Loan Portfolio", 4, 7,  bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Panel C: Credit Quality", 8, 9,  bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Panel D: Profitability", 10, 12, bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Panel E: Size",          13, 13, bold=TRUE, latex_gap_space="0.4em") |>
  kable_styling(latex_options=c("hold_position","striped"), font_size=10) |>
  footnote(general=T2_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tbl2_tex, "table2_balance_table.tex")

tbl2_html <- tbl2_disp |> select(-Panel) |>
  kbl(format="html", escape=TRUE,
      col.names=c("Variable","Complex","Non-complex","Difference","t-stat"),
      caption="<b>Table 2: Pre-Treatment Balance</b>") |>
  pack_rows("Panel A: Capital",        1, 3)  |>
  pack_rows("Panel B: Loan Portfolio", 4, 7)  |>
  pack_rows("Panel C: Credit Quality", 8, 9)  |>
  pack_rows("Panel D: Profitability", 10, 12) |>
  pack_rows("Panel E: Size",          13, 13) |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=12) |>
  footnote(general=T2_NOTE, general_title="Notes:")
html_tables[["table2"]] <- tbl2_html
cat("  [OK]\n")


# ── TABLE 3 ──────────────────────────────────────────────────────────────────
cat("--- Table 3 ---\n")
T3_NOTE <- paste0(NOTE_FE, " Trend-adjusted DiD adds Complex x EventTime linear trend. ", NOTE_STARS)

tbl3_disp <- tbl3_raw |>
  mutate(Outcome=T3_OUTCOMES, Spec=T3_SPECS,
         Est=fmt_coef(beta,se,p_value), p=formatC(p_value,format="f",digits=3),
         N=formatC(n_obs,format="d",big.mark=",")) |>
  select(Outcome, Spec, Est, p, N)

tbl3_tex <- tbl3_disp |>
  mutate(Outcome=str_replace_all(Outcome,"%","\\\\%")) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      col.names=c("Outcome","Specification","Estimate (SE)","p-value","N"),
      caption=paste0("\\textbf{Table 3: Effect of RBC Rule on Capital Adequacy} \\\\ ",
        "\\textit{DiD estimates, 2018Q1--2025Q4. Post: 2022Q1 onward.}"),
      label="capital") |>
  kable_styling(latex_options="hold_position", font_size=10) |>
  footnote(general=T3_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tbl3_tex, "table3_capital.tex")

tbl3_html <- tbl3_disp |>
  kbl(format="html", escape=TRUE,
      col.names=c("Outcome","Specification","Estimate (SE)","p-value","N"),
      caption="<b>Table 3: Effect of RBC Rule on Capital Adequacy</b>") |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=12) |>
  footnote(general=T3_NOTE, general_title="Notes:")
html_tables[["table3"]] <- tbl3_html
cat("  [OK]\n")


# ── TABLE 4 ──────────────────────────────────────────────────────────────────
cat("--- Table 4 ---\n")
T4_NOTE <- paste0(
  "Q2 and Q4 only (NCUA semi-annual rate reporting). Baseline DiD. ", NOTE_FE, " ",
  "\u2020 Commercial RE spread: ~78% missing after removing irate_re_oth fallback — supplementary only. ",
  NOTE_STARS
)

tbl4_disp <- tbl4_raw |>
  mutate(Outcome=T4_OUTCOMES, Benchmark=T4_BENCHMARKS,
         Est=fmt_coef(beta,se,p_value), p=formatC(p_value,format="f",digits=3),
         N=formatC(n_obs,format="d",big.mark=",")) |>
  select(Outcome, Benchmark, Est, p, N)

tbl4_tex <- tbl4_disp |>
  mutate(Outcome=str_replace_all(Outcome, c("%"="\\\\%","\u2020"="$^{\\\\dagger}$"))) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      col.names=c("Outcome","Benchmark","Estimate (SE)","p-value","N"),
      caption=paste0("\\textbf{Table 4: Effect of RBC Rule on Loan Rate Spreads} \\\\ ",
        "\\textit{Baseline DiD; Q2/Q4 data, 2018--2025. Positive = treated CUs raised spreads.}"),
      label="spreads") |>
  row_spec(5, italic=TRUE) |>
  kable_styling(latex_options="hold_position", font_size=10) |>
  footnote(general=T4_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tbl4_tex, "table4_loan_spreads.tex")

tbl4_html <- tbl4_disp |>
  kbl(format="html", escape=TRUE,
      col.names=c("Outcome","Benchmark","Estimate (SE)","p-value","N"),
      caption="<b>Table 4: Effect of RBC Rule on Loan Rate Spreads</b>") |>
  row_spec(5, italic=TRUE) |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=12) |>
  footnote(general=T4_NOTE, general_title="Notes:")
html_tables[["table4"]] <- tbl4_html
cat("  [OK]\n")


# ── TABLE 5 ──────────────────────────────────────────────────────────────────
cat("--- Table 5 ---\n")
T5_NOTE <- paste0(
  "Baseline DiD for all outcomes except MBL share (trend-adjusted DiD). ",
  NOTE_FE, " Growth vars: log-differences x100 (approx. QoQ % change). ", NOTE_STARS
)

tbl5_disp <- tbl5_raw |>
  mutate(Panel=T5_PANELS, Outcome=T5_OUTCOMES,
         Est=fmt_coef(beta,se,p_value), p=formatC(p_value,format="f",digits=3),
         N=formatC(n_obs,format="d",big.mark=",")) |>
  select(Panel, Outcome, Est, p, N)

panel_rle    <- rle(tbl5_disp$Panel)
panel_names  <- panel_rle$values
panel_lens   <- panel_rle$lengths
panel_ends   <- cumsum(panel_lens)
panel_starts <- c(1L, head(panel_ends,-1L)+1L)

tbl5_tex_base <- tbl5_disp |> select(-Panel) |>
  mutate(Outcome=str_replace_all(Outcome, c("%"="\\\\%","\u00d7"="$\\\\times$"))) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      col.names=c("Outcome","Estimate (SE)","p-value","N"),
      caption=paste0("\\textbf{Table 5: Portfolio, Growth, Credit Quality, and Profitability} \\\\ ",
        "\\textit{DiD estimates, 2018Q1--2025Q4.}"),
      label="portfolio_growth")
for (i in seq_along(panel_names))
  tbl5_tex_base <- pack_rows(tbl5_tex_base, panel_names[i],
                              panel_starts[i], panel_ends[i],
                              bold=TRUE, latex_gap_space="0.4em")
tbl5_tex <- tbl5_tex_base |>
  kable_styling(latex_options=c("hold_position","striped"), font_size=10) |>
  footnote(general=T5_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tbl5_tex, "table5_portfolio_growth.tex")

tbl5_html_base <- tbl5_disp |> select(-Panel) |>
  kbl(format="html", escape=TRUE,
      col.names=c("Outcome","Estimate (SE)","p-value","N"),
      caption="<b>Table 5: Portfolio, Growth, Credit Quality, and Profitability</b>")
for (i in seq_along(panel_names))
  tbl5_html_base <- pack_rows(tbl5_html_base, panel_names[i],
                               panel_starts[i], panel_ends[i])
tbl5_html <- tbl5_html_base |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=12) |>
  footnote(general=T5_NOTE, general_title="Notes:")
html_tables[["table5"]] <- tbl5_html
cat("  [OK]\n")


# ── TABLE 6 ──────────────────────────────────────────────────────────────────
cat("--- Table 6 ---\n")
T6_NOTE <- paste0(
  "All specs: CU FE + quarter-year FE. ",
  "Baseline: full sample 2018Q1-2025Q4, winsorized outcomes. ",
  "Balanced panel: only CUs observed every quarter. ",
  "COVID: drops 2020Q1-2020Q4. Control >$300M: restricts control group. ",
  "Placebo: false treatment date (should be near zero). ", NOTE_STARS
)

tbl6_wide <- tbl6_raw |>
  mutate(coef_fmt=fmt_coef(beta,se,p_value)) |>
  select(variant, outcome_group, coef_fmt) |>
  pivot_wider(names_from=outcome_group, values_from=coef_fmt) |>
  right_join(tibble(variant=T6_VARIANTS), by="variant") |>
  select(variant, all_of(T6_GROUPS))
names(tbl6_wide)[1] <- "Specification"

tbl6_tex <- tbl6_wide |>
  mutate(Specification=str_replace_all(Specification, c("\\$"="\\\\$","%"="\\\\%"))) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      caption=paste0("\\textbf{Table 6: Robustness Checks} \\\\ ",
        "\\textit{Each cell: $\\hat{\\beta}$ (SE) for column outcome under row spec.}"),
      label="robustness") |>
  pack_rows("Main specifications",     1, 4, bold=TRUE, latex_gap_space="0.4em") |>
  pack_rows("Placebo / falsification", 5, 7, bold=TRUE, latex_gap_space="0.4em") |>
  kable_styling(latex_options=c("hold_position","scale_down"), font_size=9) |>
  footnote(general=T6_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tbl6_tex, "table6_robustness.tex")

tbl6_html <- tbl6_wide |>
  kbl(format="html", escape=TRUE,
      caption="<b>Table 6: Robustness Checks</b>") |>
  pack_rows("Main specifications",     1, 4) |>
  pack_rows("Placebo / falsification", 5, 7) |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=11) |>
  footnote(general=T6_NOTE, general_title="Notes:")
html_tables[["table6"]] <- tbl6_html
cat("  [OK]\n")


# ── TABLE 7 ──────────────────────────────────────────────────────────────────
cat("--- Table 7 ---\n")
T7_NOTE <- paste0(
  "Crisis: 2004Q1-2013Q4; treatment 2008Q3. RBC: 2018Q1-2025Q4; treatment 2022Q1. ",
  "Same $500M threshold. Difference = RBC minus Crisis (Wald test). ",
  "CECL control excluded from crisis window. ", NOTE_FE, " ", NOTE_STARS
)

tbl7_disp <- tbl7_raw |>
  mutate(
    Outcome  = T7_OUTCOMES,
    Crisis   = fmt_coef(crisis_beta, crisis_se, crisis_p),
    RBC      = fmt_coef(rbc_beta,    rbc_se,    rbc_p),
    Diff     = fmt_coef(diff,        diff_se,   diff_p),
    Interp   = T7_INTERP
  ) |>
  select(Outcome, Crisis, RBC, Diff, Interp)

tbl7_tex <- tbl7_disp |>
  mutate(Outcome=str_replace_all(Outcome,"%","\\\\%")) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      col.names=c("Outcome","Crisis (2008Q3)","RBC (2022Q1)","Difference","Interpretation"),
      caption=paste0("\\textbf{Table 7: 2008 Crisis vs.\\ 2022 RBC --- Parallel DiD} \\\\ ",
        "\\textit{Wald tests: significant difference identifies the regulatory channel.}"),
      label="crisis_comparison") |>
  add_header_above(c(" "=1,"ATT Estimates"=2,"Wald Test"=1," "=1)) |>
  pack_rows("Capital",       1,1, bold=TRUE) |>
  pack_rows("Growth",        2,3, bold=TRUE) |>
  pack_rows("Portfolio",     4,4, bold=TRUE) |>
  pack_rows("Profitability", 5,6, bold=TRUE) |>
  kable_styling(latex_options=c("hold_position","scale_down"), font_size=10) |>
  footnote(general=T7_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tbl7_tex, "table7_crisis_comparison.tex")

tbl7_html <- tbl7_disp |>
  kbl(format="html", escape=TRUE,
      col.names=c("Outcome","Crisis (2008Q3)","RBC (2022Q1)","Difference","Interpretation"),
      caption="<b>Table 7: 2008 Crisis vs. 2022 RBC &mdash; Parallel DiD</b>") |>
  add_header_above(c(" "=1,"ATT Estimates"=2,"Wald Test"=1," "=1)) |>
  pack_rows("Capital",       1,1) |>
  pack_rows("Growth",        2,3) |>
  pack_rows("Portfolio",     4,4) |>
  pack_rows("Profitability", 5,6) |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=12) |>
  footnote(general=T7_NOTE, general_title="Notes:")
html_tables[["table7"]] <- tbl7_html
cat("  [OK]\n")


# ── TABLE 8 ──────────────────────────────────────────────────────────────────
cat("--- Table 8 ---\n")
T8_NOTE <- paste0(
  "Three-group DiD: non-complex (baseline), complex full RBC (beta1), complex CCULR (beta2). ",
  "Relief = beta2 - beta1. CCULR adopters inferred from capital ratio trajectories ",
  "-- verify vs. NCUA admin records before publication. ",
  NOTE_FE, " ", NOTE_STARS
)

tbl8_disp <- tbl8_raw |>
  mutate(
    Outcome  = T8_OUTCOMES,
    RBC_est  = fmt_coef(beta1,  se1,       p1),
    CCULR_est = fmt_coef(beta2, se2,       p2),
    Relief   = fmt_coef(relief, relief_se, relief_p),
    Relief_p = formatC(relief_p, format="f", digits=3)
  ) |>
  select(Outcome, RBC_est, CCULR_est, Relief, Relief_p)

tbl8_tex <- tbl8_disp |>
  mutate(Outcome=str_replace_all(Outcome,"%","\\\\%")) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      col.names=c("Outcome",
                  "Full RBC ($\\hat{\\beta}_1$)",
                  "CCULR ($\\hat{\\beta}_2$)",
                  "Relief ($\\hat{\\beta}_2 - \\hat{\\beta}_1$)",
                  "$p$(Relief)"),
      caption=paste0("\\textbf{Table 8: CCULR Relief Value Estimates} \\\\ ",
        "\\textit{Positive relief = CCULR improved outcome relative to full-RBC complex CUs.}"),
      label="cculr") |>
  kable_styling(latex_options="hold_position", font_size=10) |>
  footnote(general=T8_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tbl8_tex, "table8_cculr.tex")

tbl8_html <- tbl8_disp |>
  kbl(format="html", escape=TRUE,
      col.names=c("Outcome",
                  "Full RBC (\u03b2\u2081)",
                  "CCULR (\u03b2\u2082)",
                  "Relief (\u03b2\u2082\u2212\u03b2\u2081)",
                  "p(Relief)"),
      caption="<b>Table 8: CCULR Relief Value Estimates</b>") |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=12) |>
  footnote(general=T8_NOTE, general_title="Notes:")
html_tables[["table8"]] <- tbl8_html
cat("  [OK]\n")


# ── APPENDIX A1 ──────────────────────────────────────────────────────────────
cat("--- Appendix A1 ---\n")
TA1_NOTE <- paste0(
  "Each cell: DiD estimate (SE) for row outcome, estimated by 2021 asset size tier. ",
  "Dose-response: larger CUs show larger effects on profitability and pricing. ",
  NOTE_FE, " ", NOTE_STARS
)

tblA1_wide <- tblA1_raw |>
  mutate(coef_fmt=fmt_coef(beta,se,p_value)) |>
  select(outcome, tier, coef_fmt) |>
  pivot_wider(names_from=tier, values_from=coef_fmt) |>
  mutate(Outcome=TA1_OUTCOMES) |>
  select(Outcome, everything(), -outcome)
tier_cols <- setdiff(names(tblA1_wide),"Outcome")
stopifnot("A1 needs exactly 3 tier columns" = length(tier_cols)==3)
names(tblA1_wide)[2:4] <- c("$500M\u2013$1B","$1B\u2013$10B",">\u2009$10B")

tblA1_tex <- tblA1_wide |>
  mutate(Outcome=str_replace_all(Outcome, c("%"="\\\\%","\u00d7"="$\\\\times$")),
         across(2:4, ~str_replace_all(.x,"\\$","\\\\$"))) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      caption=paste0("\\textbf{Appendix Table A1: Heterogeneity by Asset Size Tier} \\\\ ",
        "\\textit{DiD estimates for selected outcomes by asset size tier.}"),
      label="heterogeneity") |>
  add_header_above(c(" "=1,"Asset Size Tier (2021 Average)"=3)) |>
  kable_styling(latex_options="hold_position", font_size=10) |>
  footnote(general=TA1_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tblA1_tex, "appendix_A1_heterogeneity.tex")

tblA1_html <- tblA1_wide |>
  kbl(format="html", escape=TRUE,
      caption="<b>Appendix Table A1: Heterogeneity by Asset Size Tier</b>") |>
  add_header_above(c(" "=1,"Asset Size Tier (2021 Average)"=3)) |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=12) |>
  footnote(general=TA1_NOTE, general_title="Notes:")
html_tables_app[["A1"]] <- tblA1_html
cat("  [OK]\n")


# ── APPENDIX A2 ──────────────────────────────────────────────────────────────
cat("--- Appendix A2 ---\n")
TA2_NOTE <- paste0(
  "196 CUs with avg 2021 assets $400M-$600M (100 complex, 96 near-controls). ",
  "NW ratio sign reversal: thin-buffer CUs nearest threshold lost capital trying to comply. ",
  NOTE_FE, " ", NOTE_STARS
)

tblA2_disp <- tblA2_raw |>
  mutate(Outcome=TA2_OUTCOMES,
         Est=fmt_coef(beta,se,p_value), p=formatC(p_value,format="f",digits=3),
         N=formatC(n_obs,format="d",big.mark=",")) |>
  select(Outcome, Est, p, N)

tblA2_tex <- tblA2_disp |>
  mutate(Outcome=str_replace_all(Outcome, c("%"="\\\\%","\u00d7"="$\\\\times$"))) |>
  kbl(format="latex", booktabs=TRUE, escape=FALSE,
      col.names=c("Outcome","Estimate (SE)","p-value","N"),
      caption=paste0("\\textbf{Appendix Table A2: Near-Threshold Subgroup (\\$400M--\\$600M)} \\\\ ",
        "\\textit{Sign reversal in NW ratio confirms capital pressure on thin-buffer CUs.}"),
      label="near_threshold") |>
  kable_styling(latex_options="hold_position", font_size=10) |>
  footnote(general=TA2_NOTE, general_title="Notes:", escape=FALSE, threeparttable=TRUE)
write_tex(tblA2_tex, "appendix_A2_near_threshold.tex")

tblA2_html <- tblA2_disp |>
  kbl(format="html", escape=TRUE,
      col.names=c("Outcome","Estimate (SE)","p-value","N"),
      caption="<b>Appendix Table A2: Near-Threshold Subgroup ($400M&ndash;$600M)</b>") |>
  kable_styling(bootstrap_options=c("striped","bordered","hover"),
                full_width=FALSE, font_size=12) |>
  footnote(general=TA2_NOTE, general_title="Notes:")
html_tables_app[["A2"]] <- tblA2_html
cat("  [OK]\n\n")


# =============================================================================
# 13. WORD-COMPATIBLE HTML OUTPUT
# =============================================================================
# Assembles all kableExtra HTML objects into two styled HTML files.
# TO USE IN WORD: File > Open > tables_main_word.html
#                 File > Save As > Word Document (.docx)
# No extra packages needed — kableExtra's HTML output is natively compatible.
# =============================================================================

cat("--- Assembling Word-compatible HTML files ---\n")

WORD_CSS <- "
<style>
  body  { font-family: 'Times New Roman', Times, serif; font-size: 12pt;
          margin: 2.5cm; color: #111; background: #fff; }
  h1    { font-size: 16pt; margin-bottom: 8pt; }
  h2    { font-size: 13pt; margin-top: 28pt; margin-bottom: 4pt;
          border-bottom: 1px solid #999; padding-bottom: 2pt; }
  p     { font-size: 11pt; margin-bottom: 6pt; }
  table { border-collapse: collapse; width: 100%; margin-bottom: 14pt;
          font-size: 10pt; }
  th    { background-color: #2c2419; color: #fff; font-weight: bold;
          padding: 4pt 7pt; border: 1px solid #555; text-align: left; }
  td    { padding: 4pt 7pt; border: 1px solid #bbb; text-align: left; }
  tr:nth-child(even) td { background-color: #f5f0e8; }
  caption { font-weight: bold; text-align: left; margin-bottom: 4pt;
            font-size: 10pt; caption-side: top; }
  .table-note { font-size: 9pt; color: #444; margin-top: 3pt; }
  hr    { border: none; border-top: 1px solid #ccc; margin: 20pt 0; }
  .pb   { page-break-after: always; }
</style>
"

add_section <- function(kbl_html, heading, page_break=TRUE) {
  pb <- if (page_break) '<div class="pb"></div>\n' else ""
  paste0(pb, "<h2>", heading, "</h2>\n", kbl_html, "\n<hr>\n")
}

# Main HTML
main_body <- paste0(
  "<!DOCTYPE html><html lang='en'><head>",
  "<meta charset='UTF-8'>",
  "<title>RBC Paper &mdash; Main Tables</title>",
  WORD_CSS, "</head><body>",
  "<h1>RBC Paper &mdash; Publication Tables (Main)</h1>",
  "<p><em>The Impact of Risk-Based Capital Requirements on Complex Credit Unions: ",
  "Evidence from the NCUA&rsquo;s 2022 RBC Rule</em></p>",
  "<p>Saurabh C. Datta, Ph.D. &middot; NCUA Office of the Chief Economist &middot; April 2026</p>",
  "<p><strong>To convert to Word:</strong> Open this file in Microsoft Word ",
  "&rarr; File &rarr; Save As &rarr; Word Document (.docx)</p>",
  add_section(html_tables[["table1"]], "Table 1: Summary Statistics",             page_break=FALSE),
  add_section(html_tables[["table2"]], "Table 2: Pre-Treatment Balance"),
  add_section(html_tables[["table3"]], "Table 3: Capital Adequacy DiD"),
  add_section(html_tables[["table4"]], "Table 4: Loan Rate Spreads DiD"),
  add_section(html_tables[["table5"]], "Table 5: Portfolio, Growth, Credit Quality, Profitability"),
  add_section(html_tables[["table6"]], "Table 6: Robustness Checks"),
  add_section(html_tables[["table7"]], "Table 7: Crisis vs. RBC Parallel DiD"),
  add_section(html_tables[["table8"]], "Table 8: CCULR Relief Value"),
  "</body></html>"
)
writeLines(main_body, file.path(TABLES_DIR,"tables_main_word.html"), useBytes=TRUE)
cat("  [OK] tables_main_word.html\n")

# Appendix HTML
app_body <- paste0(
  "<!DOCTYPE html><html lang='en'><head>",
  "<meta charset='UTF-8'>",
  "<title>RBC Paper &mdash; Appendix Tables</title>",
  WORD_CSS, "</head><body>",
  "<h1>RBC Paper &mdash; Appendix Tables</h1>",
  "<p>Saurabh C. Datta, Ph.D. &middot; April 2026</p>",
  add_section(html_tables_app[["A1"]],
              "Appendix Table A1: Heterogeneity by Asset Size Tier", page_break=FALSE),
  add_section(html_tables_app[["A2"]],
              "Appendix Table A2: Near-Threshold Subgroup ($400M&ndash;$600M)"),
  "</body></html>"
)
writeLines(app_body, file.path(TABLES_DIR,"tables_appendix_word.html"), useBytes=TRUE)
cat("  [OK] tables_appendix_word.html\n\n")


# =============================================================================
# 14. COMBINED LATEX INDEX FILES
# =============================================================================

cat("--- Writing LaTeX index files ---\n")

TABLES_ORDERED  <- c("table1_summary_stats.tex","table2_balance_table.tex",
                      "table3_capital.tex","table4_loan_spreads.tex",
                      "table5_portfolio_growth.tex","table6_robustness.tex",
                      "table7_crisis_comparison.tex","table8_cculr.tex")
APPENDIX_TABLES <- c("appendix_A1_heterogeneity.tex","appendix_A2_near_threshold.tex")

writeLines(
  c("% tables_main.tex  |  \\input{Tables/tables_main.tex}",
    "% Generated by 4_Paper_Tables.R | April 2026", "",
    paste0("\\input{Tables/",TABLES_ORDERED,"}",
           collapse="\n\n\\clearpage\n\n")),
  file.path(TABLES_DIR,"tables_main.tex")
)
writeLines(
  c("% tables_appendix.tex  |  \\input{Tables/tables_appendix.tex}",
    "% Generated by 4_Paper_Tables.R | April 2026", "",
    paste0("\\input{Tables/",APPENDIX_TABLES,"}",
           collapse="\n\n\\clearpage\n\n")),
  file.path(TABLES_DIR,"tables_appendix.tex")
)
cat("  [OK] tables_main.tex\n  [OK] tables_appendix.tex\n\n")


# =============================================================================
# 15. VALIDATION
# =============================================================================

cat("--- Validation checks ---\n")

# Growth label present in every growth row
stopifnot("Growth suffix in T1[8]"  = str_detect(T1_LABELS[8],  "\u00d7100"))
stopifnot("Growth suffix in T1[9]"  = str_detect(T1_LABELS[9],  "\u00d7100"))
stopifnot("Growth suffix in T5[4]"  = str_detect(T5_OUTCOMES[4],"\u00d7100"))
stopifnot("Growth suffix in T5[5]"  = str_detect(T5_OUTCOMES[5],"\u00d7100"))
stopifnot("Growth suffix in TA2[2]" = str_detect(TA2_OUTCOMES[2],"\u00d7100"))

# Row counts
stopifnot("T3=3"  = nrow(tbl3_raw)==3)
stopifnot("T4=5"  = nrow(tbl4_raw)==5)
stopifnot("T5=11" = nrow(tbl5_raw)==11)
stopifnot("T7=6"  = nrow(tbl7_raw)==6)
stopifnot("T8=4"  = nrow(tbl8_raw)==4)
stopifnot("A1=15" = nrow(tblA1_raw)==15)
stopifnot("A2=3"  = nrow(tblA2_raw)==3)

# All output files exist
all_outputs <- file.path(TABLES_DIR,
  c(TABLES_ORDERED, APPENDIX_TABLES,
    "tables_main.tex","tables_appendix.tex",
    "tables_main_word.html","tables_appendix_word.html"))
missing <- all_outputs[!file.exists(all_outputs)]
if (length(missing)>0) warning("Missing: ", paste(basename(missing),collapse=", "))
stopifnot("All outputs written" = length(missing)==0)

cat("  [OK] All checks passed\n\n")


# =============================================================================
# 16. SUMMARY
# =============================================================================

cat("=================================================================\n")
cat("  4_Paper_Tables.R  COMPLETE\n")
cat("=================================================================\n")
cat(glue("  Output: {TABLES_DIR}\n\n"))
cat("  LaTeX  — individual .tex files (include with \\input{}):\n")
walk(c(TABLES_ORDERED,APPENDIX_TABLES), ~cat(glue("    {.x}\n")))
cat("\n  LaTeX  — index files:\n")
cat("    tables_main.tex\n    tables_appendix.tex\n\n")
cat("  Word   — open in Word, save as .docx:\n")
cat("    tables_main_word.html\n    tables_appendix_word.html\n\n")
cat("  Packages used: tidyverse, kableExtra, scales, glue\n")
cat("  (officer and flextable NOT required)\n\n")
cat("  REMINDER: verify CCULR adopter inference vs NCUA admin\n")
cat("            records before journal submission (Table 8).\n")
cat("=================================================================\n")
