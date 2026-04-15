# =============================================================================
# 4_Paper_Tables.R
# RBC Rule Impact Analysis — Publication-Ready Tables & Figures
# NCUA Call Report (5300) Data
#
# Author  : [Your Name]
# Created : 2026
#
# PURPOSE:
#   Assembles all estimation results into publication-ready tables and
#   figures suitable for journal submission. Reads from saved CSVs produced
#   by scripts 0–3C so it can be run standalone without re-estimating.
#
# OUTPUT TABLES:
#   Table 1  : Summary statistics (full panel, by treatment group)
#   Table 2  : Balance table (pre-RBC characteristics)
#   Table 3  : Main DiD results — capital outcomes
#   Table 4  : Main DiD results — loan pricing (spreads)
#   Table 5  : Main DiD results — portfolio, growth, credit quality
#   Table 6  : Robustness checks
#   Table 7  : 2008 crisis vs. RBC parallel comparison (from 3A)
#   Table 8  : CCULR relief value (from 3C)
#   Table 9A : Repeal simulation — recovery fractions by scenario (from 3D)
#   Table 9B : Repeal simulation — cumulative member welfare savings (from 3D)
#   Table A1 : Heterogeneity by asset tier
#   Table A2 : Near-threshold subgroup
#
# OUTPUT FIGURES (publication quality, 300dpi):
#   Fig 1  : Net worth ratio distribution (threshold motivating figure)
#   Fig 2  : Pre-trend check (parallel trends validation)
#   Fig 3  : Main event study panel (capital + lending)
#   Fig 4  : Loan spread event study panel
#   Fig 5  : Portfolio reallocation event study panel
#   Fig 6  : 2008 crisis vs. RBC comparison panel
#   Fig 7  : CCULR three-group event study
#   Fig A1 : Heterogeneity coefficient plot
#
# Input  : output/tables/*.csv  (from scripts 1–3D)
#          output/figures/*.png (from scripts 1–3D)
#          data/analysis_panel.rds
# Output : output/paper/tables/  — formatted CSV + Word-ready
#          output/paper/figures/ — final publication PNGs
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(scales)
library(gt)           # publication tables
library(gtsummary)    # summary/balance tables
library(patchwork)    # combining figures
library(fixest)       # for inline re-estimation if needed
library(broom)

# install.packages(c("gt", "gtsummary"))


# =============================================================================
# 1. SETTINGS
# =============================================================================

PANEL_PATH   <- "data/analysis_panel.rds"
TABLE_IN     <- "output/tables/"
FIGURE_IN    <- "output/figures/"
TABLE_OUT    <- "output/paper/tables/"
FIGURE_OUT   <- "output/paper/figures/"

# Journal formatting
FONT_SIZE    <- 11
TABLE_WIDTH  <- 180   # mm (approx A4 width minus margins)

# Colors (consistent throughout project)
COL_COMPLEX    <- "#1B3A6B"
COL_NONCOMPLEX <- "#C94040"
COL_THRESHOLD  <- "#E8A838"

theme_paper <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold", size = base + 1),
      plot.subtitle    = element_text(color = "gray35", size = base - 1),
      axis.title       = element_text(size = base - 1),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      strip.text       = element_text(face = "bold", size = base - 1),
      plot.caption     = element_text(color = "gray45", size = base - 2),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

dir.create(TABLE_OUT,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_OUT, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 2. LOAD DATA & RESULTS
# =============================================================================

message("── Step 1: Loading data and results ─────────────────────────────────")

df <- readRDS(PANEL_PATH)

# Load all CSV results — use tryCatch so missing files don't break script
safe_read <- function(fname) {
  path <- file.path(TABLE_IN, fname)
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE)
  } else {
    message("  Not found: ", fname)
    NULL
  }
}

res_summary    <- safe_read("summary_stats_full_panel.csv")
res_balance    <- safe_read("balance_table_pre_rbc.csv")
res_main       <- safe_read("did_main_results.csv")
res_robust     <- safe_read("robustness_checks.csv")
res_het        <- safe_read("heterogeneity_by_tier.csv")
res_threshold  <- safe_read("near_threshold_results.csv")
res_crisis     <- safe_read("3A_summary_comparison.csv")
res_cculr      <- safe_read("3C_three_group_did.csv")
res_shape      <- safe_read("3B_summary_shape_timing.csv")
res_pretrend   <- safe_read("3C_pretrend_test.csv")
res_sim        <- safe_read("3D_simulation_summary.csv")

message("  Data and results loaded.")


# =============================================================================
# 3. TABLE 1 — SUMMARY STATISTICS
# =============================================================================

message("── Step 2: Table 1 — Summary statistics ─────────────────────────────")

sum_vars <- c(
  "networth_ratio", "cap_buffer", "well_capitalized",
  "loan_to_asset", "mbl_shr", "re_shr", "auto_shr",
  "loan_growth", "asset_growth",
  "spread_mortgage", "spread_nauto", "spread_comm_oth",
  "dq_rate_var", "chgoff_ratio", "pll_assets",
  "roa_var", "nim", "cof"
)

sum_labels <- c(
  "Net worth ratio (%)", "Capital buffer vs 10% (pp)",
  "Well-capitalized (binary)",
  "Loans / assets (%)",
  "MBL share of loans (%)", "RE share of loans (%)",
  "Auto share of loans (%)",
  "Loan growth (QoQ log × 100)", "Asset growth (QoQ log × 100)",
  "Mortgage spread over 10yr Treasury (pp)",
  "New auto spread over 2yr Treasury (pp)",
  "Comm non-RE spread over 2yr Treasury (pp)",
  "Delinquency rate (%)", "Charge-off ratio (%)",
  "PLL / assets (%)",
  "Return on assets (%)", "Net interest margin (%)",
  "Cost of funds (%)"
)

# Panel A: Full sample
compute_stats <- function(data, vars, labels, group_filter = NULL) {
  if (!is.null(group_filter)) data <- data[group_filter, ]
  map2_dfr(vars, labels, function(v, lbl) {
    if (!v %in% names(data)) return(NULL)
    x <- data[[v]]
    tibble(
      Variable = lbl,
      N        = sum(!is.na(x)),
      Mean     = round(mean(x, na.rm = TRUE), 3),
      SD       = round(sd(x,   na.rm = TRUE), 3),
      P10      = round(quantile(x, 0.10, na.rm = TRUE), 3),
      Median   = round(quantile(x, 0.50, na.rm = TRUE), 3),
      P90      = round(quantile(x, 0.90, na.rm = TRUE), 3),
      Pct_miss = round(mean(is.na(x)) * 100, 1)
    )
  })
}

# By treatment group
tbl1_full    <- compute_stats(df, sum_vars, sum_labels)
tbl1_complex <- compute_stats(df, sum_vars, sum_labels,
                               df$complex == 1 & df$post_rbc == 0)
tbl1_control <- compute_stats(df, sum_vars, sum_labels,
                               df$complex == 0 & df$post_rbc == 0)

# Combine with group labels
tbl1 <- bind_rows(
  tbl1_complex |> mutate(Group = "Complex (pre-RBC)"),
  tbl1_control |> mutate(Group = "Non-complex (pre-RBC)"),
  tbl1_full    |> mutate(Group = "Full panel")
)

# Format as wide table
tbl1_wide <- tbl1 |>
  select(Variable, Group, Mean, SD, Median, N) |>
  pivot_wider(
    names_from  = Group,
    values_from = c(Mean, SD, Median, N),
    names_sep   = "_"
  )

write_csv(tbl1_wide, file.path(TABLE_OUT, "Table1_SummaryStats.csv"))
message("  Table 1 saved.")

# gt version
tbl1_gt <- tbl1 |>
  filter(Group != "Full panel") |>
  select(Variable, Group, Mean, SD, Median) |>
  pivot_wider(names_from = Group,
              values_from = c(Mean, SD, Median)) |>
  gt() |>
  tab_header(
    title    = "Table 1. Summary Statistics",
    subtitle = "Pre-RBC period (2018 Q1 – 2021 Q4). Mean (SD) and median."
  ) |>
  tab_spanner(
    label   = "Complex CUs (>$500M)",
    columns = ends_with("Complex (pre-RBC)")
  ) |>
  tab_spanner(
    label   = "Non-complex CUs (<$500M)",
    columns = ends_with("Non-complex (pre-RBC)")
  ) |>
  tab_footnote(
    "Complex = avg assets > $500M in 2021. Source: NCUA Call Report (5300)."
  ) |>
  opt_table_font(font = "Times New Roman", size = FONT_SIZE)

gtsave(tbl1_gt, file.path(TABLE_OUT, "Table1_SummaryStats.html"))
message("  Table 1 (gt) saved.")


# =============================================================================
# 4. TABLE 2 — BALANCE TABLE
# =============================================================================

message("── Step 3: Table 2 — Balance table ──────────────────────────────────")

if (!is.null(res_balance)) {

  tbl2_gt <- res_balance |>
    select(Variable,
           `Complex (N=696)`,
           `Non-complex (N=4,488)`,
           Difference,
           `p-value`) |>
    gt() |>
    tab_header(
      title    = "Table 2. Pre-RBC Balance Table",
      subtitle = paste0(
        "CU-level means (SD) pre-RBC period. ",
        "p-value from two-sample t-test."
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    cols_label(
      Variable             = "Variable",
      `Complex (N=696)`    = "Complex CUs",
      `Non-complex (N=4,488)` = "Non-complex CUs",
      Difference           = "Difference",
      `p-value`            = "p-value"
    ) |>
    tab_footnote(
      paste0(
        "Complex = avg assets > $500M in 2021q1–2021q4. ",
        "Source: NCUA Call Report (5300)."
      )
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tbl2_gt, file.path(TABLE_OUT, "Table2_BalanceTable.html"))
  write_csv(res_balance, file.path(TABLE_OUT, "Table2_BalanceTable.csv"))
  message("  Table 2 saved.")
}


# =============================================================================
# 5. TABLE 3 — MAIN DiD: CAPITAL OUTCOMES
# =============================================================================

message("── Step 4: Table 3 — Capital DiD results ────────────────────────────")

if (!is.null(res_main)) {

  # Filter to capital outcomes, preferred specs
  cap_outcomes <- c(
    "Net worth ratio (%)",
    "Capital buffer vs 10% (pp)",
    "Well-capitalized (LPM)"
  )

  tbl3 <- res_main |>
    filter(
      (Outcome %in% cap_outcomes[1:2] & Spec == "Trend-adjusted DiD") |
        (Outcome == cap_outcomes[3]   & Spec == "Baseline DiD")
    ) |>
    mutate(
      Result = paste0(
        format(Beta, nsmall = 3), Stars,
        "\n(", format(SE, nsmall = 3), ")"
      ),
      Spec_label = case_when(
        Spec == "Trend-adjusted DiD" ~ "Trend-adj. DiD",
        Spec == "Baseline DiD"       ~ "Baseline DiD",
        TRUE                         ~ Spec
      )
    ) |>
    select(Outcome, Spec_label, Beta, Stars, SE, CI_low, CI_high,
           p_value, N_obs)

  write_csv(tbl3, file.path(TABLE_OUT, "Table3_Capital_DiD.csv"))

  tbl3_gt <- tbl3 |>
    mutate(
      Coef_SE = paste0(sprintf("%.3f", Beta), Stars,
                       " (", sprintf("%.3f", SE), ")")
    ) |>
    select(Outcome, Spec_label, Coef_SE, CI_low, CI_high, p_value, N_obs) |>
    gt() |>
    tab_header(
      title    = "Table 3. RBC Rule Impact on Capital Adequacy",
      subtitle = "DiD estimates (Complex × Post-RBC). Two-way FE (CU + quarter-year)."
    ) |>
    cols_label(
      Outcome    = "Outcome",
      Spec_label = "Specification",
      Coef_SE    = "Coef. (SE)",
      CI_low     = "95% CI Low",
      CI_high    = "95% CI High",
      p_value    = "p-value",
      N_obs      = "N"
    ) |>
    tab_footnote(
      paste0(
        "SE clustered at CU level. Trend-adjusted specs include ",
        "complex-specific linear time trend to account for pre-RBC ",
        "divergence toward the 10% threshold. ",
        "*, **, *** = 10%, 5%, 1% significance."
      )
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tbl3_gt, file.path(TABLE_OUT, "Table3_Capital_DiD.html"))
  message("  Table 3 saved.")
}


# =============================================================================
# 6. TABLE 4 — MAIN DiD: LOAN PRICING
# =============================================================================

message("── Step 5: Table 4 — Loan pricing DiD ──────────────────────────────")

if (!is.null(res_main)) {

  spread_outcomes <- c(
    "Mortgage spread (pp)", "New auto spread (pp)",
    "Used auto spread (pp)", "Comm non-RE spread (pp)",
    "Comm RE spread (pp)"
  )

  tbl4 <- res_main |>
    filter(Outcome %in% spread_outcomes,
           Spec == "Baseline DiD") |>
    mutate(
      Coef_SE   = paste0(sprintf("%.3f", Beta), Stars,
                         " (", sprintf("%.3f", SE), ")"),
      Benchmark = case_when(
        grepl("Mortgage|Comm RE", Outcome) ~ "10yr Treasury",
        grepl("Credit Card",      Outcome) ~ "1yr Treasury",
        TRUE                               ~ "2yr Treasury"
      )
    ) |>
    select(Outcome, Benchmark, Coef_SE, CI_low, CI_high, p_value, N_obs, Note)

  write_csv(tbl4, file.path(TABLE_OUT, "Table4_LoanSpreads_DiD.csv"))

  tbl4_gt <- tbl4 |>
    gt() |>
    tab_header(
      title    = "Table 4. RBC Rule Impact on Loan Rate Spreads",
      subtitle = paste0(
        "DiD estimates (Complex × Post-RBC). ",
        "Spread = loan rate minus matched-maturity Treasury benchmark."
      )
    ) |>
    cols_label(
      Outcome   = "Outcome",
      Benchmark = "Treasury Benchmark",
      Coef_SE   = "Coef. (SE)",
      CI_low    = "95% CI Low",
      CI_high   = "95% CI High",
      p_value   = "p-value",
      N_obs     = "N",
      Note      = "Note"
    ) |>
    tab_footnote(
      paste0(
        "Q2 and Q4 observations only (NCUA semi-annual rate reporting). ",
        "Two-way FE (CU + quarter-year). SE clustered at CU level. ",
        "*, **, *** = 10%, 5%, 1%."
      )
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tbl4_gt, file.path(TABLE_OUT, "Table4_LoanSpreads_DiD.html"))
  message("  Table 4 saved.")
}


# =============================================================================
# 7. TABLE 5 — MAIN DiD: PORTFOLIO, GROWTH, CREDIT QUALITY, PROFITABILITY
# =============================================================================

message("── Step 6: Table 5 — Portfolio/growth/credit DiD ────────────────────")

if (!is.null(res_main)) {

  other_outcomes <- c(
    "MBL share (%)", "RE share (%)", "Auto share (%)",
    "Loan growth (QoQ log)", "Asset growth (QoQ log)",
    "Delinquency rate (%)", "Charge-off ratio (%)", "PLL / assets (%)",
    "ROA (%)", "NIM (%)", "Cost of funds (%)"
  )

  tbl5 <- res_main |>
    filter(
      (Outcome == "MBL share (%)" & Spec == "Trend-adjusted DiD") |
        (Outcome %in% other_outcomes[2:length(other_outcomes)] &
           Spec == "Baseline DiD")
    ) |>
    mutate(
      Coef_SE  = paste0(sprintf("%.3f", Beta), Stars,
                        " (", sprintf("%.3f", SE), ")"),
      Category = case_when(
        Outcome %in% c("MBL share (%)", "RE share (%)", "Auto share (%)") ~
          "Portfolio Composition",
        Outcome %in% c("Loan growth (QoQ log)", "Asset growth (QoQ log)") ~
          "Growth",
        Outcome %in% c("Delinquency rate (%)", "Charge-off ratio (%)",
                       "PLL / assets (%)") ~ "Credit Quality",
        TRUE ~ "Profitability"
      )
    ) |>
    arrange(Category, Outcome) |>
    select(Category, Outcome, Coef_SE, CI_low, CI_high, p_value, N_obs)

  write_csv(tbl5, file.path(TABLE_OUT, "Table5_Portfolio_Growth_Credit.csv"))

  tbl5_gt <- tbl5 |>
    gt(groupname_col = "Category") |>
    tab_header(
      title    = "Table 5. RBC Rule Impact: Portfolio, Growth, Credit Quality, Profitability",
      subtitle = "DiD estimates (Complex × Post-RBC). Two-way FE (CU + quarter-year)."
    ) |>
    cols_label(
      Outcome = "Outcome", Coef_SE = "Coef. (SE)",
      CI_low  = "95% CI Low", CI_high = "95% CI High",
      p_value = "p-value",   N_obs   = "N"
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_footnote(
      paste0(
        "MBL share uses trend-adjusted spec; all others baseline DiD. ",
        "SE clustered at CU. *, **, *** = 10%, 5%, 1%."
      )
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tbl5_gt, file.path(TABLE_OUT, "Table5_Portfolio_Growth_Credit.html"))
  message("  Table 5 saved.")
}


# =============================================================================
# 8. TABLE 6 — ROBUSTNESS CHECKS
# =============================================================================

message("── Step 7: Table 6 — Robustness checks ──────────────────────────────")

if (!is.null(res_robust)) {

  tbl6 <- res_robust |>
    mutate(
      Coef_SE = paste0(sprintf("%.3f", Beta), Stars,
                       " (", sprintf("%.3f", SE), ")")
    ) |>
    select(Outcome, Spec, Coef_SE, p_value, N_obs, Note)

  write_csv(tbl6, file.path(TABLE_OUT, "Table6_Robustness.csv"))

  tbl6_gt <- tbl6 |>
    gt(groupname_col = "Outcome") |>
    tab_header(
      title    = "Table 6. Robustness Checks",
      subtitle = "DiD coefficient on Complex × Post-RBC across alternative specifications."
    ) |>
    cols_label(
      Spec    = "Specification",
      Coef_SE = "Coef. (SE)",
      p_value = "p-value",
      N_obs   = "N",
      Note    = "Note"
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_footnote(
      paste0(
        "All specs: two-way FE (CU + quarter-year), SE clustered at CU. ",
        "Placebo uses 2020 Q1 as fake treatment date on pre-RBC data. ",
        "*, **, *** = 10%, 5%, 1%."
      )
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tbl6_gt, file.path(TABLE_OUT, "Table6_Robustness.html"))
  message("  Table 6 saved.")
}


# =============================================================================
# 9. TABLE 7 — 2008 CRISIS vs. RBC COMPARISON
# =============================================================================

message("── Step 8: Table 7 — Crisis vs. RBC comparison ──────────────────────")

if (!is.null(res_crisis)) {

  tbl7 <- res_crisis |>
    mutate(
      Crisis_result = paste0(sprintf("%.3f", `Crisis Beta`),
                             `Crisis Stars`,
                             " (", sprintf("%.3f", `Crisis SE`), ")"),
      RBC_result    = paste0(sprintf("%.3f", `RBC Beta`),
                             " (implied)"),
      Diff_result   = paste0(sprintf("%.3f", `Diff (RBC-Crisis)`),
                             `Diff Stars`,
                             " (", sprintf("%.3f", `Diff SE`), ")")
    ) |>
    select(Outcome, Crisis_result, RBC_result, Diff_result,
           `Diff p-value`, Interpretation)

  write_csv(tbl7, file.path(TABLE_OUT, "Table7_Crisis_vs_RBC.csv"))

  tbl7_gt <- tbl7 |>
    gt() |>
    tab_header(
      title    = "Table 7. 2008 Financial Crisis vs. 2022 RBC Rule: Parallel DiD",
      subtitle = paste0(
        "Identical specification applied to both windows. ",
        "Diff = RBC coefficient minus crisis coefficient."
      )
    ) |>
    cols_label(
      Outcome        = "Outcome",
      Crisis_result  = "Crisis (2008 Q3)",
      RBC_result     = "RBC Rule (2022 Q1)",
      Diff_result    = "Difference (RBC − Crisis)",
      `Diff p-value` = "p-value",
      Interpretation = "Interpretation"
    ) |>
    tab_style(
      style = cell_text(color = "darkred", weight = "bold"),
      locations = cells_body(
        columns = Interpretation,
        rows    = grepl("LARGER|amplification", Interpretation)
      )
    ) |>
    tab_footnote(
      paste0(
        "Two-way FE (CU + quarter-year). SE clustered at CU. ",
        "Treatment = avg assets > $500M in pre-period. ",
        "Stars on Diff = RBC effect is statistically different from crisis. ",
        "*, **, *** = 10%, 5%, 1%."
      )
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tbl7_gt, file.path(TABLE_OUT, "Table7_Crisis_vs_RBC.html"))
  message("  Table 7 saved.")
}


# =============================================================================
# 10. TABLE 8 — CCULR RELIEF VALUE
# =============================================================================

message("── Step 9: Table 8 — CCULR relief value ─────────────────────────────")

if (!is.null(res_cculr)) {

  tbl8 <- res_cculr |>
    mutate(
      RBC_result   = paste0(sprintf("%.3f", Beta_RBC),
                            Stars_RBC,
                            " (", sprintf("%.3f", SE_RBC), ")"),
      CCULR_result = paste0(sprintf("%.3f", Beta_CCULR),
                            Stars_CCULR,
                            " (", sprintf("%.3f", SE_CCULR), ")"),
      Relief_result = paste0(sprintf("%.3f", Relief),
                             Stars_Relief,
                             " (", sprintf("%.3f", Relief_SE), ")")
    ) |>
    select(Outcome, RBC_result, CCULR_result, Relief_result, Relief_p, N)

  write_csv(tbl8, file.path(TABLE_OUT, "Table8_CCULR_Relief.csv"))

  tbl8_gt <- tbl8 |>
    gt() |>
    tab_header(
      title    = "Table 8. CCULR Adoption: Relief Value Estimates",
      subtitle = paste0(
        "Three-group DiD: Y ~ rbc_post + cculr_post + FE(CU + quarter). ",
        "Relief = beta_CCULR − beta_RBC."
      )
    ) |>
    cols_label(
      Outcome       = "Outcome",
      RBC_result    = "Full RBC (β₁)",
      CCULR_result  = "CCULR adopters (β₂)",
      Relief_result = "Relief (β₂ − β₁)",
      Relief_p      = "p(Relief)",
      N             = "N"
    ) |>
    tab_footnote(
      paste0(
        "Both groups vs. non-complex control. SE clustered at CU. ",
        "Positive Relief = CCULR dampened the RBC effect. ",
        "CCULR classification based on capital ratio inference ",
        "(no direct NCUA flag available). ",
        "*, **, *** = 10%, 5%, 1%."
      )
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tbl8_gt, file.path(TABLE_OUT, "Table8_CCULR_Relief.html"))
  message("  Table 8 saved.")
}


# =============================================================================
# 11. APPENDIX TABLE A1 — HETEROGENEITY BY TIER
# =============================================================================

message("── Step 10: Table A1 — Heterogeneity by tier ────────────────────────")

if (!is.null(res_het)) {

  tblA1 <- res_het |>
    mutate(
      Coef_SE = paste0(sprintf("%.3f", Beta), Stars,
                       " (", sprintf("%.3f", SE), ")")
    ) |>
    select(Outcome, Tier, Coef_SE, CI_low, CI_high, p_value, N)

  write_csv(tblA1, file.path(TABLE_OUT, "TableA1_Heterogeneity.csv"))

  tblA1_gt <- tblA1 |>
    gt(groupname_col = "Outcome") |>
    tab_header(
      title    = "Appendix Table A1. Heterogeneity by Asset Size Tier",
      subtitle = "DiD estimates by treated tier vs. full non-complex control."
    ) |>
    cols_label(
      Tier    = "Asset Size Tier",
      Coef_SE = "Coef. (SE)",
      CI_low  = "95% CI Low",
      CI_high = "95% CI High",
      p_value = "p-value",
      N       = "N"
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tblA1_gt, file.path(TABLE_OUT, "TableA1_Heterogeneity.html"))
  message("  Table A1 saved.")
}


# =============================================================================
# 12. APPENDIX TABLE A2 — NEAR-THRESHOLD SUBGROUP
# =============================================================================

message("── Step 11: Table A2 — Near-threshold subgroup ──────────────────────")

if (!is.null(res_threshold)) {

  tblA2 <- res_threshold |>
    mutate(
      Coef_SE = paste0(sprintf("%.3f", Beta), Stars,
                       " (", sprintf("%.3f", SE), ")")
    ) |>
    select(Outcome, Sample, Coef_SE, CI_low, CI_high, p_value, N)

  write_csv(tblA2, file.path(TABLE_OUT, "TableA2_NearThreshold.csv"))

  tblA2_gt <- tblA2 |>
    gt() |>
    tab_header(
      title    = "Appendix Table A2. Near-Threshold Subgroup Analysis",
      subtitle = "CUs with avg assets $400M–$600M (within ±20% of $500M threshold)."
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tblA2_gt, file.path(TABLE_OUT, "TableA2_NearThreshold.html"))
  message("  Table A2 saved.")
}


# =============================================================================
# 12B. TABLE 9 — REPEAL SIMULATION WELFARE SUMMARY (from 3D)
# =============================================================================
# Source CSV: output/tables/3D_simulation_summary.csv
# Expected columns (from 3D_Repeal_Simulation.R):
#   Outcome, Beta, Reversibility, HalfLife_Q, Scenario,
#   Recovered_4Q, Recovered_8Q, Recovered_16Q,
#   CumSavings_4Y_BN (positive-beta outcomes only),
#   Units, Note
# =============================================================================

message("── Step 12B: Table 9 — Repeal simulation summary ────────────────────")

if (!is.null(res_sim)) {

  # ── 9A: Recovery fractions by outcome and scenario ──────────────────────────
  tbl9a <- res_sim |>
    filter(Scenario == "Gradual") |>          # Central scenario for main table
    mutate(
      Recovered_4Q  = paste0(sprintf("%.0f", Recovered_4Q  * 100), "%"),
      Recovered_8Q  = paste0(sprintf("%.0f", Recovered_8Q  * 100), "%"),
      Recovered_16Q = paste0(sprintf("%.0f", Recovered_16Q * 100), "%")
    ) |>
    select(Outcome, Reversibility, HalfLife_Q,
           Recovered_4Q, Recovered_8Q, Recovered_16Q, Units, Note)

  write_csv(tbl9a, file.path(TABLE_OUT, "Table9A_Simulation_Recovery.csv"))

  tbl9a_gt <- tbl9a |>
    gt(groupname_col = "Reversibility") |>
    tab_header(
      title    = "Table 9A. Repeal Simulation: Recovery of RBC Effects",
      subtitle = paste0(
        "Central (Gradual) scenario. Repeal assumed effective 2026 Q1. ",
        "Recovery = fraction of DiD ATT estimate reversed at each horizon."
      )
    ) |>
    cols_label(
      Outcome       = "Outcome",
      HalfLife_Q    = "Half-life (Q)",
      Recovered_4Q  = "4Q Post-Repeal",
      Recovered_8Q  = "8Q Post-Repeal",
      Recovered_16Q = "16Q Post-Repeal",
      Units         = "Units",
      Note          = "Note"
    ) |>
    tab_spanner(
      label   = "Fraction of Effect Recovered",
      columns = c(Recovered_4Q, Recovered_8Q, Recovered_16Q)
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style     = cell_fill(color = "#fff3cd"),
      locations = cells_body(
        columns = Recovered_16Q,
        rows    = Reversibility == "Slow"
      )
    ) |>
    tab_footnote(
      paste0(
        "Fast reversibility: 4Q half-life (calibrated from 2008 crisis recovery in 3A). ",
        "Gradual: 8Q pricing / 16Q portfolio half-life (from 3B persistence ratios). ",
        "Slow: plateau at 50% after 16Q (step-change outcomes showing no reversal in event studies). ",
        "All parameters hardcoded from DiD ATT estimates; no new regressions."
      )
    ) |>
    opt_table_font(font = "Times New Roman", size = FONT_SIZE)

  gtsave(tbl9a_gt, file.path(TABLE_OUT, "Table9A_Simulation_Recovery.html"))
  message("  Table 9A saved.")

  # ── 9B: Welfare estimates — scenario comparison ──────────────────────────────
  # Only for outcomes with meaningful dollar welfare (spread outcomes)
  if ("CumSavings_4Y_BN" %in% names(res_sim)) {

    tbl9b <- res_sim |>
      filter(!is.na(CumSavings_4Y_BN)) |>
      select(Outcome, Scenario, Beta, CumSavings_4Y_BN) |>
      pivot_wider(
        names_from  = Scenario,
        values_from = CumSavings_4Y_BN
      ) |>
      mutate(
        Beta = sprintf("%.3f", Beta),
        across(where(is.numeric), ~ sprintf("$%.1fBN", .x))
      )

    write_csv(tbl9b, file.path(TABLE_OUT, "Table9B_Simulation_Welfare.csv"))

    tbl9b_gt <- tbl9b |>
      gt() |>
      tab_header(
        title    = "Table 9B. Repeal Simulation: Cumulative Member Savings",
        subtitle = paste0(
          "Cumulative spread savings to members over 4 years post-repeal, ",
          "by scenario. Applied to outstanding loan balances of 696 complex CUs."
        )
      ) |>
      cols_label(
        Outcome = "Outcome",
        Beta    = "DiD Estimate (pp)"
      ) |>
      tab_spanner(
        label   = "Cumulative 4-Year Member Savings",
        columns = -c(Outcome, Beta)
      ) |>
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_column_labels(columns = everything())
      ) |>
      tab_footnote(
        paste0(
          "Savings computed as DiD spread estimate × recovery path × outstanding loan balance, ",
          "aggregated across 696 complex CUs weighted by asset tier shares. ",
          "Fast scenario: 4Q half-life. Gradual (central): 8Q half-life. ",
          "Slow: plateau floor at 50% after 16Q. ",
          "Does not include value of restored lending volume or profitability recovery."
        )
      ) |>
      opt_table_font(font = "Times New Roman", size = FONT_SIZE)

    gtsave(tbl9b_gt, file.path(TABLE_OUT, "Table9B_Simulation_Welfare.html"))
    write_csv(tbl9b, file.path(TABLE_OUT, "Table9B_Simulation_Welfare.csv"))
    message("  Table 9B saved.")

  } else {
    message("  CumSavings_4Y_BN column not found — Table 9B skipped.")
    message("  Check that 3D_Repeal_Simulation.R wrote this column to 3D_simulation_summary.csv")
  }

} else {
  message("  3D_simulation_summary.csv not found — Tables 9A/9B skipped.")
}


# =============================================================================
# 12C. COPY REPEAL SIMULATION POLICY CHARTS (3D figures → paper/figures/)
# =============================================================================

message("── Step 12C: Copying 3D repeal simulation figures ───────────────────")

sim_figure_map <- list(
  list("policy_3d1_spread_fan_chart.png",       "Figure9_1_Spread_Fan_Chart.png"),
  list("policy_3d2_recovery_dashboard.png",     "Figure9_2_Recovery_Dashboard.png"),
  list("policy_3d3_cumulative_member_savings.png",
       "Figure9_3_Cumulative_Member_Savings.png"),
  list("policy_3d4_roa_recovery_by_tier.png",   "Figure9_4_ROA_Recovery_by_Tier.png"),
  list("policy_3d5_portfolio_rebalancing.png",  "Figure9_5_Portfolio_Rebalancing.png"),
  list("policy_3d6_horizon_comparison.png",     "Figure9_6_Horizon_Comparison.png"),
  list("policy_3d7_loan_volume_restored.png",   "Figure9_7_Loan_Volume_Restored.png"),
  list("policy_3d8_cost_of_delay.png",          "Figure9_8_Cost_of_Delay.png")
)

for (fm in sim_figure_map) {
  src  <- file.path(FIGURE_IN, fm[[1]])
  dest <- file.path(FIGURE_OUT, fm[[2]])
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = TRUE)
    message(sprintf("  Copied: %s → %s", fm[[1]], fm[[2]]))
  } else {
    message(sprintf("  Not found: %s", fm[[1]]))
  }
}


# =============================================================================
# 12D. COPY STRESS SCENARIO POLICY CHARTS (3E figures → paper/figures/)
# =============================================================================
# 3E_Stress_Scenario_Simulation.R writes policy_3e*.png to output/figures/.
# This block renames them to Figure10_* for the paper.
# Figure10_* = Section 10B (Stress Scenario) — follows Figure9_* (Section 10A repeal)
# Run 3E_Stress_Scenario_Simulation.R before running 4_Paper_Tables.R.

# Pre-flight check: warn if policy source figures are missing
# These must exist in output/figures/ before 4_Paper_Tables.R can copy them.
policy_sources <- c(
  paste0("policy_3e", 1:8, c("_net_effect_capital","_net_effect_lending",
    "_net_effect_roa","_net_welfare_spreads","_stress_multiplier_sensitivity",
    "_scenario_comparison","_dq_stress_overlay","_repeal_timing_under_stress"), ".png"),
  paste0("policy_3f", 1:8, c("_nw_distribution_regimes","_threshold_breach_rates",
    "_stress_drawdown_calibration","_survival_frontier","_vulnerability_heatmap",
    "_rule_effect_vs_tailrisk","_thin_buffer_zoom","_optimal_threshold"), ".png")
)
missing_sources <- policy_sources[!file.exists(file.path(FIGURE_IN, policy_sources))]
if (length(missing_sources) > 0) {
  message(sprintf("  NOTE: %d policy source figures not yet in output/figures/",
                  length(missing_sources)))
  message("  Run 3E and 3F scripts first, or source('copy_figures.R') after running them.")
  message("  Briefing will show placeholders for missing figures.")
} else {
  message("  All 16 policy source figures present in output/figures/")
}

message("── Step 12D: Copying 3E stress scenario figures ──────────────────────")

stress_figure_map <- list(
  list("policy_3e1_net_effect_capital.png",
       "Figure10_1_Stress_Capital.png"),
  list("policy_3e2_net_effect_lending.png",
       "Figure10_2_Stress_Lending.png"),
  list("policy_3e3_net_effect_roa.png",
       "Figure10_3_Stress_ROA_by_Tier.png"),
  list("policy_3e4_net_welfare_spreads.png",
       "Figure10_4_Stress_Net_Welfare.png"),
  list("policy_3e5_stress_multiplier_sensitivity.png",
       "Figure10_5_Stress_Multiplier_Sensitivity.png"),
  list("policy_3e6_scenario_comparison.png",
       "Figure10_6_Stress_Scenario_Comparison.png"),
  list("policy_3e7_dq_stress_overlay.png",
       "Figure10_7_Stress_DQ_Overlay.png"),
  list("policy_3e8_repeal_timing_under_stress.png",
       "Figure10_8_Stress_Repeal_Timing.png")
)

n_3e_copied <- 0L
for (fm in stress_figure_map) {
  src  <- file.path(FIGURE_IN, fm[[1]])
  dest <- file.path(FIGURE_OUT, fm[[2]])
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = TRUE)
    message(sprintf("  Copied: %s → %s", fm[[1]], fm[[2]]))
    n_3e_copied <- n_3e_copied + 1L
  } else {
    message(sprintf("  Not found (run 3E first): %s", fm[[1]]))
  }
}
message(sprintf("  3E figures: %d / %d copied.", n_3e_copied, length(stress_figure_map)))


# =============================================================================
# 12E. COPY CAPITAL STRESS TEST CHARTS (3F figures → paper/figures/)
# =============================================================================
# 3F_Capital_Adequacy_Stress_Test.R writes policy_3f*.png to output/figures/.
# This block renames them to Figure11_* for the paper.
# Figure11_* = Section 11B (Capital Adequacy Stress Test / RQ5)
# Run 3F_Capital_Adequacy_Stress_Test.R before running 4_Paper_Tables.R.

message("-- Step 12E: Copying 3F capital stress test figures")

stress_test_figure_map <- list(
  list("policy_3f1_nw_distribution_regimes.png",   "Figure11_1_NW_Distribution_Regimes.png"),
  list("policy_3f2_threshold_breach_rates.png",     "Figure11_2_Threshold_Breach_Rates.png"),
  list("policy_3f3_stress_drawdown_calibration.png","Figure11_3_Stress_Drawdown_Calibration.png"),
  list("policy_3f4_survival_frontier.png",          "Figure11_4_Survival_Frontier.png"),
  list("policy_3f5_vulnerability_heatmap.png",      "Figure11_5_Vulnerability_Heatmap.png"),
  list("policy_3f6_rule_effect_vs_tailrisk.png",    "Figure11_6_Rule_Effect_vs_Tailrisk.png"),
  list("policy_3f7_thin_buffer_zoom.png",           "Figure11_7_Thin_Buffer_Zoom.png"),
  list("policy_3f8_optimal_threshold.png",          "Figure11_8_Optimal_Threshold.png")
)

n_3f_copied <- 0L
for (fm in stress_test_figure_map) {
  src  <- file.path(FIGURE_IN, fm[[1]])
  dest <- file.path(FIGURE_OUT, fm[[2]])
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = TRUE)
    message(sprintf("  Copied: %s -> %s", fm[[1]], fm[[2]]))
    n_3f_copied <- n_3f_copied + 1L
  } else {
    message(sprintf("  Not found (run 3F first): %s", fm[[1]]))
  }
}
message(sprintf("  3F figures: %d / %d copied.", n_3f_copied,
                length(stress_test_figure_map)))

message("── Step 12: Figure 1 — NW ratio distribution ────────────────────────")

df_complex_pre <- df |>
  filter(complex == 1, post_rbc == 0)

fig1 <- ggplot(df_complex_pre, aes(x = networth_ratio)) +
  geom_histogram(
    aes(fill = networth_ratio >= 10),
    binwidth = 0.25, color = "white", linewidth = 0.2
  ) +
  geom_vline(xintercept = 10, color = COL_THRESHOLD,
             linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = 9, color = "gray55",
             linetype = "dotted", linewidth = 0.9) +
  annotate("text", x = 10.2, y = Inf, vjust = 1.5,
           label = "10% RBC threshold", hjust = 0,
           size = 3.5, color = COL_THRESHOLD, fontface = "bold") +
  annotate("text", x = 9.2, y = Inf, vjust = 3.5,
           label = "9% CCULR threshold", hjust = 0,
           size = 3.2, color = "gray45") +
  scale_fill_manual(
    values = c("TRUE" = COL_COMPLEX, "FALSE" = COL_NONCOMPLEX),
    labels = c("TRUE" = "≥ 10% (well-capitalized)",
               "FALSE" = "< 10% (below threshold)")
  ) +
  scale_x_continuous(
    limits = c(5, 25),
    labels = function(x) paste0(x, "%")
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title   = "Figure 1. Net Worth Ratio Distribution — Complex Credit Unions",
    subtitle = paste0(
      "Pre-RBC period (2018 Q1 – 2021 Q4). Each bar = 0.25 pp. ",
      "Modal bin peaks at the 10% RBC well-capitalized threshold."
    ),
    x       = "Net Worth Ratio (%)",
    y       = "CU-Quarter Observations",
    fill    = NULL,
    caption = paste0(
      "Complex CUs = avg assets > $500M in 2021. N = ",
      scales::comma(nrow(df_complex_pre)),
      " CU-quarters. Source: NCUA Call Report (5300)."
    )
  ) +
  theme_paper()

ggsave(file.path(FIGURE_OUT, "Figure1_NW_Distribution.png"),
       fig1, width = 10, height = 6, dpi = 300, bg = "white")
message("  Figure 1 saved.")


# =============================================================================
# 14. FIGURE 2 — PRE-TREND CHECK (Parallel trends validation)
# =============================================================================

message("── Step 13: Figure 2 — Pre-trend check ──────────────────────────────")

pretrend_vars <- c("networth_ratio", "cap_buffer",
                   "loan_growth", "dq_rate_var",
                   "roa_var", "mbl_shr")

pretrend_labels <- c("Net Worth Ratio (%)", "Capital Buffer (pp)",
                     "Loan Growth (log)", "Delinquency Rate (%)",
                     "ROA (%)", "MBL Share (%)")

pretrend_data <- df |>
  filter(event_time >= -16L, event_time <= 4L) |>
  mutate(
    group_label = if_else(complex == 1,
                          "Complex (treated)",
                          "Non-complex (control)")
  ) |>
  group_by(event_time, group_label) |>
  summarise(across(all_of(pretrend_vars),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_longer(
    cols      = all_of(pretrend_vars),
    names_to  = "variable",
    values_to = "mean_val"
  ) |>
  mutate(
    var_label = pretrend_labels[match(variable, pretrend_vars)]
  )

fig2 <- ggplot(
  pretrend_data,
  aes(x = event_time, y = mean_val, color = group_label)
) +
  geom_vline(xintercept = -0.5, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ var_label, scales = "free_y", ncol = 2) +
  scale_color_manual(
    values = c("Complex (treated)"    = COL_COMPLEX,
               "Non-complex (control)" = COL_NONCOMPLEX)
  ) +
  scale_x_continuous(
    breaks = seq(-16, 4, by = 4),
    labels = function(x) paste0("Q", x)
  ) +
  labs(
    title    = "Figure 2. Pre-Trend Check: Raw Group Means by Event Time",
    subtitle = paste0(
      "Quarterly group means relative to RBC effective date (Q0 = 2022 Q1). ",
      "Flat pre-period trends support the parallel trends assumption."
    ),
    x       = "Quarters Relative to RBC",
    y       = "Group Mean",
    caption = "Source: NCUA Call Report (5300)."
  ) +
  theme_paper() +
  theme(legend.position = "bottom")

ggsave(file.path(FIGURE_OUT, "Figure2_PreTrend_Check.png"),
       fig2, width = 12, height = 10, dpi = 300, bg = "white")
message("  Figure 2 saved.")


# =============================================================================
# 15. FIGURES 3–7 — COPY BEST FIGURES FROM EARLIER SCRIPTS
# =============================================================================

message("── Step 14: Copying and renaming key figures ─────────────────────────")

figure_map <- list(
  # Source file                              → Output name
  list("fig_es_panel_capital.png",            "Figure3_EventStudy_Capital.png"),
  list("fig_es_panel_spreads.png",            "Figure4_EventStudy_Spreads.png"),
  list("fig_es_loan_growth.png",             "Figure3b_EventStudy_LoanGrowth.png"),   # F3
  list("fig_es_delinquency.png",             "Figure3c_EventStudy_Delinquency.png"),  # F5
  list("fig_es_roa.png",                     "Figure3d_EventStudy_ROA.png"),          # F6
  list("fig_loan_portfolio_trends.png",       "Figure5_Portfolio_Trends.png"),
  list("3B_panel_A_capital_lending.png",      "Figure6_Crisis_vs_RBC_Capital.png"),
  list("3B_panel_B_portfolio_profitability.png",
       "Figure6b_Crisis_vs_RBC_Portfolio.png"),
  list("3C_main_panel_3group.png",            "Figure7_CCULR_EventStudy.png"),
  list("fig_heterogeneity_tier.png",          "FigureA1_Heterogeneity.png"),
  list("fig_bunching_500m.png",               "FigureA2_Bunching.png"),
  list("3A_forest_plot_crisis_vs_rbc.png",    "FigureA3_Crisis_Forest.png"),
  # Finding 5 mechanism tests (3H) — confirm why DQ increased
  list("policy_3h1_dq_mechanism_decomp.png",  "Figure_F5a_DQ_Mechanism_Tests.png"),
  list("policy_3h2_dollar_dq_test.png",       "Figure_F5b_Dollar_vs_Rate_DQ.png"),
  list("policy_3h4_capacity_channel.png",     "Figure_F5c_Capacity_Channel.png"),
  # Finding 5 supplementary: disaggregated DQ overview (3G)
  list("policy_3g1_dq_available_categories.png", "FigureA4_DQ_Categories.png"),
  list("policy_3g7_mechanism_summary.png",    "FigureA5_DQ_Causal_Chain.png")
)

for (fm in figure_map) {
  src  <- file.path(FIGURE_IN, fm[[1]])
  dest <- file.path(FIGURE_OUT, fm[[2]])
  if (file.exists(src)) {
    file.copy(src, dest, overwrite = TRUE)
    message(sprintf("  Copied: %s → %s", fm[[1]], fm[[2]]))
  } else {
    message(sprintf("  Not found: %s", fm[[1]]))
  }
}


# =============================================================================
# 16. PAPER SUMMARY — KEY RESULTS FOR ABSTRACT
# =============================================================================

message("── Step 15: Paper summary ────────────────────────────────────────────")

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║           PAPER SUMMARY — KEY QUANTITATIVE FINDINGS             ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║                                                                  ║\n")
cat("║  CAPITAL                                                         ║\n")
cat("║  • NW ratio rose +0.46pp post-RBC (trend-adj., p<0.01)          ║\n")
cat("║  • Effect temporary: V-shape, peaks Q+3, erodes by Q+9          ║\n")
cat("║  • 44.5% of complex CU-qtrs below 10% threshold pre-RBC         ║\n")
cat("║                                                                  ║\n")
cat("║  LENDING                                                         ║\n")
cat("║  • Loan growth: -0.4pp per quarter (p<0.01) — persistent        ║\n")
cat("║  • Credit crunch ~60% the size of 2008 crisis contraction       ║\n")
cat("║  • Near-threshold CUs: -0.6pp loan growth (p<0.01)              ║\n")
cat("║                                                                  ║\n")
cat("║  PORTFOLIO REALLOCATION (Regulatory-specific channel)           ║\n")
cat("║  • Auto share: -3.5pp (p<0.01) — persistent step-change         ║\n")
cat("║  • RE share:   +2.0pp (p<0.01) — lower risk-weight flight        ║\n")
cat("║  • Auto effect 2x larger than 2008 crisis (Wald p<0.01)         ║\n")
cat("║                                                                  ║\n")
cat("║  PROFITABILITY                                                   ║\n")
cat("║  • ROA: -0.33pp (p<0.01), worsening through Q+10                ║\n")
cat("║  • NIM: compressed vs. expanded during 2008 crisis              ║\n")
cat("║  • Sign reversal vs crisis = regulatory equity tax channel       ║\n")
cat("║                                                                  ║\n")
cat("║  CREDIT QUALITY                                                  ║\n")
cat("║  • DQ rate: +0.17pp (p<0.01), persistent post-RBC               ║\n")
cat("║  • Crisis DQ effect 4x larger (+0.63pp) — RBC moderate          ║\n")
cat("║                                                                  ║\n")
cat("║  CCULR RELIEF                                                    ║\n")
cat("║  • No significant lending relief (p=0.197)                      ║\n")
cat("║  • Capital worse for CCULR adopters — selection effect           ║\n")
cat("║  • Pre-trend violation confirms endogenous adoption              ║\n")
cat("║                                                                  ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║  IDENTIFICATION                                                  ║\n")
cat("║  • Placebo (2020q1): β = -0.116, p = 0.283 ✓                    ║\n")
cat("║  • No threshold manipulation (bunching test) ✓                  ║\n")
cat("║  • Balanced panel robust (3,860 CUs) ✓                          ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")


# =============================================================================
# 17. FINAL OUTPUT INVENTORY
# =============================================================================

cat("\n=== OUTPUT INVENTORY ===\n\n")

cat("Tables (output/paper/tables/):\n")
tables_list <- c(
  "Table1_SummaryStats.csv",
  "Table1_SummaryStats.html",
  "Table2_BalanceTable.csv",
  "Table2_BalanceTable.html",
  "Table3_Capital_DiD.csv",
  "Table3_Capital_DiD.html",
  "Table4_LoanSpreads_DiD.csv",
  "Table4_LoanSpreads_DiD.html",
  "Table5_Portfolio_Growth_Credit.csv",
  "Table5_Portfolio_Growth_Credit.html",
  "Table6_Robustness.csv",
  "Table6_Robustness.html",
  "Table7_Crisis_vs_RBC.csv",
  "Table7_Crisis_vs_RBC.html",
  "Table8_CCULR_Relief.csv",
  "Table8_CCULR_Relief.html",
  "Table9A_Simulation_Recovery.csv",
  "Table9A_Simulation_Recovery.html",
  "Table9B_Simulation_Welfare.csv",
  "Table9B_Simulation_Welfare.html",
  "TableA1_Heterogeneity.csv",
  "TableA1_Heterogeneity.html",
  "TableA2_NearThreshold.csv",
  "TableA2_NearThreshold.html"
)
for (t in tables_list) {
  flag <- if (file.exists(file.path(TABLE_OUT, t))) "✓" else "–"
  cat(sprintf("  %s %s\n", flag, t))
}

cat("\nFigures (output/paper/figures/):\n")
figures_list <- c(
  "Figure1_NW_Distribution.png",
  "Figure2_PreTrend_Check.png",
  "Figure3_EventStudy_Capital.png",
  "Figure3b_EventStudy_LoanGrowth.png",
  "Figure3c_EventStudy_Delinquency.png",
  "Figure3d_EventStudy_ROA.png",
  "Figure4_EventStudy_Spreads.png",
  "Figure5_Portfolio_Trends.png",
  "Figure6_Crisis_vs_RBC_Capital.png",
  "Figure6b_Crisis_vs_RBC_Portfolio.png",
  "Figure7_CCULR_EventStudy.png",
  # Finding 5 mechanism tests (3H)
  "Figure_F5a_DQ_Mechanism_Tests.png",
  "Figure_F5b_Dollar_vs_Rate_DQ.png",
  "Figure_F5c_Capacity_Channel.png",
  # Appendix: supplementary DQ analysis (3G)
  "FigureA4_DQ_Categories.png",
  "FigureA5_DQ_Causal_Chain.png",
  # 3D: Section 10A repeal simulation figures
  "Figure9_1_Spread_Fan_Chart.png",
  "Figure9_2_Recovery_Dashboard.png",
  "Figure9_3_Cumulative_Member_Savings.png",
  "Figure9_4_ROA_Recovery_by_Tier.png",
  "Figure9_5_Portfolio_Rebalancing.png",
  "Figure9_6_Horizon_Comparison.png",
  "Figure9_7_Loan_Volume_Restored.png",
  "Figure9_8_Cost_of_Delay.png",
  # 3E: Section 10B stress scenario figures (NEW)
  "Figure10_1_Stress_Capital.png",
  "Figure10_2_Stress_Lending.png",
  "Figure10_3_Stress_ROA_by_Tier.png",
  "Figure10_4_Stress_Net_Welfare.png",
  "Figure10_5_Stress_Multiplier_Sensitivity.png",
  "Figure10_6_Stress_Scenario_Comparison.png",
  "Figure10_7_Stress_DQ_Overlay.png",
  "Figure10_8_Stress_Repeal_Timing.png",
  # 3F: Section 11 — Capital adequacy stress test (NEW)

  # 3F: Section 11B capital stress test figures (NEW)
  "Figure11_1_NW_Distribution_Regimes.png",
  "Figure11_2_Threshold_Breach_Rates.png",
  "Figure11_3_Stress_Drawdown_Calibration.png",
  "Figure11_4_Survival_Frontier.png",
  "Figure11_5_Vulnerability_Heatmap.png",
  "Figure11_6_Rule_Effect_vs_Tailrisk.png",
  "Figure11_7_Thin_Buffer_Zoom.png",
  "Figure11_8_Optimal_Threshold.png",
  # Appendix
  "FigureA1_Heterogeneity.png",
  "FigureA2_Bunching.png",
  "FigureA3_Crisis_Forest.png"
)
for (f in figures_list) {
  flag <- if (file.exists(file.path(FIGURE_OUT, f))) "✓" else "–"
  cat(sprintf("  %s %s\n", flag, f))
}

message("\n── 4_Paper_Tables.R complete ✓ ──────────────────────────────────────")
message("  All outputs in output/paper/")
message("  Tables: CSV + HTML (open in Word via Insert > Object > HTML)")
message("  Figures: PNG at 300dpi, white background, ready for submission")


# =============================================================================
# END OF SCRIPT
# =============================================================================
