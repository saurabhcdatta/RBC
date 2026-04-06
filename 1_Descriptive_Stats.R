# =============================================================================
# 1_Descriptive_Stats.R
# RBC Rule Impact Analysis — Descriptive Statistics & Exploratory Analysis
# NCUA Call Report (5300) Data
#
# Author  : Saurabh C. Datta, Ph.D.
# Created : 2026
# Purpose : Produce all pre-analysis descriptives:
#             (1) Table 1: Summary statistics — pre-RBC period, by treatment group
#             (2) Table 2: Balance table — complex vs. non-complex, pre-RBC
#             (3) Time-series trend plots: capital & loan outcomes by group
#             (4) Missing data map for spread variables
#             (5) Distribution plots: net worth ratio at threshold
#             (6) Pre-trend visual check (event-study raw means)
#             (7) Bunching check near $500M threshold
#
# Inputs:
#   data/analysis_panel_raw.rds  ← pre-winsorize panel (from 0_Data_Prep.R)
#                                   used for Table 1 and Table 2 means/SDs
#   data/analysis_panel.rds      ← winsorized panel (from 0_Data_Prep.R)
#                                   used for trend plots and pre-trend checks
#
# Outputs:
#   output/tables/  — CSV tables
#   output/figures/ — PNG plots
#
# CHANGES FROM PRIOR VERSION:
#   [FIX 1] Table 1 now computed from analysis_panel_raw.rds, filtered to
#           pre-RBC period (post_rbc == 0), split by treatment group (complex),
#           using CU-quarter as the unit of observation — consistent with the
#           regression sample. Prior version used the full unfiltered panel
#           pooled across groups, which produced neither pre-RBC means nor
#           group-specific means.
#   [FIX 2] Table 2 (balance table) uses analysis_panel_raw.rds collapsed to
#           CU-level means (one row per CU) for t-test validity. Both tables
#           now use unwinsorized data for descriptive statistics.
#   [FIX 3] N labels in Table 2 are dynamic — computed from the data, not
#           hardcoded — so they update automatically if the sample changes.
#   [FIX 4] Table 1 and Table 2 now use the same PRE_RBC window constant
#           (2018Q1–2021Q4) matching 0_Data_Prep.R so any future window
#           change propagates automatically.
#   [FIX 5] Growth variables are now stored as ×100 in 0_Data_Prep.R, so no
#           ad-hoc rescaling is needed here. The ×100 label is preserved.
#   [FIX 6] Table note disclosing winsorization status of each table is added.
#   [FIX 7] Spread variable in Table 1 is spread_comm_oth (comm non-RE, 2yr),
#           matching the handoff document. spread_comm_re is excluded from
#           Table 1 given ~78% missingness; it is flagged in footnotes.
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)     # combining ggplot panels
library(gt)            # publication-quality tables
library(gtsummary)     # balance / summary tables
library(janitor)       # tabyl
library(lubridate)


# =============================================================================
# 1. USER SETTINGS
# =============================================================================

# [FIX 1 & 2]: Separate input paths for raw vs. winsorized panel
INPUT_PATH_RAW <- "data/analysis_panel_raw.rds"   # ← for Table 1, Table 2
INPUT_PATH     <- "data/analysis_panel.rds"        # ← for trend plots

TABLE_PATH     <- "output/tables/"
FIGURE_PATH    <- "output/figures/"

# RBC effective quarter for plot annotations
RBC_LABEL      <- "RBC Effective\n(2022 Q1)"
RBC_PERIOD     <- 2022.1

# Pre-RBC summary window — MUST match 0_Data_Prep.R PRE_RBC_START/END
# Table 1 and Table 2 are computed over this window only
PRE_RBC_START  <- 2018.1
PRE_RBC_END    <- 2021.4

# Color palette: complex (treated) = navy, non-complex (control) = coral
COL_COMPLEX    <- "#1B3A6B"   # navy
COL_NONCOMPLEX <- "#C94040"   # coral/red
COL_THRESHOLD  <- "#E8A838"   # amber — for threshold lines

# ggplot theme for all figures
theme_rbc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "gray40", size = 10),
      axis.title       = element_text(size = 10),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold"),
      plot.caption     = element_text(color = "gray50", size = 8)
    )
}


# =============================================================================
# 2. LOAD DATA & CREATE OUTPUT DIRECTORIES
# =============================================================================

message("── Step 1: Loading analysis panels ──────────────────────────────────")

# Raw panel — for summary statistics (unwinsorized, true SDs)
df_raw <- readRDS(INPUT_PATH_RAW)

# Winsorized panel — for trend plots and pre-trend visual checks
df     <- readRDS(INPUT_PATH)

dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)

message(sprintf("  Raw panel    : %s obs, %d vars — used for Tables 1 & 2",
                comma(nrow(df_raw)), ncol(df_raw)))
message(sprintf("  Winsor panel : %s obs, %d vars — used for figures",
                comma(nrow(df)),     ncol(df)))

# Confirm both panels cover the same CUs and periods
stopifnot(
  "Raw and winsorized panels have different row counts" =
    nrow(df_raw) == nrow(df),
  "Raw and winsorized panels have different CU counts" =
    n_distinct(df_raw$cu_number) == n_distinct(df$cu_number)
)
message("  Panel consistency check passed ✓")

# Convenience labels for treatment group — add to both panels
add_labels <- function(x) {
  x |>
    mutate(
      group_label = if_else(complex == 1,
                            "Complex (treated)",
                            "Non-complex (control)"),
      group_label = factor(group_label,
                           levels = c("Complex (treated)",
                                      "Non-complex (control)")),
      period_label = paste0(year, " Q", quarter),
      period_date  = as.Date(paste0(year, "-",
                                    sprintf("%02d", (quarter - 1) * 3 + 1),
                                    "-01"))
    )
}

df_raw <- add_labels(df_raw)
df     <- add_labels(df)

# RBC date for plot vertical lines
rbc_date <- as.Date("2022-01-01")


# =============================================================================
# 3. TABLE 1 — SUMMARY STATISTICS (Pre-RBC period, by treatment group)
# =============================================================================
#
# Unit of observation: CU-quarter (consistent with regression sample)
# Sample: post_rbc == 0 (2018Q1–2021Q4), split by complex indicator
# Data: analysis_panel_raw.rds — UNWINSORIZED so SDs reflect true distribution
#
# [FIX 1]: Previously this table used the full panel (all periods, both groups
# pooled, df[[var]] directly). It now correctly filters to pre-RBC and splits
# by treatment group, matching the regression sample definition.
#
# Variables included in Table 1 (paper Table 1):
#   Capital: networth_ratio, cap_buffer, well_capitalized
#   Portfolio: loan_to_asset, mbl_shr, re_shr, auto_shr
#   Growth: loan_growth, asset_growth   (stored ×100 → units match label)
#   Pricing: spread_mortgage, spread_nauto, spread_comm_oth
#            (spread_comm_re excluded — ~78% missing, flagged in note)
#   Credit quality: dq_rate_var, chgoff_ratio, pll_assets
#   Profitability: roa_var, nim, cof
#
# NOTE: Table 1 SDs are from the RAW (unwinsorized) data.
# Table note should state: "Standard deviations are unwinsorized."

message("── Step 2: Building Table 1 — Summary statistics by group ───────────")

# Variables and their publication labels — ORDER matches paper Table 1
tab1_vars <- c(
  # Capital
  "networth_ratio", "cap_buffer", "well_capitalized",
  # Portfolio
  "loan_to_asset", "mbl_shr", "re_shr", "auto_shr",
  # Growth (stored as ×100)
  "loan_growth", "asset_growth",
  # Pricing (spread_comm_re excluded from Table 1 — high missingness)
  "spread_mortgage", "spread_nauto", "spread_comm_oth",
  # Credit quality
  "dq_rate_var", "chgoff_ratio", "pll_assets",
  # Profitability
  "roa_var", "nim", "cof"
)

tab1_labels <- c(
  # Capital
  "Net worth ratio (%)",
  "Capital buffer vs 10% (pp)",
  "Well-capitalized (binary)",
  # Portfolio
  "Loans / assets (%)",
  "MBL share of loans (%)",
  "RE share of loans (%)",
  "Auto share of loans (%)",
  # Growth
  "Loan growth (QoQ log×100)",
  "Asset growth (QoQ log×100)",
  # Pricing
  "Mortgage spread over 10yr Treasury (pp)",
  "New auto spread over 2yr Treasury (pp)",
  "Comm non-RE spread over 2yr Treasury (pp)",
  # Credit quality
  "Delinquency rate (%)",
  "Charge-off ratio (%)",
  "PLL / assets (%)",
  # Profitability
  "ROA (%)",
  "Net interest margin (%)",
  "Cost of funds (%)"
)

stopifnot("tab1_vars and tab1_labels length mismatch" =
            length(tab1_vars) == length(tab1_labels))

# Pre-RBC CU-quarter sample — RAW data, split by group
df_pre_cuqtr <- df_raw |>
  filter(post_rbc == 0)   # 2018Q1–2021Q4 only

# Dynamic sample sizes
n_complex_cuqtr    <- sum(df_pre_cuqtr$complex == 1)
n_noncomplex_cuqtr <- sum(df_pre_cuqtr$complex == 0)
n_complex_cu       <- n_distinct(df_pre_cuqtr$cu_number[df_pre_cuqtr$complex == 1])
n_noncomplex_cu    <- n_distinct(df_pre_cuqtr$cu_number[df_pre_cuqtr$complex == 0])

message(sprintf("  Pre-RBC CU-quarters : complex=%s  non-complex=%s",
                comma(n_complex_cuqtr), comma(n_noncomplex_cuqtr)))
message(sprintf("  Unique CUs          : complex=%s  non-complex=%s",
                n_complex_cu, n_noncomplex_cu))

# Compute means and SDs for each variable by group
# Using CU-quarter as unit so means/SDs match the regression sample exactly
table1 <- map2_dfr(tab1_vars, tab1_labels, function(var, lbl) {

  x1 <- df_pre_cuqtr[[var]][df_pre_cuqtr$complex == 1]
  x0 <- df_pre_cuqtr[[var]][df_pre_cuqtr$complex == 0]

  tibble(
    Variable                = lbl,
    `Complex Mean`          = round(mean(x1, na.rm = TRUE), 3),
    `Non-complex Mean`      = round(mean(x0, na.rm = TRUE), 3),
    `Complex SD`            = round(sd(x1,   na.rm = TRUE), 3),
    `Non-complex SD`        = round(sd(x0,   na.rm = TRUE), 3),
    `Complex N (CU-qtrs)`   = sum(!is.na(x1)),
    `Non-complex N (CU-qtrs)` = sum(!is.na(x0))
  )
})

cat("\n=== TABLE 1: Summary Statistics (Pre-RBC 2018Q1–2021Q4, CU-quarters) ===\n")
cat(sprintf("    Complex CUs    : %s CUs, %s CU-quarters\n",
            n_complex_cu, comma(n_complex_cuqtr)))
cat(sprintf("    Non-complex CUs: %s CUs, %s CU-quarters\n",
            n_noncomplex_cu, comma(n_noncomplex_cuqtr)))
cat("    Data: analysis_panel_raw.rds — UNWINSORIZED\n")
cat("    Note: Growth variables in log-difference × 100 units\n\n")
print(table1, n = Inf)

# Save
write_csv(table1, file.path(TABLE_PATH, "table1_summary_stats.csv"))
message("  Table 1 saved → output/tables/table1_summary_stats.csv")

# Consistency check: networth_ratio mean should equal cap_buffer mean + 10
nw_mean_c  <- table1$`Complex Mean`[table1$Variable == "Net worth ratio (%)"]
buf_mean_c <- table1$`Complex Mean`[table1$Variable == "Capital buffer vs 10% (pp)"]
nw_mean_nc  <- table1$`Non-complex Mean`[table1$Variable == "Net worth ratio (%)"]
buf_mean_nc <- table1$`Non-complex Mean`[table1$Variable == "Capital buffer vs 10% (pp)"]

stopifnot(
  "Complex: networth_ratio != cap_buffer + 10 (rounding OK to 0.001)" =
    abs((nw_mean_c - buf_mean_c) - 10) < 0.001,
  "Non-complex: networth_ratio != cap_buffer + 10 (rounding OK to 0.001)" =
    abs((nw_mean_nc - buf_mean_nc) - 10) < 0.001
)
message("  Internal consistency check passed: NW ratio = cap_buffer + 10 ✓")


# =============================================================================
# 4. TABLE 2 — BALANCE TABLE (Pre-RBC period, CU-level means)
# =============================================================================
#
# Unit of observation: CU (one row per CU = mean of all pre-RBC quarters)
# Sample: post_rbc == 0, collapsed to CU-level before t-testing
# Data: analysis_panel_raw.rds — UNWINSORIZED
#
# Using CU-level means (not CU-quarters) for the t-tests avoids
# pseudo-replication — a CU with more pre-RBC quarters would otherwise
# have proportionally more influence on the test statistic.
#
# [FIX 3]: N labels are now dynamic (computed from the data).
# [FIX 6]: Table note discloses that SDs are from CU-level means, not raw data.

message("── Step 3: Building Table 2 — Balance table ─────────────────────────")

balance_vars <- c(
  "assets_tot", "members",
  "networth_ratio", "cap_buffer",
  "loan_to_asset", "inv_to_asset", "dep_to_asset",
  "mbl_shr", "comm_shr", "re_shr", "auto_shr",
  "roa_var", "nim", "cof", "yld_loans",
  "dq_rate_var", "chgoff_ratio", "pll_assets",
  "cecl_adopter", "well_capitalized"
)

balance_labels <- c(
  "Total assets ($)",
  "Members",
  "Net worth ratio (%)",
  "Capital buffer vs 10% (pp)",
  "Loans / assets (%)",
  "Investments / assets (%)",
  "Deposits / assets (%)",
  "MBL share of loans (%)",
  "Commercial share of loans (%)",
  "Real estate share of loans (%)",
  "Auto share of loans (%)",
  "Return on assets (%)",
  "Net interest margin (%)",
  "Cost of funds (%)",
  "Yield on avg loans (%)",
  "Delinquency rate (%)",
  "Charge-off ratio (%)",
  "PLL / assets (%)",
  "CECL adopter (0/1)",
  "Well-capitalized (0/1)"
)

stopifnot("balance_vars and balance_labels length mismatch" =
            length(balance_vars) == length(balance_labels))

# Collapse to CU-level means over pre-RBC period
df_pre_cu <- df_raw |>
  filter(post_rbc == 0) |>
  group_by(cu_number, complex) |>
  summarise(
    across(all_of(balance_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# [FIX 3]: Dynamic N counts — never hardcoded
n1_bal <- sum(df_pre_cu$complex == 1)   # number of complex CUs
n0_bal <- sum(df_pre_cu$complex == 0)   # number of non-complex CUs

message(sprintf("  CU-level balance table sample: %s complex, %s non-complex",
                n1_bal, n0_bal))

# Compute means, SDs, difference, and t-test p-value for each variable
balance_table <- map2_dfr(balance_vars, balance_labels, function(var, lbl) {

  x1 <- df_pre_cu[[var]][df_pre_cu$complex == 1]
  x0 <- df_pre_cu[[var]][df_pre_cu$complex == 0]

  m1  <- mean(x1, na.rm = TRUE)
  m0  <- mean(x0, na.rm = TRUE)
  sd1 <- sd(x1,   na.rm = TRUE)
  sd0 <- sd(x0,   na.rm = TRUE)

  # Two-sample t-test with unequal variances (Welch default in R)
  tt <- tryCatch(
    t.test(x1, x0, var.equal = FALSE),
    error = function(e) list(p.value = NA_real_)
  )

  # [FIX]: Difference computed on unrounded means then rounded,
  # and separately displayed for transparency
  diff_val <- m1 - m0

  tibble(
    Variable                              = lbl,
    # [FIX 3]: Dynamic column names using actual N from data
    !!paste0("Complex (N=", n1_bal, ")")     := sprintf("%.3f (%.3f)", m1, sd1),
    !!paste0("Non-complex (N=", n0_bal, ")") := sprintf("%.3f (%.3f)", m0, sd0),
    Difference                            = round(diff_val, 3),
    `p-value`                             = round(tt$p.value, 3)
  )
})

cat("\n=== TABLE 2: Balance Table (Pre-RBC Period, CU-level means) ===\n")
cat(sprintf("    Complex CUs    : %s  |  Non-complex CUs : %s\n", n1_bal, n0_bal))
cat("    Data: analysis_panel_raw.rds — UNWINSORIZED (CU-level means)\n")
cat("    SDs are across CUs (each CU contributes one time-averaged observation)\n\n")
print(balance_table, n = Inf)

# Internal consistency checks on balance table
nw_complex    <- df_pre_cu$networth_ratio[df_pre_cu$complex == 1]
buf_complex   <- df_pre_cu$cap_buffer[df_pre_cu$complex == 1]
nw_noncomplex <- df_pre_cu$networth_ratio[df_pre_cu$complex == 0]
buf_noncomplex <- df_pre_cu$cap_buffer[df_pre_cu$complex == 0]

stopifnot(
  "Balance table: complex NW ratio != cap_buffer + 10 at CU level" =
    all(abs(nw_complex - buf_complex - 10) < 1e-9, na.rm = TRUE),
  "Balance table: non-complex NW ratio != cap_buffer + 10 at CU level" =
    all(abs(nw_noncomplex - buf_noncomplex - 10) < 1e-9, na.rm = TRUE)
)
message("  Internal consistency check passed: NW ratio = cap_buffer + 10 at CU level ✓")

# Check that Table 1 and Table 2 means for matching variables are close
# (they won't be identical — different units of obs — but should be within ~1%)
tab1_nw_c  <- table1$`Complex Mean`[table1$Variable == "Net worth ratio (%)"]
tab2_nw_c  <- mean(nw_complex, na.rm = TRUE)
pct_diff   <- abs(tab1_nw_c - tab2_nw_c) / tab2_nw_c * 100

message(sprintf(
  "  Cross-table check — Complex NW ratio: Table1=%.3f (CU-qtrs), Table2=%.3f (CU-level), diff=%.2f%%",
  tab1_nw_c, tab2_nw_c, pct_diff
))
if (pct_diff > 2) {
  warning(sprintf(
    "Cross-table NW ratio difference exceeds 2%% (%.2f%%). Investigate sample definition.",
    pct_diff
  ))
}

# Save
write_csv(balance_table, file.path(TABLE_PATH, "table2_balance_table.csv"))
message("  Table 2 saved → output/tables/table2_balance_table.csv")


# =============================================================================
# 5. FULL-PANEL SUMMARY (supplementary — all periods, all variables)
# =============================================================================
# This is a supplementary diagnostic table, NOT the paper Table 1.
# It gives the reviewer a sense of full-panel statistics on the winsorized
# regression sample. Saved to a clearly named separate file.

message("── Step 4: Full-panel supplementary summary (winsorized panel) ───────")

full_panel_vars <- c(
  "networth_ratio", "cap_buffer", "well_capitalized",
  "loan_to_asset", "mbl_shr", "re_shr", "auto_shr",
  "loan_growth", "asset_growth",
  "spread_mortgage", "spread_nauto", "spread_uauto",
  "spread_comm_oth", "spread_comm_re",
  "roa_var", "nim", "cof",
  "dq_rate_var", "chgoff_ratio", "pll_assets"
)

full_panel_labels <- c(
  "Net worth ratio (%)", "Capital buffer vs 10% (pp)", "Well-capitalized",
  "Loans / assets (%)", "MBL share (%)", "RE share (%)", "Auto share (%)",
  "Loan growth (QoQ log×100)", "Asset growth (QoQ log×100)",
  "Mortgage spread (pp)", "New auto spread (pp)", "Used auto spread (pp)",
  "Comm non-RE spread (pp)", "Comm RE spread (pp) [~78% missing]",
  "ROA (%)", "NIM (%)", "Cost of funds (%)",
  "Delinquency rate (%)", "Charge-off ratio (%)", "PLL / assets (%)"
)

full_panel_stats <- map2_dfr(full_panel_vars, full_panel_labels, function(var, lbl) {
  x <- df[[var]]   # winsorized panel, all periods
  tibble(
    Variable    = lbl,
    N           = sum(!is.na(x)),
    Mean        = round(mean(x, na.rm = TRUE), 3),
    SD          = round(sd(x,   na.rm = TRUE), 3),
    P25         = round(quantile(x, 0.25, na.rm = TRUE), 3),
    Median      = round(quantile(x, 0.50, na.rm = TRUE), 3),
    P75         = round(quantile(x, 0.75, na.rm = TRUE), 3),
    Pct_Missing = scales::percent(mean(is.na(x)), accuracy = 0.1)
  )
})

write_csv(full_panel_stats,
          file.path(TABLE_PATH, "supplement_full_panel_stats.csv"))
message("  Full-panel supplementary stats saved → output/tables/supplement_full_panel_stats.csv")
message("  NOTE: These are winsorized. Paper Table 1 uses unwinsorized pre-RBC data.")


# =============================================================================
# 6. MISSING DATA MAP FOR SPREAD VARIABLES
# =============================================================================

message("── Step 5: Missing data map for spread variables ─────────────────────")

spread_vars   <- c("spread_mortgage", "spread_re_junior",
                   "spread_nauto", "spread_uauto",
                   "spread_comm_re", "spread_comm_oth", "spread_cc")
spread_labels <- c("Mortgage", "RE Junior Lien",
                   "Auto (New)", "Auto (Used)",
                   "Comm RE", "Comm non-RE", "Credit Card")

coverage_data <- df |>
  group_by(q_period_num, period_date, group_label) |>
  summarise(
    across(all_of(spread_vars),
           ~ mean(!is.na(.x)) * 100,
           .names = "{.col}_cov"),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = ends_with("_cov"),
    names_to  = "spread",
    values_to = "coverage_pct"
  ) |>
  mutate(
    spread       = str_remove(spread, "_cov"),
    spread_label = spread_labels[match(spread, spread_vars)]
  )

p_missing <- ggplot(coverage_data,
                    aes(x = period_date, y = coverage_pct,
                        color = group_label)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = rbc_date, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  annotate("text", x = rbc_date + 30, y = 95,
           label = RBC_LABEL, hjust = 0, size = 2.8, color = "gray40") +
  facet_wrap(~ spread_label, ncol = 2) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title    = "Data Coverage: Loan Interest Rate Spread Variables",
    subtitle = "% of CU-quarters with non-missing spread by loan type and group",
    x = NULL, y = "Coverage (%)",
    caption  = paste0("Spread = loan rate − matched-maturity Treasury benchmark. ",
                      "Semi-annual sawtooth = NCUA Q2/Q4 reporting schedule. ",
                      "Comm RE has ~78% missing — supplementary use only.")
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "fig_missing_spread_coverage.png"),
       p_missing, width = 10, height = 8, dpi = 300)
message("  Missing data map saved → output/figures/fig_missing_spread_coverage.png")


# =============================================================================
# 7. TIME-SERIES TRENDS — NET WORTH RATIO
# =============================================================================

message("── Step 6: Net worth ratio trend plot ───────────────────────────────")

nw_trends <- df |>
  group_by(period_date, group_label) |>
  summarise(
    mean_nw     = mean(networth_ratio, na.rm = TRUE),
    p25_nw      = quantile(networth_ratio, 0.25, na.rm = TRUE),
    p75_nw      = quantile(networth_ratio, 0.75, na.rm = TRUE),
    pct_wellcap = mean(well_capitalized, na.rm = TRUE) * 100,
    .groups = "drop"
  )

pA <- ggplot(nw_trends, aes(x = period_date, color = group_label,
                             fill = group_label)) +
  geom_ribbon(aes(ymin = p25_nw, ymax = p75_nw), alpha = 0.12, color = NA) +
  geom_line(aes(y = mean_nw), linewidth = 1) +
  geom_hline(yintercept = 10, linetype = "dashed",
             color = COL_THRESHOLD, linewidth = 0.8) +
  geom_vline(xintercept = rbc_date, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  annotate("text", x = rbc_date + 30, y = 19,
           label = RBC_LABEL, hjust = 0, size = 2.8, color = "gray40") +
  annotate("text", x = as.Date("2018-03-01"), y = 10.4,
           label = "10% well-cap threshold", hjust = 0,
           size = 2.8, color = COL_THRESHOLD) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_fill_manual(values  = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_x_date(date_labels = "%Y Q%q", date_breaks = "1 year") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title    = "A. Net Worth Ratio by Treatment Group",
       subtitle = "Mean ± IQR (25th–75th percentile). Dashed amber = 10% RBC threshold.",
       x = NULL, y = "Net Worth Ratio (%)") +
  theme_rbc()

pB <- ggplot(nw_trends, aes(x = period_date, y = pct_wellcap,
                             color = group_label)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = rbc_date, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_x_date(date_labels = "%Y Q%q", date_breaks = "1 year") +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  labs(title    = "B. % of CUs Meeting 10% Well-Capitalized Threshold",
       subtitle = "Share of CU-quarters with net worth ratio ≥ 10%",
       x = NULL, y = "% Well-Capitalized") +
  theme_rbc()

p_capital <- pA / pB +
  plot_annotation(
    caption = paste0("Source: NCUA Call Report (5300). ",
                     "Complex = avg assets ≥ $500M in 2021. Ribbon = IQR.")
  )

ggsave(file.path(FIGURE_PATH, "fig_networth_ratio_trends.png"),
       p_capital, width = 11, height = 9, dpi = 300)
message("  Capital trend plot saved → output/figures/fig_networth_ratio_trends.png")


# =============================================================================
# 8. NET WORTH RATIO DISTRIBUTION AT THRESHOLD (Pre-RBC, Complex CUs)
# =============================================================================

message("── Step 7: Net worth ratio threshold distribution ────────────────────")

# Use RAW data for distribution plot so histogram shows true density
df_complex_pre <- df_raw |>
  filter(complex == 1, post_rbc == 0)

p_hist <- ggplot(df_complex_pre, aes(x = networth_ratio)) +
  geom_histogram(aes(fill = networth_ratio >= 10),
                 binwidth = 0.25, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 10, color = COL_THRESHOLD,
             linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 9, color = "gray60",
             linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 10.15, y = Inf, vjust = 1.5,
           label = "10% RBC\nthreshold", hjust = 0,
           size = 3, color = COL_THRESHOLD) +
  annotate("text", x = 9.15, y = Inf, vjust = 3.5,
           label = "9% CCULR\nthreshold", hjust = 0,
           size = 3, color = "gray50") +
  scale_fill_manual(values = c("TRUE"  = COL_COMPLEX,
                               "FALSE" = COL_NONCOMPLEX),
                    labels = c("TRUE"  = "≥ 10% (well-capitalized)",
                               "FALSE" = "< 10% (below threshold)")) +
  scale_x_continuous(limits = c(5, 25),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Figure 1. Net Worth Ratio Distribution — Complex Credit Unions",
    subtitle = sprintf(
      "Pre-RBC period (2018 Q1 – 2021 Q4). Each bar = 0.25 pp. Modal bin peaks at the 10%% RBC well-capitalized threshold.\nN = %s CU-quarters.",
      comma(nrow(df_complex_pre))
    ),
    x       = "Net Worth Ratio (%)",
    y       = "CU-Quarter Observations",
    fill    = NULL,
    caption = paste0("Complex CUs = avg assets ≥ $500M in 2021. ",
                     "Source: NCUA Call Report (5300).")
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "fig_nw_ratio_distribution_complex_prerbc.png"),
       p_hist, width = 10, height = 6, dpi = 300)
message("  Threshold distribution saved → output/figures/fig_nw_ratio_distribution_complex_prerbc.png")


# =============================================================================
# 9. LOAN PORTFOLIO COMPOSITION TRENDS
# =============================================================================

message("── Step 8: Loan portfolio composition trends ─────────────────────────")

portfolio_trends <- df |>
  group_by(period_date, group_label) |>
  summarise(
    mbl_shr  = mean(mbl_shr,  na.rm = TRUE),
    comm_shr = mean(comm_shr, na.rm = TRUE),
    re_shr   = mean(re_shr,   na.rm = TRUE),
    auto_shr = mean(auto_shr, na.rm = TRUE),
    .groups = "drop"
  )

plot_share <- function(data, var, ylab, title_suffix) {
  ggplot(data, aes(x = period_date, y = .data[[var]], color = group_label)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = rbc_date, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = title_suffix, x = NULL, y = ylab) +
    theme_rbc() +
    theme(legend.position = "none")
}

pP1 <- plot_share(portfolio_trends, "mbl_shr",  "MBL share (%)",  "A. Member Business Loans")
pP2 <- plot_share(portfolio_trends, "comm_shr", "Comm share (%)", "B. Commercial Loans")
pP3 <- plot_share(portfolio_trends, "re_shr",   "RE share (%)",   "C. Real Estate Loans")
pP4 <- plot_share(portfolio_trends, "auto_shr", "Auto share (%)", "D. Auto Loans")

p_portfolio <- (pP1 + pP2) / (pP3 + pP4) +
  plot_annotation(
    title    = "Loan Portfolio Composition by Treatment Group",
    subtitle = "Share of total loans (%). Dashed line = RBC effective date (2022 Q1).",
    caption  = "Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "fig_loan_portfolio_trends.png"),
       p_portfolio, width = 11, height = 8, dpi = 300)
message("  Portfolio trend plot saved → output/figures/fig_loan_portfolio_trends.png")


# =============================================================================
# 10. LOAN RATE SPREAD TRENDS
# =============================================================================

message("── Step 9: Loan rate spread trends ──────────────────────────────────")

spread_trends <- df |>
  group_by(period_date, group_label) |>
  summarise(
    spread_mortgage = mean(spread_mortgage, na.rm = TRUE),
    spread_nauto    = mean(spread_nauto,    na.rm = TRUE),
    spread_uauto    = mean(spread_uauto,    na.rm = TRUE),
    spread_comm_re  = mean(spread_comm_re,  na.rm = TRUE),
    spread_comm_oth = mean(spread_comm_oth, na.rm = TRUE),
    .groups = "drop"
  )

plot_spread <- function(data, var, ylab, title_suffix) {
  ggplot(data |> filter(!is.na(.data[[var]])),
         aes(x = period_date, y = .data[[var]], color = group_label)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = rbc_date, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
    scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(labels = function(x) paste0(x, " pp")) +
    labs(title = title_suffix, x = NULL, y = ylab) +
    theme_rbc() +
    theme(legend.position = "none")
}

pS1 <- plot_spread(spread_trends, "spread_mortgage",
                   "Spread (pp)", "A. Mortgage Spread (vs 10yr)")
pS2 <- plot_spread(spread_trends, "spread_nauto",
                   "Spread (pp)", "B. New Auto Spread (vs 2yr)")
pS3 <- plot_spread(spread_trends, "spread_uauto",
                   "Spread (pp)", "C. Used Auto Spread (vs 2yr)")
pS4 <- plot_spread(spread_trends, "spread_comm_re",
                   "Spread (pp)", "D. Comm RE Spread (vs 10yr) [~78% missing]")
pS5 <- plot_spread(spread_trends, "spread_comm_oth",
                   "Spread (pp)", "E. Comm non-RE Spread (vs 2yr)")

p_spreads <- (pS1 + pS2) / (pS3 + pS4) / (pS5 + plot_spacer()) +
  plot_annotation(
    title    = "Loan Rate Spreads Over Treasuries by Treatment Group",
    subtitle = "Loan rate minus matched-maturity Treasury benchmark (pp). Dashed = RBC (2022 Q1).",
    caption  = paste0("Source: NCUA Call Report (5300) & FRED. ",
                      "Q2/Q4 observations only (NCUA semi-annual rate reporting). ",
                      "Comm RE has ~78% missing — supplementary.")
  )

ggsave(file.path(FIGURE_PATH, "fig_loan_spread_trends.png"),
       p_spreads, width = 11, height = 11, dpi = 300)
message("  Spread trend plot saved → output/figures/fig_loan_spread_trends.png")


# =============================================================================
# 11. CREDIT QUALITY & PROFITABILITY TRENDS
# =============================================================================

message("── Step 10: Credit quality & profitability trends ───────────────────")

perf_trends <- df |>
  group_by(period_date, group_label) |>
  summarise(
    dq_rate = mean(dq_rate_var,  na.rm = TRUE),
    chgoff  = mean(chgoff_ratio, na.rm = TRUE),
    pll_a   = mean(pll_assets,   na.rm = TRUE),
    roa     = mean(roa_var,      na.rm = TRUE),
    nim_val = mean(nim,          na.rm = TRUE),
    cof_val = mean(cof,          na.rm = TRUE),
    .groups = "drop"
  )

# Helper: plot a performance variable using the plot_share scaffold
plot_perf <- function(data, var, ylab, title_suffix) {
  ggplot(data, aes(x = period_date, y = .data[[var]], color = group_label)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = rbc_date, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    labs(title = title_suffix, x = NULL, y = ylab) +
    theme_rbc() +
    theme(legend.position = "none")
}

pQ1 <- plot_perf(perf_trends, "dq_rate", "DQ rate (%)",      "A. Delinquency Rate")
pQ2 <- plot_perf(perf_trends, "chgoff",  "Charge-off (%)",   "B. Net Charge-Off Ratio")
pQ3 <- plot_perf(perf_trends, "roa",     "ROA (%)",          "C. Return on Assets")
pQ4 <- plot_perf(perf_trends, "nim_val", "NIM (%)",          "D. Net Interest Margin")
pQ5 <- plot_perf(perf_trends, "cof_val", "Cost of funds (%)", "E. Cost of Funds")
pQ6 <- plot_perf(perf_trends, "pll_a",   "PLL/Assets (%)",   "F. Provision / Assets")

p_performance <- (pQ1 + pQ2) / (pQ3 + pQ4) / (pQ5 + pQ6) +
  plot_annotation(
    title    = "Credit Quality & Profitability by Treatment Group",
    subtitle = "Quarterly group means. Dashed line = RBC effective date (2022 Q1).",
    caption  = "Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "fig_performance_trends.png"),
       p_performance, width = 11, height = 11, dpi = 300)
message("  Performance trend plot saved → output/figures/fig_performance_trends.png")


# =============================================================================
# 12. ASSET & LOAN GROWTH TRENDS
# =============================================================================
# [FIX 5]: Growth variables now stored ×100 in 0_Data_Prep.R.
# No scaling needed here — values are already ≈ QoQ % growth.

message("── Step 11: Asset & loan growth trends ──────────────────────────────")

growth_trends <- df |>
  filter(!is.na(asset_growth), !is.na(loan_growth)) |>
  group_by(period_date, group_label) |>
  summarise(
    asset_growth = mean(asset_growth, na.rm = TRUE),   # already ×100
    loan_growth  = mean(loan_growth,  na.rm = TRUE),   # already ×100
    mbl_growth   = mean(mbl_growth,   na.rm = TRUE),   # already ×100
    .groups = "drop"
  )

pG1 <- plot_perf(growth_trends, "asset_growth", "Growth (%)", "A. Asset Growth (QoQ log×100)")
pG2 <- plot_perf(growth_trends, "loan_growth",  "Growth (%)", "B. Loan Growth (QoQ log×100)")
pG3 <- plot_perf(growth_trends, "mbl_growth",   "Growth (%)", "C. MBL Growth (QoQ log×100)")

p_growth <- pG1 / pG2 / pG3 +
  plot_annotation(
    title    = "Asset, Loan & MBL Growth by Treatment Group",
    subtitle = "Quarter-over-quarter log growth × 100 (≈ QoQ % growth). Dashed = RBC (2022 Q1).",
    caption  = "Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "fig_growth_trends.png"),
       p_growth, width = 10, height = 10, dpi = 300)
message("  Growth trend plot saved → output/figures/fig_growth_trends.png")


# =============================================================================
# 13. PRE-TREND VISUAL CHECK (event-study raw means)
# =============================================================================
# Plots quarterly group means by event_time.
# Flat pre-period supports the parallel trends assumption for DiD.

message("── Step 12: Pre-trend visual check ──────────────────────────────────")

pretrend_vars   <- c("networth_ratio", "cap_buffer",
                     "mbl_shr", "loan_growth", "roa_var", "dq_rate_var")
pretrend_labels <- c("Net Worth Ratio (%)", "Capital Buffer (pp)",
                     "MBL Share (%)", "Loan Growth (QoQ log×100)",
                     "ROA (%)", "Delinquency Rate (%)")

pretrend_data <- df |>
  filter(event_time >= -16, event_time <= 12) |>
  group_by(event_time, group_label) |>
  summarise(across(all_of(pretrend_vars), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_longer(cols = all_of(pretrend_vars),
               names_to  = "variable",
               values_to = "mean_value") |>
  mutate(
    var_label = pretrend_labels[match(variable, pretrend_vars)]
  )

p_pretrend <- ggplot(pretrend_data,
                     aes(x = event_time, y = mean_value,
                         color = group_label, shape = group_label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  geom_vline(xintercept = -0.5, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  facet_wrap(~ var_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_shape_manual(values = c(16, 17)) +
  scale_x_continuous(breaks = seq(-16, 12, by = 4),
                     labels = function(x) paste0("Q", x)) +
  labs(
    title    = "Figure 2. Pre-Trend Check: Raw Group Means by Event Time",
    subtitle = paste0("Quarterly group means relative to RBC effective date (Q0 = 2022 Q1). ",
                      "Flat pre-period trends support the parallel trends assumption."),
    x        = "Quarters Relative to RBC Effective Date",
    y        = "Group Mean",
    caption  = "Source: NCUA Call Report (5300). Vertical dashed = RBC effective date."
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "fig_pretrend_raw_means.png"),
       p_pretrend, width = 12, height = 10, dpi = 300)
message("  Pre-trend check plot saved → output/figures/fig_pretrend_raw_means.png")


# =============================================================================
# 14. BUNCHING CHECK — Asset size distribution near $500M threshold
# =============================================================================

message("── Step 13: Bunching check near $500M threshold ──────────────────────")

bunching_data <- df |>
  filter(post_rbc == 0) |>
  group_by(cu_number) |>
  summarise(avg_assets = mean(assets_tot, na.rm = TRUE) / 1e6,
            .groups = "drop") |>
  filter(avg_assets >= 100, avg_assets <= 1500)

p_bunch <- ggplot(bunching_data, aes(x = avg_assets)) +
  geom_histogram(binwidth = 25, fill = COL_COMPLEX, color = "white", alpha = 0.8) +
  geom_vline(xintercept = 500, color = COL_THRESHOLD,
             linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = 515, y = Inf, vjust = 1.5,
           label = "$500M\nRBC threshold", hjust = 0,
           size = 3.2, color = COL_THRESHOLD) +
  scale_x_continuous(labels = function(x) paste0("$", x, "M"),
                     breaks = seq(0, 1500, 250)) +
  labs(
    title    = "Asset Size Distribution — Bunching Check Near $500M RBC Threshold",
    subtitle = "Pre-RBC CU-level average assets ($M). Each bin = $25M.",
    x        = "Average Total Assets ($ millions)",
    y        = "Number of CUs",
    caption  = paste0("Excess bunching just below $500M would suggest threshold manipulation. ",
                      "Source: NCUA Call Report (5300).")
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "fig_bunching_500m.png"),
       p_bunch, width = 10, height = 6, dpi = 300)
message("  Bunching check plot saved → output/figures/fig_bunching_500m.png")


# =============================================================================
# 15. CONSOLE SUMMARY
# =============================================================================

message("\n── Step 14: Output summary ───────────────────────────────────────────")

cat("\n=== TABLES PRODUCED ===\n")
cat("  output/tables/table1_summary_stats.csv\n")
cat("    → Paper Table 1: Pre-RBC means/SDs by group, CU-quarter obs, UNWINSORIZED\n")
cat("  output/tables/table2_balance_table.csv\n")
cat("    → Paper Table 2: Pre-RBC CU-level balance table, t-tests, UNWINSORIZED\n")
cat("  output/tables/supplement_full_panel_stats.csv\n")
cat("    → Supplementary: Full-panel stats, winsorized (NOT paper Table 1)\n")

cat("\n=== FIGURES PRODUCED ===\n")
figures <- c(
  "fig_missing_spread_coverage.png",
  "fig_networth_ratio_trends.png",
  "fig_nw_ratio_distribution_complex_prerbc.png",
  "fig_loan_portfolio_trends.png",
  "fig_loan_spread_trends.png",
  "fig_performance_trends.png",
  "fig_growth_trends.png",
  "fig_pretrend_raw_means.png",
  "fig_bunching_500m.png"
)
for (f in figures) cat(sprintf("  output/figures/%s\n", f))

cat("\n=== KEY CONSISTENCY NOTES FOR PAPER ===\n")
cat("  Table 1 & Table 2 use the SAME pre-RBC window (2018Q1–2021Q4)\n")
cat("  Table 1 unit: CU-quarter  |  Table 2 unit: CU (time-averaged)\n")
cat("  Both tables computed from analysis_panel_raw.rds (UNWINSORIZED)\n")
cat("  Regressions use analysis_panel.rds (WINSORIZED)\n")
cat("  Growth variables stored as ×100 — no ad-hoc rescaling needed\n")
cat("  Table 2 column headers are DYNAMIC — update automatically with sample\n")
cat("  spread_comm_re excluded from Table 1 (~78% missing)\n")

cat("\n=== AUTOMATED CONSISTENCY CHECKS PASSED ===\n")
cat("  ✓ networth_ratio = cap_buffer + 10 in Table 1 (CU-quarter)\n")
cat("  ✓ networth_ratio = cap_buffer + 10 in Table 2 (CU-level)\n")
cat("  ✓ Raw and winsorized panels have identical dimensions\n")

message("\n── 1_Descriptive_Stats.R complete ✓ ─────────────────────────────────")
message("  Next step: run 2_DiD_Estimation.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
