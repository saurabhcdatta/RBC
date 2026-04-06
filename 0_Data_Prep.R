# =============================================================================
# 0_Data_Prep.R
# RBC Rule Impact Analysis — Data Preparation
# NCUA Call Report (5300) Data
#
# Author  : [Your Name]
# Created : 2026
# Purpose : Load, clean, and structure the call report panel data for
#           the RBC rule difference-in-differences analysis.
#           Output: data/analysis_panel.rds
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)   # data wrangling
library(lubridate)   # date handling
library(fredr)       # FRED API for Treasury benchmarks
library(janitor)     # clean_names(), tabyl()
library(skimr)       # skim() for data quality summary
library(haven)       # in case .dta version is ever needed


# =============================================================================
# 1. USER SETTINGS — EDIT THESE
# =============================================================================

# Path to raw call report RDS file
RAW_DATA_PATH <- "call_report.rds"

# FRED API key (get free key at https://fred.stlouisfed.org/docs/api/api_key.html)
FRED_API_KEY  <- "YOUR_FRED_API_KEY_HERE"

# Output path for cleaned analysis panel
OUTPUT_PATH   <- "data/analysis_panel.rds"

# Asset threshold (in dollars) defining a "complex" credit union under RBC
COMPLEX_THRESHOLD <- 500e6   # $500 million

# RBC effective date (Q1 2022 = period 2022.1)
RBC_EFFECTIVE_PERIOD <- 2022.1

# Analysis window: keep quarters from Q1 2018 through Q4 2024
PERIOD_START <- 2018.1
PERIOD_END   <- 2024.4

# Minimum number of quarters a CU must appear to be included
MIN_QUARTERS <- 8


# =============================================================================
# 2. LOAD RAW DATA
# =============================================================================

message("── Step 1: Loading raw data ──────────────────────────────────────────")

df_raw <- readRDS(RAW_DATA_PATH)

message(sprintf("  Rows loaded    : %s", scales::comma(nrow(df_raw))))
message(sprintf("  Columns loaded : %s", ncol(df_raw)))
message(sprintf("  Unique CUs     : %s", n_distinct(df_raw$cu_number)))
message(sprintf("  Period range   : %s to %s",
                min(df_raw$q_period_num, na.rm = TRUE),
                max(df_raw$q_period_num, na.rm = TRUE)))


# =============================================================================
# 3. INITIAL FILTER — ANALYSIS WINDOW
# =============================================================================

message("── Step 2: Filtering to analysis window ──────────────────────────────")

df <- df_raw |>
  filter(
    q_period_num >= PERIOD_START,
    q_period_num <= PERIOD_END
  )

message(sprintf("  Rows after window filter: %s", scales::comma(nrow(df))))


# =============================================================================
# 4. REMOVE MERGED / ACQUIRED / INACTIVE CUs
# =============================================================================
# outcome == 1  : Active
# outcome == 2  : Merger/Liquidation
# outcome == 3  : Charter cancellation
# Use the 'acquiredcu' flag to exclude quarters where a CU is reporting
# on behalf of an acquired institution (avoids double-counting assets).

message("── Step 3: Removing merged/acquired/inactive CUs ─────────────────────")

# Flag CUs that experienced a merger or liquidation DURING the analysis window
merged_cus <- df |>
  filter(outcome %in% c(2, 3)) |>
  pull(cu_number) |>
  unique()

message(sprintf("  CUs with merger/liquidation outcome: %s", length(merged_cus)))

df <- df |>
  filter(
    !cu_number %in% merged_cus,           # drop merged CUs entirely
    is.na(acquiredcu) | acquiredcu == 0   # drop acquired-CU reporting quarters
  )

message(sprintf("  Rows after merger filter: %s", scales::comma(nrow(df))))


# =============================================================================
# 5. REQUIRE MINIMUM PANEL OBSERVATIONS PER CU
# =============================================================================

message("── Step 4: Requiring minimum panel observations ──────────────────────")

cu_obs <- df |>
  count(cu_number, name = "n_quarters")

thin_cus <- cu_obs |>
  filter(n_quarters < MIN_QUARTERS) |>
  pull(cu_number)

df <- df |>
  filter(!cu_number %in% thin_cus)

message(sprintf("  CUs dropped (< %d quarters): %s", MIN_QUARTERS, length(thin_cus)))
message(sprintf("  CUs retained               : %s", n_distinct(df$cu_number)))


# =============================================================================
# 6. CECL TRANSITION FLAG
# =============================================================================
# CECL adoption changed how loan loss allowances (and thus net worth) are
# measured. We create a flag so models can include it as a control or 
# analysts can run robustness checks on non-CECL CUs only.

message("── Step 5: CECL transition flag ──────────────────────────────────────")

df <- df |>
  mutate(
    cecl_adopter = as.integer(!is.na(cecl) & cecl == 1)
  )

message(sprintf("  CECL adopters in panel: %s",
                df |> filter(cecl_adopter == 1) |> distinct(cu_number) |> nrow()))


# =============================================================================
# 7. DEFINE TREATMENT — COMPLEX CU CLASSIFICATION
# =============================================================================
# A CU is "complex" (treated) if it had assets > $500M *before* RBC took
# effect. We use the average asset level in the 4 quarters prior to 2022q1
# (i.e., 2021q1 through 2021q4) to avoid post-treatment reclassification.
# This is a pre-treatment, time-invariant treatment indicator.

message("── Step 6: Defining complex CU treatment indicator ───────────────────")

pre_rbc_assets <- df |>
  filter(q_period_num >= 2021.1, q_period_num <= 2021.4) |>
  group_by(cu_number) |>
  summarise(
    avg_assets_pre = mean(assets_tot, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    complex = as.integer(avg_assets_pre >= COMPLEX_THRESHOLD)
  )

message(sprintf("  Complex CUs (treatment)    : %s",
                sum(pre_rbc_assets$complex, na.rm = TRUE)))
message(sprintf("  Non-complex CUs (control)  : %s",
                sum(pre_rbc_assets$complex == 0, na.rm = TRUE)))

# Merge treatment indicator back to panel
df <- df |>
  left_join(pre_rbc_assets |> select(cu_number, avg_assets_pre, complex),
            by = "cu_number")

# Drop CUs with no pre-period asset data (cannot classify)
df <- df |>
  filter(!is.na(complex))

message(sprintf("  Rows after treatment merge: %s", scales::comma(nrow(df))))


# =============================================================================
# 8. DEFINE POST-TREATMENT INDICATOR AND INTERACTION
# =============================================================================

message("── Step 7: Defining DiD time indicators ──────────────────────────────")

df <- df |>
  mutate(
    post_rbc   = as.integer(q_period_num >= RBC_EFFECTIVE_PERIOD),
    treat_post = complex * post_rbc,    # DiD interaction term

    # Quarters relative to RBC treatment (for event study)
    # 2022q1 = period 0
    event_time = case_when(
      quarter == 1 ~ (year - 2022L) * 4 + 0L,
      quarter == 2 ~ (year - 2022L) * 4 + 1L,
      quarter == 3 ~ (year - 2022L) * 4 + 2L,
      quarter == 4 ~ (year - 2022L) * 4 + 3L
    )
  )


# =============================================================================
# 9. CORE OUTCOME VARIABLES
# =============================================================================

message("── Step 8: Constructing outcome variables ────────────────────────────")

df <- df |>
  mutate(

    # ── 9a. Capital variables ──────────────────────────────────────────────
    cap_ratio      = pcanetworth,                  # net worth ratio (PCA)
    cap_buffer     = pcanetworth - 10,             # buffer above 10% threshold
    cap_buffer_9   = pcanetworth - 9,              # buffer above 9% (CCULR threshold)
    ln_networth    = log(networth_tot + 1),

    # ── 9b. Loan portfolio shares (% of total loans) ──────────────────────
    mbl_shr        = if_else(lns_tot > 0, lns_mbl   / lns_tot * 100, NA_real_),
    comm_shr       = if_else(lns_tot > 0, lns_comm  / lns_tot * 100, NA_real_),
    re_shr         = if_else(lns_tot > 0, lns_re    / lns_tot * 100, NA_real_),
    resid_shr      = if_else(lns_tot > 0,
                             (lns_resid_1 + lns_resid_2) / lns_tot * 100,
                             NA_real_),
    auto_shr       = if_else(lns_tot > 0, lns_auto  / lns_tot * 100, NA_real_),

    # ── 9c. Asset & loan growth (quarter-over-quarter log change) ──────────
    # NOTE: computed after sorting; fill in after arrange()
    ln_assets      = log(assets_tot),
    ln_loans       = log(lns_tot + 1),
    ln_mbl         = log(lns_mbl + 1),

    # ── 9d. Profitability / efficiency ────────────────────────────────────
    roa_var        = roa,
    nim            = netintmrg,          # net interest margin
    cof            = costfds,            # cost of funds
    yld_loans      = yldavgloans,        # yield on average loans

    # ── 9e. Credit quality ─────────────────────────────────────────────────
    dq_rate_var    = dq_rate,            # total delinquency rate
    dq_mbl_rate_var= dq_mbl_rate,        # MBL delinquency rate
    chgoff_ratio   = chg_tot_ratio,      # net charge-off ratio
    pll_assets     = if_else(assets_tot > 0,
                             (pll_pcl) / assets_tot * 100,
                             NA_real_),  # PLL as % of assets

    # ── 9f. Leverage / balance sheet ──────────────────────────────────────
    loan_to_asset  = if_else(assets_tot > 0, lns_tot / assets_tot * 100, NA_real_),
    inv_to_asset   = if_else(assets_tot > 0, inv_tot / assets_tot * 100, NA_real_),
    dep_to_asset   = if_else(assets_tot > 0, dep_tot / assets_tot * 100, NA_real_)

  )

# ── Quarter-over-quarter growth rates (requires sorting by CU and time) ───────
df <- df |>
  arrange(cu_number, q_period_num) |>
  group_by(cu_number) |>
  mutate(
    asset_growth  = ln_assets - lag(ln_assets),
    loan_growth   = ln_loans  - lag(ln_loans),
    mbl_growth    = ln_mbl    - lag(ln_mbl)
  ) |>
  ungroup()


# =============================================================================
# 10. FETCH TREASURY BENCHMARK RATES FROM FRED
# =============================================================================
# Used to construct loan-Treasury spreads, which control for the interest
# rate cycle (critical given the 2022 Fed tightening coincides with RBC).

message("── Step 9: Fetching Treasury benchmarks from FRED ────────────────────")

fredr_set_key(FRED_API_KEY)

# Helper: pull quarterly average from FRED
pull_fred_quarterly <- function(series_id, col_name) {
  fredr(
    series_id         = series_id,
    observation_start = as.Date("2018-01-01"),
    observation_end   = as.Date("2024-12-31"),
    frequency         = "q",
    aggregation_method = "avg"
  ) |>
    mutate(
      year         = year(date),
      quarter      = quarter(date),
      q_period_num = year + (quarter - 1) * 0.1
    ) |>
    select(q_period_num, !!col_name := value)
}

treasury_rates <- pull_fred_quarterly("DGS10", "t10yr") |>
  left_join(pull_fred_quarterly("DGS2",  "t2yr"),  by = "q_period_num") |>
  left_join(pull_fred_quarterly("DGS1",  "t1yr"),  by = "q_period_num") |>
  left_join(pull_fred_quarterly("FEDFUNDS", "fed_funds"), by = "q_period_num")

df <- df |>
  left_join(treasury_rates, by = "q_period_num")


# =============================================================================
# 11. LOAN RATE SPREADS OVER TREASURIES
# =============================================================================
# These are the KEY outcome variables for the loan pricing analysis.
# Using spreads rather than raw rates removes the common rate-cycle component.

message("── Step 10: Constructing loan-Treasury spreads ───────────────────────")

df <- df |>
  mutate(
    # First mortgages / long-term RE → benchmark to 10yr Treasury
    spread_1stmort     = irate_1stmort  - t10yr,
    spread_resid_1     = irate_resid_1  - t10yr,
    spread_comm_re     = irate_comm_resec - t10yr,

    # Auto and short-term consumer → benchmark to 2yr Treasury
    spread_nauto       = irate_nauto    - t2yr,
    spread_uauto       = irate_uauto    - t2yr,

    # MBL / commercial non-RE → benchmark to 2yr or 10yr depending on tenure
    spread_comm_oth    = irate_comm_oth - t2yr,

    # Other RE (junior liens, etc.)
    spread_resid_2     = irate_resid_2  - t10yr,
    spread_re_oth      = irate_re_oth   - t10yr
  )


# =============================================================================
# 12. ASSET SIZE TIERS (for heterogeneity analysis)
# =============================================================================

message("── Step 11: Assigning asset size tiers ───────────────────────────────")

df <- df |>
  mutate(
    asset_tier = case_when(
      assets_tot >= 10e9  ~ "1_Mega    (>$10B)",
      assets_tot >= 1e9   ~ "2_Large   ($1B-$10B)",
      assets_tot >= 500e6 ~ "3_Complex ($500M-$1B)",
      assets_tot >= 100e6 ~ "4_Mid     ($100M-$500M)",
      TRUE                ~ "5_Small   (<$100M)"
    ),
    # Binary near-threshold indicator (within +/- 20% of $500M cutoff)
    near_threshold = as.integer(
      avg_assets_pre >= 400e6 & avg_assets_pre <= 600e6
    )
  )


# =============================================================================
# 13. SELECT AND ORDER FINAL VARIABLES
# =============================================================================

message("── Step 12: Selecting final analysis variables ───────────────────────")

df_panel <- df |>
  select(
    # ── Identifiers ────────────────────────────────────────────────────────
    cu_number, cu_name, cu_type,
    year, quarter, q_period_num,
    state, region,

    # ── Treatment & time indicators ────────────────────────────────────────
    complex, post_rbc, treat_post, event_time,
    avg_assets_pre, asset_tier, near_threshold,

    # ── Controls / CU characteristics ─────────────────────────────────────
    assets_tot, ln_assets, members,
    cecl_adopter,
    loan_to_asset, inv_to_asset, dep_to_asset,
    shortform,       # 1 = filed short form (smaller CU indicator)

    # ── Capital outcomes ───────────────────────────────────────────────────
    cap_ratio, cap_buffer, cap_buffer_9,
    networth_tot, eq_tot, ln_networth,
    subdebt,

    # ── Loan portfolio share outcomes ─────────────────────────────────────
    lns_tot, ln_loans,
    mbl_shr, comm_shr, re_shr, resid_shr, auto_shr,
    lns_mbl, lns_comm, lns_re,

    # ── Asset & loan growth ────────────────────────────────────────────────
    asset_growth, loan_growth, mbl_growth,

    # ── Loan rate spreads (key outcomes) ──────────────────────────────────
    spread_1stmort, spread_resid_1, spread_resid_2,
    spread_nauto, spread_uauto,
    spread_comm_re, spread_comm_oth, spread_re_oth,

    # ── Raw loan interest rates ────────────────────────────────────────────
    irate_1stmort, irate_resid_1, irate_resid_2,
    irate_nauto, irate_uauto,
    irate_comm_resec, irate_comm_oth, irate_re_oth,

    # ── Treasury benchmarks ────────────────────────────────────────────────
    t10yr, t2yr, t1yr, fed_funds,

    # ── Credit quality ─────────────────────────────────────────────────────
    dq_rate_var, dq_mbl_rate_var, chgoff_ratio, pll_assets,
    pll, pcl, pll_pcl,

    # ── Profitability ──────────────────────────────────────────────────────
    roa_var, nim, cof, yld_loans,
    inc_int_net, inc_net,

    # ── Merger / outcome tracking ──────────────────────────────────────────
    outcome, reason
  )


# =============================================================================
# 14. WINSORIZE CONTINUOUS OUTCOMES (1st / 99th percentile)
# =============================================================================
# Prevents extreme outliers from driving DiD estimates.

message("── Step 13: Winsorizing continuous outcomes ──────────────────────────")

winsorize <- function(x, low = 0.01, high = 0.99) {
  q <- quantile(x, probs = c(low, high), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

winsor_vars <- c(
  "cap_buffer", "spread_1stmort", "spread_resid_1", "spread_resid_2",
  "spread_nauto", "spread_uauto", "spread_comm_re", "spread_comm_oth",
  "mbl_shr", "comm_shr", "asset_growth", "loan_growth",
  "dq_rate_var", "chgoff_ratio", "pll_assets", "roa_var"
)

df_panel <- df_panel |>
  mutate(across(all_of(winsor_vars), winsorize))

message(sprintf("  Winsorized %d variables at [1%%, 99%%]", length(winsor_vars)))


# =============================================================================
# 15. DATA QUALITY REPORT
# =============================================================================

message("── Step 14: Data quality summary ────────────────────────────────────")

cat("\n=== PANEL SUMMARY ===\n")
cat(sprintf("  Total obs          : %s\n",  scales::comma(nrow(df_panel))))
cat(sprintf("  Unique CUs         : %s\n",  scales::comma(n_distinct(df_panel$cu_number))))
cat(sprintf("  Quarters covered   : %s\n",  n_distinct(df_panel$q_period_num)))
cat(sprintf("  Complex (treated)  : %s\n",
            scales::comma(df_panel |> distinct(cu_number, complex) |>
                            filter(complex == 1) |> nrow())))
cat(sprintf("  Non-complex (ctrl) : %s\n",
            scales::comma(df_panel |> distinct(cu_number, complex) |>
                            filter(complex == 0) |> nrow())))
cat(sprintf("  Near threshold ($400M-$600M): %s\n",
            scales::comma(df_panel |> distinct(cu_number, near_threshold) |>
                            filter(near_threshold == 1) |> nrow())))

cat("\n=== MISSING VALUES (key outcomes) ===\n")
key_outcomes <- c("cap_buffer", "spread_1stmort", "spread_nauto",
                  "mbl_shr", "dq_rate_var", "roa_var")
df_panel |>
  summarise(across(all_of(key_outcomes),
                   ~ scales::percent(mean(is.na(.)), accuracy = 0.1))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") |>
  print()

cat("\n=== TREATMENT GROUP MEANS (Pre-RBC period) ===\n")
df_panel |>
  filter(post_rbc == 0) |>
  group_by(complex) |>
  summarise(
    n_obs         = n(),
    avg_cap_ratio = round(mean(cap_ratio,    na.rm = TRUE), 2),
    avg_mbl_shr   = round(mean(mbl_shr,      na.rm = TRUE), 2),
    avg_roa       = round(mean(roa_var,       na.rm = TRUE), 3),
    avg_dq_rate   = round(mean(dq_rate_var,  na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  print()


# =============================================================================
# 16. SAVE OUTPUT
# =============================================================================

message("── Step 15: Saving analysis panel ───────────────────────────────────")

# Create output directory if it doesn't exist
if (!dir.exists(dirname(OUTPUT_PATH))) dir.create(dirname(OUTPUT_PATH), recursive = TRUE)

saveRDS(df_panel, OUTPUT_PATH)

message(sprintf("  Saved to: %s", OUTPUT_PATH))
message(sprintf("  File size: %s",
                utils::object.size(df_panel) |> format(units = "MB")))

message("\n── 0_Data_Prep.R complete ✓ ──────────────────────────────────────────")
message("  Next step: run 1_Descriptive_Stats.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
