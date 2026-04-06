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
#
# CAPITAL RATIO NOTE:
#   Primary capital measure: networth_ratio = networth_tot / assets_tot * 100
#   This is computed directly from balance sheet items, available for ALL CUs,
#   and directly corresponds to the RBC well-capitalized threshold of >= 10%.
#   pcanetworth (PCA ratio) is retained as a secondary robustness variable
#   but had ~51% missingness (concentrated in short-form filers) so is NOT
#   used as the main outcome.
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

# Capital ratio thresholds under RBC rule
NW_WELLCAP_THRESHOLD <- 10   # well-capitalized: >= 10% net worth ratio
NW_CCULR_THRESHOLD   <-  9   # well-capitalized: >= 9%  (CCULR opt-in)


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
# outcome == 2  : Merger / Liquidation
# outcome == 3  : Charter cancellation
# acquiredcu    : CU reporting on behalf of an acquired institution
#                 (drop these quarters to avoid double-counting assets)

message("── Step 3: Removing merged/acquired/inactive CUs ─────────────────────")

merged_cus <- df |>
  filter(outcome %in% c(2, 3)) |>
  pull(cu_number) |>
  unique()

message(sprintf("  CUs with merger/liquidation outcome: %s", length(merged_cus)))

df <- df |>
  filter(
    !cu_number %in% merged_cus,
    is.na(acquiredcu) | acquiredcu == 0
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
# CECL adoption changed how loan loss allowances are measured and causes a
# one-time downward jump in net worth at adoption (day-1 adjustment).
# Always include cecl_adopter as a control in capital ratio regressions.

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
# effect. We average assets over the 4 quarters immediately before treatment
# (2021q1–2021q4) to create a pre-treatment, time-invariant indicator.
# This avoids post-treatment reclassification bias.

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

message(sprintf("  Complex CUs (treatment)   : %s",
                sum(pre_rbc_assets$complex, na.rm = TRUE)))
message(sprintf("  Non-complex CUs (control) : %s",
                sum(pre_rbc_assets$complex == 0, na.rm = TRUE)))

df <- df |>
  left_join(pre_rbc_assets |> select(cu_number, avg_assets_pre, complex),
            by = "cu_number") |>
  filter(!is.na(complex))   # drop CUs with no pre-period data

message(sprintf("  Rows after treatment merge: %s", scales::comma(nrow(df))))


# =============================================================================
# 8. DEFINE POST-TREATMENT INDICATOR, INTERACTION, AND EVENT TIME
# =============================================================================

message("── Step 7: Defining DiD time indicators ──────────────────────────────")

df <- df |>
  mutate(
    post_rbc   = as.integer(q_period_num >= RBC_EFFECTIVE_PERIOD),
    treat_post = complex * post_rbc,    # DiD interaction term

    # Quarters relative to RBC (2022q1 = 0), used for event study plots
    event_time = case_when(
      quarter == 1 ~ (year - 2022L) * 4 + 0L,
      quarter == 2 ~ (year - 2022L) * 4 + 1L,
      quarter == 3 ~ (year - 2022L) * 4 + 2L,
      quarter == 4 ~ (year - 2022L) * 4 + 3L
    )
  )


# =============================================================================
# 9. PRIMARY CAPITAL VARIABLE — NET WORTH RATIO
# =============================================================================
# networth_ratio = networth_tot / assets_tot * 100
#
# Constructed directly from balance sheet items reported by ALL CUs.
# Maps directly to the RBC well-capitalized threshold (>= 10%).
# Covers ~100% of observations vs. ~49% for pcanetworth.
#
# CECL note: networth_tot already excludes the CECL transition provision
# per the variable dictionary (networth_tot = "Total net worth ($) excluding
# CECL transition provision"). This is the correct numerator for RBC purposes.
#
# pcanetworth is retained as cap_ratio_pca for robustness checks on the
# subset of full-form filers where it is available.

message("── Step 8: Computing net worth ratio (primary capital variable) ───────")

df <- df |>
  mutate(

    # ── PRIMARY: Net worth ratio ───────────────────────────────────────────
    networth_ratio = if_else(
      assets_tot > 0,
      networth_tot / assets_tot * 100,
      NA_real_
    ),

    # Buffer above RBC well-capitalized threshold (10%)
    # Positive = above threshold; negative = undercapitalized
    cap_buffer = networth_ratio - NW_WELLCAP_THRESHOLD,

    # Buffer above CCULR well-capitalized threshold (9%)
    cap_buffer_cculr = networth_ratio - NW_CCULR_THRESHOLD,

    # Binary: well-capitalized under RBC (>= 10%)
    well_capitalized = as.integer(networth_ratio >= NW_WELLCAP_THRESHOLD),

    # Log dollar net worth (for level regressions)
    ln_networth = if_else(networth_tot > 0, log(networth_tot), NA_real_),

    # ── SECONDARY: pcanetworth (robustness, full-form filers only) ─────────
    cap_ratio_pca = pcanetworth   # ~49% coverage

  )

# Coverage diagnostic
nw_coverage <- df |>
  summarise(
    pct_nw_ratio_present = scales::percent(mean(!is.na(networth_ratio)), 0.1),
    pct_pca_present      = scales::percent(mean(!is.na(cap_ratio_pca)),  0.1)
  )
message(sprintf("  networth_ratio coverage : %s  ← PRIMARY",
                nw_coverage$pct_nw_ratio_present))
message(sprintf("  pcanetworth coverage    : %s  ← robustness only",
                nw_coverage$pct_pca_present))


# =============================================================================
# 10. CORE OUTCOME VARIABLES
# =============================================================================

message("── Step 9: Constructing outcome variables ────────────────────────────")

df <- df |>
  mutate(

    # ── 10a. Balance sheet log levels ─────────────────────────────────────
    ln_assets = log(assets_tot),
    ln_loans  = log(lns_tot + 1),
    ln_mbl    = log(coalesce(lns_mbl, lns_mbl_part723) + 1),

    # ── 10b. MBL: coalesce lns_mbl with part723 fallback ──────────────────
    lns_mbl_use = coalesce(lns_mbl, lns_mbl_part723),

    # ── 10c. Loan portfolio shares (% of total loans) ─────────────────────
    mbl_shr   = if_else(lns_tot > 0, lns_mbl_use / lns_tot * 100, NA_real_),
    comm_shr  = if_else(lns_tot > 0, lns_comm    / lns_tot * 100, NA_real_),
    re_shr    = if_else(lns_tot > 0, lns_re      / lns_tot * 100, NA_real_),
    resid_shr = if_else(lns_tot > 0,
                        (lns_resid_1 + lns_resid_2) / lns_tot * 100,
                        NA_real_),
    auto_shr  = if_else(lns_tot > 0, lns_auto    / lns_tot * 100, NA_real_),
    cc_shr    = if_else(lns_tot > 0, lns_cc      / lns_tot * 100, NA_real_),

    # ── 10d. Balance sheet ratios (controls) ──────────────────────────────
    loan_to_asset = if_else(assets_tot > 0, lns_tot / assets_tot * 100, NA_real_),
    inv_to_asset  = if_else(assets_tot > 0, inv_tot / assets_tot * 100, NA_real_),
    dep_to_asset  = if_else(assets_tot > 0, dep_tot / assets_tot * 100, NA_real_),

    # Short-term investment share (capital management signal)
    inv_short_shr = if_else(inv_tot > 0,
                            coalesce(inv_tot_1y, 0) / inv_tot * 100,
                            NA_real_),

    # ── 10e. Profitability ─────────────────────────────────────────────────
    roa_var   = roa,
    nim       = netintmrg,
    cof       = costfds,
    yld_loans = yldavgloans,

    # ── 10f. Credit quality ────────────────────────────────────────────────
    dq_rate_var      = dq_rate,
    dq_mbl_rate_var  = dq_mbl_rate,
    dq_comm_rate_var = dq_comm_rate,
    chgoff_ratio     = chg_tot_ratio,
    chgoff_mbl_ratio = if_else(lns_mbl_use > 0,
                               chg_mbl / lns_mbl_use * 100,
                               NA_real_),
    pll_assets       = if_else(assets_tot > 0,
                               pll_pcl / assets_tot * 100,
                               NA_real_),

    # ── 10g. Unified interest rates (handles 2022q1 variable break) ────────
    # Pre-2022: irate_1stmort; Post-2022: irate_resid_1
    irate_mortgage    = coalesce(irate_resid_1, irate_1stmort),
    irate_re_junior   = coalesce(irate_resid_2, irate_re_oth),
    irate_comm_re_use = coalesce(irate_comm_resec, irate_re_oth),
    irate_comm_oth_use = irate_comm_oth,
    irate_cc           = irate_cc

  )

# ── QoQ log growth rates (requires sort by CU and period) ────────────────────
df <- df |>
  arrange(cu_number, q_period_num) |>
  group_by(cu_number) |>
  mutate(
    asset_growth    = ln_assets   - lag(ln_assets),
    loan_growth     = ln_loans    - lag(ln_loans),
    mbl_growth      = ln_mbl      - lag(ln_mbl),
    networth_growth = ln_networth - lag(ln_networth)
  ) |>
  ungroup()


# =============================================================================
# 11. FETCH TREASURY BENCHMARK RATES FROM FRED
# =============================================================================
# Loan-Treasury spreads are the KEY outcome for the pricing analysis.
# Using spreads removes the common rate-cycle component — critical because
# the 2022 Fed tightening cycle coincides exactly with RBC implementation.

message("── Step 10: Fetching Treasury benchmarks from FRED ───────────────────")

fredr_set_key(FRED_API_KEY)

pull_fred_quarterly <- function(series_id, col_name) {
  fredr(
    series_id          = series_id,
    observation_start  = as.Date("2018-01-01"),
    observation_end    = as.Date("2024-12-31"),
    frequency          = "q",
    aggregation_method = "avg"
  ) |>
    mutate(
      year         = year(date),
      quarter      = quarter(date),
      q_period_num = year + (quarter - 1) * 0.1
    ) |>
    select(q_period_num, !!col_name := value)
}

treasury_rates <- pull_fred_quarterly("DGS10",    "t10yr")      |>
  left_join(pull_fred_quarterly("DGS2",     "t2yr"),      by = "q_period_num") |>
  left_join(pull_fred_quarterly("DGS1",     "t1yr"),      by = "q_period_num") |>
  left_join(pull_fred_quarterly("FEDFUNDS", "fed_funds"), by = "q_period_num")

df <- df |>
  left_join(treasury_rates, by = "q_period_num")

message(sprintf("  Treasury rates merged for %d quarters",
                n_distinct(treasury_rates$q_period_num)))


# =============================================================================
# 12. LOAN RATE SPREADS OVER TREASURIES
# =============================================================================
# Benchmark each loan type to the Treasury maturity matching its duration.

message("── Step 11: Constructing loan-Treasury spreads ───────────────────────")

df <- df |>
  mutate(
    # Long-duration RE → 10yr Treasury
    spread_mortgage  = irate_mortgage     - t10yr,
    spread_re_junior = irate_re_junior    - t10yr,
    spread_comm_re   = irate_comm_re_use  - t10yr,

    # Short-duration consumer & auto → 2yr Treasury
    spread_nauto     = irate_nauto        - t2yr,
    spread_uauto     = irate_uauto        - t2yr,
    spread_cc        = irate_cc           - t2yr,

    # Commercial non-RE → 2yr Treasury
    spread_comm_oth  = irate_comm_oth_use - t2yr
  )

# Coverage check
message("  Spread coverage:")
df |>
  summarise(
    spread_mortgage  = scales::percent(mean(!is.na(spread_mortgage)),  0.1),
    spread_nauto     = scales::percent(mean(!is.na(spread_nauto)),     0.1),
    spread_comm_re   = scales::percent(mean(!is.na(spread_comm_re)),   0.1),
    spread_comm_oth  = scales::percent(mean(!is.na(spread_comm_oth)),  0.1)
  ) |>
  pivot_longer(everything()) |>
  mutate(msg = sprintf("    %-20s : %s", name, value)) |>
  pull(msg) |>
  walk(message)


# =============================================================================
# 13. ASSET SIZE TIERS (for heterogeneity analysis)
# =============================================================================

message("── Step 12: Assigning asset size tiers ───────────────────────────────")

df <- df |>
  mutate(
    asset_tier = case_when(
      assets_tot >= 10e9  ~ "1_Mega    (>$10B)",
      assets_tot >= 1e9   ~ "2_Large   ($1B-$10B)",
      assets_tot >= 500e6 ~ "3_Complex ($500M-$1B)",
      assets_tot >= 100e6 ~ "4_Mid     ($100M-$500M)",
      TRUE                ~ "5_Small   (<$100M)"
    ),
    # Within +/- 20% of $500M cutoff (RD / bunching robustness)
    near_threshold = as.integer(
      avg_assets_pre >= 400e6 & avg_assets_pre <= 600e6
    )
  )


# =============================================================================
# 14. SELECT AND ORDER FINAL VARIABLES
# =============================================================================

message("── Step 13: Selecting final analysis variables ───────────────────────")

df_panel <- df |>
  select(

    # ── Identifiers ──────────────────────────────────────────────────────
    cu_number, cu_name, cu_type,
    year, quarter, q_period_num,
    state, region,

    # ── Treatment & time indicators ───────────────────────────────────────
    complex, post_rbc, treat_post, event_time,
    avg_assets_pre, asset_tier, near_threshold,

    # ── Controls / CU characteristics ────────────────────────────────────
    assets_tot, ln_assets, members,
    cecl_adopter,
    loan_to_asset, inv_to_asset, dep_to_asset, inv_short_shr,
    shortform,

    # ── Capital outcomes — PRIMARY ────────────────────────────────────────
    # networth_ratio = networth_tot / assets_tot * 100
    # ~100% coverage; maps directly to RBC 10% threshold
    networth_ratio,      # PRIMARY capital outcome
    cap_buffer,          # networth_ratio − 10  (RBC buffer)
    cap_buffer_cculr,    # networth_ratio −  9  (CCULR buffer)
    well_capitalized,    # binary: networth_ratio >= 10%
    networth_tot,        # dollar net worth
    eq_tot,              # total equity
    ln_networth,         # log net worth
    networth_growth,     # QoQ log change in net worth
    subdebt,             # subordinated debt in net worth

    # ── Capital outcomes — SECONDARY (robustness, full-form filers only) ──
    cap_ratio_pca,       # pcanetworth; ~49% coverage

    # ── Loan portfolio shares ─────────────────────────────────────────────
    lns_tot, ln_loans,
    mbl_shr, comm_shr, re_shr, resid_shr, auto_shr, cc_shr,
    lns_mbl_use, lns_comm, lns_re,

    # ── Growth rates ──────────────────────────────────────────────────────
    asset_growth, loan_growth, mbl_growth,

    # ── Loan-Treasury spreads (KEY pricing outcomes) ──────────────────────
    spread_mortgage, spread_re_junior, spread_comm_re,
    spread_nauto, spread_uauto,
    spread_comm_oth, spread_cc,

    # ── Raw loan rates ────────────────────────────────────────────────────
    irate_mortgage, irate_re_junior,
    irate_nauto, irate_uauto,
    irate_comm_re_use, irate_comm_oth_use, irate_cc,

    # ── Treasury benchmarks ───────────────────────────────────────────────
    t10yr, t2yr, t1yr, fed_funds,

    # ── Credit quality ────────────────────────────────────────────────────
    dq_rate_var, dq_mbl_rate_var, dq_comm_rate_var,
    chgoff_ratio, chgoff_mbl_ratio, pll_assets,
    pll, pcl, pll_pcl,

    # ── Profitability ─────────────────────────────────────────────────────
    roa_var, nim, cof, yld_loans,
    inc_int_net, inc_net,

    # ── Merger / outcome tracking ─────────────────────────────────────────
    outcome, reason
  )


# =============================================================================
# 15. WINSORIZE CONTINUOUS OUTCOMES (1st / 99th percentile)
# =============================================================================

message("── Step 14: Winsorizing continuous outcomes ──────────────────────────")

winsorize <- function(x, low = 0.01, high = 0.99) {
  q <- quantile(x, probs = c(low, high), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

winsor_vars <- c(
  # Capital (primary)
  "networth_ratio", "cap_buffer", "cap_buffer_cculr",
  # Loan spreads
  "spread_mortgage", "spread_re_junior", "spread_comm_re",
  "spread_nauto", "spread_uauto", "spread_comm_oth", "spread_cc",
  # Portfolio shares
  "mbl_shr", "comm_shr", "re_shr", "auto_shr",
  # Growth rates
  "asset_growth", "loan_growth", "mbl_growth",
  # Credit quality
  "dq_rate_var", "chgoff_ratio", "pll_assets",
  # Profitability
  "roa_var", "nim", "cof"
)

df_panel <- df_panel |>
  mutate(across(all_of(winsor_vars), winsorize))

message(sprintf("  Winsorized %d variables at [1%%, 99%%]", length(winsor_vars)))


# =============================================================================
# 16. DATA QUALITY REPORT
# =============================================================================

message("── Step 15: Data quality summary ────────────────────────────────────")

cat("\n=== PANEL SUMMARY ===\n")
cat(sprintf("  Total obs              : %s\n", scales::comma(nrow(df_panel))))
cat(sprintf("  Unique CUs             : %s\n", scales::comma(n_distinct(df_panel$cu_number))))
cat(sprintf("  Quarters covered       : %s\n", n_distinct(df_panel$q_period_num)))
cat(sprintf("  Complex (treated)      : %s\n",
            scales::comma(df_panel |> distinct(cu_number, complex) |>
                            filter(complex == 1) |> nrow())))
cat(sprintf("  Non-complex (control)  : %s\n",
            scales::comma(df_panel |> distinct(cu_number, complex) |>
                            filter(complex == 0) |> nrow())))
cat(sprintf("  Near threshold ($400M-$600M): %s\n",
            scales::comma(df_panel |> distinct(cu_number, near_threshold) |>
                            filter(near_threshold == 1) |> nrow())))

cat("\n=== MISSING VALUES (key outcomes) ===\n")
key_outcomes <- c(
  "networth_ratio", "cap_buffer",    # primary — expect ~0% missing
  "cap_ratio_pca",                   # secondary — expect ~51% missing
  "spread_mortgage", "spread_nauto", "spread_comm_re",
  "mbl_shr", "dq_rate_var", "roa_var"
)
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
    n_obs              = n(),
    avg_networth_ratio = round(mean(networth_ratio,   na.rm = TRUE), 2),
    avg_cap_buffer     = round(mean(cap_buffer,       na.rm = TRUE), 2),
    pct_well_cap       = round(mean(well_capitalized, na.rm = TRUE) * 100, 1),
    avg_mbl_shr        = round(mean(mbl_shr,          na.rm = TRUE), 2),
    avg_roa            = round(mean(roa_var,           na.rm = TRUE), 3),
    avg_dq_rate        = round(mean(dq_rate_var,      na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  print()

cat("\n=== NET WORTH RATIO DISTRIBUTION (Pre-RBC, by treatment group) ===\n")
for (grp in c(1, 0)) {
  lbl <- if (grp == 1) "Complex (treated)" else "Non-complex (control)"
  cat(sprintf("\n  %s:\n", lbl))
  df_panel |>
    filter(post_rbc == 0, complex == grp) |>
    pull(networth_ratio) |>
    quantile(probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
             na.rm = TRUE) |>
    round(2) |>
    print()
}

cat("\n=== CUs NEAR / BELOW 10% THRESHOLD PRE-RBC (complex only) ===\n")
df_panel |>
  filter(post_rbc == 0, complex == 1) |>
  summarise(
    n_obs           = n(),
    n_below_10pct   = sum(networth_ratio < 10,  na.rm = TRUE),
    pct_below_10pct = round(mean(networth_ratio < 10,  na.rm = TRUE) * 100, 1),
    n_below_9pct    = sum(networth_ratio < 9,   na.rm = TRUE),
    pct_below_9pct  = round(mean(networth_ratio < 9,   na.rm = TRUE) * 100, 1),
    n_10_to_11pct   = sum(networth_ratio >= 10 & networth_ratio < 11, na.rm = TRUE),
    pct_10_to_11pct = round(mean(networth_ratio >= 10 &
                                   networth_ratio < 11, na.rm = TRUE) * 100, 1)
  ) |>
  print()


# =============================================================================
# 17. SAVE OUTPUT
# =============================================================================

message("── Step 16: Saving analysis panel ───────────────────────────────────")

if (!dir.exists(dirname(OUTPUT_PATH))) {
  dir.create(dirname(OUTPUT_PATH), recursive = TRUE)
}

saveRDS(df_panel, OUTPUT_PATH)

message(sprintf("  Saved to  : %s", OUTPUT_PATH))
message(sprintf("  Rows      : %s", scales::comma(nrow(df_panel))))
message(sprintf("  Variables : %d", ncol(df_panel)))
message(sprintf("  File size : %s",
                utils::object.size(df_panel) |> format(units = "MB")))

message("\n── 0_Data_Prep.R complete ✓ ──────────────────────────────────────────")
message("  Next step: run 1_Descriptive_Stats.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
