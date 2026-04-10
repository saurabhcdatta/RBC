# =============================================================================
# 0B_Crisis_Panel_Prep.R
# RBC Rule Impact Analysis — Full Time-Series Panel for Crisis Calibration
#
# Author  : Saurabh C. Datta, Ph.D.
# Created : 2026
#
# PURPOSE:
#   0_Data_Prep.R filtered call_report.rds to 2018Q1-2025Q4 for the RBC
#   study window. This meant analysis_panel_raw.rds has NO pre-2018 data,
#   so 3F cannot access 2004-2007 steady-state ROA or 2008-2010 crisis
#   drawdowns needed for the capital stress calibration.
#
#   This companion script reads call_report.rds directly and produces:
#
#   OUTPUT 1: data/analysis_panel_full_raw.rds  (unwinsorized, 2000-2025)
#   OUTPUT 2: data/analysis_panel_full.rds      (winsorized,   2000-2025)
#
#   Both panels have IDENTICAL variable names to analysis_panel_raw.rds
#   so 3F_Capital_Adequacy_Stress_Test.R works without any other changes —
#   just update PANEL_RAW_PATH and PANEL_PATH at the top of 3F.
#
# RUN ORDER:
#   0_Data_Prep.R          (produces analysis_panel*.rds for main study)
#   0B_Crisis_Panel_Prep.R (produces analysis_panel_full*.rds for 3F)
#   3F_Capital_Adequacy_Stress_Test.R
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(lubridate)
library(scales)


# =============================================================================
# 1. SETTINGS — must match 0_Data_Prep.R exactly
# =============================================================================

RAW_DATA_PATH   <- "call_report.rds"
OUTPUT_FULL_RAW <- "data/analysis_panel_full_raw.rds"   # unwinsorized
OUTPUT_FULL     <- "data/analysis_panel_full.rds"       # winsorized

COMPLEX_THRESHOLD <- 500e6     # $500M — same as 0_Data_Prep.R
COMPLEX_AVG_START <- 2021.1    # same 4-quarter averaging window
COMPLEX_AVG_END   <- 2021.4
NW_WELLCAP        <- 10.0
NW_CCULR          <-  9.0
RBC_EFFECTIVE     <- 2022.1
MIN_QUARTERS      <- 4         # relaxed from 8 (many pre-2010 CUs have fewer)

FULL_START <- 2000.1           # 2001 recession included for context
FULL_END   <- 2025.4

message("== 0B_Crisis_Panel_Prep.R starting ==")
message(sprintf("  Full window: %.1f to %.1f", FULL_START, FULL_END))


# =============================================================================
# 2. LOAD
# =============================================================================

message("-- Step 1: Loading call_report.rds")

df_raw <- readRDS(RAW_DATA_PATH)

message(sprintf("  Rows    : %s", comma(nrow(df_raw))))
message(sprintf("  Columns : %d", ncol(df_raw)))
message(sprintf("  CUs     : %s", comma(n_distinct(df_raw$cu_number))))
message(sprintf("  Range   : %.1f to %.1f",
                min(df_raw$q_period_num, na.rm = TRUE),
                max(df_raw$q_period_num, na.rm = TRUE)))


# =============================================================================
# 3. FILTER TO FULL WINDOW
# =============================================================================

message("-- Step 2: Filtering to full window")

df <- df_raw |>
  filter(q_period_num >= FULL_START, q_period_num <= FULL_END)

message(sprintf("  Rows after window filter: %s", comma(nrow(df))))


# =============================================================================
# 4. REMOVE MERGED / ACQUIRED / INACTIVE CUs
# =============================================================================
# Identical to 0_Data_Prep.R Step 3.

message("-- Step 3: Removing merged/acquired CUs")

merged_cus <- df |>
  filter(outcome %in% c(2, 3)) |>
  pull(cu_number) |>
  unique()

df <- df |>
  filter(
    !cu_number %in% merged_cus,
    is.na(acquiredcu) | acquiredcu == 0
  )

message(sprintf("  Merged CUs excluded: %d", length(merged_cus)))
message(sprintf("  Rows remaining     : %s", comma(nrow(df))))


# =============================================================================
# 5. MINIMUM QUARTERS
# =============================================================================

message("-- Step 4: Minimum quarters filter")

cu_obs   <- df |> count(cu_number, name = "n_quarters")
thin_cus <- cu_obs |> filter(n_quarters < MIN_QUARTERS) |> pull(cu_number)
df       <- df |> filter(!cu_number %in% thin_cus)

message(sprintf("  CUs dropped (< %d Q): %d", MIN_QUARTERS, length(thin_cus)))
message(sprintf("  CUs retained        : %s", comma(n_distinct(df$cu_number))))


# =============================================================================
# 6. CECL FLAG
# =============================================================================

df <- df |>
  mutate(cecl_adopter = as.integer(!is.na(cecl) & cecl == 1))


# =============================================================================
# 7. COMPLEX CU CLASSIFICATION (identical to 0_Data_Prep.R)
# =============================================================================

message("-- Step 5: Complex CU classification")

pre_rbc_assets <- df |>
  filter(q_period_num >= COMPLEX_AVG_START,
         q_period_num <= COMPLEX_AVG_END) |>
  group_by(cu_number) |>
  summarise(avg_assets_pre = mean(assets_tot, na.rm = TRUE), .groups = "drop") |>
  mutate(complex = as.integer(avg_assets_pre >= COMPLEX_THRESHOLD))

message(sprintf("  Complex (treated)  : %d",
                sum(pre_rbc_assets$complex == 1, na.rm = TRUE)))
message(sprintf("  Non-complex (ctrl) : %d",
                sum(pre_rbc_assets$complex == 0, na.rm = TRUE)))

df <- df |>
  left_join(pre_rbc_assets |> select(cu_number, avg_assets_pre, complex),
            by = "cu_number") |>
  filter(!is.na(complex))

message(sprintf("  Rows after join: %s", comma(nrow(df))))


# =============================================================================
# 8. TIME INDICATORS
# =============================================================================

message("-- Step 6: Time indicators")

df <- df |>
  mutate(
    post_rbc   = as.integer(q_period_num >= RBC_EFFECTIVE),
    treat_post = complex * post_rbc,
    event_time = case_when(
      quarter == 1 ~ (year - 2022L) * 4 + 0L,
      quarter == 2 ~ (year - 2022L) * 4 + 1L,
      quarter == 3 ~ (year - 2022L) * 4 + 2L,
      quarter == 4 ~ (year - 2022L) * 4 + 3L
    )
  )


# =============================================================================
# 9. VARIABLE CONSTRUCTION
# =============================================================================
# ALL variable names match analysis_panel_raw.rds EXACTLY.
# Key mappings (from 0_Data_Prep.R Step 9-10):
#   roa          -> roa_var
#   netintmrg    -> nim
#   costfds      -> cof
#   yldavgloans  -> yld_loans
#   dq_rate      -> dq_rate_var
#   chg_tot_ratio-> chgoff_ratio

message("-- Step 7: Variable construction")

df <- df |>
  mutate(

    # Capital
    networth_ratio   = if_else(assets_tot > 0,
                               networth_tot / assets_tot * 100, NA_real_),
    cap_buffer       = networth_ratio - NW_WELLCAP,
    cap_buffer_cculr = networth_ratio - NW_CCULR,
    well_capitalized = as.integer(networth_ratio >= NW_WELLCAP),
    ln_networth      = if_else(networth_tot > 0, log(networth_tot), NA_real_),
    cap_ratio_pca    = pcanetworth,

    # Balance sheet
    ln_assets   = log(assets_tot),
    ln_loans    = log(lns_tot + 1),
    lns_mbl_use = coalesce(lns_mbl, lns_mbl_part723),
    ln_mbl      = log(lns_mbl_use + 1),

    # Portfolio shares
    mbl_shr  = if_else(lns_tot > 0, lns_mbl_use / lns_tot * 100, NA_real_),
    re_shr   = if_else(lns_tot > 0, lns_re      / lns_tot * 100, NA_real_),
    auto_shr = if_else(lns_tot > 0, lns_auto    / lns_tot * 100, NA_real_),
    comm_shr = if_else(lns_tot > 0, lns_comm    / lns_tot * 100, NA_real_),

    # Controls
    loan_to_asset = if_else(assets_tot > 0,
                            lns_tot / assets_tot * 100, NA_real_),

    # Profitability — renaming to match analysis_panel_raw.rds
    roa_var   = roa,
    nim       = netintmrg,
    cof       = costfds,
    yld_loans = yldavgloans,

    # Credit quality — renaming to match analysis_panel_raw.rds
    dq_rate_var  = dq_rate,
    chgoff_ratio = chg_tot_ratio,
    pll_assets   = if_else(assets_tot > 0,
                           pll_pcl / assets_tot * 100, NA_real_),

    # Size tier
    asset_tier = case_when(
      assets_tot >= 10e9  ~ "1_Mega    (>$10B)",
      assets_tot >= 1e9   ~ "2_Large   ($1B-$10B)",
      assets_tot >= 500e6 ~ "3_Complex ($500M-$1B)",
      assets_tot >= 100e6 ~ "4_Mid     ($100M-$500M)",
      TRUE                ~ "5_Small   (<$100M)"
    ),
    near_threshold = as.integer(
      avg_assets_pre >= 400e6 & avg_assets_pre <= 600e6
    )

  )

# Growth rates (log-difference x100, identical to 0_Data_Prep.R)
df <- df |>
  arrange(cu_number, q_period_num) |>
  group_by(cu_number) |>
  mutate(
    asset_growth    = (ln_assets   - lag(ln_assets))   * 100,
    loan_growth     = (ln_loans    - lag(ln_loans))    * 100,
    mbl_growth      = (ln_mbl      - lag(ln_mbl))      * 100,
    networth_growth = (ln_networth - lag(ln_networth)) * 100
  ) |>
  ungroup()

message("  All variables constructed.")


# =============================================================================
# 10. SELECT FINAL VARIABLES
# =============================================================================
# Mirrors 0_Data_Prep.R Step 13 (spreads omitted — FRED not fetched here).

message("-- Step 8: Selecting final variables")

df_panel <- df |>
  select(
    cu_number, cu_name,
    year, quarter, q_period_num,
    state,
    complex, post_rbc, treat_post, event_time,
    avg_assets_pre, asset_tier, near_threshold,
    assets_tot, ln_assets, members,
    cecl_adopter, loan_to_asset,
    networth_ratio, cap_buffer, cap_buffer_cculr,
    well_capitalized, networth_tot, ln_networth,
    networth_growth, cap_ratio_pca,
    lns_tot, ln_loans,
    mbl_shr, re_shr, auto_shr, comm_shr,
    asset_growth, loan_growth, mbl_growth,
    dq_rate_var, chgoff_ratio, pll_assets,
    roa_var, nim, cof, yld_loans,
    outcome
  )

message(sprintf("  Variables: %d | Rows: %s", ncol(df_panel), comma(nrow(df_panel))))


# =============================================================================
# 11. DATA QUALITY DIAGNOSTICS
# =============================================================================

message("-- Step 9: Diagnostics")

cat("\n=== PANEL COVERAGE ===\n")
cat(sprintf("  Total obs           : %s\n", comma(nrow(df_panel))))
cat(sprintf("  Unique CUs (total)  : %s\n", comma(n_distinct(df_panel$cu_number))))
cat(sprintf("  Complex CUs         : %s\n",
            comma(n_distinct(df_panel$cu_number[df_panel$complex == 1]))))
cat(sprintf("  Period range        : %.1f to %.1f\n",
            min(df_panel$q_period_num, na.rm = TRUE),
            max(df_panel$q_period_num, na.rm = TRUE)))

# Obs count per sub-window for complex CUs
windows <- list(
  "Pre-crisis  (2004-2007)" = c(2004.1, 2007.4),
  "Crisis      (2008-2010)" = c(2008.1, 2010.4),
  "Post-crisis (2011-2017)" = c(2011.1, 2017.4),
  "RBC study   (2018-2025)" = c(2018.1, 2025.4)
)

cat("\n=== COMPLEX CU OBS BY SUB-WINDOW ===\n")
for (nm in names(windows)) {
  w <- windows[[nm]]
  n <- df_panel |>
    filter(complex == 1,
           q_period_num >= w[1], q_period_num <= w[2]) |>
    nrow()
  cat(sprintf("  %-30s: %s\n", nm, comma(n)))
}

# Key diagnostics for the crisis window
crisis_complex <- df_panel |>
  filter(complex == 1, q_period_num >= 2008.1, q_period_num <= 2010.4)

cat(sprintf("\n=== CRISIS WINDOW (2008-2010) — COMPLEX CUs ===\n"))
cat(sprintf("  CU-quarters: %s\n", comma(nrow(crisis_complex))))

if (nrow(crisis_complex) > 0) {

  # Coverage
  crisis_complex |>
    summarise(
      pct_nw_ratio = percent(mean(!is.na(networth_ratio)), 0.1),
      pct_roa_var  = percent(mean(!is.na(roa_var)),        0.1),
      pct_chgoff   = percent(mean(!is.na(chgoff_ratio)),   0.1),
      pct_dq_rate  = percent(mean(!is.na(dq_rate_var)),    0.1)
    ) |>
    pivot_longer(everything(), names_to = "variable", values_to = "coverage") |>
    print()

  # Annual summary
  cat("\n  Annual means — complex CUs:\n")
  crisis_complex |>
    group_by(year) |>
    summarise(
      n             = n(),
      mean_nw_ratio = round(mean(networth_ratio, na.rm = TRUE), 3),
      mean_roa      = round(mean(roa_var,         na.rm = TRUE), 3),
      mean_chgoff   = round(mean(chgoff_ratio,    na.rm = TRUE), 3),
      .groups = "drop"
    ) |>
    print()

  # NW ratio distribution at crisis trough
  cat("\n  NW ratio distribution (2008-2010 complex CUs):\n")
  crisis_complex |>
    pull(networth_ratio) |>
    quantile(probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
             na.rm = TRUE) |>
    round(3) |>
    print()

  # Pre-crisis steady-state ROA
  precrisis_roa <- df_panel |>
    filter(complex == 1, q_period_num >= 2004.1, q_period_num <= 2007.4) |>
    summarise(mean_roa = mean(roa_var, na.rm = TRUE)) |>
    pull(mean_roa)
  cat(sprintf("\n  Pre-crisis steady-state ROA (2004-2007): %.3f%%\n", precrisis_roa))

  # Institution-level peak-to-trough drawdown
  cu_drawdown_diag <- df_panel |>
    filter(complex == 1,
           q_period_num >= 2008.1,
           q_period_num <= 2010.4,
           !is.na(networth_ratio)) |>
    group_by(cu_number) |>
    arrange(q_period_num) |>
    summarise(
      nw_start  = first(networth_ratio),
      nw_trough = min(networth_ratio, na.rm = TRUE),
      drawdown  = nw_start - nw_trough,
      n_q       = n(),
      .groups   = "drop"
    ) |>
    filter(n_q >= 4, is.finite(drawdown), drawdown >= 0)

  cat(sprintf("\n  CUs with crisis drawdown data: %d\n", nrow(cu_drawdown_diag)))
  if (nrow(cu_drawdown_diag) > 0) {
    cat(sprintf("  Mean drawdown   : %.3fpp\n",
                mean(cu_drawdown_diag$drawdown, na.rm = TRUE)))
    cat(sprintf("  P50 drawdown    : %.3fpp\n",
                median(cu_drawdown_diag$drawdown, na.rm = TRUE)))
    cat(sprintf("  P90 drawdown    : %.3fpp\n",
                quantile(cu_drawdown_diag$drawdown, 0.90, na.rm = TRUE)))
    cat(sprintf("  P95 drawdown    : %.3fpp\n",
                quantile(cu_drawdown_diag$drawdown, 0.95, na.rm = TRUE)))
  }

} else {
  cat("  WARNING: Zero observations in crisis window.\n")
  cat("  The call_report.rds may not contain pre-2018 data.\n")
  cat("  Verify: min(df_raw$q_period_num, na.rm = TRUE)\n")
}


# =============================================================================
# 12. SAVE PANELS
# =============================================================================

message("-- Step 10: Saving panels")

dir.create(dirname(OUTPUT_FULL_RAW), showWarnings = FALSE, recursive = TRUE)

# Save unwinsorized (for 3F Component A distributional analysis)
saveRDS(df_panel, OUTPUT_FULL_RAW)
message(sprintf("  Saved (raw): %s  [%s rows, %d cols]",
                OUTPUT_FULL_RAW, comma(nrow(df_panel)), ncol(df_panel)))

# Winsorize and save
winsorize <- function(x, low = 0.01, high = 0.99) {
  q <- quantile(x, probs = c(low, high), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

winsor_vars <- intersect(
  c("networth_ratio", "cap_buffer", "cap_buffer_cculr",
    "mbl_shr", "re_shr", "auto_shr", "comm_shr",
    "asset_growth", "loan_growth", "mbl_growth",
    "dq_rate_var", "chgoff_ratio", "pll_assets",
    "roa_var", "nim", "cof"),
  names(df_panel)
)

df_panel_w <- df_panel |>
  mutate(across(all_of(winsor_vars), winsorize))

saveRDS(df_panel_w, OUTPUT_FULL)
message(sprintf("  Saved (win): %s  [%s rows, %d cols]",
                OUTPUT_FULL, comma(nrow(df_panel_w)), ncol(df_panel_w)))


# =============================================================================
# 13. INSTRUCTIONS FOR 3F
# =============================================================================

cat("\n=== UPDATE 3F_Capital_Adequacy_Stress_Test.R ===\n")
cat("  Change lines 70-71 from:\n")
cat("    PANEL_RAW_PATH <- 'data/analysis_panel_raw.rds'\n")
cat("    PANEL_PATH     <- 'data/analysis_panel.rds'\n")
cat("  To:\n")
cat("    PANEL_RAW_PATH <- 'data/analysis_panel_full_raw.rds'\n")
cat("    PANEL_PATH     <- 'data/analysis_panel_full.rds'\n")
cat("  All variable names are identical. No other changes required.\n")

message("\n== 0B_Crisis_Panel_Prep.R complete ==")
message("  Next: run 3F_Capital_Adequacy_Stress_Test.R with updated paths")
