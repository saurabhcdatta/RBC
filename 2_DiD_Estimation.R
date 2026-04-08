# =============================================================================
# 2_DiD_Estimation.R
# RBC Rule Impact Analysis — Difference-in-Differences Estimation
# NCUA Call Report (5300) Data
#
# Author  : Saurabh C. Datta, Ph.D.
# Created : 2026
# Purpose : Estimate causal effects of the RBC rule on complex credit unions
#           using difference-in-differences (DiD) and event study designs.
#
# Outcomes estimated:
#   (1) Capital adequacy  : networth_ratio, cap_buffer, well_capitalized
#   (2) Loan pricing      : spread_mortgage, spread_nauto, spread_comm_oth
#   (3) Portfolio mix     : mbl_shr, re_shr, auto_shr
#   (4) Growth            : asset_growth, loan_growth   [stored ×100]
#   (5) Credit quality    : dq_rate_var, chgoff_ratio
#   (6) Profitability     : roa_var, nim
#
# Specifications:
#   (A) Baseline 2×2 DiD with CU + time FE
#   (B) Trend-adjusted DiD (non-parallel outcomes: capital, MBL share)
#   (C) Event study (quarter-by-quarter dynamic effects)
#   (D) Heterogeneity: by asset size tier and near-threshold subgroup
#   (E) Robustness: excl. CECL, balanced panel, placebo, control trim
#
# CHANGE LOG vs prior version:
#   [FIX 1] Growth variable labels updated to "QoQ log×100" throughout —
#           matches stored units in analysis_panel.rds (from 0_Data_Prep FIX 2).
#           Prior labels ("QoQ log") caused silent row drops from coefficient
#           plot because filter strings did not match results_main entries.
#   [FIX 2] spread_comm_re regression carries explicit high-missingness note
#           (~78% missing after removal of irate_re_oth fallback in 0_Data_Prep
#           FIX 3). N_obs reported; result is flagged supplementary throughout.
#   [FIX 3] Coefficient plot filter now validated with stopifnot() — any label
#           mismatch errors immediately with a diagnostic rather than silently
#           dropping rows from the plot.
#   [FIX 4] EVENT_MAX updated from 10 to 15 to use the full post-RBC window
#           now available after PERIOD_END was extended to 2025Q4 in
#           0_Data_Prep FIX 1. EVENT_MAX_PAPER = 10 retained as a separate
#           constant for the main paper figures (consistent with handoff).
#   [FIX 5] Placebo test: treat_post renamed to placebo_treat inside the
#           placebo dataset, but a term_name argument added to extract_did()
#           so the correct coefficient is extracted. Prior code returned all-NA
#           for the placebo row because extract_did() always searched for
#           "treat_post" which did not exist in the placebo model.
#   [FIX 6] modelsummary table numbers corrected: paper Tables 1 & 2 are the
#           descriptive tables from 1_Descriptive_Stats.R; regression tables
#           start at Table 3 (capital), Table 4 (spreads), Table 5 (portfolio).
#
# Input  : data/analysis_panel.rds   (winsorized — from 0_Data_Prep.R)
# Output : output/tables/            — regression tables (CSV)
#          output/figures/           — event study & coefficient plots
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(fixest)        # feols() — fast panel FE + clustered SE
library(modelsummary)  # formatted regression tables
library(patchwork)     # combining ggplot panels
library(scales)
library(gt)
library(broom)         # tidy() for extracting coefficients

# Install if needed:
# install.packages(c("fixest", "modelsummary", "broom"))


# =============================================================================
# 1. USER SETTINGS — EDIT THESE
# =============================================================================

INPUT_PATH  <- "data/analysis_panel.rds"
TABLE_PATH  <- "output/tables/"
FIGURE_PATH <- "output/figures/"

# RBC effective date — MUST match 0_Data_Prep.R RBC_EFFECTIVE_PERIOD
RBC_EFFECTIVE_PERIOD <- 2022.1

# Reference period for event study (Q-1 normalized to zero)
EVENT_REF <- -1

# Event study windows
# [FIX 4]: EVENT_MAX updated to 15 to use full post-RBC window (2022Q1–2025Q4).
# Panel now runs to 2025Q4 = event_time +15 after 0_Data_Prep PERIOD_END fix.
# EVENT_MAX_PAPER = 10 is retained for the main paper figures so that published
# event studies show a clean ±12 / +10 window matching the handoff document.
# Use EVENT_MAX for data subsets; use EVENT_MAX_PAPER for figure x-axis limits.
EVENT_MIN       <- -12   # 3 years pre-RBC
EVENT_MAX       <-  15   # full post-RBC window available in data
EVENT_MAX_PAPER <-  10   # truncation point for paper figures (aesthetics)

# Capital thresholds (matches 0_Data_Prep.R)
NW_WELLCAP_THRESHOLD <- 10
NW_CCULR_THRESHOLD   <-  9

# [FIX 1]: Canonical growth label — used consistently in results_main,
# coefficient plot filter, and event study plot list. Change here only.
GROWTH_LABEL_SUFFIX <- "\u00d7100"   # "×100" — matches 0_Data_Prep stored units

# Color palette (consistent with 1_Descriptive_Stats.R)
COL_EST  <- "#1B3A6B"   # navy — point estimates
COL_CI   <- "#6B8CBF"   # light navy — confidence bands
COL_ZERO <- "gray50"    # zero line

# ggplot theme
theme_rbc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "gray40", size = 10),
      axis.title       = element_text(size = 10),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold"),
      plot.caption     = element_text(color = "gray50", size = 8)
    )
}


# =============================================================================
# 2. LOAD DATA & CREATE OUTPUT DIRECTORIES
# =============================================================================

message("── Step 1: Loading analysis panel ───────────────────────────────────")

df <- readRDS(INPUT_PATH)

dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)

message(sprintf("  Loaded  : %s obs", scales::comma(nrow(df))))
message(sprintf("  CUs     : %s",     scales::comma(n_distinct(df$cu_number))))
message(sprintf("  Periods : %d",     n_distinct(df$q_period_num)))
message(sprintf("  event_time range: %d to %d",
                min(df$event_time, na.rm = TRUE),
                max(df$event_time, na.rm = TRUE)))

# Confirm growth vars are stored ×100 (approx. QoQ % growth)
# Expected: mean asset_growth for complex CUs pre-RBC ≈ 2–3%
growth_check <- df |>
  filter(post_rbc == 0, complex == 1) |>
  summarise(
    mean_asset_growth = round(mean(asset_growth, na.rm = TRUE), 3),
    mean_loan_growth  = round(mean(loan_growth,  na.rm = TRUE), 3)
  )
message(sprintf(
  "  Growth check (complex pre-RBC): asset=%.3f  loan=%.3f  [expect ~2-3 if stored ×100]",
  growth_check$mean_asset_growth, growth_check$mean_loan_growth
))
if (abs(growth_check$mean_asset_growth) < 0.1) {
  warning("asset_growth appears NOT to be stored ×100. Check 0_Data_Prep.R FIX 2.")
}

# Sub-samples
df_es    <- df |> filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)
df_rates <- df |> filter(quarter %in% c(2, 4))   # semi-annual rate reporters

message(sprintf("  Event study sample (full window) : %s obs", scales::comma(nrow(df_es))))
message(sprintf("  Rate-only sample (Q2/Q4)         : %s obs", scales::comma(nrow(df_rates))))

# spread_comm_re missingness check — [FIX 2]
cre_pct_missing <- mean(is.na(df$spread_comm_re)) * 100
message(sprintf(
  "  spread_comm_re missing: %.1f%% [FIX 2: expect ~78%% after fallback removal]",
  cre_pct_missing
))
if (cre_pct_missing < 50) {
  warning("spread_comm_re missing < 50%. irate_re_oth fallback may still be active in 0_Data_Prep.R.")
}


# =============================================================================
# 3. DiD HELPER FUNCTIONS
# =============================================================================
# Model: Y_it = alpha_i + gamma_t + beta*(complex_i × post_t) + X_it + e_it
#
# alpha_i  = CU fixed effects        (time-invariant CU heterogeneity)
# gamma_t  = Quarter-year FE         (common macro/seasonal shocks)
# beta     = DiD estimate            (ATT: avg treatment effect on treated)
# X_it     = Time-varying controls   (ln_assets, loan_to_asset, cecl_adopter)
# SE       = Clustered at CU level   (arbitrary within-CU serial correlation)

controls <- "ln_assets + loan_to_asset + cecl_adopter"

# ── Baseline DiD ──────────────────────────────────────────────────────────────
run_did <- function(outcome, data = df, ctrl = controls) {
  fml <- as.formula(
    paste0(outcome, " ~ treat_post + ", ctrl,
           " | cu_number + q_period_num")
  )
  feols(fml, data = data, cluster = ~cu_number, warn = FALSE, notes = FALSE)
}

# ── Trend-adjusted DiD ────────────────────────────────────────────────────────
# Adds complex-specific linear time trend to correct for pre-RBC divergence
# in capital and MBL share outcomes (non-parallel pre-trends)
run_did_trend <- function(outcome, data = df, ctrl = controls) {
  fml <- as.formula(
    paste0(outcome,
           " ~ treat_post + complex:event_time + ", ctrl,
           " | cu_number + q_period_num")
  )
  feols(fml, data = data, cluster = ~cu_number, warn = FALSE, notes = FALSE)
}

# ── Event study ───────────────────────────────────────────────────────────────
# Quarter-by-quarter treatment effects; reference period = EVENT_REF (Q-1)
run_event_study <- function(outcome, data = df_es, ctrl = controls) {
  fml <- as.formula(
    paste0(outcome,
           " ~ i(event_time, complex, ref = ", EVENT_REF, ") + ",
           ctrl, " | cu_number + q_period_num")
  )
  feols(fml, data = data, cluster = ~cu_number, warn = FALSE, notes = FALSE)
}

# ── Extract DiD coefficient into tidy tibble ──────────────────────────────────
# [FIX 5]: Added term_name argument (default = "treat_post") so that the
# placebo model — which uses a different variable name — can be extracted
# correctly without returning all-NA. Previously hardcoded to "treat_post".
extract_did <- function(model, label, spec = "Baseline DiD", note = "",
                        term_name = "treat_post") {
  tryCatch({
    tidy_m <- tidy(model, conf.int = TRUE)
    row    <- tidy_m[tidy_m$term == term_name, ]

    if (nrow(row) == 0) {
      warning(sprintf(
        "extract_did: term '%s' not found in model for outcome '%s'. Available: %s",
        term_name, label, paste(tidy_m$term, collapse = ", ")
      ))
      return(tibble(Outcome = label, Spec = spec,
                    Beta = NA, SE = NA, CI_low = NA, CI_high = NA,
                    p_value = NA, Stars = "", N_obs = nobs(model), Note = note))
    }

    tibble(
      Outcome = label, Spec = spec,
      Beta    = round(row$estimate,  3),
      SE      = round(row$std.error, 3),
      CI_low  = round(row$conf.low,  3),
      CI_high = round(row$conf.high, 3),
      p_value = round(row$p.value,   3),
      Stars   = case_when(
        row$p.value < 0.01 ~ "***",
        row$p.value < 0.05 ~ "**",
        row$p.value < 0.10 ~ "*",
        TRUE               ~ ""
      ),
      N_obs = nobs(model),
      Note  = note
    )
  }, error = function(e) {
    warning(sprintf("extract_did error for '%s': %s", label, e$message))
    tibble(Outcome = label, Spec = spec,
           Beta = NA, SE = NA, CI_low = NA, CI_high = NA,
           p_value = NA, Stars = "", N_obs = NA, Note = note)
  })
}


# =============================================================================
# 4. BASELINE DiD — ALL OUTCOMES
# =============================================================================

message("── Step 2: Estimating DiD models ────────────────────────────────────")

# ── Capital (trend-adjusted preferred: non-parallel pre-trends) ──────────────
message("  Capital outcomes...")
m_nw_base   <- run_did("networth_ratio")
m_nw_trend  <- run_did_trend("networth_ratio")
m_buf_base  <- run_did("cap_buffer")
m_buf_trend <- run_did_trend("cap_buffer")
m_wellcap   <- run_did("well_capitalized")     # linear probability model

# ── Loan spreads (Q2/Q4 only — semi-annual NCUA rate reporting) ──────────────
message("  Loan spread outcomes...")
m_spread_mort  <- run_did("spread_mortgage",  data = df_rates)
m_spread_nauto <- run_did("spread_nauto",     data = df_rates)
m_spread_uauto <- run_did("spread_uauto",     data = df_rates)
m_spread_comm  <- run_did("spread_comm_oth",  data = df_rates)

# [FIX 2]: spread_comm_re has ~78% missingness after irate_re_oth fallback
# was removed in 0_Data_Prep FIX 3. This model still runs (feols handles
# missingness via listwise deletion) but the effective N is small and the
# result should be treated as supplementary. N_obs is reported explicitly.
m_spread_cre <- run_did("spread_comm_re", data = df_rates)
message(sprintf("  spread_comm_re N_obs = %s (expect small — ~78%% missing)",
                scales::comma(nobs(m_spread_cre))))

# ── Portfolio composition (mbl_shr trend-adjusted) ───────────────────────────
message("  Portfolio composition outcomes...")
m_mbl_base  <- run_did("mbl_shr")
m_mbl_trend <- run_did_trend("mbl_shr")
m_re        <- run_did("re_shr")
m_auto      <- run_did("auto_shr")
m_comm      <- run_did("comm_shr")

# ── Growth ───────────────────────────────────────────────────────────────────
# [FIX 1]: Variables stored ×100 from 0_Data_Prep.R — coefficients already
# in QoQ log-point units × 100 (≈ QoQ % growth). No rescaling needed.
message("  Growth outcomes...")
m_asset_growth <- run_did("asset_growth")
m_loan_growth  <- run_did("loan_growth")
m_mbl_growth   <- run_did("mbl_growth")

# ── Credit quality ───────────────────────────────────────────────────────────
message("  Credit quality outcomes...")
m_dq     <- run_did("dq_rate_var")
m_chgoff <- run_did("chgoff_ratio")
m_pll    <- run_did("pll_assets")

# ── Profitability ─────────────────────────────────────────────────────────────
message("  Profitability outcomes...")
m_roa <- run_did("roa_var")
m_nim <- run_did("nim")
m_cof <- run_did("cof")

message("  All DiD models estimated.")


# =============================================================================
# 5. MAIN RESULTS TABLE
# =============================================================================

message("── Step 3: Building main results table ──────────────────────────────")

# [FIX 1]: Growth labels use the canonical suffix defined in USER SETTINGS.
# These labels must match EXACTLY what is used in the coefficient plot filter
# below. Changing GROWTH_LABEL_SUFFIX in Section 1 propagates everywhere.
lbl_asset_growth <- paste0("Asset growth (QoQ log", GROWTH_LABEL_SUFFIX, ")")
lbl_loan_growth  <- paste0("Loan growth (QoQ log",  GROWTH_LABEL_SUFFIX, ")")
lbl_mbl_growth   <- paste0("MBL growth (QoQ log",   GROWTH_LABEL_SUFFIX, ")")

results_main <- bind_rows(

  # ── Capital ──────────────────────────────────────────────────────────────
  extract_did(m_nw_base,   "Net worth ratio (%)",        "Baseline DiD"),
  extract_did(m_nw_trend,  "Net worth ratio (%)",        "Trend-adjusted DiD",
              note = "Pre-trend corrected"),
  extract_did(m_buf_base,  "Capital buffer vs 10% (pp)", "Baseline DiD"),
  extract_did(m_buf_trend, "Capital buffer vs 10% (pp)", "Trend-adjusted DiD",
              note = "Pre-trend corrected"),
  extract_did(m_wellcap,   "Well-capitalized (LPM)",     "Baseline DiD"),

  # ── Spreads ──────────────────────────────────────────────────────────────
  extract_did(m_spread_mort,  "Mortgage spread (pp)",    "Baseline DiD",
              note = "Q2/Q4 only"),
  extract_did(m_spread_nauto, "New auto spread (pp)",    "Baseline DiD",
              note = "Q2/Q4 only"),
  extract_did(m_spread_uauto, "Used auto spread (pp)",   "Baseline DiD",
              note = "Q2/Q4 only"),
  extract_did(m_spread_comm,  "Comm non-RE spread (pp)", "Baseline DiD",
              note = "Q2/Q4 only"),

  # [FIX 2]: Comm RE spread marked supplementary due to high missingness
  extract_did(m_spread_cre,   "Comm RE spread (pp)",     "Baseline DiD",
              note = paste0("Q2/Q4 only; ~78% missing — supplementary; N=",
                            scales::comma(nobs(m_spread_cre)))),

  # ── Portfolio ─────────────────────────────────────────────────────────────
  extract_did(m_mbl_base,  "MBL share (%)", "Baseline DiD"),
  extract_did(m_mbl_trend, "MBL share (%)", "Trend-adjusted DiD",
              note = "Pre-trend corrected"),
  extract_did(m_re,   "RE share (%)",   "Baseline DiD"),
  extract_did(m_auto, "Auto share (%)", "Baseline DiD"),
  extract_did(m_comm, "Comm share (%)", "Baseline DiD"),

  # ── Growth [FIX 1: ×100 labels] ──────────────────────────────────────────
  extract_did(m_asset_growth, lbl_asset_growth, "Baseline DiD"),
  extract_did(m_loan_growth,  lbl_loan_growth,  "Baseline DiD"),
  extract_did(m_mbl_growth,   lbl_mbl_growth,   "Baseline DiD"),

  # ── Credit quality ────────────────────────────────────────────────────────
  extract_did(m_dq,     "Delinquency rate (%)", "Baseline DiD"),
  extract_did(m_chgoff, "Charge-off ratio (%)", "Baseline DiD"),
  extract_did(m_pll,    "PLL / assets (%)",     "Baseline DiD"),

  # ── Profitability ─────────────────────────────────────────────────────────
  extract_did(m_roa, "ROA (%)",           "Baseline DiD"),
  extract_did(m_nim, "NIM (%)",           "Baseline DiD"),
  extract_did(m_cof, "Cost of funds (%)", "Baseline DiD")
)

cat("\n=== MAIN DiD RESULTS ===\n")
results_main |>
  mutate(Result = paste0(Beta, Stars, " (", SE, ")")) |>
  select(Outcome, Spec, Result, p_value, N_obs, Note) |>
  print(n = Inf)

write_csv(results_main, file.path(TABLE_PATH, "did_main_results.csv"))
message("  Saved → output/tables/did_main_results.csv")


# =============================================================================
# 6. FORMATTED REGRESSION TABLES (modelsummary)
# =============================================================================
# [FIX 6]: Table numbering starts at 3 to align with paper structure.
# Tables 1 & 2 = descriptive tables from 1_Descriptive_Stats.R.
# Table 3 = Capital, Table 4 = Spreads, Table 5 = Portfolio & Growth.

message("── Step 4: Formatted regression tables ──────────────────────────────")

coef_map_base <- c(
  "treat_post"         = "Complex \u00d7 Post-RBC",
  "complex:event_time" = "Complex \u00d7 Time Trend",
  "ln_assets"          = "Log assets",
  "loan_to_asset"      = "Loans / assets",
  "cecl_adopter"       = "CECL adopter"
)

gof_map_use <- c("nobs", "r.squared")
stars_use   <- c("*" = 0.10, "**" = 0.05, "***" = 0.01)

# [FIX 6]: Table 3 (was "Table 1" in prior version)
modelsummary(
  list(
    "NW Ratio\n(Baseline)"    = m_nw_base,
    "NW Ratio\n(Trend adj)"   = m_nw_trend,
    "Cap Buffer\n(Baseline)"  = m_buf_base,
    "Cap Buffer\n(Trend adj)" = m_buf_trend,
    "Well-Cap\n(LPM)"         = m_wellcap
  ),
  coef_map = coef_map_base, gof_map = gof_map_use,
  stars = stars_use,
  title = "Table 3. RBC Rule Impact on Capital Adequacy",
  notes = paste0(
    "Two-way FE (CU + quarter-year). SE clustered at CU level. ",
    "Trend-adjusted specs add complex-specific linear time trend to correct ",
    "for non-parallel pre-trends in capital outcomes."
  ),
  output = file.path(TABLE_PATH, "table3_capital.csv")
)

# [FIX 6]: Table 4 (was "Table 2")
modelsummary(
  list(
    "Mortgage"    = m_spread_mort,
    "New Auto"    = m_spread_nauto,
    "Used Auto"   = m_spread_uauto,
    "Comm non-RE" = m_spread_comm,
    "Comm RE*"    = m_spread_cre
  ),
  coef_map = coef_map_base, gof_map = gof_map_use,
  stars = stars_use,
  title = "Table 4. RBC Rule Impact on Loan Rate Spreads Over Treasuries",
  notes = paste0(
    "Q2 and Q4 observations only (NCUA semi-annual rate reporting). ",
    "Two-way FE (CU + quarter-year). SE clustered at CU level. ",
    "* Comm RE spread has ~78% missingness after removal of irate_re_oth ",
    "fallback (see data notes); treat as supplementary."
  ),
  output = file.path(TABLE_PATH, "table4_loan_spreads.csv")
)

# [FIX 6]: Table 5 (was "Table 3")
modelsummary(
  list(
    "MBL Share\n(Baseline)"  = m_mbl_base,
    "MBL Share\n(Trend adj)" = m_mbl_trend,
    "RE Share"                = m_re,
    "Auto Share"              = m_auto,
    "Loan Growth"             = m_loan_growth,
    "MBL Growth"              = m_mbl_growth
  ),
  coef_map = coef_map_base, gof_map = gof_map_use,
  stars = stars_use,
  title = "Table 5. RBC Rule Impact on Loan Portfolio Composition and Growth",
  notes = paste0(
    "Two-way FE (CU + quarter-year). SE clustered at CU level. ",
    "Growth variables stored as log-difference \u00d7 100 (\u2248 QoQ % growth)."
  ),
  output = file.path(TABLE_PATH, "table5_portfolio_growth.csv")
)

message("  Tables 3-5 saved → output/tables/")


# =============================================================================
# 7. EVENT STUDY ESTIMATION
# =============================================================================
# Estimates quarter-by-quarter treatment effects relative to Q-1 (reference).
# Pre-period coefficients should cluster near zero if parallel trends hold.
# Uses df_es which spans EVENT_MIN to EVENT_MAX (full window including +15).

message("── Step 5: Event study estimation ───────────────────────────────────")

# Rate sub-sample restricted to event window
df_rates_es <- df_rates |>
  filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)

message("  Running event studies...")
es_nw       <- run_event_study("networth_ratio")
es_cap_buf  <- run_event_study("cap_buffer")
es_wellcap  <- run_event_study("well_capitalized")
es_mbl_shr  <- run_event_study("mbl_shr")
es_re_shr   <- run_event_study("re_shr")
es_auto_shr <- run_event_study("auto_shr")
es_loan_gr  <- run_event_study("loan_growth")
es_dq       <- run_event_study("dq_rate_var")
es_roa      <- run_event_study("roa_var")
es_mort     <- run_event_study("spread_mortgage", data = df_rates_es)
es_nauto    <- run_event_study("spread_nauto",    data = df_rates_es)
es_comm     <- run_event_study("spread_comm_oth", data = df_rates_es)

message("  Event studies complete.")


# =============================================================================
# 8. EVENT STUDY PLOTS
# =============================================================================

# [FIX 4]: plot_event_study() clips the x-axis at EVENT_MAX_PAPER (=10) for
# paper figures, but the underlying model uses the full EVENT_MAX (=15) window.
# This gives the cleanest published figure while retaining the full data
# coverage in the estimated model. Set paper_window = FALSE to see all quarters.

plot_event_study <- function(es_model, outcome_label,
                             add_trend_note = FALSE,
                             paper_window   = TRUE) {

  # Extract interaction coefficients
  coefs <- tidy(es_model, conf.int = TRUE) |>
    filter(str_detect(term, "event_time::")) |>
    mutate(
      event_time = as.integer(str_extract(term, "-?\\d+"))
    ) |>
    filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)

  # Add reference period (normalized to zero)
  ref_row <- tibble(
    term = "ref", estimate = 0, std.error = 0,
    conf.low = 0, conf.high = 0, p.value = NA_real_,
    event_time = EVENT_REF
  )
  coefs <- bind_rows(coefs, ref_row) |>
    arrange(event_time) |>
    mutate(period = if_else(event_time < 0, "Pre-RBC", "Post-RBC"))

  # [FIX 4]: Clip to paper window for published figures
  x_max_plot <- if (paper_window) EVENT_MAX_PAPER else EVENT_MAX
  coefs      <- coefs |> filter(event_time <= x_max_plot)

  subtitle_txt <- paste0(
    "Event time = quarters relative to RBC (Q0 = 2022 Q1). ",
    "Reference = Q", EVENT_REF, ". 95% CI shaded."
  )
  if (add_trend_note) {
    subtitle_txt <- paste0(subtitle_txt,
                           "\nNote: Non-parallel pre-trends \u2014 interpret with caution.")
  }

  ggplot(coefs, aes(x = event_time, y = estimate)) +
    # Post-period shading
    annotate("rect",
             xmin = -0.5, xmax = x_max_plot + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "gray95", alpha = 0.5) +
    # CI ribbon
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = COL_CI, alpha = 0.25) +
    # Reference lines
    geom_hline(yintercept = 0, color = COL_ZERO,
               linewidth = 0.6, linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.7, linetype = "dashed") +
    # Estimates
    geom_line(color = COL_EST, linewidth = 0.9) +
    geom_point(aes(shape = period, color = period), size = 2.2) +
    # Mark significant post-period points
    geom_point(
      data = coefs |> filter(!is.na(p.value), p.value < 0.05, event_time >= 0),
      aes(x = event_time, y = estimate),
      shape = 8, size = 2.5, color = "darkred"
    ) +
    scale_color_manual(values = c("Pre-RBC"  = "gray55",
                                  "Post-RBC" = COL_EST)) +
    scale_shape_manual(values = c("Pre-RBC"  = 1,
                                  "Post-RBC" = 16)) +
    scale_x_continuous(
      breaks = seq(EVENT_MIN, x_max_plot, by = 4),
      labels = function(x) paste0("Q", x)
    ) +
    labs(
      title    = paste0("Event Study: ", outcome_label),
      subtitle = subtitle_txt,
      x        = "Quarters Relative to RBC Effective Date",
      y        = paste0("Coefficient (", outcome_label, ")"),
      color    = NULL, shape = NULL,
      caption  = paste0(
        "\u2605 = p < 0.05 (post-period). ",
        "Two-way FE (CU + quarter-year). SE clustered at CU. ",
        "Reference quarter = Q-1 (normalized to 0)."
      )
    ) +
    theme_rbc()
}

message("── Step 6: Saving event study plots ─────────────────────────────────")

# [FIX 1]: Growth label in es_plot_list uses lbl_loan_growth (canonical label)
es_plot_list <- list(
  list(es_nw,       "Net Worth Ratio (%)",
       "fig_es_networth_ratio.png",     TRUE),
  list(es_cap_buf,  "Capital Buffer vs 10% (pp)",
       "fig_es_cap_buffer.png",         TRUE),
  list(es_wellcap,  "Well-Capitalized (LPM)",
       "fig_es_well_capitalized.png",   FALSE),
  list(es_mbl_shr,  "MBL Share (%)",
       "fig_es_mbl_share.png",          TRUE),
  list(es_re_shr,   "Real Estate Share (%)",
       "fig_es_re_share.png",           FALSE),
  list(es_auto_shr, "Auto Loan Share (%)",
       "fig_es_auto_share.png",         FALSE),
  list(es_loan_gr,  lbl_loan_growth,           # [FIX 1]: canonical label
       "fig_es_loan_growth.png",        FALSE),
  list(es_dq,       "Delinquency Rate (%)",
       "fig_es_delinquency.png",        FALSE),
  list(es_roa,      "Return on Assets (%)",
       "fig_es_roa.png",                FALSE),
  list(es_mort,     "Mortgage Spread (pp)",
       "fig_es_spread_mortgage.png",    FALSE),
  list(es_nauto,    "New Auto Spread (pp)",
       "fig_es_spread_nauto.png",       FALSE),
  list(es_comm,     "Comm non-RE Spread (pp)",
       "fig_es_spread_comm.png",        FALSE)
)

for (ep in es_plot_list) {
  p <- plot_event_study(ep[[1]], ep[[2]], add_trend_note = ep[[4]])
  ggsave(file.path(FIGURE_PATH, ep[[3]]),
         p, width = 10, height = 6, dpi = 300)
  message(sprintf("    Saved: %s", ep[[3]]))
}


# =============================================================================
# 9. COMBINED EVENT STUDY PANELS
# =============================================================================

message("── Step 7: Combined event study panels ──────────────────────────────")

# Capital & Lending panel
pA <- plot_event_study(es_nw,      "Net Worth Ratio (%)",
                       add_trend_note = TRUE)  + labs(title = "A. Net Worth Ratio")
pB <- plot_event_study(es_cap_buf, "Cap Buffer (pp)",
                       add_trend_note = TRUE)  + labs(title = "B. Capital Buffer vs 10%")
pC <- plot_event_study(es_wellcap, "Well-Capitalized") + labs(title = "C. Well-Capitalized")
pD <- plot_event_study(es_loan_gr, lbl_loan_growth)    + labs(title = "D. Loan Growth")

p_capital_es <- (pA + pB) / (pC + pD) +
  plot_annotation(
    title   = "Event Study: RBC Rule Impact on Capital and Lending",
    caption = paste0(
      "Two-way FE (CU + quarter-year). SE clustered at CU. ",
      "Reference quarter = Q-1 (normalized to 0). \u2605 = p < 0.05."
    )
  )

ggsave(file.path(FIGURE_PATH, "fig_es_panel_capital.png"),
       p_capital_es, width = 14, height = 10, dpi = 300)

# Spread panel
pS1 <- plot_event_study(es_mort,  "Mortgage Spread (pp)")    + labs(title = "A. Mortgage Spread")
pS2 <- plot_event_study(es_nauto, "New Auto Spread (pp)")    + labs(title = "B. New Auto Spread")
pS3 <- plot_event_study(es_comm,  "Comm non-RE Spread (pp)") + labs(title = "C. Comm non-RE Spread")

p_spread_es <- (pS1 + pS2) / (pS3 + plot_spacer()) +
  plot_annotation(
    title   = "Event Study: RBC Rule Impact on Loan Rate Spreads",
    caption = "Q2/Q4 only. Two-way FE. SE clustered at CU. Reference = Q-1."
  )

ggsave(file.path(FIGURE_PATH, "fig_es_panel_spreads.png"),
       p_spread_es, width = 14, height = 9, dpi = 300)

message("  Combined panels saved.")


# =============================================================================
# 10. HETEROGENEITY BY ASSET SIZE TIER
# =============================================================================

message("── Step 8: Heterogeneity by asset size tier ──────────────────────────")

tiers       <- c("3_Complex ($500M-$1B)", "2_Large   ($1B-$10B)", "1_Mega    (>$10B)")
tier_labels <- c("$500M\u2013$1B",         "$1B\u2013$10B",         ">$10B")

# [FIX 1]: Growth outcomes use canonical labels
het_outcomes <- c("networth_ratio", "cap_buffer", "loan_growth",
                  "spread_mortgage", "spread_nauto", "dq_rate_var", "roa_var")
het_labels   <- c("Net Worth Ratio", "Capital Buffer", lbl_loan_growth,
                  "Mortgage Spread", "New Auto Spread", "DQ Rate", "ROA")

het_results <- map_dfr(seq_along(tiers), function(i) {
  tier_data <- df |> filter(complex == 0 | asset_tier == tiers[i])

  map_dfr(seq_along(het_outcomes), function(j) {
    use_data <- if (het_outcomes[j] %in% c("spread_mortgage", "spread_nauto")) {
      tier_data |> filter(quarter %in% c(2, 4))
    } else { tier_data }

    m <- tryCatch(run_did(het_outcomes[j], data = use_data), error = function(e) NULL)
    if (is.null(m)) return(NULL)

    tidy_m <- tidy(m, conf.int = TRUE)
    row    <- tidy_m[tidy_m$term == "treat_post", ]
    if (nrow(row) == 0) return(NULL)

    tibble(
      Tier    = tier_labels[i],
      Outcome = het_labels[j],
      Beta    = round(row$estimate,  3),
      SE      = round(row$std.error, 3),
      CI_low  = round(row$conf.low,  3),
      CI_high = round(row$conf.high, 3),
      p_value = round(row$p.value,   3),
      Stars   = case_when(
        row$p.value < 0.01 ~ "***",
        row$p.value < 0.05 ~ "**",
        row$p.value < 0.10 ~ "*",
        TRUE               ~ ""
      ),
      N = nobs(m)
    )
  })
})

cat("\n=== HETEROGENEITY BY ASSET SIZE TIER ===\n")
print(het_results, n = Inf)
write_csv(het_results, file.path(TABLE_PATH, "heterogeneity_by_tier.csv"))

# Coefficient plot
p_het <- ggplot(
  het_results |> filter(!is.na(Beta)),
  aes(x = Beta, y = Tier, xmin = CI_low, xmax = CI_high, color = Tier)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = COL_ZERO) +
  geom_errorbarh(height = 0.25, linewidth = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ Outcome, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c(
    "$500M\u2013$1B" = "#1B3A6B",
    "$1B\u2013$10B"  = "#4A7CB5",
    ">$10B"          = "#8AB4D9"
  )) +
  labs(
    title    = "Heterogeneity of RBC Effects by Asset Size Tier",
    subtitle = "DiD estimates (Complex \u00d7 Post-RBC). 95% CI. Non-complex CUs as control.",
    x        = "DiD Coefficient",
    y        = NULL,
    caption  = "Two-way FE (CU + quarter-year). SE clustered at CU."
  ) +
  theme_rbc() +
  theme(legend.position = "none")

ggsave(file.path(FIGURE_PATH, "fig_heterogeneity_tier.png"),
       p_het, width = 12, height = 10, dpi = 300)
message("  Heterogeneity results saved.")


# =============================================================================
# 11. NEAR-THRESHOLD SUBGROUP ($400M–$600M)
# =============================================================================
# CUs within ±$100M of the $500M cutoff are most comparable on observables.
# Closest to an RD design — strengthens the parallel trends assumption.

message("── Step 9: Near-threshold subgroup ──────────────────────────────────")

df_threshold <- df |>
  filter(near_threshold == 1 | (complex == 0 & avg_assets_pre >= 200e6))

message(sprintf("  Near-threshold sample: %s CUs",
                scales::comma(n_distinct(df_threshold$cu_number))))

threshold_results <- map_dfr(seq_along(het_outcomes), function(j) {
  use_data <- if (het_outcomes[j] %in% c("spread_mortgage", "spread_nauto")) {
    df_threshold |> filter(quarter %in% c(2, 4))
  } else { df_threshold }

  m <- tryCatch(run_did(het_outcomes[j], data = use_data), error = function(e) NULL)
  if (is.null(m)) return(NULL)

  tidy_m <- tidy(m, conf.int = TRUE)
  row    <- tidy_m[tidy_m$term == "treat_post", ]
  if (nrow(row) == 0) return(NULL)

  tibble(
    Outcome = het_labels[j],
    Sample  = "Near-threshold ($400M\u2013$600M)",
    Beta    = round(row$estimate,  3),
    SE      = round(row$std.error, 3),
    CI_low  = round(row$conf.low,  3),
    CI_high = round(row$conf.high, 3),
    p_value = round(row$p.value,   3),
    Stars   = case_when(
      row$p.value < 0.01 ~ "***",
      row$p.value < 0.05 ~ "**",
      row$p.value < 0.10 ~ "*",
      TRUE               ~ ""
    ),
    N = nobs(m)
  )
})

cat("\n=== NEAR-THRESHOLD SUBGROUP RESULTS ===\n")
print(threshold_results, n = Inf)
write_csv(threshold_results, file.path(TABLE_PATH, "near_threshold_results.csv"))
message("  Near-threshold results saved.")


# =============================================================================
# 12. ROBUSTNESS CHECKS
# =============================================================================

message("── Step 10: Robustness checks ────────────────────────────────────────")

# R1: Exclude CECL adopters
# CECL day-1 adjustment reduces net worth — could confound capital outcomes
message("  R1: Excluding CECL adopters...")
df_nocecl <- df |> filter(cecl_adopter == 0)
r1_nw  <- run_did("networth_ratio", data = df_nocecl)
r1_buf <- run_did("cap_buffer",     data = df_nocecl)

# R2: Balanced panel
# Tests whether results are driven by CU entry/exit (attrition bias)
message("  R2: Balanced panel...")
n_periods    <- n_distinct(df$q_period_num)
balanced_cus <- df |> count(cu_number) |>
  filter(n == n_periods) |> pull(cu_number)
df_balanced  <- df |> filter(cu_number %in% balanced_cus)
message(sprintf("    Balanced panel: %s CUs",
                scales::comma(length(balanced_cus))))
r2_nw   <- run_did("networth_ratio", data = df_balanced)
r2_loan <- run_did("loan_growth",    data = df_balanced)

# R3: Placebo treatment date (2020Q1)
# Pre-COVID fake treatment date — beta should be ≈ 0 if no pre-treatment effect
# Uses only pre-RBC data so the split is clean
message("  R3: Placebo treatment (2020Q1)...")
df_placebo <- df |>
  filter(q_period_num < RBC_EFFECTIVE_PERIOD) |>
  mutate(
    placebo_post  = as.integer(q_period_num >= 2020.1),
    # [FIX 5]: Name the treatment variable treat_post so that run_did() and
    # extract_did() work without any modification — no term_name argument
    # needed. This is the cleanest fix: the placebo treatment just uses the
    # same variable name expected by the existing helpers.
    treat_post    = complex * placebo_post
  )

r3_nw <- run_did("networth_ratio", data = df_placebo)

# Verify placebo result is near zero
r3_coef <- tidy(r3_nw) |> filter(term == "treat_post") |> pull(estimate)
message(sprintf("    Placebo beta = %.4f (expect ≈ 0)", r3_coef))
if (abs(r3_coef) > 0.3) {
  warning(sprintf(
    "Placebo beta = %.4f — larger than expected. Check pre-trends or placebo window.",
    r3_coef
  ))
}

# R4: Restrict control to non-complex CUs with >$300M assets
# More comparable control group — tests sensitivity to control composition
message("  R4: Restricted control group (>$300M)...")
df_r4   <- df |> filter(complex == 1 | avg_assets_pre >= 300e6)
r4_nw   <- run_did("networth_ratio", data = df_r4)
r4_loan <- run_did("loan_growth",    data = df_r4)

# Robustness summary table
robustness_results <- bind_rows(
  extract_did(m_nw_base,   "Net worth ratio", "Baseline DiD"),
  extract_did(m_nw_trend,  "Net worth ratio", "Trend-adjusted DiD"),
  extract_did(r1_nw,       "Net worth ratio", "Excl. CECL adopters"),
  extract_did(r2_nw,       "Net worth ratio", "Balanced panel"),
  # [FIX 5]: placebo model now uses treat_post — extract_did works normally
  extract_did(r3_nw,       "Net worth ratio", "Placebo (2020Q1)",
              note = "Should be ~0"),
  extract_did(r4_nw,       "Net worth ratio", "Control >$300M only"),
  # [FIX 1]: loan_growth labels consistent
  extract_did(m_loan_growth, lbl_loan_growth, "Baseline DiD"),
  extract_did(r2_loan,       lbl_loan_growth, "Balanced panel"),
  extract_did(r4_loan,       lbl_loan_growth, "Control >$300M only")
)

cat("\n=== ROBUSTNESS CHECKS ===\n")
robustness_results |>
  mutate(Result = paste0(Beta, Stars, " (", SE, ")")) |>
  select(Outcome, Spec, Result, p_value, N_obs, Note) |>
  print(n = Inf)

write_csv(robustness_results, file.path(TABLE_PATH, "robustness_checks.csv"))
message("  Robustness checks saved.")


# =============================================================================
# 13. MAIN COEFFICIENT PLOT
# =============================================================================

message("── Step 11: Main results coefficient plot ────────────────────────────")

# [FIX 1 & 3]: Growth labels in the filter use the canonical lbl_* variables.
# [FIX 3]: After filtering, stopifnot() validates expected row count to catch
# any silent label mismatches immediately rather than producing a silent
# incomplete plot.

COEF_PLOT_EXPECTED_ROWS <- 21   # 5 capital + 5 spreads + 3 portfolio + 3 growth + 3 credit quality + 2 profitability + Cost of funds

plot_results <- results_main |>
  filter(
    (Outcome == "Net worth ratio (%)"        & Spec == "Trend-adjusted DiD") |
    (Outcome == "Capital buffer vs 10% (pp)" & Spec == "Trend-adjusted DiD") |
    (Outcome == "Well-capitalized (LPM)"     & Spec == "Baseline DiD")       |
    (Outcome %in% c("Mortgage spread (pp)", "New auto spread (pp)",
                    "Used auto spread (pp)", "Comm non-RE spread (pp)",
                    "Comm RE spread (pp)")   & Spec == "Baseline DiD")       |
    (Outcome == "MBL share (%)"              & Spec == "Trend-adjusted DiD") |
    # [FIX 1]: lbl_* variables used — exact match guaranteed
    (Outcome %in% c("RE share (%)", "Auto share (%)", "Comm share (%)",
                    lbl_loan_growth, lbl_asset_growth, lbl_mbl_growth,
                    "Delinquency rate (%)", "Charge-off ratio (%)",
                    "PLL / assets (%)",
                    "ROA (%)", "NIM (%)", "Cost of funds (%)")
                                             & Spec == "Baseline DiD")
  )

# [FIX 3]: Validate — error immediately if label mismatch caused silent drops
if (nrow(plot_results) != COEF_PLOT_EXPECTED_ROWS) {
  cat("\n=== COEFFICIENT PLOT DIAGNOSTIC ===\n")
  cat("Outcomes matched:\n")
  print(unique(plot_results$Outcome))
  cat("\nOutcomes in results_main but NOT in plot:\n")
  print(setdiff(
    unique(results_main$Outcome[results_main$Spec %in%
                                  c("Baseline DiD", "Trend-adjusted DiD")]),
    unique(plot_results$Outcome)
  ))
  stop(sprintf(
    "[FIX 3] Coefficient plot has %d rows but expected %d. Check label consistency between results_main and plot filter.",
    nrow(plot_results), COEF_PLOT_EXPECTED_ROWS
  ))
}
message(sprintf("  Coefficient plot validation passed: %d rows ✓",
                nrow(plot_results)))

plot_results <- plot_results |>
  mutate(
    Category = case_when(
      str_detect(Outcome, "worth|buffer|Well|well")  ~ "1. Capital",
      str_detect(Outcome, "spread")                  ~ "2. Loan Pricing",
      str_detect(Outcome, "share|growth|Growth")     ~ "3. Portfolio & Growth",
      str_detect(Outcome, "Delinq|Charge|PLL")       ~ "4. Credit Quality",
      str_detect(Outcome, "ROA|NIM|Cost|cost")       ~ "5. Profitability",
      TRUE                                           ~ "6. Other"
    ),
    Outcome = factor(Outcome, levels = rev(unique(Outcome))),
    Sig     = !is.na(p_value) & p_value < 0.05
  )

p_coef <- ggplot(
  plot_results,
  aes(x = Beta, y = Outcome, xmin = CI_low, xmax = CI_high, color = Sig)
) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.6) +
  geom_errorbarh(height = 0.3, linewidth = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ Category, scales = "free_y", ncol = 1) +
  scale_color_manual(
    values = c("TRUE" = COL_EST, "FALSE" = "gray65"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05")
  ) +
  labs(
    title    = "RBC Rule Impact: Main DiD Estimates (Complex \u00d7 Post-RBC)",
    subtitle = paste0(
      "Two-way FE (CU + quarter-year). SE clustered at CU. 95% CI. ",
      "Capital & MBL share: trend-adjusted spec. All others: baseline DiD. ",
      "Comm RE spread supplementary (\u224878% missing)."
    ),
    x       = "DiD Coefficient",
    y       = NULL,
    color   = NULL,
    caption = paste0("Source: NCUA Call Report (5300) & FRED. ",
                     "Growth vars in log\u00d7100 units (\u2248 QoQ % growth).")
  ) +
  theme_rbc() +
  theme(legend.position = "top")

ggsave(file.path(FIGURE_PATH, "fig_main_coef_plot.png"),
       p_coef, width = 11, height = 14, dpi = 300)
message("  Main coefficient plot saved.")


# =============================================================================
# 14. FINAL SUMMARY
# =============================================================================

message("\n── Step 12: Summary ──────────────────────────────────────────────────")

cat("\n=== DiD ESTIMATION COMPLETE ===\n\n")

cat("Tables (output/tables/):\n")
for (t in c("did_main_results.csv",
            "table3_capital.csv",          # [FIX 6]: renamed from table1
            "table4_loan_spreads.csv",     # [FIX 6]: renamed from table2
            "table5_portfolio_growth.csv", # [FIX 6]: renamed from table3
            "heterogeneity_by_tier.csv",
            "near_threshold_results.csv",
            "robustness_checks.csv")) {
  cat("  \u2192", t, "\n")
}

cat("\nFigures (output/figures/):\n")
for (f in c("fig_main_coef_plot.png",
            "fig_es_panel_capital.png",
            "fig_es_panel_spreads.png",
            "fig_heterogeneity_tier.png",
            "fig_es_networth_ratio.png",
            "fig_es_cap_buffer.png",
            "fig_es_loan_growth.png",
            "fig_es_delinquency.png",
            "fig_es_roa.png",
            "fig_es_spread_mortgage.png",
            "fig_es_spread_nauto.png",
            "fig_es_spread_comm.png",
            "fig_es_mbl_share.png",
            "fig_es_re_share.png",
            "fig_es_auto_share.png",
            "fig_es_well_capitalized.png")) {
  cat("  \u2192", f, "\n")
}

cat("\nFIX SUMMARY (verify these on first run):\n")
cat("  [FIX 1] Growth labels: check did_main_results.csv — should show",
    lbl_loan_growth, "\n")
cat("  [FIX 2] Comm RE N_obs: check table4_loan_spreads.csv — expect small N\n")
cat("  [FIX 3] Coef plot rows:", COEF_PLOT_EXPECTED_ROWS,
    "— validated by stopifnot()\n")
cat("  [FIX 4] Event study window: models use EVENT_MAX=", EVENT_MAX,
    " / figures clipped at EVENT_MAX_PAPER=", EVENT_MAX_PAPER, "\n")
cat("  [FIX 5] Placebo beta: check robustness_checks.csv — should be ~0\n")
cat("  [FIX 6] Table files: table3/4/5 (not table1/2/3)\n")

cat("\nPriority checks:\n")
cat("  1. fig_es_panel_capital.png  — pre-period coefs near zero?\n")
cat("  2. robustness_checks.csv     — placebo Beta ≈ 0 (not NA)?\n")
cat("  3. fig_main_coef_plot.png    — all 17 rows present?\n")
cat("  4. near_threshold_results.csv — capital sign reversal present?\n")

message("\n── 2_DiD_Estimation.R complete \u2713 ────────────────────────────────────")
message("  Next step: run 3A_2008_Crisis_Parallel_DiD.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
