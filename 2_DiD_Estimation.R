# =============================================================================
# 2_DiD_Estimation.R
# RBC Rule Impact Analysis — Difference-in-Differences Estimation
# NCUA Call Report (5300) Data
#
# Author  : [Your Name]
# Created : 2026
# Purpose : Estimate causal effects of the RBC rule on complex credit unions
#           using difference-in-differences (DiD) and event study designs.
#
# Outcomes estimated:
#   (1) Capital adequacy  : networth_ratio, cap_buffer, well_capitalized
#   (2) Loan pricing      : spread_mortgage, spread_nauto, spread_comm_oth
#   (3) Portfolio mix     : mbl_shr, re_shr, auto_shr
#   (4) Growth            : asset_growth, loan_growth
#   (5) Credit quality    : dq_rate_var, chgoff_ratio
#   (6) Profitability     : roa_var, nim
#
# Specifications:
#   (A) Baseline 2x2 DiD with CU + time FE
#   (B) Trend-adjusted DiD (for non-parallel outcomes: capital, MBL share)
#   (C) Event study (Callaway-Sant'Anna for staggered treatment robustness)
#   (D) Heterogeneity: by asset tier and near-threshold subgroup
#
# Input  : data/analysis_panel.rds
# Output : output/tables/  — regression tables (CSV + gt)
#          output/figures/ — event study plots
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(fixest)       # feols() — fast FE + clustered SE
library(modelsummary) # regression tables
library(patchwork)    # combining plots
library(scales)
library(gt)

# Install if needed:
# install.packages(c("fixest", "modelsummary"))


# =============================================================================
# 1. USER SETTINGS
# =============================================================================

INPUT_PATH  <- "data/analysis_panel.rds"
TABLE_PATH  <- "output/tables/"
FIGURE_PATH <- "output/figures/"

# Reference period for event study (quarter before RBC = -1)
EVENT_REF   <- -1

# Event study window to display
EVENT_MIN   <- -12    # 3 years pre-RBC
EVENT_MAX   <-  10    # 2.5 years post-RBC

# Color palette (consistent with 1_Descriptive_Stats.R)
COL_EST     <- "#1B3A6B"   # point estimates
COL_CI      <- "#6B8CBF"   # confidence intervals
COL_ZERO    <- "gray50"    # zero line

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
# 2. LOAD DATA
# =============================================================================

message("── Step 1: Loading analysis panel ───────────────────────────────────")

df <- readRDS(INPUT_PATH)

dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)

message(sprintf("  Loaded: %s obs, %d CUs",
                scales::comma(nrow(df)), n_distinct(df$cu_number)))

# Restrict event study to display window (keep full data for main DiD)
df_es <- df |>
  filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)

# Flag: Q2/Q4 only for spread outcomes (rate data collected semi-annually)
df_rates <- df |>
  filter(quarter %in% c(2, 4))

message(sprintf("  Rate-restricted sample: %s obs", scales::comma(nrow(df_rates))))


# =============================================================================
# 3. BASELINE DiD SPECIFICATION
# =============================================================================
# Model: Y_it = alpha_i + gamma_t + beta*(complex_i x post_t) + X_it + e_it
#
# - alpha_i  : CU fixed effects (absorb all time-invariant CU heterogeneity)
# - gamma_t  : Quarter-year fixed effects (absorb common macro shocks)
# - beta     : DiD estimate = ATT (average treatment effect on treated)
# - X_it     : Time-varying controls: ln_assets, loan_to_asset, cecl_adopter
# - SE       : Clustered at CU level (allows arbitrary within-CU correlation)
#
# NOTE: treat_post = complex * post_rbc is the DiD interaction.
#       With two-way FE, the main effects of complex and post_rbc are
#       absorbed by the fixed effects.

message("── Step 2: Baseline 2x2 DiD estimation ──────────────────────────────")

# Time-varying controls (included in all specifications)
controls <- "ln_assets + loan_to_asset + cecl_adopter"

# Helper: run baseline DiD for a given outcome
run_did <- function(outcome, data = df, ctrl = controls) {
  fml <- as.formula(
    paste0(outcome, " ~ treat_post + ", ctrl,
           " | cu_number + q_period_num")
  )
  feols(fml, data = data, cluster = ~cu_number,
        warn = FALSE, notes = FALSE)
}

# Helper: run trend-adjusted DiD (adds complex-specific linear time trend)
# Used for outcomes with non-parallel pre-trends (capital, MBL share)
run_did_trend <- function(outcome, data = df, ctrl = controls) {
  fml <- as.formula(
    paste0(outcome, " ~ treat_post + complex:event_time + ", ctrl,
           " | cu_number + q_period_num")
  )
  feols(fml, data = data, cluster = ~cu_number,
        warn = FALSE, notes = FALSE)
}

# ── 3A. Capital outcomes ───────────────────────────────────────────────────
# NOTE: Using trend-adjusted spec for capital (non-parallel pre-trends)
message("  Estimating capital outcomes...")

m_nw_base  <- run_did("networth_ratio")
m_nw_trend <- run_did_trend("networth_ratio")
m_buf_base  <- run_did("cap_buffer")
m_buf_trend <- run_did_trend("cap_buffer")
m_wellcap   <- run_did("well_capitalized")    # LPM; binary outcome

# ── 3B. Loan pricing outcomes (Q2/Q4 only) ────────────────────────────────
message("  Estimating loan spread outcomes...")

m_spread_mort  <- run_did("spread_mortgage",  data = df_rates)
m_spread_nauto <- run_did("spread_nauto",     data = df_rates)
m_spread_uauto <- run_did("spread_uauto",     data = df_rates)
m_spread_comm  <- run_did("spread_comm_oth",  data = df_rates)
m_spread_cre   <- run_did("spread_comm_re",   data = df_rates)

# ── 3C. Loan portfolio composition ────────────────────────────────────────
# NOTE: mbl_shr has non-parallel pre-trend — use trend-adjusted spec
message("  Estimating portfolio composition outcomes...")

m_mbl_base  <- run_did("mbl_shr")
m_mbl_trend <- run_did_trend("mbl_shr")
m_re        <- run_did("re_shr")
m_auto      <- run_did("auto_shr")
m_comm      <- run_did("comm_shr")

# ── 3D. Growth outcomes ────────────────────────────────────────────────────
message("  Estimating growth outcomes...")

m_asset_growth <- run_did("asset_growth")
m_loan_growth  <- run_did("loan_growth")
m_mbl_growth   <- run_did("mbl_growth")

# ── 3E. Credit quality ────────────────────────────────────────────────────
message("  Estimating credit quality outcomes...")

m_dq      <- run_did("dq_rate_var")
m_chgoff  <- run_did("chgoff_ratio")
m_pll     <- run_did("pll_assets")

# ── 3F. Profitability ─────────────────────────────────────────────────────
message("  Estimating profitability outcomes...")

m_roa <- run_did("roa_var")
m_nim <- run_did("nim")
m_cof <- run_did("cof")


# =============================================================================
# 4. RESULTS SUMMARY TABLE — Main DiD Estimates
# =============================================================================

message("── Step 3: Building main results table ──────────────────────────────")

# Extract treat_post coefficient, SE, p-value and stars
extract_did <- function(model, label,
                        spec = "Baseline DiD",
                        note = "") {
  coef_row <- tryCatch({
    tidy_m <- broom::tidy(model, conf.int = TRUE)
    row    <- tidy_m[tidy_m$term == "treat_post", ]
    tibble(
      Outcome    = label,
      Spec       = spec,
      Beta       = round(row$estimate,   3),
      SE         = round(row$std.error,  3),
      CI_low     = round(row$conf.low,   3),
      CI_high    = round(row$conf.high,  3),
      p_value    = round(row$p.value,    3),
      Stars      = case_when(
        row$p.value < 0.01 ~ "***",
        row$p.value < 0.05 ~ "**",
        row$p.value < 0.10 ~ "*",
        TRUE               ~ ""
      ),
      N_obs      = nobs(model),
      N_CU       = model$nobs_origin,
      Note       = note
    )
  }, error = function(e) {
    tibble(Outcome = label, Spec = spec,
           Beta = NA, SE = NA, CI_low = NA, CI_high = NA,
           p_value = NA, Stars = "", N_obs = NA, N_CU = NA, Note = note)
  })
  coef_row
}

results_main <- bind_rows(
  # Capital
  extract_did(m_nw_base,  "Net worth ratio (%)",
              "Baseline DiD"),
  extract_did(m_nw_trend, "Net worth ratio (%)",
              "Trend-adjusted DiD",
              "Pre-trend corrected"),
  extract_did(m_buf_base, "Capital buffer vs 10% (pp)",
              "Baseline DiD"),
  extract_did(m_buf_trend,"Capital buffer vs 10% (pp)",
              "Trend-adjusted DiD",
              "Pre-trend corrected"),
  extract_did(m_wellcap,  "Well-capitalized (LPM)",
              "Baseline DiD"),

  # Loan spreads
  extract_did(m_spread_mort,  "Mortgage spread (pp)",
              "Baseline DiD", "Q2/Q4 only"),
  extract_did(m_spread_nauto, "New auto spread (pp)",
              "Baseline DiD", "Q2/Q4 only"),
  extract_did(m_spread_uauto, "Used auto spread (pp)",
              "Baseline DiD", "Q2/Q4 only"),
  extract_did(m_spread_comm,  "Comm non-RE spread (pp)",
              "Baseline DiD", "Q2/Q4 only"),
  extract_did(m_spread_cre,   "Comm RE spread (pp)",
              "Baseline DiD", "Q2/Q4 only"),

  # Portfolio
  extract_did(m_mbl_base,  "MBL share (%)",
              "Baseline DiD"),
  extract_did(m_mbl_trend, "MBL share (%)",
              "Trend-adjusted DiD", "Pre-trend corrected"),
  extract_did(m_re,    "RE share (%)",    "Baseline DiD"),
  extract_did(m_auto,  "Auto share (%)",  "Baseline DiD"),
  extract_did(m_comm,  "Comm share (%)",  "Baseline DiD"),

  # Growth
  extract_did(m_asset_growth, "Asset growth (QoQ log)",
              "Baseline DiD"),
  extract_did(m_loan_growth,  "Loan growth (QoQ log)",
              "Baseline DiD"),
  extract_did(m_mbl_growth,   "MBL growth (QoQ log)",
              "Baseline DiD"),

  # Credit quality
  extract_did(m_dq,     "Delinquency rate (%)", "Baseline DiD"),
  extract_did(m_chgoff, "Charge-off ratio (%)", "Baseline DiD"),
  extract_did(m_pll,    "PLL / assets (%)",     "Baseline DiD"),

  # Profitability
  extract_did(m_roa, "ROA (%)", "Baseline DiD"),
  extract_did(m_nim, "NIM (%)", "Baseline DiD"),
  extract_did(m_cof, "Cost of funds (%)", "Baseline DiD")
)

# Console print
cat("\n=== MAIN DiD RESULTS (treat_post coefficient) ===\n")
results_main |>
  mutate(Result = paste0(Beta, Stars, " (", SE, ")")) |>
  select(Outcome, Spec, Result, p_value, N_obs, Note) |>
  print(n = Inf)

# Save
write_csv(results_main,
          file.path(TABLE_PATH, "did_main_results.csv"))
message("  Main results saved → output/tables/did_main_results.csv")


# =============================================================================
# 5. FORMATTED REGRESSION TABLE (modelsummary)
# =============================================================================

message("── Step 4: Formatted regression tables ──────────────────────────────")

# Table 1: Capital outcomes
cap_models <- list(
  "NW Ratio\n(Baseline)"  = m_nw_base,
  "NW Ratio\n(Trend adj)" = m_nw_trend,
  "Cap Buffer\n(Baseline)" = m_buf_base,
  "Cap Buffer\n(Trend adj)"= m_buf_trend,
  "Well-Cap\n(LPM)"        = m_wellcap
)

modelsummary(
  cap_models,
  coef_map    = c(
    "treat_post"        = "Complex × Post-RBC",
    "complex:event_time"= "Complex × Time Trend",
    "ln_assets"         = "Log assets",
    "loan_to_asset"     = "Loans/assets",
    "cecl_adopter"      = "CECL adopter"
  ),
  gof_map     = c("nobs", "r.squared", "FE: cu_number", "FE: q_period_num"),
  stars       = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  title       = "Table 1. RBC Rule Impact on Capital Adequacy",
  notes       = paste0(
    "Two-way FE (CU + quarter-year). SE clustered at CU level. ",
    "Trend-adjusted specs include complex-specific linear time trend ",
    "to account for pre-RBC convergence toward the 10% threshold."
  ),
  output      = file.path(TABLE_PATH, "table1_capital.csv")
)

# Table 2: Loan pricing (spreads)
spread_models <- list(
  "Mortgage"     = m_spread_mort,
  "New Auto"     = m_spread_nauto,
  "Used Auto"    = m_spread_uauto,
  "Comm non-RE"  = m_spread_comm,
  "Comm RE"      = m_spread_cre
)

modelsummary(
  spread_models,
  coef_map = c(
    "treat_post"  = "Complex × Post-RBC",
    "ln_assets"   = "Log assets",
    "loan_to_asset" = "Loans/assets",
    "cecl_adopter"= "CECL adopter"
  ),
  gof_map  = c("nobs", "r.squared"),
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  title    = "Table 2. RBC Rule Impact on Loan Rate Spreads Over Treasuries",
  notes    = paste0(
    "Q2 and Q4 observations only (NCUA semi-annual rate reporting). ",
    "Two-way FE (CU + quarter-year). SE clustered at CU level. ",
    "Spread = loan rate minus matched-maturity Treasury benchmark."
  ),
  output   = file.path(TABLE_PATH, "table2_loan_spreads.csv")
)

# Table 3: Portfolio + Growth
port_models <- list(
  "MBL Share\n(Baseline)"   = m_mbl_base,
  "MBL Share\n(Trend adj)"  = m_mbl_trend,
  "RE Share"                 = m_re,
  "Auto Share"               = m_auto,
  "Loan Growth"              = m_loan_growth,
  "MBL Growth"               = m_mbl_growth
)

modelsummary(
  port_models,
  coef_map = c(
    "treat_post"         = "Complex × Post-RBC",
    "complex:event_time" = "Complex × Time Trend",
    "ln_assets"          = "Log assets",
    "loan_to_asset"      = "Loans/assets",
    "cecl_adopter"       = "CECL adopter"
  ),
  gof_map  = c("nobs", "r.squared"),
  stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  title    = "Table 3. RBC Rule Impact on Loan Portfolio Composition and Growth",
  notes    = "Two-way FE (CU + quarter-year). SE clustered at CU level.",
  output   = file.path(TABLE_PATH, "table3_portfolio_growth.csv")
)

message("  Tables 1–3 saved → output/tables/")


# =============================================================================
# 6. EVENT STUDY ESTIMATION
# =============================================================================
# Estimates quarter-by-quarter treatment effects relative to the period
# just before RBC (event_time = -1 is the reference/normalized to zero).
# Coefficients on event_time dummies * complex indicator show:
#   - Pre-period: should be ~zero if parallel trends holds
#   - Post-period: captures dynamic treatment effects
#
# Specification:
#   Y_it = alpha_i + gamma_t + sum_{k != -1} beta_k*(complex_i x 1[t=k]) + X + e

message("── Step 5: Event study estimation ───────────────────────────────────")

run_event_study <- function(outcome, data = df_es, ctrl = controls) {
  fml <- as.formula(
    paste0(outcome,
           " ~ i(event_time, complex, ref = ", EVENT_REF, ") + ",
           ctrl, " | cu_number + q_period_num")
  )
  feols(fml, data = data, cluster = ~cu_number,
        warn = FALSE, notes = FALSE)
}

# Run event studies for key outcomes
message("  Running event studies...")

es_nw      <- run_event_study("networth_ratio")
es_cap_buf <- run_event_study("cap_buffer")
es_wellcap <- run_event_study("well_capitalized")
es_mbl_shr <- run_event_study("mbl_shr")
es_re_shr  <- run_event_study("re_shr")
es_auto_shr<- run_event_study("auto_shr")
es_loan_gr <- run_event_study("loan_growth")
es_dq      <- run_event_study("dq_rate_var")
es_roa     <- run_event_study("roa_var")

# Also run for spreads (Q2/Q4 only)
es_mort    <- run_event_study("spread_mortgage", data = df_rates |>
                                filter(event_time >= EVENT_MIN,
                                       event_time <= EVENT_MAX))
es_nauto   <- run_event_study("spread_nauto",    data = df_rates |>
                                filter(event_time >= EVENT_MIN,
                                       event_time <= EVENT_MAX))
es_comm    <- run_event_study("spread_comm_oth", data = df_rates |>
                                filter(event_time >= EVENT_MIN,
                                       event_time <= EVENT_MAX))

message("  Event studies complete.")


# =============================================================================
# 7. EVENT STUDY PLOTS
# =============================================================================

# Helper: extract event study coefficients and plot
plot_event_study <- function(es_model, outcome_label,
                             ref_period = EVENT_REF,
                             add_trend_note = FALSE) {

  # Extract coefficients on event_time x complex interactions
  coefs <- broom::tidy(es_model, conf.int = TRUE) |>
    filter(str_detect(term, "event_time::")) |>
    mutate(
      event_time = as.integer(
        str_extract(term, "-?\\d+")
      )
    ) |>
    filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)

  # Add reference period (normalized to zero)
  ref_row <- tibble(
    term       = paste0("event_time::", ref_period, ":complex"),
    estimate   = 0,
    std.error  = 0,
    conf.low   = 0,
    conf.high  = 0,
    p.value    = NA_real_,
    event_time = ref_period
  )

  coefs <- bind_rows(coefs, ref_row) |>
    arrange(event_time)

  # Separate pre and post
  coefs <- coefs |>
    mutate(period = if_else(event_time < 0, "Pre-RBC", "Post-RBC"))

  subtitle_text <- paste0(
    "Event time = quarters relative to RBC (2022 Q1 = 0). ",
    "Reference period = Q", ref_period, ". ",
    "Dashed = RBC effective date. 95% CI shaded."
  )
  if (add_trend_note) {
    subtitle_text <- paste0(subtitle_text,
                            "\nNote: Non-parallel pre-trends — interpret with caution.")
  }

  ggplot(coefs, aes(x = event_time, y = estimate)) +
    # Confidence interval ribbon
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = COL_CI, alpha = 0.25) +
    # Pre/post period shading
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "gray95", alpha = 0.4) +
    # Zero line
    geom_hline(yintercept = 0, color = COL_ZERO,
               linewidth = 0.6, linetype = "dashed") +
    # RBC line
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.7, linetype = "dashed") +
    # Point estimates
    geom_line(color = COL_EST, linewidth = 0.9) +
    geom_point(aes(shape = period, color = period),
               size = 2.2) +
    # Significance markers
    geom_point(data = coefs |>
                 filter(!is.na(p.value), p.value < 0.05),
               aes(x = event_time, y = estimate),
               shape = 8, size = 2, color = "darkred") +
    scale_color_manual(values = c("Pre-RBC" = "gray50",
                                  "Post-RBC" = COL_EST)) +
    scale_shape_manual(values = c("Pre-RBC" = 1, "Post-RBC" = 16)) +
    scale_x_continuous(breaks = seq(EVENT_MIN, EVENT_MAX, by = 4),
                       labels = function(x) paste0("Q", x)) +
    labs(
      title    = paste0("Event Study: ", outcome_label),
      subtitle = subtitle_text,
      x        = "Quarters Relative to RBC Effective Date",
      y        = paste0("Coefficient (", outcome_label, ")"),
      color    = NULL, shape = NULL,
      caption  = paste0("★ = significant at 5%. Source: NCUA 5300.",
                        " CU + quarter-year FE. SE clustered at CU level.")
    ) +
    theme_rbc()
}

# Generate and save all event study plots
message("  Generating event study plots...")

es_plots <- list(
  list(es_nw,      "Net Worth Ratio (%)",
       "fig_es_networth_ratio.png",    TRUE),
  list(es_cap_buf, "Capital Buffer vs 10% (pp)",
       "fig_es_cap_buffer.png",        TRUE),
  list(es_wellcap, "Well-Capitalized (pp, LPM)",
       "fig_es_well_capitalized.png",  FALSE),
  list(es_mbl_shr, "MBL Share (%)",
       "fig_es_mbl_share.png",         TRUE),
  list(es_re_shr,  "Real Estate Share (%)",
       "fig_es_re_share.png",          FALSE),
  list(es_auto_shr,"Auto Loan Share (%)",
       "fig_es_auto_share.png",        FALSE),
  list(es_loan_gr, "Loan Growth (QoQ log × 100)",
       "fig_es_loan_growth.png",       FALSE),
  list(es_dq,      "Delinquency Rate (%)",
       "fig_es_delinquency.png",       FALSE),
  list(es_roa,     "Return on Assets (%)",
       "fig_es_roa.png",               FALSE),
  list(es_mort,    "Mortgage Spread (pp)",
       "fig_es_spread_mortgage.png",   FALSE),
  list(es_nauto,   "New Auto Spread (pp)",
       "fig_es_spread_nauto.png",      FALSE),
  list(es_comm,    "Comm non-RE Spread (pp)",
       "fig_es_spread_comm.png",       FALSE)
)

for (ep in es_plots) {
  p <- plot_event_study(ep[[1]], ep[[2]], add_trend_note = ep[[4]])
  ggsave(file.path(FIGURE_PATH, ep[[3]]),
         p, width = 10, height = 6, dpi = 300)
  message(sprintf("    Saved: %s", ep[[3]]))
}


# =============================================================================
# 8. COMBINED EVENT STUDY PANEL (capital outcomes)
# =============================================================================

message("── Step 6: Combined event study panels ──────────────────────────────")

p_es_nw   <- plot_event_study(es_nw,      "Net Worth Ratio (%)",
                               add_trend_note = TRUE) +
  labs(title = "A. Net Worth Ratio")
p_es_buf  <- plot_event_study(es_cap_buf, "Cap Buffer (pp)",
                               add_trend_note = TRUE) +
  labs(title = "B. Capital Buffer vs 10%")
p_es_wc   <- plot_event_study(es_wellcap, "Well-Capitalized") +
  labs(title = "C. Well-Capitalized (LPM)")
p_es_loan <- plot_event_study(es_loan_gr, "Loan Growth") +
  labs(title = "D. Loan Growth")

p_capital_es <- (p_es_nw + p_es_buf) / (p_es_wc + p_es_loan) +
  plot_annotation(
    title   = "Event Study: RBC Rule Impact on Capital and Lending",
    caption = paste0("Two-way FE (CU + quarter-year). SE clustered at CU. ",
                     "Reference quarter = Q-1 (normalized to 0). ★ = p<0.05.")
  )

ggsave(file.path(FIGURE_PATH, "fig_es_panel_capital.png"),
       p_capital_es, width = 14, height = 10, dpi = 300)

# Combined spread panel
p_es_mort  <- plot_event_study(es_mort,  "Mortgage Spread (pp)") +
  labs(title = "A. Mortgage Spread")
p_es_nauto <- plot_event_study(es_nauto, "New Auto Spread (pp)") +
  labs(title = "B. New Auto Spread")
p_es_comm  <- plot_event_study(es_comm,  "Comm non-RE Spread (pp)") +
  labs(title = "C. Comm non-RE Spread")

p_spread_es <- (p_es_mort + p_es_nauto) / (p_es_comm + plot_spacer()) +
  plot_annotation(
    title   = "Event Study: RBC Rule Impact on Loan Rate Spreads",
    caption = "Q2/Q4 only. Two-way FE. SE clustered at CU. Reference = Q-1."
  )

ggsave(file.path(FIGURE_PATH, "fig_es_panel_spreads.png"),
       p_spread_es, width = 14, height = 9, dpi = 300)

message("  Combined panels saved.")


# =============================================================================
# 9. HETEROGENEITY ANALYSIS — By Asset Size Tier
# =============================================================================
# Tests whether RBC effects differ across CU size groups within the
# "complex" classification (e.g., $500M–$1B vs. >$1B).

message("── Step 7: Heterogeneity by asset size tier ──────────────────────────")

tiers <- c("3_Complex ($500M-$1B)",
           "2_Large   ($1B-$10B)",
           "1_Mega    (>$10B)")

tier_labels <- c("$500M–$1B", "$1B–$10B", ">$10B")

het_outcomes <- c("networth_ratio", "cap_buffer",
                  "loan_growth", "spread_mortgage",
                  "spread_nauto", "dq_rate_var", "roa_var")

het_labels <- c("Net Worth Ratio", "Capital Buffer",
                "Loan Growth", "Mortgage Spread",
                "New Auto Spread", "DQ Rate", "ROA")

# Run DiD within each treated tier vs. full control group
het_results <- map_dfr(seq_along(tiers), function(i) {
  tier_df <- df |>
    filter(complex == 0 | asset_tier == tiers[i])

  map_dfr(seq_along(het_outcomes), function(j) {
    m <- tryCatch(
      run_did(het_outcomes[j], data = tier_df),
      error = function(e) NULL
    )
    if (is.null(m)) return(NULL)

    tidy_m <- broom::tidy(m, conf.int = TRUE)
    row    <- tidy_m[tidy_m$term == "treat_post", ]
    if (nrow(row) == 0) return(NULL)

    tibble(
      Tier     = tier_labels[i],
      Outcome  = het_labels[j],
      Beta     = round(row$estimate,  3),
      SE       = round(row$std.error, 3),
      CI_low   = round(row$conf.low,  3),
      CI_high  = round(row$conf.high, 3),
      p_value  = round(row$p.value,   3),
      Stars    = case_when(
        row$p.value < 0.01 ~ "***",
        row$p.value < 0.05 ~ "**",
        row$p.value < 0.10 ~ "*",
        TRUE               ~ ""
      ),
      N        = nobs(m)
    )
  })
})

cat("\n=== HETEROGENEITY BY ASSET SIZE TIER ===\n")
print(het_results, n = Inf)

write_csv(het_results,
          file.path(TABLE_PATH, "heterogeneity_by_tier.csv"))

# Coefficient plot: heterogeneity across tiers
p_het <- ggplot(
  het_results |> filter(!is.na(Beta)),
  aes(x = Beta, y = Tier, color = Tier,
      xmin = CI_low, xmax = CI_high)
) +
  geom_vline(xintercept = 0, color = COL_ZERO,
             linetype = "dashed") +
  geom_errorbarh(height = 0.2, linewidth = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ Outcome, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c(
    "$500M–$1B" = "#1B3A6B",
    "$1B–$10B"  = "#4A7CB5",
    ">$10B"     = "#8AB4D9"
  )) +
  labs(
    title    = "Heterogeneity of RBC Effects by Asset Size Tier",
    subtitle = "DiD estimates (Complex × Post-RBC). 95% CI. Non-complex CUs as control.",
    x        = "DiD Coefficient (Complex × Post-RBC)",
    y        = NULL,
    caption  = "Two-way FE (CU + quarter-year). SE clustered at CU."
  ) +
  theme_rbc() +
  theme(legend.position = "none")

ggsave(file.path(FIGURE_PATH, "fig_heterogeneity_tier.png"),
       p_het, width = 12, height = 10, dpi = 300)

message("  Heterogeneity by tier saved.")


# =============================================================================
# 10. NEAR-THRESHOLD SUBGROUP ANALYSIS
# =============================================================================
# Restricts sample to CUs within +/- $100M of $500M threshold
# ($400M–$600M). CUs in this band are more similar on observables,
# strengthening the parallel trends assumption.

message("── Step 8: Near-threshold subgroup analysis ──────────────────────────")

df_threshold <- df |>
  filter(near_threshold == 1 | complex == 0) |>
  # For control group, also restrict to non-complex CUs of similar size
  filter(avg_assets_pre >= 200e6 | complex == 1)

message(sprintf("  Near-threshold sample: %s CUs",
                n_distinct(df_threshold$cu_number)))

threshold_results <- map_dfr(seq_along(het_outcomes), function(j) {
  m <- tryCatch(
    run_did(het_outcomes[j], data = df_threshold),
    error = function(e) NULL
  )
  if (is.null(m)) return(NULL)

  tidy_m <- broom::tidy(m, conf.int = TRUE)
  row    <- tidy_m[tidy_m$term == "treat_post", ]
  if (nrow(row) == 0) return(NULL)

  tibble(
    Outcome = het_labels[j],
    Sample  = "Near-threshold ($400M–$600M)",
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

write_csv(threshold_results,
          file.path(TABLE_PATH, "near_threshold_results.csv"))

message("  Near-threshold results saved.")


# =============================================================================
# 11. ROBUSTNESS CHECKS
# =============================================================================

message("── Step 9: Robustness checks ─────────────────────────────────────────")

# ── R1: Exclude CECL adopters ─────────────────────────────────────────────
# CECL adoption affects net worth ratios at adoption (day-1 adjustment).
# Excluding CECL adopters tests whether capital results are driven by
# CECL accounting rather than RBC behavioral response.

message("  R1: Excluding CECL adopters...")

df_nocecl <- df |> filter(cecl_adopter == 0)

r1_nw  <- run_did("networth_ratio", data = df_nocecl)
r1_buf <- run_did("cap_buffer",     data = df_nocecl)

# ── R2: Balanced panel only ───────────────────────────────────────────────
# Restricts to CUs observed in every quarter of the analysis window.
# Tests whether results are driven by CU entry/exit (attrition bias).

message("  R2: Balanced panel...")

n_periods <- n_distinct(df$q_period_num)
balanced_cus <- df |>
  count(cu_number) |>
  filter(n == n_periods) |>
  pull(cu_number)

df_balanced <- df |> filter(cu_number %in% balanced_cus)

message(sprintf("    Balanced panel: %s CUs",
                scales::comma(length(balanced_cus))))

r2_nw   <- run_did("networth_ratio", data = df_balanced)
r2_loan <- run_did("loan_growth",    data = df_balanced)

# ── R3: Placebo test — fake treatment date (2020q1) ──────────────────────
# If the DiD picks up a real RBC effect, a placebo treatment date
# (before COVID, well before actual RBC) should show no effect.

message("  R3: Placebo treatment (2020q1)...")

df_placebo <- df |>
  filter(q_period_num < RBC_PERIOD) |>   # pre-RBC data only
  mutate(
    placebo_post  = as.integer(q_period_num >= 2020.1),
    placebo_treat = complex * placebo_post
  )

placebo_fml <- networth_ratio ~
  placebo_treat + ln_assets + loan_to_asset + cecl_adopter |
  cu_number + q_period_num

r3_nw <- feols(placebo_fml, data = df_placebo,
               cluster = ~cu_number, warn = FALSE, notes = FALSE)

# ── R4: Alternative control: drop large non-complex CUs ───────────────────
# Non-complex CUs with >$300M assets are more similar to treated group.
# This tests sensitivity to control group composition.

message("  R4: Restricted control group (>$300M non-complex)...")

df_r4 <- df |>
  filter(complex == 1 | avg_assets_pre >= 300e6)

r4_nw   <- run_did("networth_ratio", data = df_r4)
r4_loan <- run_did("loan_growth",    data = df_r4)

# ── Robustness summary table ──────────────────────────────────────────────
robustness_results <- bind_rows(
  extract_did(m_nw_base,   "Net worth ratio", "Baseline DiD"),
  extract_did(m_nw_trend,  "Net worth ratio", "Trend-adjusted"),
  extract_did(r1_nw,       "Net worth ratio", "Excl. CECL adopters"),
  extract_did(r2_nw,       "Net worth ratio", "Balanced panel"),
  extract_did(r3_nw,       "Net worth ratio", "Placebo (2020q1)",
              note = "Should be ~0"),
  extract_did(r4_nw,       "Net worth ratio", "Control >$300M only"),
  extract_did(m_loan_growth,"Loan growth",    "Baseline DiD"),
  extract_did(r2_loan,      "Loan growth",    "Balanced panel"),
  extract_did(r4_loan,      "Loan growth",    "Control >$300M only")
)

cat("\n=== ROBUSTNESS CHECKS ===\n")
robustness_results |>
  mutate(Result = paste0(Beta, Stars, " (", SE, ")")) |>
  select(Outcome, Spec, Result, p_value, N_obs, Note) |>
  print(n = Inf)

write_csv(robustness_results,
          file.path(TABLE_PATH, "robustness_checks.csv"))

message("  Robustness checks saved → output/tables/robustness_checks.csv")


# =============================================================================
# 12. COEFFICIENT PLOT — MAIN RESULTS OVERVIEW
# =============================================================================

message("── Step 10: Main results coefficient plot ────────────────────────────")

# Use the primary specification for each outcome
plot_results <- results_main |>
  filter(
    # Keep primary spec for each outcome
    (Outcome == "Net worth ratio (%)" & Spec == "Trend-adjusted DiD") |
      (Outcome == "Capital buffer vs 10% (pp)" & Spec == "Trend-adjusted DiD") |
      (Outcome == "Well-capitalized (LPM)" & Spec == "Baseline DiD") |
      (Outcome %in% c("Mortgage spread (pp)", "New auto spread (pp)",
                      "Comm non-RE spread (pp)", "Comm RE spread (pp)") &
         Spec == "Baseline DiD") |
      (Outcome == "MBL share (%)" & Spec == "Trend-adjusted DiD") |
      (Outcome %in% c("RE share (%)", "Auto share (%)",
                      "Loan growth (QoQ log)", "Asset growth (QoQ log)",
                      "Delinquency rate (%)", "Charge-off ratio (%)",
                      "ROA (%)", "NIM (%)", "Cost of funds (%)") &
         Spec == "Baseline DiD")
  ) |>
  mutate(
    Category = case_when(
      str_detect(Outcome, "worth|buffer|well-cap|Well") ~ "Capital",
      str_detect(Outcome, "spread")                     ~ "Loan Pricing",
      str_detect(Outcome, "share|growth")               ~ "Portfolio & Growth",
      str_detect(Outcome, "Delinq|Charge|PLL")          ~ "Credit Quality",
      str_detect(Outcome, "ROA|NIM|Cost")               ~ "Profitability",
      TRUE                                              ~ "Other"
    ),
    Outcome = factor(Outcome, levels = rev(unique(Outcome))),
    Sig     = p_value < 0.05
  )

p_coef <- ggplot(plot_results,
                 aes(x = Beta, y = Outcome,
                     xmin = CI_low, xmax = CI_high,
                     color = Sig)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.6) +
  geom_errorbarh(height = 0.3, linewidth = 0.8) +
  geom_point(size = 3) +
  facet_col(~ Category, scales = "free_y", space = "free") +
  scale_color_manual(values = c("TRUE" = COL_EST, "FALSE" = "gray60"),
                     labels = c("TRUE" = "p < 0.05",
                                "FALSE" = "p ≥ 0.05")) +
  labs(
    title    = "RBC Rule Impact: Main DiD Estimates (Complex × Post-RBC)",
    subtitle = paste0(
      "Two-way FE (CU + quarter). SE clustered at CU level. 95% CI. ",
      "Capital outcomes use trend-adjusted spec; others use baseline DiD."
    ),
    x        = "DiD Coefficient",
    y        = NULL,
    color    = NULL,
    caption  = "Source: NCUA Call Report (5300) & FRED."
  ) +
  theme_rbc() +
  theme(legend.position = "top")

ggsave(file.path(FIGURE_PATH, "fig_main_coef_plot.png"),
       p_coef, width = 11, height = 12, dpi = 300)

message("  Main coefficient plot saved.")


# =============================================================================
# 13. CONSOLE SUMMARY
# =============================================================================

message("\n── Step 11: Summary ──────────────────────────────────────────────────")

cat("\n=== DiD ESTIMATION COMPLETE ===\n\n")

cat("Tables saved:\n")
tables_saved <- c(
  "did_main_results.csv",
  "table1_capital.csv",
  "table2_loan_spreads.csv",
  "table3_portfolio_growth.csv",
  "heterogeneity_by_tier.csv",
  "near_threshold_results.csv",
  "robustness_checks.csv"
)
for (t in tables_saved) cat("  →", file.path(TABLE_PATH, t), "\n")

cat("\nFigures saved:\n")
figs_saved <- c(
  "fig_main_coef_plot.png",
  "fig_es_panel_capital.png",
  "fig_es_panel_spreads.png",
  "fig_heterogeneity_tier.png",
  "fig_es_networth_ratio.png",
  "fig_es_cap_buffer.png",
  "fig_es_loan_growth.png",
  "fig_es_delinquency.png",
  "fig_es_roa.png",
  "[+ individual event study PNGs]"
)
for (f in figs_saved) cat("  →", file.path(FIGURE_PATH, f), "\n")

cat("\nKey results to report:\n")
cat("  1. Check fig_es_panel_capital.png — are pre-period coefficients ~0?\n")
cat("  2. Check fig_main_coef_plot.png — overall pattern of effects\n")
cat("  3. Placebo test in robustness_checks.csv should show Beta ≈ 0\n")
cat("  4. Near-threshold sample tests RD-like identification\n")
cat("  5. Trend-adjusted spec is preferred for capital and MBL share\n")

message("\n── 2_DiD_Estimation.R complete ✓ ────────────────────────────────────")
message("  Next step: run 3_Tables_Report.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
