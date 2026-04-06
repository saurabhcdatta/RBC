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
#   (B) Trend-adjusted DiD (non-parallel outcomes: capital, MBL share)
#   (C) Event study (quarter-by-quarter dynamic effects)
#   (D) Heterogeneity: by asset size tier and near-threshold subgroup
#   (E) Robustness: excl. CECL, balanced panel, placebo, control trim
#
# FIXES vs prior version:
#   - RBC_EFFECTIVE_PERIOD now defined in Section 1 (was missing from session)
#   - facet_col() replaced with facet_wrap(ncol=1) — no lemon package needed
#
# Input  : data/analysis_panel.rds
# Output : output/tables/  — regression tables (CSV)
#          output/figures/ — event study & coefficient plots
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

# RBC effective date — MUST match 0_Data_Prep.R setting
RBC_EFFECTIVE_PERIOD <- 2022.1

# Reference period for event study (quarter before RBC = -1, normalized to 0)
EVENT_REF <- -1

# Event study window
EVENT_MIN <- -12    # 3 years pre-RBC
EVENT_MAX <-  10    # 2.5 years post-RBC

# Capital thresholds (matches 0_Data_Prep.R)
NW_WELLCAP_THRESHOLD <- 10
NW_CCULR_THRESHOLD   <-  9

# Color palette (consistent with 1_Descriptive_Stats.R)
COL_EST      <- "#1B3A6B"   # navy — point estimates
COL_CI       <- "#6B8CBF"   # light navy — confidence bands
COL_ZERO     <- "gray50"    # zero line

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

message(sprintf("  Loaded : %s obs", scales::comma(nrow(df))))
message(sprintf("  CUs    : %s",     scales::comma(n_distinct(df$cu_number))))
message(sprintf("  Periods: %d",     n_distinct(df$q_period_num)))

# Sub-samples used in estimation
df_es    <- df |> filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)
df_rates <- df |> filter(quarter %in% c(2, 4))   # semi-annual rate reporters

message(sprintf("  Event study sample : %s obs", scales::comma(nrow(df_es))))
message(sprintf("  Rate-only sample   : %s obs", scales::comma(nrow(df_rates))))


# =============================================================================
# 3. DiD HELPER FUNCTIONS
# =============================================================================
# Model: Y_it = alpha_i + gamma_t + beta*(complex_i x post_t) + X_it + e_it
#
# alpha_i  = CU fixed effects        (time-invariant CU heterogeneity)
# gamma_t  = Quarter-year FE         (common macro/seasonal shocks)
# beta     = DiD estimate            (ATT: avg treatment effect on treated)
# X_it     = Time-varying controls   (ln_assets, loan_to_asset, cecl_adopter)
# SE       = Clustered at CU level   (arbitrary within-CU serial correlation)

controls <- "ln_assets + loan_to_asset + cecl_adopter"

# Baseline DiD
run_did <- function(outcome, data = df, ctrl = controls) {
  fml <- as.formula(
    paste0(outcome, " ~ treat_post + ", ctrl,
           " | cu_number + q_period_num")
  )
  feols(fml, data = data, cluster = ~cu_number,
        warn = FALSE, notes = FALSE)
}

# Trend-adjusted DiD — adds complex-specific linear time trend
# Corrects for pre-RBC divergence in capital and MBL share outcomes
run_did_trend <- function(outcome, data = df, ctrl = controls) {
  fml <- as.formula(
    paste0(outcome,
           " ~ treat_post + complex:event_time + ", ctrl,
           " | cu_number + q_period_num")
  )
  feols(fml, data = data, cluster = ~cu_number,
        warn = FALSE, notes = FALSE)
}

# Event study — estimates quarter-by-quarter treatment effects
run_event_study <- function(outcome, data = df_es, ctrl = controls) {
  fml <- as.formula(
    paste0(outcome,
           " ~ i(event_time, complex, ref = ", EVENT_REF, ") + ",
           ctrl, " | cu_number + q_period_num")
  )
  feols(fml, data = data, cluster = ~cu_number,
        warn = FALSE, notes = FALSE)
}

# Extract DiD coefficient + stats into tidy tibble
extract_did <- function(model, label, spec = "Baseline DiD", note = "") {
  tryCatch({
    tidy_m <- tidy(model, conf.int = TRUE)
    row    <- tidy_m[tidy_m$term == "treat_post", ]
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
    tibble(Outcome = label, Spec = spec,
           Beta = NA, SE = NA, CI_low = NA, CI_high = NA,
           p_value = NA, Stars = "", N_obs = NA, Note = note)
  })
}


# =============================================================================
# 4. BASELINE DiD — ALL OUTCOMES
# =============================================================================

message("── Step 2: Estimating DiD models ────────────────────────────────────")

# ── Capital (trend-adjusted preferred due to non-parallel pre-trends) ──────
message("  Capital outcomes...")
m_nw_base   <- run_did("networth_ratio")
m_nw_trend  <- run_did_trend("networth_ratio")
m_buf_base  <- run_did("cap_buffer")
m_buf_trend <- run_did_trend("cap_buffer")
m_wellcap   <- run_did("well_capitalized")     # linear probability model

# ── Loan spreads (Q2/Q4 only — semi-annual NCUA rate reporting) ────────────
message("  Loan spread outcomes...")
m_spread_mort  <- run_did("spread_mortgage",  data = df_rates)
m_spread_nauto <- run_did("spread_nauto",     data = df_rates)
m_spread_uauto <- run_did("spread_uauto",     data = df_rates)
m_spread_comm  <- run_did("spread_comm_oth",  data = df_rates)
m_spread_cre   <- run_did("spread_comm_re",   data = df_rates)

# ── Portfolio composition (mbl_shr trend-adjusted) ────────────────────────
message("  Portfolio composition outcomes...")
m_mbl_base  <- run_did("mbl_shr")
m_mbl_trend <- run_did_trend("mbl_shr")
m_re        <- run_did("re_shr")
m_auto      <- run_did("auto_shr")
m_comm      <- run_did("comm_shr")

# ── Growth ─────────────────────────────────────────────────────────────────
message("  Growth outcomes...")
m_asset_growth <- run_did("asset_growth")
m_loan_growth  <- run_did("loan_growth")
m_mbl_growth   <- run_did("mbl_growth")

# ── Credit quality ─────────────────────────────────────────────────────────
message("  Credit quality outcomes...")
m_dq     <- run_did("dq_rate_var")
m_chgoff <- run_did("chgoff_ratio")
m_pll    <- run_did("pll_assets")

# ── Profitability ──────────────────────────────────────────────────────────
message("  Profitability outcomes...")
m_roa <- run_did("roa_var")
m_nim <- run_did("nim")
m_cof <- run_did("cof")

message("  All DiD models estimated.")


# =============================================================================
# 5. MAIN RESULTS TABLE
# =============================================================================

message("── Step 3: Building main results table ──────────────────────────────")

results_main <- bind_rows(
  # Capital
  extract_did(m_nw_base,   "Net worth ratio (%)",        "Baseline DiD"),
  extract_did(m_nw_trend,  "Net worth ratio (%)",        "Trend-adjusted DiD",
              note = "Pre-trend corrected"),
  extract_did(m_buf_base,  "Capital buffer vs 10% (pp)", "Baseline DiD"),
  extract_did(m_buf_trend, "Capital buffer vs 10% (pp)", "Trend-adjusted DiD",
              note = "Pre-trend corrected"),
  extract_did(m_wellcap,   "Well-capitalized (LPM)",     "Baseline DiD"),
  # Spreads
  extract_did(m_spread_mort,  "Mortgage spread (pp)",     "Baseline DiD",
              note = "Q2/Q4 only"),
  extract_did(m_spread_nauto, "New auto spread (pp)",     "Baseline DiD",
              note = "Q2/Q4 only"),
  extract_did(m_spread_uauto, "Used auto spread (pp)",    "Baseline DiD",
              note = "Q2/Q4 only"),
  extract_did(m_spread_comm,  "Comm non-RE spread (pp)",  "Baseline DiD",
              note = "Q2/Q4 only"),
  extract_did(m_spread_cre,   "Comm RE spread (pp)",      "Baseline DiD",
              note = "Q2/Q4 only"),
  # Portfolio
  extract_did(m_mbl_base,  "MBL share (%)",  "Baseline DiD"),
  extract_did(m_mbl_trend, "MBL share (%)",  "Trend-adjusted DiD",
              note = "Pre-trend corrected"),
  extract_did(m_re,   "RE share (%)",        "Baseline DiD"),
  extract_did(m_auto, "Auto share (%)",      "Baseline DiD"),
  extract_did(m_comm, "Comm share (%)",      "Baseline DiD"),
  # Growth
  extract_did(m_asset_growth, "Asset growth (QoQ log)", "Baseline DiD"),
  extract_did(m_loan_growth,  "Loan growth (QoQ log)",  "Baseline DiD"),
  extract_did(m_mbl_growth,   "MBL growth (QoQ log)",   "Baseline DiD"),
  # Credit quality
  extract_did(m_dq,     "Delinquency rate (%)", "Baseline DiD"),
  extract_did(m_chgoff, "Charge-off ratio (%)", "Baseline DiD"),
  extract_did(m_pll,    "PLL / assets (%)",     "Baseline DiD"),
  # Profitability
  extract_did(m_roa, "ROA (%)",            "Baseline DiD"),
  extract_did(m_nim, "NIM (%)",            "Baseline DiD"),
  extract_did(m_cof, "Cost of funds (%)", "Baseline DiD")
)

cat("\n=== MAIN DiD RESULTS ===\n")
results_main |>
  mutate(Result = paste0(Beta, Stars, " (", SE, ")")) |>
  select(Outcome, Spec, Result, p_value, N_obs, Note) |>
  print(n = Inf)

write_csv(results_main,
          file.path(TABLE_PATH, "did_main_results.csv"))
message("  Saved → output/tables/did_main_results.csv")


# =============================================================================
# 6. FORMATTED REGRESSION TABLES (modelsummary)
# =============================================================================

message("── Step 4: Formatted regression tables ──────────────────────────────")

coef_map_base <- c(
  "treat_post"         = "Complex × Post-RBC",
  "complex:event_time" = "Complex × Time Trend",
  "ln_assets"          = "Log assets",
  "loan_to_asset"      = "Loans / assets",
  "cecl_adopter"       = "CECL adopter"
)

gof_map_use <- c("nobs", "r.squared")
stars_use   <- c("*" = 0.10, "**" = 0.05, "***" = 0.01)

# Table 1: Capital
modelsummary(
  list(
    "NW Ratio\n(Baseline)"   = m_nw_base,
    "NW Ratio\n(Trend adj)"  = m_nw_trend,
    "Cap Buffer\n(Baseline)" = m_buf_base,
    "Cap Buffer\n(Trend adj)"= m_buf_trend,
    "Well-Cap\n(LPM)"        = m_wellcap
  ),
  coef_map = coef_map_base, gof_map = gof_map_use,
  stars = stars_use,
  title = "Table 1. RBC Rule Impact on Capital Adequacy",
  notes = paste0(
    "Two-way FE (CU + quarter-year). SE clustered at CU level. ",
    "Trend-adjusted specs add complex-specific linear time trend."
  ),
  output = file.path(TABLE_PATH, "table1_capital.csv")
)

# Table 2: Loan spreads
modelsummary(
  list(
    "Mortgage"    = m_spread_mort,
    "New Auto"    = m_spread_nauto,
    "Used Auto"   = m_spread_uauto,
    "Comm non-RE" = m_spread_comm,
    "Comm RE"     = m_spread_cre
  ),
  coef_map = coef_map_base, gof_map = gof_map_use,
  stars = stars_use,
  title = "Table 2. RBC Rule Impact on Loan Rate Spreads Over Treasuries",
  notes = paste0(
    "Q2 and Q4 observations only (NCUA semi-annual rate reporting). ",
    "Two-way FE (CU + quarter-year). SE clustered at CU level."
  ),
  output = file.path(TABLE_PATH, "table2_loan_spreads.csv")
)

# Table 3: Portfolio & growth
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
  title = "Table 3. RBC Rule Impact on Loan Portfolio Composition and Growth",
  notes = "Two-way FE (CU + quarter-year). SE clustered at CU level.",
  output = file.path(TABLE_PATH, "table3_portfolio_growth.csv")
)

message("  Tables 1–3 saved → output/tables/")


# =============================================================================
# 7. EVENT STUDY ESTIMATION
# =============================================================================
# Estimates quarter-by-quarter treatment effects (event_time relative to RBC).
# Reference period = Q-1 (normalized to zero).
# Pre-period coefficients should be ~0 for parallel trends to hold.

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

plot_event_study <- function(es_model, outcome_label,
                             add_trend_note = FALSE) {

  # Extract interaction coefficients
  coefs <- tidy(es_model, conf.int = TRUE) |>
    filter(str_detect(term, "event_time::")) |>
    mutate(
      event_time = as.integer(str_extract(term, "-?\\d+"))
    ) |>
    filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)

  # Add reference period at zero
  ref_row <- tibble(
    term = paste0("ref"), estimate = 0, std.error = 0,
    conf.low = 0, conf.high = 0, p.value = NA_real_,
    event_time = EVENT_REF
  )
  coefs <- bind_rows(coefs, ref_row) |>
    arrange(event_time) |>
    mutate(period = if_else(event_time < 0, "Pre-RBC", "Post-RBC"))

  subtitle_txt <- paste0(
    "Event time = quarters relative to RBC (Q0 = 2022 Q1). ",
    "Reference = Q", EVENT_REF, ". 95% CI shaded."
  )
  if (add_trend_note) {
    subtitle_txt <- paste0(subtitle_txt,
                           "\nNote: Non-parallel pre-trends — interpret with caution.")
  }

  ggplot(coefs, aes(x = event_time, y = estimate)) +
    # Post-period shading
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX + 0.5,
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
      data = coefs |> filter(!is.na(p.value), p.value < 0.05,
                             event_time >= 0),
      aes(x = event_time, y = estimate),
      shape = 8, size = 2.5, color = "darkred"
    ) +
    scale_color_manual(values = c("Pre-RBC"  = "gray55",
                                  "Post-RBC" = COL_EST)) +
    scale_shape_manual(values = c("Pre-RBC"  = 1,
                                  "Post-RBC" = 16)) +
    scale_x_continuous(
      breaks = seq(EVENT_MIN, EVENT_MAX, by = 4),
      labels = function(x) paste0("Q", x)
    ) +
    labs(
      title    = paste0("Event Study: ", outcome_label),
      subtitle = subtitle_txt,
      x        = "Quarters Relative to RBC Effective Date",
      y        = paste0("Coefficient (", outcome_label, ")"),
      color = NULL, shape = NULL,
      caption  = paste0(
        "★ = p < 0.05 (post-period). ",
        "Two-way FE (CU + quarter-year). SE clustered at CU."
      )
    ) +
    theme_rbc()
}

message("── Step 6: Saving event study plots ─────────────────────────────────")

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
  list(es_loan_gr,  "Loan Growth (QoQ log × 100)",
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

# Capital panel
pA <- plot_event_study(es_nw,      "Net Worth Ratio (%)",
                       add_trend_note = TRUE)  + labs(title = "A. Net Worth Ratio")
pB <- plot_event_study(es_cap_buf, "Cap Buffer (pp)",
                       add_trend_note = TRUE)  + labs(title = "B. Capital Buffer vs 10%")
pC <- plot_event_study(es_wellcap, "Well-Capitalized")   + labs(title = "C. Well-Capitalized")
pD <- plot_event_study(es_loan_gr, "Loan Growth")        + labs(title = "D. Loan Growth")

p_capital_es <- (pA + pB) / (pC + pD) +
  plot_annotation(
    title   = "Event Study: RBC Rule Impact on Capital and Lending",
    caption = paste0(
      "Two-way FE (CU + quarter-year). SE clustered at CU. ",
      "Reference quarter = Q-1 (normalized to 0). ★ = p < 0.05."
    )
  )

ggsave(file.path(FIGURE_PATH, "fig_es_panel_capital.png"),
       p_capital_es, width = 14, height = 10, dpi = 300)

# Spread panel
pS1 <- plot_event_study(es_mort,  "Mortgage Spread (pp)")  + labs(title = "A. Mortgage Spread")
pS2 <- plot_event_study(es_nauto, "New Auto Spread (pp)")  + labs(title = "B. New Auto Spread")
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
tier_labels <- c("$500M–$1B",             "$1B–$10B",              ">$10B")

het_outcomes <- c("networth_ratio", "cap_buffer", "loan_growth",
                  "spread_mortgage", "spread_nauto", "dq_rate_var", "roa_var")
het_labels   <- c("Net Worth Ratio", "Capital Buffer", "Loan Growth",
                  "Mortgage Spread", "New Auto Spread", "DQ Rate", "ROA")

het_results <- map_dfr(seq_along(tiers), function(i) {
  tier_data <- df |> filter(complex == 0 | asset_tier == tiers[i])

  map_dfr(seq_along(het_outcomes), function(j) {
    use_data <- if (het_outcomes[j] %in% c("spread_mortgage","spread_nauto")) {
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

write_csv(het_results,
          file.path(TABLE_PATH, "heterogeneity_by_tier.csv"))

# Coefficient plot
p_het <- ggplot(
  het_results |> filter(!is.na(Beta)),
  aes(x = Beta, y = Tier,
      xmin = CI_low, xmax = CI_high, color = Tier)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = COL_ZERO) +
  geom_errorbarh(height = 0.25, linewidth = 0.8) +
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
# This is closest to a regression discontinuity design and strengthens
# the parallel trends assumption.

message("── Step 9: Near-threshold subgroup ──────────────────────────────────")

df_threshold <- df |>
  filter(near_threshold == 1 | (complex == 0 & avg_assets_pre >= 200e6))

message(sprintf("  Near-threshold sample: %s CUs",
                scales::comma(n_distinct(df_threshold$cu_number))))

threshold_results <- map_dfr(seq_along(het_outcomes), function(j) {
  use_data <- if (het_outcomes[j] %in% c("spread_mortgage","spread_nauto")) {
    df_threshold |> filter(quarter %in% c(2, 4))
  } else { df_threshold }

  m <- tryCatch(run_did(het_outcomes[j], data = use_data), error = function(e) NULL)
  if (is.null(m)) return(NULL)

  tidy_m <- tidy(m, conf.int = TRUE)
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
# 12. ROBUSTNESS CHECKS
# =============================================================================

message("── Step 10: Robustness checks ────────────────────────────────────────")

# R1: Exclude CECL adopters
# CECL day-1 adjustment reduces net worth — could confound capital results
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

# R3: Placebo treatment date (2020q1)
# A pre-COVID fake treatment date should show beta ≈ 0
# Uses only pre-RBC data so treatment/post split is clean
message("  R3: Placebo treatment (2020q1)...")
df_placebo <- df |>
  filter(q_period_num < RBC_EFFECTIVE_PERIOD) |>
  mutate(
    placebo_post  = as.integer(q_period_num >= 2020.1),
    placebo_treat = complex * placebo_post
  )

r3_nw <- feols(
  networth_ratio ~ placebo_treat + ln_assets + loan_to_asset + cecl_adopter |
    cu_number + q_period_num,
  data    = df_placebo,
  cluster = ~cu_number,
  warn = FALSE, notes = FALSE
)

# R4: Restrict control to non-complex CUs with >$300M assets
# More comparable control group — tests sensitivity to control composition
message("  R4: Restricted control group (>$300M)...")
df_r4   <- df |> filter(complex == 1 | avg_assets_pre >= 300e6)
r4_nw   <- run_did("networth_ratio", data = df_r4)
r4_loan <- run_did("loan_growth",    data = df_r4)

# Robustness summary
robustness_results <- bind_rows(
  extract_did(m_nw_base,    "Net worth ratio", "Baseline DiD"),
  extract_did(m_nw_trend,   "Net worth ratio", "Trend-adjusted DiD"),
  extract_did(r1_nw,        "Net worth ratio", "Excl. CECL adopters"),
  extract_did(r2_nw,        "Net worth ratio", "Balanced panel"),
  extract_did(r3_nw,        "Net worth ratio", "Placebo (2020q1)",
              note = "Should be ~0"),
  extract_did(r4_nw,        "Net worth ratio", "Control >$300M only"),
  extract_did(m_loan_growth, "Loan growth",    "Baseline DiD"),
  extract_did(r2_loan,       "Loan growth",    "Balanced panel"),
  extract_did(r4_loan,       "Loan growth",    "Control >$300M only")
)

cat("\n=== ROBUSTNESS CHECKS ===\n")
robustness_results |>
  mutate(Result = paste0(Beta, Stars, " (", SE, ")")) |>
  select(Outcome, Spec, Result, p_value, N_obs, Note) |>
  print(n = Inf)

write_csv(robustness_results,
          file.path(TABLE_PATH, "robustness_checks.csv"))

message("  Robustness checks saved.")


# =============================================================================
# 13. MAIN COEFFICIENT PLOT
# =============================================================================

message("── Step 11: Main results coefficient plot ────────────────────────────")

plot_results <- results_main |>
  filter(
    (Outcome == "Net worth ratio (%)"        & Spec == "Trend-adjusted DiD") |
    (Outcome == "Capital buffer vs 10% (pp)" & Spec == "Trend-adjusted DiD") |
    (Outcome == "Well-capitalized (LPM)"     & Spec == "Baseline DiD")       |
    (Outcome %in% c("Mortgage spread (pp)", "New auto spread (pp)",
                    "Used auto spread (pp)", "Comm non-RE spread (pp)",
                    "Comm RE spread (pp)")   & Spec == "Baseline DiD")       |
    (Outcome == "MBL share (%)"              & Spec == "Trend-adjusted DiD") |
    (Outcome %in% c("RE share (%)", "Auto share (%)", "Comm share (%)",
                    "Loan growth (QoQ log)", "Asset growth (QoQ log)",
                    "MBL growth (QoQ log)",
                    "Delinquency rate (%)", "Charge-off ratio (%)",
                    "PLL / assets (%)",
                    "ROA (%)", "NIM (%)", "Cost of funds (%)")
                                             & Spec == "Baseline DiD")
  ) |>
  mutate(
    Category = case_when(
      str_detect(Outcome, "worth|buffer|Well|well")  ~ "1. Capital",
      str_detect(Outcome, "spread")                  ~ "2. Loan Pricing",
      str_detect(Outcome, "share|growth")            ~ "3. Portfolio & Growth",
      str_detect(Outcome, "Delinq|Charge|PLL")       ~ "4. Credit Quality",
      str_detect(Outcome, "ROA|NIM|Cost|cost")       ~ "5. Profitability",
      TRUE                                           ~ "6. Other"
    ),
    Outcome  = factor(Outcome, levels = rev(unique(Outcome))),
    Sig      = !is.na(p_value) & p_value < 0.05
  )

p_coef <- ggplot(
  plot_results,
  aes(x = Beta, y = Outcome,
      xmin = CI_low, xmax = CI_high,
      color = Sig)
) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.6) +
  geom_errorbarh(height = 0.3, linewidth = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ Category, scales = "free_y", ncol = 1) +   # ← fixed
  scale_color_manual(
    values = c("TRUE" = COL_EST, "FALSE" = "gray65"),
    labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05")
  ) +
  labs(
    title    = "RBC Rule Impact: Main DiD Estimates (Complex × Post-RBC)",
    subtitle = paste0(
      "Two-way FE (CU + quarter-year). SE clustered at CU. 95% CI. ",
      "Capital & MBL share use trend-adjusted spec; all others baseline DiD."
    ),
    x       = "DiD Coefficient",
    y       = NULL,
    color   = NULL,
    caption = "Source: NCUA Call Report (5300) & FRED."
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
for (t in c("did_main_results.csv", "table1_capital.csv",
            "table2_loan_spreads.csv", "table3_portfolio_growth.csv",
            "heterogeneity_by_tier.csv", "near_threshold_results.csv",
            "robustness_checks.csv")) {
  cat("  →", t, "\n")
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
  cat("  →", f, "\n")
}

cat("\nPriority checks:\n")
cat("  1. fig_es_panel_capital.png  — pre-period coefs near zero?\n")
cat("  2. robustness_checks.csv     — placebo Beta ≈ 0?\n")
cat("  3. fig_main_coef_plot.png    — overall pattern of effects\n")
cat("  4. near_threshold_results.csv — RD-like robustness\n")

message("\n── 2_DiD_Estimation.R complete ✓ ────────────────────────────────────")
message("  Next step: run 3_Tables_Report.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
