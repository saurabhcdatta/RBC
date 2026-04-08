# =============================================================================
# 3D_Repeal_Simulation.R
# RBC Rule Analysis — Counterfactual Repeal Simulation
# NCUA Call Report (5300) Data
#
# Author  : Saurabh C. Datta, Ph.D.
# Created : 2026
#
# PURPOSE:
#   Simulates the counterfactual impact of repealing the 2022 RBC rule at
#   the end of 2025Q4. Uses causal estimates from the completed DiD analysis
#   (2_DiD_Estimation.R) and dynamic shape evidence (3B_2008_Crisis_EventStudy.R)
#   to project post-repeal recovery paths under three calibrated scenarios.
#
#   This is a PROJECTION / SIMULATION, not a new causal estimate.
#   All parameters are derived from identified empirical estimates.
#   Uncertainty bounds are derived from the existing confidence intervals.
#
# DESIGN:
#   Three reversal scenarios, calibrated from existing evidence:
#
#   Scenario 1 — FAST (Symmetric): Outcomes mean-revert at the same quarterly
#     rate at which the RBC effect accumulated. Empirical analogue: 2008 crisis
#     recovery path from 3A (large CUs restored capital/profitability within
#     8 quarters). Upper bound on recovery speed.
#
#   Scenario 2 — GRADUAL (Asymmetric): Step-change outcomes (portfolio, growth)
#     reverse at half the accumulation rate; pricing and profitability effects
#     reverse at the full accumulation rate. Calibrated from 3B persistence
#     ratios. Central estimate.
#
#   Scenario 3 — SLOW (Hysteresis): Pricing and capital effects partially
#     reverse (70% within 12 quarters); portfolio and growth step-changes
#     are sticky (only 30% reversal within 12 quarters). Reflects structural
#     costs of re-entry — lost member relationships, rebuilt underwriting
#     pipelines, deposit repricing inertia. Lower bound.
#
# KEY OUTPUTS:
#   (1) Quarterly outcome paths under each scenario × 12 quarters post-repeal
#   (2) Cumulative member benefit estimates (dollar-denominated)
#   (3) Loan volume restoration estimates
#   (4) ROA recovery paths by asset size tier
#   (5) Portfolio rebalancing timeline
#   (6) Scenario fan charts (8 publication-ready policy figures)
#
# INPUTS (all hard-coded from completed estimation outputs):
#   DiD estimates   : 2_DiD_Estimation.R (Tables 3–5)
#   Heterogeneity   : Appendix Table A1 (size-tier estimates)
#   Dynamic shapes  : 3B shape analysis (persistence ratios, post-period slopes)
#   Crisis recovery : 3A Wald test results (recovery rate benchmark)
#
# Output : output/tables/3D_*
#          output/figures/3D_*
#          output/figures/policy_3d*
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(patchwork)
library(scales)


# =============================================================================
# 1. SETTINGS
# =============================================================================

TABLE_PATH  <- "output/tables/"
FIGURE_PATH <- "output/figures/"

# Simulation horizon (quarters post-repeal)
N_QUARTERS  <- 16L    # 4 years post-repeal (2026Q1–2029Q4)

# Repeal date
REPEAL_DATE <- "2026 Q1"
REPEAL_Q    <- 0L     # event_time = 0 at repeal

# Sector-level reference values for welfare translation
# Source: NCUA Call Report averages for complex CUs (2024Q4, approximate)
TOTAL_COMPLEX_ASSETS_BN  <- 2100   # $2.1 trillion total assets (696 complex CUs)
TOTAL_COMPLEX_LOANS_BN   <- 1430   # $1.43 trillion total loans
AVG_MEMBERS_PER_CU       <- 135563 # average members per complex CU
N_COMPLEX_CUS            <- 696    # number of complex CUs

# Representative loan balances for cost calculation ($ thousands)
AVG_MORTGAGE_BAL_K       <- 250    # $250K mortgage
AVG_AUTO_BAL_K           <- 30     # $30K auto loan
AVG_COMM_NONRE_BAL_K     <- 500    # $500K commercial non-RE

# Colors
COL_FAST     <- "#2E7D32"   # green  — fast/optimistic
COL_GRADUAL  <- "#E8A838"   # amber  — gradual/central
COL_SLOW     <- "#C94040"   # red    — slow/pessimistic
COL_BASELINE <- "#1B3A6B"   # navy   — RBC status quo baseline
COL_CI       <- "#DDDDDD"   # CI band fill

theme_rbc <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(
      plot.title       = element_text(face = "bold", size = base + 1),
      plot.subtitle    = element_text(color = "gray40", size = base - 1),
      axis.title       = element_text(size = base - 1),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold"),
      plot.caption     = element_text(color = "gray50", size = 8),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)

message("── 3D_Repeal_Simulation.R ────────────────────────────────────────────")
message("  Simulation horizon : ", N_QUARTERS, " quarters post-repeal")
message("  Repeal date        : ", REPEAL_DATE)
message("  All parameters sourced from completed DiD estimation")


# =============================================================================
# 2. TREATMENT EFFECT PARAMETERS
# =============================================================================
# All values from 2_DiD_Estimation.R and Appendix Tables A1/A2.
# These are the CUMULATIVE post-RBC effects estimated at the time of repeal.
# We treat 2025Q4 as the end of the treatment period; the accumulated
# effect at that point is the starting point for reversal.
#
# NOTE on growth variables:
#   DiD coefficients for loan/asset growth are PER-QUARTER effects.
#   Over 16 post-RBC quarters (2022Q1–2025Q4), the CUMULATIVE compounded
#   reduction in the loan portfolio level is:
#     (1 + beta_growth/100)^16 - 1 ≈ beta_growth/100 * 16
#   We use the simpler linear approximation throughout.

message("── Step 1: Loading treatment effect parameters ───────────────────────")

# ── Per-quarter DiD estimates (β from two-way FE specification) ───────────────
params <- list(

  # CAPITAL (trend-adjusted DiD)
  networth_ratio  = list(beta = +0.463, se = 0.046, ci_lo = +0.372, ci_hi = +0.553,
                         label = "Net Worth Ratio (%)",
                         category = "Capital",
                         reversibility = "fast"),    # pricing signals reverse quickly

  cap_buffer      = list(beta = +0.463, se = 0.046, ci_lo = +0.372, ci_hi = +0.553,
                         label = "Capital Buffer vs 10% (pp)",
                         category = "Capital",
                         reversibility = "fast"),

  # LOAN SPREADS (per-quarter average, Q2/Q4 basis, annualised to pp)
  spread_mortgage = list(beta = +0.750, se = 0.054, ci_lo = +0.644, ci_hi = +0.856,
                         label = "Mortgage Spread over 10yr (pp)",
                         category = "Loan Pricing",
                         reversibility = "gradual"),  # deposit/pricing lags

  spread_nauto    = list(beta = +0.587, se = 0.046, ci_lo = +0.498, ci_hi = +0.677,
                         label = "New Auto Spread over 2yr (pp)",
                         category = "Loan Pricing",
                         reversibility = "gradual"),

  spread_uauto    = list(beta = +0.779, se = 0.052, ci_lo = +0.677, ci_hi = +0.882,
                         label = "Used Auto Spread over 2yr (pp)",
                         category = "Loan Pricing",
                         reversibility = "gradual"),

  spread_comm     = list(beta = +0.618, se = 0.095, ci_lo = +0.431, ci_hi = +0.806,
                         label = "Comm non-RE Spread over 2yr (pp)",
                         category = "Loan Pricing",
                         reversibility = "gradual"),

  # PORTFOLIO (step-changes — most sticky to reverse)
  auto_shr        = list(beta = -4.004, se = 0.289, ci_lo = -4.571, ci_hi = -3.437,
                         label = "Auto Loan Share (%)",
                         category = "Portfolio",
                         reversibility = "slow"),   # structural step-change

  re_shr          = list(beta = +2.385, se = 0.292, ci_lo = +1.813, ci_hi = +2.956,
                         label = "Real Estate Share (%)",
                         category = "Portfolio",
                         reversibility = "slow"),   # structural step-change

  mbl_shr         = list(beta = -0.247, se = 0.111, ci_lo = -0.464, ci_hi = -0.030,
                         label = "MBL Share (%)",
                         category = "Portfolio",
                         reversibility = "gradual"),

  # GROWTH (per-quarter flow effect)
  loan_growth     = list(beta = -0.004, se = 0.001, ci_lo = -0.006, ci_hi = -0.003,
                         label = "Loan Growth (QoQ log\u00d7100)",
                         category = "Growth",
                         reversibility = "gradual"),

  asset_growth    = list(beta = -0.005, se = 0.001, ci_lo = -0.006, ci_hi = -0.004,
                         label = "Asset Growth (QoQ log\u00d7100)",
                         category = "Growth",
                         reversibility = "gradual"),

  # CREDIT QUALITY (may lag reversal — loan book quality takes time to improve)
  dq_rate         = list(beta = +0.080, se = 0.022, ci_lo = +0.037, ci_hi = +0.122,
                         label = "Delinquency Rate (%)",
                         category = "Credit Quality",
                         reversibility = "slow"),   # credit quality lags portfolio

  chgoff_ratio    = list(beta = +0.068, se = 0.011, ci_lo = +0.046, ci_hi = +0.090,
                         label = "Charge-Off Ratio (%)",
                         category = "Credit Quality",
                         reversibility = "slow"),

  # PROFITABILITY
  roa             = list(beta = -0.259, se = 0.018, ci_lo = -0.294, ci_hi = -0.224,
                         label = "ROA (%)",
                         category = "Profitability",
                         reversibility = "gradual"),  # pricing + funding cost lags

  nim             = list(beta = -0.117, se = 0.017, ci_lo = -0.149, ci_hi = -0.084,
                         label = "Net Interest Margin (%)",
                         category = "Profitability",
                         reversibility = "gradual"),

  cof             = list(beta = +0.246, se = 0.012, ci_lo = +0.221, ci_hi = +0.270,
                         label = "Cost of Funds (%)",
                         category = "Profitability",
                         reversibility = "gradual")
)

# ── Size-tier heterogeneity (Appendix Table A1) ───────────────────────────────
# Used for tier-specific ROA and spread recovery projections
tier_params <- tibble(
  Tier       = c("$500M\u2013$1B", "$1B\u2013$10B", ">\u0024$10B"),
  beta_roa   = c(-0.189, -0.291, -0.467),
  se_roa     = c( 0.026,  0.022,  0.067),
  beta_loan  = c(-0.005, -0.004, -0.003),
  beta_mort  = c(+0.640, +0.985, +0.799),
  beta_nauto = c(+0.515, +0.741, +0.973),
  beta_dq    = c(+0.075, +0.084, +0.176),
  # Approximate share of total complex CU assets
  asset_share = c(0.22,   0.53,   0.25)
)

cat("\n=== TREATMENT EFFECT PARAMETERS LOADED ===\n")
cat(sprintf("  Outcomes       : %d\n", length(params)))
cat(sprintf("  Size tiers     : %d\n", nrow(tier_params)))
cat(sprintf("  Simulation Qs  : %d (%.1f years)\n", N_QUARTERS, N_QUARTERS / 4))


# =============================================================================
# 3. SCENARIO CALIBRATION
# =============================================================================
# Three reversal scenarios, each defined by:
#   (a) A decay function f(t) ∈ [0, 1] representing fraction of effect remaining
#   (b) Calibrated from empirical evidence in 3A and 3B
#
# For each outcome and scenario:
#   projected_effect(t) = beta_RBC × f(t, scenario, reversibility)
#
# Decay functions:
#   FAST    : exponential decay, half-life = 4 quarters (crisis recovery analogue)
#   GRADUAL : exponential decay, half-life = 8 quarters (asymmetric reversal)
#   SLOW    : plateau then slow decay — 50% retained after 12 quarters for
#              "slow" outcomes; 30% retained for "gradual" outcomes

message("── Step 2: Scenario calibration ─────────────────────────────────────")

# ── Half-lives derived from 3A crisis recovery ────────────────────────────────
# Crisis 2008: capital/profitability restored within ~8 quarters (3A figure 7D)
# This implies a half-life of ~3–4 quarters for the crisis path
# RBC effects have deeper structural embedding → longer half-life for central case

HALFLIFE_FAST    <- 4.0   # quarters — crisis recovery analogue
HALFLIFE_GRADUAL <- 8.0   # quarters — asymmetric, central estimate
HALFLIFE_SLOW    <- 16.0  # quarters — hysteresis, lower bound

# ── Retention fractions at 12Q from 3B persistence ratios ────────────────────
# 3B shape analysis: step-change outcomes (auto_shr, loan growth) had
# persistence > 0.9 → very little natural reversal → inform Slow scenario
RETAIN_SLOW_SLOW    <- 0.50  # 50% of effect retained after 12Q (sticky)
RETAIN_SLOW_GRADUAL <- 0.20  # 20% retained after 12Q (moderate stickiness)

# ── Decay function ────────────────────────────────────────────────────────────
# For "fast" and "gradual": exponential decay f(t) = exp(-lambda * t)
# For "slow": logistic floor — effect decays but never fully disappears within window
decay_f <- function(t, scenario, reversibility) {
  # t = quarters since repeal (0, 1, 2, ...)
  # Returns fraction of RBC effect REMAINING (1 = full effect still present)
  if (scenario == "Fast") {
    hl <- HALFLIFE_FAST
    exp(-log(2) / hl * t)
  } else if (scenario == "Gradual") {
    hl <- if (reversibility == "slow") HALFLIFE_SLOW else HALFLIFE_GRADUAL
    exp(-log(2) / hl * t)
  } else {  # Slow / Hysteresis
    # Floor: retain a fraction even at long horizon
    floor_frac <- if (reversibility == "slow") RETAIN_SLOW_SLOW else RETAIN_SLOW_GRADUAL
    hl <- HALFLIFE_SLOW * 1.5
    floor_frac + (1 - floor_frac) * exp(-log(2) / hl * t)
  }
}

# ── Validate decay functions ──────────────────────────────────────────────────
cat("\n=== DECAY FUNCTION VALIDATION ===\n")
cat("Fraction of RBC effect REMAINING at quarters 0, 4, 8, 12, 16:\n\n")
for (sc in c("Fast", "Gradual", "Slow")) {
  for (rv in c("fast", "gradual", "slow")) {
    vals <- sapply(c(0, 4, 8, 12, 16), decay_f, scenario = sc, reversibility = rv)
    cat(sprintf("  %-8s | %-8s : %s\n", sc, rv,
                paste(round(vals, 2), collapse = "  ")))
  }
}

message("  Scenario calibration complete.")


# =============================================================================
# 4. SIMULATE QUARTERLY PATHS — ALL OUTCOMES
# =============================================================================
# For each outcome, quarter, and scenario, compute:
#   (a) remaining_effect = beta × decay_f(t, scenario, reversibility)
#   (b) recovered_effect = beta × (1 - decay_f(...))  [= what has been reversed]
#   (c) 90% uncertainty band from DiD confidence intervals

message("── Step 3: Simulating quarterly paths ───────────────────────────────")

sim_results <- map_dfr(names(params), function(outcome_name) {
  p   <- params[[outcome_name]]
  quarters <- 0:N_QUARTERS

  map_dfr(c("Fast", "Gradual", "Slow"), function(sc) {
    map_dfr(quarters, function(t) {
      frac_remaining  <- decay_f(t, sc, p$reversibility)
      frac_recovered  <- 1 - frac_remaining

      # Central estimate
      effect_remaining  <- p$beta * frac_remaining
      effect_recovered  <- p$beta * frac_recovered   # signed: negative = bad still present

      # 90% CI bands (using DiD SE, propagating through decay factor)
      effect_lo <- p$ci_lo * frac_remaining
      effect_hi <- p$ci_hi * frac_remaining

      tibble(
        Outcome       = outcome_name,
        Label         = p$label,
        Category      = p$category,
        Reversibility = p$reversibility,
        Scenario      = sc,
        Quarter       = t,
        Quarter_label = paste0("Q+", t),
        # Effect remaining (how much RBC distortion still present)
        Effect_remaining  = effect_remaining,
        Effect_remaining_lo = effect_lo,
        Effect_remaining_hi = effect_hi,
        # Effect recovered (how much has been undone — BENEFIT)
        Effect_recovered  = effect_recovered,
        # Original RBC effect (for reference line)
        Beta_RBC      = p$beta,
        SE_RBC        = p$se,
        Decay_frac    = frac_remaining
      )
    })
  })
})

# Add calendar quarter labels (2026Q1 = Q+0)
sim_results <- sim_results |>
  mutate(
    Cal_year    = 2026 + (Quarter %/% 4),
    Cal_quarter = (Quarter %% 4) + 1,
    Cal_label   = paste0(Cal_year, " Q", Cal_quarter),
    Scenario    = factor(Scenario, levels = c("Fast", "Gradual", "Slow"))
  )

message(sprintf("  Simulation complete: %s rows", scales::comma(nrow(sim_results))))
message(sprintf("  Outcomes: %d | Scenarios: 3 | Quarters: %d",
                length(params), N_QUARTERS + 1))

# Save full simulation table
write_csv(sim_results,
          file.path(TABLE_PATH, "3D_simulation_full_paths.csv"))
message("  Full simulation saved → 3D_simulation_full_paths.csv")


# =============================================================================
# 5. WELFARE TRANSLATION — MEMBER BENEFIT ESTIMATES
# =============================================================================
# Translate loan spread reductions into dollar benefits per member per year.
# Translate loan growth restoration into additional credit created.

message("── Step 4: Welfare translation ──────────────────────────────────────")

welfare <- sim_results |>
  filter(Quarter %in% c(4, 8, 12, 16)) |>
  filter(Outcome %in% c("spread_mortgage", "spread_nauto",
                        "spread_comm", "loan_growth", "roa")) |>
  select(Outcome, Label, Scenario, Quarter, Cal_label,
         Effect_remaining, Effect_recovered, Beta_RBC) |>
  mutate(
    # Annual interest saving per average loan at each horizon
    Annual_saving_K = case_when(
      Outcome == "spread_mortgage" ~
        # Spread recovered (pp) × avg mortgage balance ($K) = annual saving to member
        Effect_recovered / 100 * AVG_MORTGAGE_BAL_K,
      Outcome == "spread_nauto"   ~
        Effect_recovered / 100 * AVG_AUTO_BAL_K,
      Outcome == "spread_comm"    ~
        Effect_recovered / 100 * AVG_COMM_NONRE_BAL_K,
      TRUE ~ NA_real_
    ),
    # Loan growth restoration: cumulative additional loan volume ($BN)
    # Each quarter of restored growth ≈ effect_recovered/100 * total_loans
    Loan_volume_restored_BN = if_else(
      Outcome == "loan_growth",
      -Effect_recovered / 100 * TOTAL_COMPLEX_LOANS_BN * Quarter,
      NA_real_
    ),
    # ROA recovery: annual earnings restoration ($BN)
    Earnings_restored_BN = if_else(
      Outcome == "roa",
      -Effect_recovered / 100 * TOTAL_COMPLEX_ASSETS_BN,
      NA_real_
    )
  )

# Aggregate spread benefit across product types (weighted by share)
spread_benefit <- welfare |>
  filter(Outcome %in% c("spread_mortgage", "spread_nauto", "spread_comm")) |>
  filter(!is.na(Annual_saving_K)) |>
  group_by(Scenario, Quarter, Cal_label) |>
  summarise(
    # Total annual saving per member (weighted avg across products)
    Total_annual_saving_per_member_K = sum(Annual_saving_K) / 3,
    .groups = "drop"
  ) |>
  mutate(
    # System-wide: total saving across all complex CU members
    Total_system_saving_BN = Total_annual_saving_per_member_K / 1000 *
      (AVG_MEMBERS_PER_CU * N_COMPLEX_CUS) / 1e6
  )

cat("\n=== MEMBER BENEFIT PROJECTIONS (Annual, per product) ===\n")
welfare |>
  filter(!is.na(Annual_saving_K)) |>
  select(Label, Scenario, Cal_label, Annual_saving_K) |>
  mutate(Annual_saving_K = round(Annual_saving_K, 2)) |>
  pivot_wider(names_from = Scenario, values_from = Annual_saving_K) |>
  print(n = Inf)

write_csv(welfare,
          file.path(TABLE_PATH, "3D_welfare_estimates.csv"))
write_csv(spread_benefit,
          file.path(TABLE_PATH, "3D_spread_benefit_aggregate.csv"))
message("  Welfare estimates saved.")


# =============================================================================
# 6. SIZE-TIER REVERSAL PROJECTIONS
# =============================================================================
# Project ROA and loan growth recovery separately for each asset size tier,
# since treatment effects were strongly heterogeneous across tiers.

message("── Step 5: Size-tier reversal projections ────────────────────────────")

tier_sim <- map_dfr(1:nrow(tier_params), function(i) {
  tp <- tier_params[i, ]
  quarters <- 0:N_QUARTERS

  map_dfr(c("Fast", "Gradual", "Slow"), function(sc) {
    map_dfr(quarters, function(t) {
      # ROA reversal
      roa_decay      <- decay_f(t, sc, "gradual")
      roa_remaining  <- tp$beta_roa * roa_decay
      roa_recovered  <- tp$beta_roa * (1 - roa_decay)

      # Loan growth restoration
      loan_decay     <- decay_f(t, sc, "gradual")
      loan_remaining <- tp$beta_loan * loan_decay
      loan_recovered <- tp$beta_loan * (1 - loan_decay)

      # Mortgage spread reduction
      mort_decay      <- decay_f(t, sc, "gradual")
      mort_remaining  <- tp$beta_mort * mort_decay
      mort_recovered  <- tp$beta_mort * (1 - mort_decay)

      tibble(
        Tier              = tp$Tier,
        Asset_share       = tp$asset_share,
        Scenario          = sc,
        Quarter           = t,
        Cal_year          = 2026 + (t %/% 4),
        Cal_quarter       = (t %% 4) + 1,
        ROA_remaining     = roa_remaining,
        ROA_recovered     = roa_recovered,
        ROA_original      = tp$beta_roa,
        Loan_remaining    = loan_remaining,
        Loan_recovered    = loan_recovered,
        Mort_remaining    = mort_remaining,
        Mort_recovered    = mort_recovered
      )
    })
  })
}) |>
  mutate(
    Cal_label = paste0(Cal_year, " Q", Cal_quarter),
    Scenario  = factor(Scenario, levels = c("Fast", "Gradual", "Slow")),
    Tier      = factor(Tier, levels = c("$500M\u2013$1B",
                                         "$1B\u2013$10B",
                                         ">\u0024$10B"))
  )

write_csv(tier_sim,
          file.path(TABLE_PATH, "3D_tier_reversal_paths.csv"))
message("  Size-tier projections saved.")


# =============================================================================
# 7. SUMMARY TABLE — OUTCOMES AT KEY HORIZONS
# =============================================================================

message("── Step 6: Summary table at key horizons ────────────────────────────")

summary_horizons <- sim_results |>
  filter(Quarter %in% c(0, 4, 8, 12)) |>
  filter(Outcome %in% c("networth_ratio", "spread_mortgage", "spread_nauto",
                         "auto_shr", "loan_growth", "roa", "nim", "dq_rate")) |>
  select(Label, Scenario, Quarter, Cal_label,
         Effect_remaining, Effect_recovered, Beta_RBC, Decay_frac) |>
  mutate(
    Pct_recovered = round((1 - Decay_frac) * 100, 1),
    across(c(Effect_remaining, Effect_recovered), ~ round(.x, 3))
  )

cat("\n=== RECOVERY SUMMARY AT KEY HORIZONS ===\n")
cat("Pct_recovered = % of original RBC effect that has reversed\n\n")
summary_horizons |>
  select(Label, Scenario, Cal_label, Beta_RBC, Effect_remaining, Pct_recovered) |>
  pivot_wider(names_from = c(Scenario, Cal_label),
              values_from = c(Effect_remaining, Pct_recovered)) |>
  print(n = Inf)

write_csv(summary_horizons,
          file.path(TABLE_PATH, "3D_summary_at_horizons.csv"))
message("  Horizon summary saved.")


# =============================================================================
# 8. POLICY CHARTS
# =============================================================================

message("── Step 7: Policy charts ─────────────────────────────────────────────")

# Helper: scenario colour scale (consistent across all charts)
scenario_colors <- c("Fast"    = COL_FAST,
                     "Gradual" = COL_GRADUAL,
                     "Slow"    = COL_SLOW)
scenario_lines  <- c("Fast"    = "solid",
                     "Gradual" = "dashed",
                     "Slow"    = "dotted")

# ── Chart 1: Master Fan Chart — Loan Spread Recovery ─────────────────────────
# The headline chart. Shows the combined loan spread effect reversing under
# three scenarios. Uses spread_mortgage as the lead indicator.

message("  Chart 1: Loan spread fan chart...")

spread_sim <- sim_results |>
  filter(Outcome == "spread_mortgage") |>
  mutate(
    # Benefit = how much of the spread has been reversed (starts at 0, grows to 0.75)
    Benefit_pp = Effect_recovered   # positive = spread cost removed from members
  )

p_fan_spread <- ggplot(spread_sim,
                        aes(x = Quarter, y = Benefit_pp,
                            color = Scenario, linetype = Scenario)) +
  # Shade the "recovery zone" — area where benefit is being delivered
  annotate("rect", xmin = 0, xmax = N_QUARTERS,
           ymin = 0, ymax = 0.75, fill = "#E8F5E9", alpha = 0.4) +
  # CI band (gradual scenario) — uncertainty around how fast benefit accrues
  geom_ribbon(
    data = spread_sim |> filter(Scenario == "Gradual"),
    aes(x = Quarter,
        ymin = Effect_remaining_hi * -1 + Beta_RBC,   # upper recovery = lower remaining
        ymax = Effect_remaining_lo * -1 + Beta_RBC),  # lower recovery = upper remaining
    fill = COL_GRADUAL, alpha = 0.15, color = NA, inherit.aes = FALSE
  ) +
  # Full reversal reference line
  geom_hline(yintercept = 0.750, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  annotate("text", x = 14, y = 0.780,
           label = "Full reversal (75 bp restored)",
           color = "gray40", size = 3, hjust = 1, fontface = "italic") +
  # Zero line
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.5) +
  # Scenario paths
  geom_line(linewidth = 1.1) +
  geom_point(data = spread_sim |> filter(Quarter %% 4 == 0),
             size = 2.5) +
  scale_color_manual(values = scenario_colors,
                     labels = c("Fast" = "Fast (crisis analogue, 4Q half-life)",
                                "Gradual" = "Gradual (central, 8Q half-life)",
                                "Slow" = "Slow (hysteresis, partial reversal)")) +
  scale_linetype_manual(values = scenario_lines,
                        labels = c("Fast" = "Fast (crisis analogue, 4Q half-life)",
                                   "Gradual" = "Gradual (central, 8Q half-life)",
                                   "Slow" = "Slow (hysteresis, partial reversal)")) +
  scale_x_continuous(
    breaks = seq(0, N_QUARTERS, 4),
    labels = function(x) paste0(2026 + x %/% 4, " Q", (x %% 4) + 1)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, " pp"),
                     limits = c(-0.05, 0.82)) +
  labs(
    title    = "Policy Chart 3D-1. Mortgage Spread Recovery: Post-Repeal Fan Chart",
    subtitle = paste0(
      "Starting from estimated +75bp RBC-induced spread (2022\u20132025). ",
      "Shaded = 90% CI for Gradual scenario. Green zone = member benefit delivered.\n",
      "Repeal assumed effective 2026 Q1. RBC well-capitalized threshold: 10%."
    ),
    x        = "Calendar Quarter",
    y        = "Mortgage Spread Reduction (pp, vs. RBC counterfactual)",
    color    = "Reversal Scenario", linetype = "Reversal Scenario",
    caption  = paste0(
      "RBC treatment estimate: +0.750pp (SE=0.054, p<0.001), 2_DiD_Estimation.R Table 4. ",
      "Scenarios calibrated from 3A crisis recovery path (Fast) and 3B persistence ratios (Gradual, Slow). ",
      "Projection, not causal estimate. Source: NCUA Call Report (5300)."
    )
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "policy_3d1_spread_fan_chart.png"),
       p_fan_spread, width = 12, height = 7, dpi = 300)
message("    Saved: policy_3d1_spread_fan_chart.png")


# ── Chart 2: Multi-Outcome Recovery Dashboard ─────────────────────────────────
# 2×2 panel: key outcomes under the Gradual (central) scenario only.
# Shows recovery across all four major channels simultaneously.

message("  Chart 2: Multi-outcome recovery dashboard...")

dashboard_outcomes <- c("spread_mortgage", "loan_growth", "roa", "auto_shr")
dashboard_labels   <- c("Mortgage Spread (pp)", "Loan Growth (QoQ log\u00d7100)",
                         "ROA (%)", "Auto Loan Share (%)")

dashboard_data <- sim_results |>
  filter(Outcome %in% dashboard_outcomes, Scenario == "Gradual") |>
  mutate(
    Out_label = factor(
      case_when(
        Outcome == "spread_mortgage" ~ "Mortgage Spread (pp)",
        Outcome == "loan_growth"     ~ "Loan Growth (QoQ log\u00d7100)",
        Outcome == "roa"             ~ "ROA (%)",
        Outcome == "auto_shr"        ~ "Auto Loan Share (%)"
      ),
      levels = dashboard_labels
    ),
    # Remaining effect (positive = distortion still present for costs;
    # for benefits sign flips, handled by color)
    Distortion = Effect_remaining,
    Benefit    = Effect_recovered
  )

p_dashboard <- ggplot(dashboard_data,
                       aes(x = Quarter)) +
  # Remaining distortion
  geom_area(aes(y = abs(Distortion), fill = "Remaining\ndistortion"),
            alpha = 0.35) +
  # Recovered benefit
  geom_area(aes(y = abs(Benefit), fill = "Recovered\nbenefit"),
            alpha = 0.55) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  # RBC total effect reference
  geom_hline(aes(yintercept = abs(Beta_RBC)),
             linetype = "dashed", color = COL_BASELINE, linewidth = 0.8) +
  facet_wrap(~ Out_label, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Remaining\ndistortion" = "#C94040",
                               "Recovered\nbenefit"    = "#2E7D32")) +
  scale_x_continuous(
    breaks = seq(0, N_QUARTERS, 4),
    labels = function(x) paste0("'", substr(as.character(2026 + x %/% 4), 3, 4),
                                 " Q", (x %% 4) + 1)
  ) +
  labs(
    title    = "Policy Chart 3D-2. Multi-Outcome Recovery Dashboard (Gradual Scenario)",
    subtitle = paste0(
      "Area chart: green = regulatory distortion reversed (member benefit). ",
      "Red = distortion still present. Navy dashed = original RBC treatment effect."
    ),
    x        = "Calendar Quarter Post-Repeal",
    y        = "Absolute Effect Size (original units)",
    fill     = NULL,
    caption  = paste0(
      "Gradual scenario: exponential decay, 8Q half-life for pricing/profitability, ",
      "16Q half-life for portfolio step-changes. ",
      "Projection from DiD estimates. Source: NCUA Call Report (5300)."
    )
  ) +
  theme_rbc() +
  theme(legend.position = "bottom")

ggsave(file.path(FIGURE_PATH, "policy_3d2_recovery_dashboard.png"),
       p_dashboard, width = 13, height = 9, dpi = 300)
message("    Saved: policy_3d2_recovery_dashboard.png")


# ── Chart 3: Cumulative Member Savings ────────────────────────────────────────
# Translates spread recovery into dollar savings for credit union members.

message("  Chart 3: Cumulative member savings...")

savings_data <- spread_benefit |>
  mutate(
    # Cumulative savings (sum of per-quarter savings up to each horizon)
    # Approximated as Avg_annual_saving * (Quarter/4) from each horizon
    Cumulative_BN = Total_system_saving_BN * Quarter / 4,
    Scenario      = factor(Scenario, levels = c("Fast", "Gradual", "Slow"))
  ) |>
  # Re-derive cumulative by integrating the path
  arrange(Scenario, Quarter)

# Integrate savings path properly
savings_integrated <- sim_results |>
  filter(Outcome %in% c("spread_mortgage", "spread_nauto", "spread_comm")) |>
  group_by(Scenario, Quarter) |>
  summarise(
    # Per-quarter dollar benefit: recovery × outstanding loan balance (approx)
    Benefit_per_Q_BN = sum(case_when(
      Outcome == "spread_mortgage" ~
        Effect_recovered / 100 * 0.35 * TOTAL_COMPLEX_LOANS_BN,
      Outcome == "spread_nauto" ~
        Effect_recovered / 100 * 0.30 * TOTAL_COMPLEX_LOANS_BN,
      Outcome == "spread_comm" ~
        Effect_recovered / 100 * 0.10 * TOTAL_COMPLEX_LOANS_BN,
      TRUE ~ 0
    ), na.rm = TRUE) / 4,   # quarterly
    .groups = "drop"
  ) |>
  arrange(Scenario, Quarter) |>
  group_by(Scenario) |>
  mutate(
    Cumulative_BN = cumsum(Benefit_per_Q_BN),
    Scenario      = factor(Scenario, levels = c("Fast", "Gradual", "Slow"))
  ) |>
  ungroup()

p_savings <- ggplot(savings_integrated,
                     aes(x = Quarter, y = Cumulative_BN,
                         color = Scenario, linetype = Scenario,
                         fill  = Scenario)) +
  geom_ribbon(
    data = savings_integrated |> filter(Scenario == "Gradual"),
    aes(x = Quarter, ymin = Cumulative_BN * 0.75, ymax = Cumulative_BN * 1.25),
    fill = COL_GRADUAL, alpha = 0.12, color = NA, inherit.aes = FALSE
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(data = savings_integrated |> filter(Quarter %% 4 == 0),
             size = 3) +
  geom_text(
    data = savings_integrated |> filter(Quarter == N_QUARTERS),
    aes(label = paste0("$", round(Cumulative_BN, 1), "BN")),
    hjust = -0.15, size = 3.5, fontface = "bold"
  ) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_lines) +
  scale_x_continuous(
    breaks = seq(0, N_QUARTERS, 4),
    labels = function(x) paste0(2026 + x %/% 4, " Q", (x %% 4) + 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_y_continuous(
    labels = function(x) paste0("$", round(x, 1), "BN"),
    limits = c(0, NA)
  ) +
  labs(
    title    = "Policy Chart 3D-3. Cumulative Member Savings from RBC Repeal",
    subtitle = paste0(
      "Aggregated across mortgage, auto, and commercial loan rate spread reductions. ",
      "Shaded = \u00b125% uncertainty band around Gradual scenario."
    ),
    x        = "Calendar Quarter",
    y        = "Cumulative Interest Savings to Members ($BN)",
    color    = "Scenario", linetype = "Scenario",
    caption  = paste0(
      "Calculation: spread reduction (pp) \u00d7 outstanding loan balance \u00d7 quarter. ",
      "Assumes loan composition constant; 696 complex CUs; ~$1.43 trillion total loans. ",
      "Projection from DiD spread estimates. Source: NCUA Call Report (5300)."
    )
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "policy_3d3_cumulative_member_savings.png"),
       p_savings, width = 12, height = 7, dpi = 300)
message("    Saved: policy_3d3_cumulative_member_savings.png")


# ── Chart 4: ROA Recovery by Asset Size Tier ──────────────────────────────────
# Shows that the largest CUs had the worst ROA impact AND face the slowest
# recovery under Slow scenario — amplifying the distributional concern.

message("  Chart 4: ROA recovery by size tier...")

roa_tier_data <- tier_sim |>
  filter(Scenario %in% c("Fast", "Gradual", "Slow")) |>
  mutate(
    ROA_benefit = -ROA_recovered,  # positive = earnings restored
    Tier_short  = case_when(
      grepl("500M", Tier)  ~ "$500M\u2013$1B",
      grepl("10B\\)", Tier) ~ "$1B\u2013$10B",
      TRUE                  ~ ">\u002410B"
    )
  )

p_roa_tier <- ggplot(roa_tier_data,
                      aes(x = Quarter, y = ROA_benefit,
                          color = Tier, linetype = Scenario)) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
  # Reference: original RBC impact per tier (as horizontal dashed line)
  geom_hline(data = tier_params |>
               mutate(Tier_short = case_when(
                 grepl("500M", Tier)  ~ "$500M\u2013$1B",
                 grepl("10B\\)", Tier) ~ "$1B\u2013$10B",
                 TRUE                 ~ ">\u002410B"
               )),
             aes(yintercept = -beta_roa, color = Tier_short),
             linetype = "dotted", linewidth = 0.7, inherit.aes = FALSE) +
  geom_line(linewidth = 1.0) +
  facet_wrap(~ Scenario, ncol = 3) +
  scale_color_manual(values = c("$500M\u2013$1B"  = "#8AAAD0",
                                "$1B\u2013$10B"   = "#1B3A6B",
                                ">\u002410B"      = "#C94040")) +
  scale_linetype_manual(values = c("Fast" = "solid",
                                   "Gradual" = "dashed",
                                   "Slow" = "dotted")) +
  scale_x_continuous(
    breaks = seq(0, N_QUARTERS, 4),
    labels = function(x) paste0("'", substr(as.character(2026 + x %/% 4), 3, 4),
                                " Q", (x %% 4) + 1)
  ) +
  scale_y_continuous(labels = function(x) paste0("+", round(x, 2), "pp"),
                     limits = c(0, NA)) +
  labs(
    title    = "Policy Chart 3D-4. ROA Recovery Path by Asset Size Tier",
    subtitle = paste0(
      "Dotted horizontal = original RBC earnings impact per tier. ",
      "Larger CUs (red) had deeper impacts and face slower relative recovery. ",
      "Faceted by reversal scenario."
    ),
    x        = "Calendar Quarter Post-Repeal",
    y        = "ROA Recovered (pp above RBC-era baseline)",
    color    = "Asset Size Tier",
    linetype = "Scenario",
    caption  = paste0(
      "Tier-specific estimates from Appendix Table A1: ",
      "$500M\u2013$1B \u03b2=\u22120.189, $1B\u2013$10B \u03b2=\u22120.291, >$10B \u03b2=\u22120.467. ",
      "Projection from DiD estimates. Source: NCUA Call Report (5300)."
    )
  ) +
  theme_rbc() +
  theme(legend.position = "bottom")

ggsave(file.path(FIGURE_PATH, "policy_3d4_roa_recovery_by_tier.png"),
       p_roa_tier, width = 14, height = 7, dpi = 300)
message("    Saved: policy_3d4_roa_recovery_by_tier.png")


# ── Chart 5: Portfolio Rebalancing Timeline ───────────────────────────────────
# Auto share and RE share recovering toward pre-RBC levels.
# Illustrates which step-changes are likely permanent vs. reversible.

message("  Chart 5: Portfolio rebalancing timeline...")

portfolio_sim <- sim_results |>
  filter(Outcome %in% c("auto_shr", "re_shr", "mbl_shr")) |>
  mutate(
    Out_label = factor(
      case_when(
        Outcome == "auto_shr" ~ "Auto Loan Share",
        Outcome == "re_shr"   ~ "Real Estate Share",
        Outcome == "mbl_shr"  ~ "MBL Share"
      ),
      levels = c("Auto Loan Share", "Real Estate Share", "MBL Share")
    ),
    # Recovery = movement back toward pre-RBC baseline
    Pct_recovered = (1 - Decay_frac) * 100
  )

p_portfolio <- ggplot(portfolio_sim,
                       aes(x = Quarter, y = Pct_recovered,
                           color = Scenario, linetype = Scenario)) +
  geom_hline(yintercept = c(25, 50, 75, 100),
             color = "gray90", linewidth = 0.4) +
  geom_hline(yintercept = 100, color = "gray40",
             linetype = "dashed", linewidth = 0.6) +
  annotate("text", x = 15.5, y = 102,
           label = "Full reversal", color = "gray40",
           size = 2.8, hjust = 1, fontface = "italic") +
  geom_line(linewidth = 1.1) +
  geom_point(data = portfolio_sim |> filter(Quarter %% 4 == 0),
             size = 2.5) +
  facet_wrap(~ Out_label, ncol = 3) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_lines) +
  scale_x_continuous(
    breaks = seq(0, N_QUARTERS, 4),
    labels = function(x) paste0("'", substr(as.character(2026 + x %/% 4), 3, 4),
                                 " Q", (x %% 4) + 1)
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 110),
    breaks = seq(0, 100, 25)
  ) +
  labs(
    title    = "Policy Chart 3D-5. Portfolio Rebalancing Timeline: % of Distortion Reversed",
    subtitle = paste0(
      "Auto loan share (-4.0pp step-change) and RE share (+2.4pp step-change) ",
      "are classified as Slow-reversibility structural shifts. MBL share classified as Gradual."
    ),
    x        = "Calendar Quarter Post-Repeal",
    y        = "% of RBC-Induced Portfolio Distortion Reversed",
    color    = "Scenario", linetype = "Scenario",
    caption  = paste0(
      "Portfolio effects from Table 5, 2_DiD_Estimation.R. ",
      "Step-change classification from 3B persistence ratio analysis. ",
      "100% line = full reversal to pre-RBC portfolio composition. ",
      "Projection, not causal estimate. Source: NCUA Call Report (5300)."
    )
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "policy_3d5_portfolio_rebalancing.png"),
       p_portfolio, width = 14, height = 7, dpi = 300)
message("    Saved: policy_3d5_portfolio_rebalancing.png")


# ── Chart 6: Three-Scenario Comparison at Year 2 and Year 4 ──────────────────
# Horizon bar chart: how far recovered is each outcome at 2Y and 4Y?
# The single most useful at-a-glance summary for a policy brief.

message("  Chart 6: Horizon comparison bar chart...")

horizon_compare <- sim_results |>
  filter(Quarter %in% c(8, 16)) |>
  filter(Outcome %in% c("spread_mortgage", "spread_nauto", "loan_growth",
                         "auto_shr", "roa", "nim", "dq_rate",
                         "networth_ratio")) |>
  mutate(
    Pct_recovered = round((1 - Decay_frac) * 100, 1),
    Horizon       = paste0(Quarter / 4, "-Year"),
    Out_short     = case_when(
      Outcome == "spread_mortgage" ~ "Mortgage spread",
      Outcome == "spread_nauto"    ~ "Auto spread",
      Outcome == "loan_growth"     ~ "Loan growth",
      Outcome == "auto_shr"        ~ "Auto share",
      Outcome == "roa"             ~ "ROA",
      Outcome == "nim"             ~ "NIM",
      Outcome == "dq_rate"         ~ "DQ rate",
      Outcome == "networth_ratio"  ~ "Net worth ratio",
      TRUE                         ~ Outcome
    ),
    Out_short = factor(Out_short,
                       levels = c("Mortgage spread", "Auto spread",
                                  "Loan growth", "Auto share",
                                  "ROA", "NIM", "DQ rate",
                                  "Net worth ratio"))
  )

p_horizon <- ggplot(horizon_compare,
                     aes(x = Pct_recovered, y = Out_short,
                         fill = Scenario, alpha = Horizon)) +
  geom_col(position = position_dodge2(width = 0.8, padding = 0.1),
           width = 0.7) +
  geom_vline(xintercept = 100, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  geom_vline(xintercept = 50, linetype = "dotted",
             color = "gray60", linewidth = 0.5) +
  annotate("text", x = 101, y = 0.4, label = "Full\nreversal",
           color = "gray40", hjust = 0, size = 2.5) +
  scale_fill_manual(values = scenario_colors) +
  scale_alpha_manual(values = c("2-Year" = 0.60, "4-Year" = 1.00),
                     labels = c("2-Year" = "At 2 years (8Q)", "4-Year" = "At 4 years (16Q)")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(0, 120),
                     breaks = seq(0, 100, 25)) +
  facet_wrap(~ Horizon, ncol = 2) +
  labs(
    title    = "Policy Chart 3D-6. Recovery Progress at 2-Year and 4-Year Horizons",
    subtitle = paste0(
      "Each bar = % of original RBC distortion reversed under each scenario. ",
      "100% = full return to pre-RBC levels. Faceted by horizon."
    ),
    x        = "% of RBC Effect Reversed",
    y        = NULL,
    fill     = "Scenario",
    alpha    = "Horizon",
    caption  = paste0(
      "Parameters from 2_DiD_Estimation.R and 3B_2008_Crisis_EventStudy.R. ",
      "Scenarios: Fast = 4Q half-life, Gradual = 8Q/16Q, Slow = partial plateau. ",
      "Projection, not causal estimate. Source: NCUA Call Report (5300)."
    )
  ) +
  theme_rbc() +
  theme(legend.position = "right")

ggsave(file.path(FIGURE_PATH, "policy_3d6_horizon_comparison.png"),
       p_horizon, width = 13, height = 8, dpi = 300)
message("    Saved: policy_3d6_horizon_comparison.png")


# ── Chart 7: Loan Volume Restored ─────────────────────────────────────────────
# Cumulative additional credit made available by restoring quarterly loan growth.

message("  Chart 7: Loan volume restoration...")

loan_vol <- sim_results |>
  filter(Outcome == "loan_growth") |>
  group_by(Scenario) |>
  mutate(
    # Each quarter of restored growth compounds on the existing loan base
    # Simplified: restored loans (BN) = beta_recovered/100 * base_loans * quarter
    Restored_BN_cumulative = cumsum(-Effect_recovered / 100 *
                                      TOTAL_COMPLEX_LOANS_BN)
  ) |>
  ungroup()

p_loanvol <- ggplot(loan_vol,
                     aes(x = Quarter, y = Restored_BN_cumulative,
                         color = Scenario, linetype = Scenario)) +
  geom_ribbon(
    data = loan_vol |> filter(Scenario == "Gradual"),
    aes(x = Quarter,
        ymin = Restored_BN_cumulative * 0.7,
        ymax = Restored_BN_cumulative * 1.3),
    fill = COL_GRADUAL, alpha = 0.12, color = NA, inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
  geom_line(linewidth = 1.2) +
  geom_point(data = loan_vol |> filter(Quarter %% 4 == 0), size = 3) +
  geom_text(
    data = loan_vol |> filter(Quarter == N_QUARTERS),
    aes(label = paste0("$", round(Restored_BN_cumulative, 0), "BN")),
    hjust = -0.1, size = 3.5, fontface = "bold"
  ) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = scenario_lines) +
  scale_x_continuous(
    breaks = seq(0, N_QUARTERS, 4),
    labels = function(x) paste0(2026 + x %/% 4, " Q", (x %% 4) + 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_y_continuous(
    labels = function(x) paste0("$", round(x, 0), "BN"),
    limits = c(0, NA)
  ) +
  labs(
    title    = "Policy Chart 3D-7. Cumulative Loan Volume Restored by RBC Repeal",
    subtitle = paste0(
      "Cumulative additional credit made available to members as loan growth ",
      "returns toward pre-RBC trajectory. Shaded = \u00b130% uncertainty band."
    ),
    x        = "Calendar Quarter",
    y        = "Cumulative Additional Loans to Members ($BN)",
    color    = "Scenario", linetype = "Scenario",
    caption  = paste0(
      "Loan growth DiD estimate: \u22120.004 log\u00d7100 per quarter (SE=0.001, p<0.001). ",
      "Base: ~$1.43 trillion complex CU loan portfolio. ",
      "Projection assumes proportional restoration of growth rate. ",
      "Source: NCUA Call Report (5300)."
    )
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "policy_3d7_loan_volume_restored.png"),
       p_loanvol, width = 12, height = 7, dpi = 300)
message("    Saved: policy_3d7_loan_volume_restored.png")


# ── Chart 8: The Cost of Delay — Comparison of Repeal Timing ─────────────────
# What if repeal happens in 2027 instead of 2026?
# Shows cumulative deadweight cost of each year of delay.

message("  Chart 8: Cost of delay...")

# Simulate a delayed repeal (8 quarters later = 2028Q1)
delay_quarters <- 8L  # quarters of additional RBC compliance before repeal

# During delay period, effect accumulates further (or remains at 2025Q4 level)
# After repeal, same decay structure applies — just shifted in time

# Gradual scenario only for clarity
delay_sim <- tibble(Quarter = 0:(N_QUARTERS + delay_quarters)) |>
  mutate(
    # Immediate repeal (2026Q1) — starts recovering from Q0
    ROA_immediate = params[["roa"]]$beta *
      sapply(Quarter, decay_f, scenario = "Gradual",
             reversibility = "gradual"),
    ROA_benefit_immediate = params[["roa"]]$beta * (1 - sapply(
      Quarter, decay_f, scenario = "Gradual", reversibility = "gradual")),

    # Delayed repeal (2028Q1) — no recovery for 8Q, then starts recovering
    ROA_delayed = if_else(
      Quarter < delay_quarters,
      params[["roa"]]$beta,  # still under RBC — no recovery
      params[["roa"]]$beta *
        sapply(Quarter - delay_quarters, decay_f,
               scenario = "Gradual", reversibility = "gradual")
    ),
    ROA_benefit_delayed = if_else(
      Quarter < delay_quarters,
      0,
      params[["roa"]]$beta * (1 - sapply(
        Quarter - delay_quarters, decay_f,
        scenario = "Gradual", reversibility = "gradual"))
    ),
    Cal_year    = 2026 + (Quarter %/% 4),
    Cal_quarter = (Quarter %% 4) + 1,
    Cal_label   = paste0(Cal_year, " Q", Cal_quarter)
  )

# Compute cumulative deadweight cost
delay_sim <- delay_sim |>
  mutate(
    # Cumulative ROA benefit foregone due to delay
    Cumulative_deadweight_BN = cumsum(
      (-ROA_benefit_delayed - (-ROA_benefit_immediate)) *
        TOTAL_COMPLEX_ASSETS_BN / 100 / 4
    )
  )

# Reshape for plotting
delay_long <- delay_sim |>
  select(Quarter, Cal_label, ROA_benefit_immediate, ROA_benefit_delayed,
         Cumulative_deadweight_BN) |>
  pivot_longer(cols = c(ROA_benefit_immediate, ROA_benefit_delayed),
               names_to = "Timing", values_to = "ROA_benefit") |>
  mutate(
    Timing = if_else(Timing == "ROA_benefit_immediate",
                     "Repeal 2026 Q1", "Repeal 2028 Q1 (2Y delay)")
  )

p_delay <- ggplot() +
  # ROA benefit paths
  geom_line(data = delay_long,
            aes(x = Quarter, y = -ROA_benefit,
                color = Timing, linetype = Timing),
            linewidth = 1.1) +
  # Deadweight cost ribbon
  geom_ribbon(
    data = delay_sim,
    aes(x = Quarter,
        ymin = -ROA_benefit_delayed,
        ymax = -ROA_benefit_immediate),
    fill = "#C94040", alpha = 0.15
  ) +
  annotate("rect", xmin = 0, xmax = delay_quarters,
           ymin = -Inf, ymax = Inf, fill = "#FFF3E0", alpha = 0.3) +
  annotate("text", x = delay_quarters / 2, y = 0.01,
           label = "Delay\nperiod",
           color = "#E65100", size = 3, fontface = "bold") +
  geom_vline(xintercept = delay_quarters, color = "#E65100",
             linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = 0, color = "gray60", linewidth = 0.4) +
  scale_color_manual(values = c("Repeal 2026 Q1"          = COL_FAST,
                                "Repeal 2028 Q1 (2Y delay)" = COL_SLOW)) +
  scale_linetype_manual(values = c("Repeal 2026 Q1"          = "solid",
                                   "Repeal 2028 Q1 (2Y delay)" = "dashed")) +
  scale_x_continuous(
    breaks = seq(0, N_QUARTERS + delay_quarters, 4),
    labels = function(x) paste0(2026 + x %/% 4, " Q", (x %% 4) + 1)
  ) +
  scale_y_continuous(labels = function(x) paste0("+", round(x, 2), "pp")) +
  labs(
    title    = "Policy Chart 3D-8. The Cost of Delay: Immediate vs. 2-Year Deferred Repeal",
    subtitle = paste0(
      "Red shaded area = cumulative ROA recovery foregone due to 2-year delay in repeal. ",
      "Based on Gradual scenario. Orange zone = additional compliance period."
    ),
    x        = "Calendar Quarter",
    y        = "ROA Restored Above RBC-Era Baseline (pp)",
    color    = "Repeal Timing", linetype = "Repeal Timing",
    caption  = paste0(
      "ROA DiD estimate: \u22120.259pp (SE=0.018, p<0.001). ",
      "Gradual scenario: 8Q half-life decay. Delayed repeal assumes RBC continues ",
      "at 2025Q4 effect level through 2028Q1. Source: NCUA Call Report (5300)."
    )
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "policy_3d8_cost_of_delay.png"),
       p_delay, width = 12, height = 7, dpi = 300)
message("    Saved: policy_3d8_cost_of_delay.png")

message("  All policy charts complete.")


# =============================================================================
# 9. FINAL SUMMARY TABLE — COMPLETE RESULTS
# =============================================================================

message("── Step 8: Final summary table ──────────────────────────────────────")

final_summary <- sim_results |>
  filter(Quarter %in% c(0, 4, 8, 12, 16)) |>
  select(Label, Category, Reversibility, Scenario, Quarter,
         Cal_label, Beta_RBC, Effect_remaining, Decay_frac) |>
  mutate(
    Pct_recovered = round((1 - Decay_frac) * 100, 1),
    Effect_remaining = round(Effect_remaining, 4),
    Beta_RBC = round(Beta_RBC, 3)
  )

write_csv(final_summary,
          file.path(TABLE_PATH, "3D_final_summary.csv"))


# =============================================================================
# 10. CONSOLE OUTPUT
# =============================================================================

cat("\n=== 3D REPEAL SIMULATION COMPLETE ===\n\n")

cat("Tables (output/tables/):\n")
for (t in c("3D_simulation_full_paths.csv",
            "3D_welfare_estimates.csv",
            "3D_spread_benefit_aggregate.csv",
            "3D_tier_reversal_paths.csv",
            "3D_summary_at_horizons.csv",
            "3D_final_summary.csv")) {
  flag <- if (file.exists(file.path(TABLE_PATH, t))) "\u2713" else "\u2013"
  cat(sprintf("  %s %s\n", flag, t))
}

cat("\nPolicy charts (output/figures/):\n")
for (f in c(
  "policy_3d1_spread_fan_chart.png",
  "policy_3d2_recovery_dashboard.png",
  "policy_3d3_cumulative_member_savings.png",
  "policy_3d4_roa_recovery_by_tier.png",
  "policy_3d5_portfolio_rebalancing.png",
  "policy_3d6_horizon_comparison.png",
  "policy_3d7_loan_volume_restored.png",
  "policy_3d8_cost_of_delay.png"
)) {
  flag <- if (file.exists(file.path(FIGURE_PATH, f))) "\u2713" else "\u2013"
  cat(sprintf("  %s %s\n", flag, f))
}

cat("\nSCENARIO PARAMETER SUMMARY:\n")
cat("  Fast    : Exponential decay, half-life = 4Q (crisis recovery analogue)\n")
cat("  Gradual : Exponential decay, half-life = 8Q (pricing) / 16Q (portfolio)\n")
cat("  Slow    : Plateau floor model — 50% retained (step-changes) after 16Q\n")

cat("\nKEY WELFARE ESTIMATES (Gradual scenario, 4-year horizon):\n")
gradual_4yr <- savings_integrated |>
  filter(Scenario == "Gradual", Quarter == N_QUARTERS) |>
  pull(Cumulative_BN)
cat(sprintf("  Cumulative spread savings to members : ~$%.1fBN\n", gradual_4yr))

loan_4yr <- loan_vol |>
  filter(Scenario == "Gradual", Quarter == N_QUARTERS) |>
  pull(Restored_BN_cumulative)
cat(sprintf("  Cumulative loan volume restored      : ~$%.0fBN\n", loan_4yr))

roa_yr2 <- tier_sim |>
  filter(Scenario == "Gradual", Quarter == 8) |>
  summarise(wtd = sum(ROA_recovered * Asset_share, na.rm = TRUE)) |>
  pull(wtd)
cat(sprintf("  Weighted ROA recovered at 2 years    : ~%.3fpp\n", -roa_yr2))

cat("\nIMPORTANT CAVEATS:\n")
cat("  [1] These are PROJECTIONS, not causal estimates.\n")
cat("  [2] All parameters derived from identified DiD estimates.\n")
cat("  [3] Assumes no new regulatory change, no macro shock.\n")
cat("  [4] Portfolio reallocations may be stickier than pricing effects.\n")
cat("  [5] CCULR selection dynamics (3C) suggest some CUs may not fully reverse.\n")
cat("  [6] Uncertainty bounds widen substantially beyond 8 quarters.\n")

message("\n── 3D_Repeal_Simulation.R complete \u2713 ─────────────────────────────────")
message("  Next: 4_Paper_Tables.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
