# =============================================================================
# copy_figures.R
# Copies all policy figures (3D, 3E, 3F) from output/figures/ to
# output/paper/figures/ with their publication names (Figure9_*, 
# Figure10_*, Figure11_*).
#
# Run this after ANY of the following scripts to refresh the briefing figures:
#   3D_Repeal_Simulation.R
#   3E_Stress_Scenario_Simulation.R  
#   3F_Capital_Adequacy_Stress_Test.R
#
# You do NOT need to re-run the full 4_Paper_Tables.R just to update figures.
# Usage: source("copy_figures.R")
# =============================================================================

FIGURE_IN  <- "output/figures/"
FIGURE_OUT <- "output/paper/figures/"

dir.create(FIGURE_OUT, showWarnings = FALSE, recursive = TRUE)

figure_map <- list(
  # 3D — Repeal Simulation (Section 9)
  list("policy_3d1_spread_fan_chart.png",         "Figure9_1_Spread_Fan_Chart.png"),
  list("policy_3d2_recovery_dashboard.png",        "Figure9_2_Recovery_Dashboard.png"),
  list("policy_3d3_cumulative_member_savings.png", "Figure9_3_Cumulative_Member_Savings.png"),
  list("policy_3d4_roa_recovery_by_tier.png",      "Figure9_4_ROA_Recovery_by_Tier.png"),
  list("policy_3d5_portfolio_rebalancing.png",     "Figure9_5_Portfolio_Rebalancing.png"),
  list("policy_3d6_horizon_comparison.png",        "Figure9_6_Horizon_Comparison.png"),
  list("policy_3d7_loan_volume_restored.png",      "Figure9_7_Loan_Volume_Restored.png"),
  list("policy_3d8_cost_of_delay.png",             "Figure9_8_Cost_of_Delay.png"),

  # 3E — Stress Scenario (Section 10)
  list("policy_3e1_net_effect_capital.png",               "Figure10_1_Stress_Capital.png"),
  list("policy_3e2_net_effect_lending.png",               "Figure10_2_Stress_Lending.png"),
  list("policy_3e3_net_effect_roa.png",                   "Figure10_3_Stress_ROA_by_Tier.png"),
  list("policy_3e4_net_welfare_spreads.png",              "Figure10_4_Stress_Net_Welfare.png"),
  list("policy_3e5_stress_multiplier_sensitivity.png",    "Figure10_5_Stress_Multiplier_Sensitivity.png"),
  list("policy_3e6_scenario_comparison.png",              "Figure10_6_Stress_Scenario_Comparison.png"),
  list("policy_3e7_dq_stress_overlay.png",                "Figure10_7_Stress_DQ_Overlay.png"),
  list("policy_3e8_repeal_timing_under_stress.png",       "Figure10_8_Stress_Repeal_Timing.png"),

  # 3F — Capital Stress Test (Section 11)
  list("policy_3f1_nw_distribution_regimes.png",    "Figure11_1_NW_Distribution_Regimes.png"),
  list("policy_3f2_threshold_breach_rates.png",     "Figure11_2_Threshold_Breach_Rates.png"),
  list("policy_3f3_stress_drawdown_calibration.png","Figure11_3_Stress_Drawdown_Calibration.png"),
  list("policy_3f4_survival_frontier.png",          "Figure11_4_Survival_Frontier.png"),
  list("policy_3f5_vulnerability_heatmap.png",      "Figure11_5_Vulnerability_Heatmap.png"),
  list("policy_3f6_rule_effect_vs_tailrisk.png",    "Figure11_6_Rule_Effect_vs_Tailrisk.png"),
  list("policy_3f7_thin_buffer_zoom.png",           "Figure11_7_Thin_Buffer_Zoom.png"),
  list("policy_3f8_optimal_threshold.png",          "Figure11_8_Optimal_Threshold.png")
)

cat("\n=== COPYING POLICY FIGURES TO output/paper/figures/ ===\n\n")
n_copied <- 0L
n_missing <- 0L

for (fm in figure_map) {
  src  <- file.path(FIGURE_IN,  fm[[1]])
  dest <- file.path(FIGURE_OUT, fm[[2]])
  if (file.exists(src)) {
    ok <- file.copy(src, dest, overwrite = TRUE)
    cat(sprintf("  %s  %s\n", if (ok) "\u2713" else "!", fm[[2]]))
    n_copied <- n_copied + 1L
  } else {
    cat(sprintf("  -  NOT FOUND: %s\n", fm[[1]]))
    n_missing <- n_missing + 1L
  }
}

cat(sprintf(
  "\nResult: %d copied, %d not found.\n", n_copied, n_missing
))

if (n_missing > 0) {
  cat(sprintf(paste0(
    "\nMissing figures — check which scripts have been run:\n",
    "  Section 9  (Figure9_*)  : run 3D_Repeal_Simulation.R\n",
    "  Section 10 (Figure10_*) : run 3E_Stress_Scenario_Simulation.R\n",
    "  Section 11 (Figure11_*) : run 3F_Capital_Adequacy_Stress_Test.R\n"
  )))
}

cat("\nAfter copying, re-render the briefing with: source('render_briefing.R')\n\n")
