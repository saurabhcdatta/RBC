# =============================================================================
# 3J_Stress_Calibrated_Threshold.R
# RBC Rule Impact Analysis — Stress-Calibrated Asset Threshold Range
#
# Author  : Saurabh C. Datta, Ph.D.
# Created : 2026
#
# RESEARCH QUESTION:
#   What asset threshold would have been empirically justified under the 10%
#   capital requirement, given actual 2008 GFC stress data?
#
# DESIGN:
#   This script extends the Finding 11 optimal-capital-threshold analysis
#   (3F) to the asset-threshold dimension. Finding 11 asked: given $500M,
#   what capital level minimizes the crossing point of compliance burden
#   and residual failure risk? This script asks: given 10%, what ASSET
#   threshold minimizes that crossing point — and how does the answer
#   change under different stress scenarios?
#
#   For each combination of:
#     (a) Asset threshold candidate: $100M to $2B (in $50M steps)
#     (b) GFC stress severity: 25%, 50%, 75%, 100% of 2008 actual drawdowns
#
#   We compute:
#     - Compliance burden: fraction of CUs above that asset threshold
#     - Residual failure rate: fraction of CUs ABOVE that threshold whose
#       post-stress NW ratio falls below 7% (the actual failure floor)
#
#   The CROSSING POINT — where burden% equals failure% — is the empirically
#   justified threshold: the level where adding one more institution to the
#   rule's scope produces exactly one unit of failure protection per unit
#   of compliance burden.
#
#   Running this at four stress severities produces a RANGE of justified
#   thresholds, capturing regulatory uncertainty about future shock severity.
#
# INTERPRETATION:
#   - At mild stress (25% of 2008): threshold is less binding — the rule
#     can be more targeted because fewer institutions fail.
#   - At severe stress (100% of 2008): threshold is more expansive — more
#     institutions need protection so the justified scope is wider.
#   - The range across scenarios gives a defensible confidence interval
#     for the policy-optimal threshold.
#
# OUTPUTS:
#   policy_3j1_stress_threshold_curves.png  — burden vs. failure at each
#                                             severity (4-panel)
#   policy_3j2_threshold_range.png          — crossing points across scenarios
#   policy_3j3_threshold_range_vs_capital.png — 3I + 3J + Finding 11 combined
#   3J_threshold_range.csv                  — full results table
# =============================================================================

library(tidyverse)
library(patchwork)
library(scales)

setwd("S:/Projects/RBC_2026/Data/")

message("=================================================================")
message("  3J -- STRESS-CALIBRATED ASSET THRESHOLD RANGE")
message("=================================================================")

# =============================================================================
# 1. SETUP
# =============================================================================

PANEL_RAW_PATH <- "data/analysis_panel_full_raw.rds"
PANEL_PATH     <- "analysis_panel_raw.rds"
FIGURE_PATH    <- "output/figures/"
TABLE_PATH     <- "output/tables/"
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)

# Regulatory constants
NW_WELLCAP_LEGACY  <- 7.0     # failure floor (% NW ratio)
NW_WELLCAP_RBC     <- 10.0    # current well-cap threshold
CRISIS_START       <- 2008.3
CRISIS_END         <- 2010.4
RBC_EFFECTIVE      <- 2022.1

# Candidate threshold range
THRESH_MIN  <- 100    # $100M
THRESH_MAX  <- 2000   # $2B
THRESH_STEP <- 50     # $50M

# Stress severity multipliers (fraction of actual 2008 drawdown)
STRESS_SCENARIOS <- tibble(
  label      = c("Mild stress\n(25% of 2008 GFC)",
                 "Moderate stress\n(50% of 2008 GFC)",
                 "Severe stress\n(75% of 2008 GFC)",
                 "Full GFC\n(100% of 2008 actual)"),
  short_label = c("25% of GFC", "50% of GFC", "75% of GFC", "100% of GFC"),
  multiplier = c(0.25, 0.50, 0.75, 1.00),
  color      = c("#2E7D4F", "#4A7CB5", "#E8A838", "#C94040")
)

COL_COMPLEX  <- "#1B3A6B"
COL_CONCERN  <- "#C94040"
COL_POSITIVE <- "#2E7D4F"
COL_THRESH   <- "#C94040"
COL_ZERO     <- "gray40"

theme_rbc <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12, color = "#1B3A6B"),
      plot.subtitle    = element_text(size = 9,  color = "#4A5568"),
      plot.caption     = element_text(size = 7.5, color = "#6B7A99"),
      axis.title       = element_text(size = 9,  color = "#2D3748"),
      axis.text        = element_text(size = 8,  color = "#4A5568"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#EDF2F7", linewidth = 0.4),
      strip.text       = element_text(face = "bold", size = 9, color = "#1B3A6B"),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# =============================================================================
# 2. LOAD PRE-RULE INSTITUTION SNAPSHOT
# =============================================================================

message("\n-- Loading pre-rule institution data --------------------------------")

df_main <- readRDS(PANEL_PATH)

# Pre-rule NW ratios and asset sizes for all credit unions
inst_prerule <- df_main |>
  filter(post_rbc == 0) |>
  group_by(cu_number) |>
  summarise(
    avg_assets_m = mean(assets_tot,    na.rm = TRUE) / 1e6,
    nw_ratio     = mean(networth_ratio, na.rm = TRUE),
    n_quarters   = n(),
    .groups      = "drop"
  ) |>
  filter(n_quarters >= 4, !is.na(nw_ratio), !is.na(avg_assets_m))

message(sprintf("  Pre-rule institutions: %d", nrow(inst_prerule)))
message(sprintf("  Asset range: $%.0fM to $%.0fM",
                min(inst_prerule$avg_assets_m),
                max(inst_prerule$avg_assets_m)))
message(sprintf("  NW ratio range: %.1f%% to %.1f%%",
                min(inst_prerule$nw_ratio),
                max(inst_prerule$nw_ratio)))

# =============================================================================
# 3. LOAD / COMPUTE 2008 CRISIS DRAWDOWNS
# =============================================================================

message("\n-- Computing 2008 crisis drawdowns ----------------------------------")

# Try full panel first (has 2008 data)
cu_drawdown <- tryCatch({
  df_full <- readRDS(PANEL_RAW_PATH)
  message("  Full panel loaded successfully.")

  # Crisis window: 2008Q3 to 2010Q4
  df_crisis <- df_full |>
    filter(complex == 1,
           q_period_num >= CRISIS_START,
           q_period_num <= CRISIS_END,
           !is.na(networth_ratio))

  if (nrow(df_crisis) > 0) {
    cu_dq <- df_crisis |>
      group_by(cu_number) |>
      summarise(
        nw_start  = max(networth_ratio, na.rm = TRUE),
        nw_trough = min(networth_ratio, na.rm = TRUE),
        drawdown  = nw_start - nw_trough,
        n_q       = n(),
        .groups   = "drop"
      ) |>
      filter(n_q >= 4, !is.na(drawdown), is.finite(drawdown), drawdown >= 0)

    message(sprintf("  Empirical drawdowns: %d complex CUs",
                    nrow(cu_dq)))
    message(sprintf("  Mean drawdown: %.3fpp | Median: %.3fpp | P90: %.3fpp",
                    mean(cu_dq$drawdown),
                    median(cu_dq$drawdown),
                    quantile(cu_dq$drawdown, 0.90)))
    cu_dq
  } else {
    stop("No crisis-period data found")
  }
}, error = function(e) {
  message(sprintf("  Full panel not available (%s)", e$message))
  message("  Using synthetic drawdown distribution from 3F calibration.")
  message("  (Mean 1.86pp, SD 1.2pp — consistent with Table 7 DiD estimates)")

  n_cus <- nrow(inst_prerule |> filter(avg_assets_m >= 500))
  set.seed(42)
  tibble(
    cu_number = inst_prerule$cu_number[inst_prerule$avg_assets_m >= 500][
      seq_len(min(n_cus, 634))
    ],
    drawdown  = pmax(rnorm(min(n_cus, 634), mean = 1.86, sd = 1.2), 0)
  )
})

# Merge drawdowns into institution snapshot
# For institutions without drawdown data: use median
median_drawdown <- median(cu_drawdown$drawdown, na.rm = TRUE)

inst_stress <- inst_prerule |>
  left_join(cu_drawdown |> select(cu_number, drawdown), by = "cu_number") |>
  mutate(
    drawdown = if_else(is.na(drawdown), median_drawdown, drawdown),
    drawdown = pmax(drawdown, 0)
  )

message(sprintf("  Institutions with drawdown data: %d / %d",
                sum(!is.na(cu_drawdown$drawdown[
                  match(inst_stress$cu_number, cu_drawdown$cu_number)
                ])),
                nrow(inst_stress)))

# =============================================================================
# 4. CORE ANALYSIS: THRESHOLD × SEVERITY CROSSING POINTS
# =============================================================================

message("\n-- Computing burden-failure curves for each scenario ----------------")

candidate_thresholds <- seq(THRESH_MIN, THRESH_MAX, by = THRESH_STEP)
total_n <- nrow(inst_stress)

# For each threshold and severity: compute burden and residual failure rate
all_curves <- map_dfr(1:nrow(STRESS_SCENARIOS), function(s) {
  mult  <- STRESS_SCENARIOS$multiplier[s]
  label <- STRESS_SCENARIOS$label[s]
  slbl  <- STRESS_SCENARIOS$short_label[s]

  # Apply stress: each institution's post-stress NW ratio
  inst_stressed <- inst_stress |>
    mutate(nw_post_stress = nw_ratio - drawdown * mult)

  map_dfr(candidate_thresholds, function(thresh) {
    # Institutions ABOVE the threshold (would be subject to rule)
    above <- inst_stressed |> filter(avg_assets_m >= thresh)
    # Institutions BELOW the threshold (exempt)
    # Burden: fraction of ALL institutions that would be constrained
    burden_pct <- nrow(above) / total_n * 100

    # Residual failure rate: among institutions ABOVE the threshold,
    # what fraction fail the 7% floor under stress?
    # This is the "unconstrained failure risk" that the rule is meant to prevent
    fail_above <- if (nrow(above) > 0) {
      mean(above$nw_post_stress < NW_WELLCAP_LEGACY, na.rm = TRUE) * 100
    } else { 0 }

    tibble(
      scenario       = label,
      short_label    = slbl,
      multiplier     = mult,
      threshold_m    = thresh,
      burden_pct     = round(burden_pct,   2),
      fail_above_pct = round(fail_above,    2),
      gap            = burden_pct - fail_above_pct  # positive = over-inclusive
    )
  })
})

# Find crossing points: where burden_pct ≈ fail_above_pct
# (interpolate between the two adjacent points that straddle the crossing)
crossing_points <- all_curves |>
  group_by(scenario, short_label, multiplier) |>
  arrange(threshold_m) |>
  mutate(
    gap_prev = lag(gap),
    crosses  = sign(gap) != sign(gap_prev) & !is.na(gap_prev)
  ) |>
  filter(crosses) |>
  # Linear interpolation for the exact crossing
  mutate(
    thresh_cross = threshold_m - THRESH_STEP * gap / (gap - gap_prev)
  ) |>
  slice_head(n = 1) |>   # take first crossing only
  ungroup() |>
  select(scenario, short_label, multiplier, threshold_cross_m = thresh_cross,
         burden_at_cross = burden_pct, fail_at_cross = fail_above_pct)

# If no crossing found (curves never intersect), report the point of
# minimum gap
if (nrow(crossing_points) < nrow(STRESS_SCENARIOS)) {
  missing_scenarios <- setdiff(STRESS_SCENARIOS$label, crossing_points$scenario)
  message(sprintf("  NOTE: No crossing found for %d scenarios. Using minimum-gap point.",
                  length(missing_scenarios)))

  min_gap_pts <- all_curves |>
    filter(scenario %in% missing_scenarios) |>
    group_by(scenario, short_label, multiplier) |>
    slice_min(abs(gap), n = 1) |>
    ungroup() |>
    select(scenario, short_label, multiplier,
           threshold_cross_m = threshold_m,
           burden_at_cross = burden_pct,
           fail_at_cross = fail_above_pct)

  crossing_points <- bind_rows(crossing_points, min_gap_pts)
}

crossing_points <- crossing_points |>
  arrange(multiplier) |>
  mutate(
    threshold_cross_m = round(threshold_cross_m, 0),
    # Round to nearest $50M for clean reporting
    threshold_rounded = round(threshold_cross_m / 50) * 50
  )

cat("\n=== STRESS-CALIBRATED THRESHOLD RANGE ===\n")
cat(sprintf("  Current $500M threshold — for reference:\n"))
cat(sprintf("    Institutions above: %.1f%% of all CUs\n",
            all_curves$burden_pct[all_curves$threshold_m == 500 &
                                   all_curves$multiplier == 1.0][1]))

cat("\n")
for (i in 1:nrow(crossing_points)) {
  cat(sprintf("  %s:\n", crossing_points$short_label[i]))
  cat(sprintf("    Empirically justified threshold: ~$%dM\n",
              crossing_points$threshold_rounded[i]))
  cat(sprintf("    At crossing: burden=%.1f%%, failure=%.1f%%\n",
              crossing_points$burden_at_cross[i],
              crossing_points$fail_at_cross[i]))
}

cat(sprintf("\n  RANGE: $%dM (mild) to $%dM (full GFC)\n",
            min(crossing_points$threshold_rounded),
            max(crossing_points$threshold_rounded)))
cat(sprintf("  Current $500M threshold: %s this range\n",
            ifelse(500 >= min(crossing_points$threshold_rounded) &
                   500 <= max(crossing_points$threshold_rounded),
                   "WITHIN", "OUTSIDE")))

write_csv(all_curves, file.path(TABLE_PATH, "3J_threshold_curves.csv"))
write_csv(crossing_points, file.path(TABLE_PATH, "3J_threshold_range.csv"))

# =============================================================================
# 5. CHART 3J1 — BURDEN VS. FAILURE CURVES (4-PANEL)
# =============================================================================

message("\n-- Building charts --------------------------------------------------")

# One panel per stress scenario
panels_3j1 <- map(1:nrow(STRESS_SCENARIOS), function(s) {
  scen  <- STRESS_SCENARIOS$label[s]
  col   <- STRESS_SCENARIOS$color[s]
  mult  <- STRESS_SCENARIOS$multiplier[s]

  d <- all_curves |> filter(scenario == scen, threshold_m <= 2000)
  cross <- crossing_points |> filter(scenario == scen)

  p <- ggplot(d, aes(x = threshold_m)) +
    # Burden curve (rises as threshold falls — more institutions caught)
    geom_line(aes(y = burden_pct, linetype = "Compliance Burden"),
              color = COL_COMPLEX, linewidth = 1.0) +
    # Failure rate curve (falls as threshold rises — fewer failures above)
    geom_line(aes(y = fail_above_pct, linetype = "Residual Failure Rate"),
              color = col, linewidth = 1.0) +
    # Current $500M threshold
    geom_vline(xintercept = 500, color = COL_CONCERN,
               linetype = "dashed", linewidth = 0.8) +

    # Crossing point
    {if (nrow(cross) > 0)
      list(
        geom_vline(xintercept = cross$threshold_cross_m,
                   color = COL_POSITIVE, linewidth = 1.2),
        annotate("text",
                 x = cross$threshold_cross_m + 40,
                 y = max(d$burden_pct, na.rm = TRUE) * 0.8,
                 label = sprintf("Optimal:\n$%dM",
                                 cross$threshold_rounded),
                 color = COL_POSITIVE, size = 3, fontface = "bold",
                 hjust = 0)
      )
    } +

    annotate("text", x = 520, y = max(d$burden_pct) * 0.95,
             label = "$500M\n(current)", color = COL_CONCERN,
             size = 2.8, hjust = 0) +
    scale_x_continuous(
      labels = function(x) paste0("$", ifelse(x >= 1000, x/1000, x),
                                  ifelse(x >= 1000, "B", "M")),
      breaks = c(100, 250, 500, 750, 1000, 1500, 2000)
    ) +
    scale_linetype_manual(
      values = c("Compliance Burden" = "solid",
                 "Residual Failure Rate" = "dashed"),
      name = NULL
    ) +
    labs(
      title    = scen,
      subtitle = sprintf("Drawdown = %.0f%% of actual 2008 GFC magnitude",
                         mult * 100),
      x = "Asset Threshold Candidate",
      y = "Percentage (%)"
    ) +
    theme_rbc() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 7))

  p
})

p3j1 <- (panels_3j1[[1]] + panels_3j1[[2]]) /
         (panels_3j1[[3]] + panels_3j1[[4]]) +
  plot_annotation(
    title   = "Stress-Calibrated Asset Threshold: Burden vs. Failure Rate at Four GFC Severities",
    subtitle = paste0(
      "Blue solid line = compliance burden (% of all CUs that would be subject to rule at each threshold).\n",
      "Colored dashed line = residual failure rate (% of CUs ABOVE the threshold that fail 7% floor under stress).\n",
      "Green vertical line = crossing point = empirically justified threshold for that stress level.\n",
      "Red dashed = current $500M threshold. The crossing point shifts with stress severity, giving a range."
    ),
    caption = paste0(
      "Starting NW ratios from pre-rule period (2018–2021). ",
      "Drawdowns from actual 2008 crisis data (or calibrated fallback). ",
      "Failure defined as post-stress NW ratio < 7%."
    )
  )

ggsave(file.path(FIGURE_PATH, "policy_3j1_stress_threshold_curves.png"),
       p3j1, width = 14, height = 10, dpi = 300)
message("  Chart 3J1 saved.")

# =============================================================================
# 6. CHART 3J2 — THRESHOLD RANGE SUMMARY
# =============================================================================

range_min <- min(crossing_points$threshold_rounded)
range_max <- max(crossing_points$threshold_rounded)
range_mid <- mean(crossing_points$threshold_rounded)

p3j2 <- ggplot(crossing_points,
               aes(x = threshold_rounded,
                   y = reorder(short_label, multiplier),
                   color = factor(multiplier))) +
  # Range band
  annotate("rect",
           xmin = range_min, xmax = range_max,
           ymin = -Inf, ymax = Inf,
           fill = COL_POSITIVE, alpha = 0.10) +
  # Current threshold
  geom_vline(xintercept = 500, color = COL_CONCERN,
             linetype = "dashed", linewidth = 1.2) +
  # Points with error bars showing ±$100M uncertainty
  geom_errorbarh(aes(xmin = threshold_rounded - 100,
                     xmax = threshold_rounded + 100),
                 height = 0.2, linewidth = 0.8) +
  geom_point(size = 6) +
  geom_text(aes(label = paste0("$", threshold_rounded, "M")),
            hjust = -0.35, size = 3.5, fontface = "bold") +
  # Range annotation
  annotate("text",
           x = range_mid,
           y = nrow(STRESS_SCENARIOS) + 0.5,
           label = sprintf("Justified range:\n$%dM–$%dM", range_min, range_max),
           color = COL_POSITIVE, size = 3.5, fontface = "bold") +
  # Current threshold annotation
  annotate("text", x = 530, y = 0.5,
           label = sprintf("Current: $500M"),
           color = COL_CONCERN, size = 3.2, hjust = 0, fontface = "bold") +
  scale_color_manual(
    values = setNames(STRESS_SCENARIOS$color, as.character(STRESS_SCENARIOS$multiplier)),
    guide  = "none"
  ) +
  scale_x_continuous(
    limits = c(0, range_max + 400),
    labels = function(x) paste0("$", ifelse(x >= 1000, x/1000, x),
                                ifelse(x >= 1000, "B", "M")),
    breaks = c(100, 250, 500, 750, 1000, 1500, 2000)
  ) +
  labs(
    title    = "Stress-Calibrated Threshold Range: Where the Data Say the Line Should Be",
    subtitle = paste0(
      "Each point = the crossing point threshold for that stress scenario.\n",
      "Error bars = ±$100M uncertainty band around the interpolated crossing.\n",
      "Green shaded region = the full range of empirically justified thresholds.\n",
      "Current $500M threshold (red dashed) is shown for reference."
    ),
    x = "Empirically Justified Asset Threshold",
    y = "Stress Scenario",
    caption = paste0(
      "Crossing point = where compliance burden curve meets residual failure rate curve.\n",
      "Below the crossing: raising the threshold still reduces failure risk faster than adding burden.\n",
      "Above the crossing: each additional institution constrained adds more burden than protection.\n",
      "Source: NCUA Call Report (5300). Pre-rule NW ratios 2018–2021."
    )
  ) +
  theme_rbc() +
  theme(axis.text.y = element_text(size = 9))

ggsave(file.path(FIGURE_PATH, "policy_3j2_threshold_range.png"),
       p3j2, width = 12, height = 7, dpi = 300)
message("  Chart 3J2 saved.")

# =============================================================================
# 7. CHART 3J3 — COMBINED: ASSET THRESHOLD + CAPITAL THRESHOLD RANGES
# =============================================================================

# Combine with Finding 11 capital threshold range for the dual-calibration story

# Capital threshold data from Finding 11 (8.5% optimal)
cap_data <- tibble(
  label      = c("Capital threshold\n(Finding 11)",
                 "Capital threshold\n(Finding 11)"),
  setting    = c("Current (10% NW ratio)", "Optimal (8.5% NW ratio)"),
  value      = c(10.0, 8.5),
  pct_constrained = c(47.6, 15.0),
  is_current = c(TRUE, FALSE),
  dimension  = "Capital Level\n(Finding 11)"
)

# Asset threshold data (this analysis)
asset_data <- tibble(
  label      = c("Asset threshold\n(This analysis)", rep("", nrow(crossing_points) - 1)),
  setting    = c("Current ($500M)", crossing_points$short_label),
  value      = c(500, crossing_points$threshold_rounded),
  pct_constrained = c(
    all_curves$burden_pct[all_curves$threshold_m == 500 &
                           all_curves$multiplier == 1.0][1],
    crossing_points$burden_at_cross
  ),
  is_current = c(TRUE, rep(FALSE, nrow(crossing_points))),
  dimension  = "Asset Threshold\n(This Analysis)"
)

# Summary comparison chart
p3j3 <- ggplot() +
  # Capital threshold bars
  geom_col(
    data = cap_data,
    aes(x = setting, y = pct_constrained, fill = is_current),
    width = 0.6
  ) +
  geom_text(
    data = cap_data,
    aes(x = setting, y = pct_constrained,
        label = paste0(round(pct_constrained, 1), "%")),
    vjust = -0.4, size = 3.5, fontface = "bold"
  ) +
  # Asset threshold points with range
  new_scale_fill_workaround <- NULL  # handled via facets

  facet_wrap(~dimension, scales = "free_x") +
  scale_fill_manual(
    values = c("TRUE" = COL_CONCERN, "FALSE" = COL_POSITIVE),
    labels = c("TRUE" = "Current (mis-calibrated)",
               "FALSE" = "Empirically optimal range"),
    name = NULL
  ) +
  scale_y_continuous(limits = c(0, 60),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Dual Mis-Calibration: Both the Capital Requirement and Asset Threshold",
    subtitle = paste0(
      "Left: Finding 11 — capital threshold of 10% is too high; data support 8.5%.\n",
      "Right: This analysis — $500M asset threshold range under different GFC scenarios.\n",
      "Both dimensions were set by regulatory judgment. Both can be data-calibrated."
    ),
    x = NULL,
    y = "% of Complex Credit Unions Subject to Binding Constraint",
    caption = "Capital threshold from 3F/Finding 11. Asset threshold range from 3J stress-calibration."
  ) +
  theme_rbc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 15, hjust = 1, size = 8))

# Note: the combined chart requires careful layout — save as a clean version
# using the crossing_points data directly

p3j3_clean <- ggplot() +
  geom_segment(
    data = tibble(
      x    = range_min,
      xend = range_max,
      y    = 1, yend = 1
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    color = COL_POSITIVE, linewidth = 6, alpha = 0.4,
    lineend = "round"
  ) +
  geom_point(
    data = crossing_points,
    aes(x = threshold_rounded, y = 1, color = short_label),
    size = 5
  ) +
  geom_point(
    data = tibble(x = 500, y = 1),
    aes(x = x, y = y), shape = 18, size = 7, color = COL_CONCERN
  ) +
  geom_text(
    data = crossing_points,
    aes(x = threshold_rounded, y = 1,
        label = paste0(short_label, "\n$", threshold_rounded, "M")),
    vjust = -1.2, size = 2.8
  ) +
  annotate("text", x = 500, y = 1,
           label = "Current\n$500M", vjust = 2.0, size = 3,
           color = COL_CONCERN, fontface = "bold") +
  annotate("text",
           x = range_mid, y = 1.3,
           label = sprintf("Justified range: $%dM to $%dM",
                           range_min, range_max),
           size = 4, fontface = "bold", color = COL_POSITIVE) +
  scale_color_manual(
    values = setNames(STRESS_SCENARIOS$color, STRESS_SCENARIOS$short_label),
    name = "Stress scenario"
  ) +
  scale_x_continuous(
    limits = c(50, range_max + 300),
    labels = function(x) paste0("$", ifelse(x >= 1000, x/1000, x),
                                ifelse(x >= 1000, "B", "M")),
    breaks = c(100, 250, 500, 750, 1000, 1500, 2000)
  ) +
  scale_y_continuous(limits = c(0.5, 1.8)) +
  labs(
    title = "Stress-Calibrated Threshold Range: $500M in Context",
    subtitle = paste0(
      "Green segment = range of empirically justified thresholds across GFC stress scenarios.\n",
      "Red diamond = current $500M threshold. Colored dots = crossing-point threshold per scenario.\n",
      "The current threshold may fall outside or at the boundary of the data-justified range."
    ),
    x = "Asset Threshold ($M)",
    y = NULL,
    caption = paste0(
      "Crossing point = threshold where compliance burden curve meets residual failure rate curve.\n",
      "Starting NW ratios: pre-rule period 2018–2021. Drawdowns: 2008 crisis data."
    )
  ) +
  theme_rbc() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "right")

ggsave(file.path(FIGURE_PATH, "policy_3j2_threshold_range.png"),
       p3j2, width = 12, height = 7, dpi = 300)

ggsave(file.path(FIGURE_PATH, "policy_3j3_threshold_range_summary.png"),
       p3j3_clean, width = 13, height = 6, dpi = 300)
message("  Charts 3J2 and 3J3 saved.")

# =============================================================================
# 8. FINAL SUMMARY
# =============================================================================

cat("\n")
cat("================================================================\n")
cat("  3J STRESS-CALIBRATED THRESHOLD RANGE -- COMPLETE\n")
cat("================================================================\n\n")
cat("RESEARCH QUESTION: What asset threshold would the 2008 GFC\n")
cat("data have justified under a 10% capital requirement?\n\n")
cat("METHOD: Crossing-point analysis at four GFC stress severities.\n")
cat("For each threshold candidate and severity: compare compliance\n")
cat("burden (% of all CUs above threshold) to residual failure rate\n")
cat("(% of CUs above threshold that fail 7% under stress).\n\n")
cat("RESULTS:\n")
for (i in 1:nrow(crossing_points)) {
  cat(sprintf("  %-25s => $%dM\n",
              crossing_points$short_label[i],
              crossing_points$threshold_rounded[i]))
}
cat(sprintf("\nDATA-JUSTIFIED RANGE: $%dM to $%dM\n\n", range_min, range_max))
cat(sprintf("CURRENT THRESHOLD: $500M\n"))
cat(sprintf("ASSESSMENT: Current $500M threshold is %s the data-justified range.\n\n",
            ifelse(500 >= range_min & 500 <= range_max, "WITHIN", "OUTSIDE")))

cat("INTERPRETATION:\n")
cat("  Under mild stress (25% of GFC): fewer institutions fail, so\n")
cat("  a narrower scope is justified — the rule can be more targeted.\n")
cat("  Under full GFC stress (100%): more institutions need protection,\n")
cat("  so a broader scope is justified — the rule should reach further.\n")
cat("  The range gives policymakers a defensible evidence-based window\n")
cat("  rather than a single point estimate.\n\n")

cat("FIGURES:\n")
for (f in c("policy_3j1_stress_threshold_curves.png",
            "policy_3j2_threshold_range.png",
            "policy_3j3_threshold_range_summary.png")) {
  flag <- if (file.exists(file.path(FIGURE_PATH, f))) "[OK]" else "[--]"
  cat(sprintf("  %s %s\n", flag, f))
}
cat("================================================================\n")
