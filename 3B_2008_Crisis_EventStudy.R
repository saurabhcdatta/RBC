# =============================================================================
# 3B_2008_Crisis_EventStudy.R
# RBC Rule Analysis — 2008 Crisis vs. RBC Event Study Comparison
# NCUA Call Report (5300) Data
#
# Author  : [Your Name]
# Created : 2026
#
# PURPOSE:
#   Compares the DYNAMIC path of treatment effects around the 2008 crisis
#   vs. the 2022 RBC rule using event study designs on both windows.
#   While 3A established AVERAGE differences via Wald tests, 3B asks:
#
#   (1) Do the SHAPES of the impulse responses differ?
#       - Crisis: sharp shock, fast recovery?
#       - RBC: gradual accumulation, persistent effect?
#
#   (2) Are pre-trends flat in both windows?
#       - Flat pre-crisis trends validate the crisis as an exogenous shock
#       - Flat pre-RBC trends (for non-capital outcomes) validate DiD
#
#   (3) Which effects are FRONT-LOADED vs. BACK-LOADED?
#       - Front-loaded: immediate capital/lending response
#       - Back-loaded: portfolio reallocation builds over time
#
#   (4) Capital buffer dynamics: buffer proximity hypothesis
#       - Split complex CUs by pre-shock capital buffer
#       - CUs near the threshold should respond MORE (dose-response)
#
# KEY ANALYTICAL INNOVATION vs. 3A:
#   3A asked: "Are average effects the same?"
#   3B asks:  "Are the dynamic paths the same?"
#   A crisis causes a sharp V-shape (shock + recovery).
#   A regulation causes a step-change (new permanent equilibrium).
#   If RBC shows a step-change while crisis shows a V-shape,
#   that's strong evidence of a regulatory-specific mechanism.
#
# Input  : call_report.rds  (full raw data)
#          NOTE: Reuses df_crisis and df_rbc from 3A if in memory.
#                If running standalone, set STANDALONE <- TRUE below.
# Output : output/figures/3B_*
#          output/tables/3B_*
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(fixest)
library(patchwork)
library(scales)
library(broom)
library(lubridate)


# =============================================================================
# 1. SETTINGS
# =============================================================================

RAW_DATA_PATH <- "call_report.rds"
TABLE_PATH    <- "output/tables/"
FIGURE_PATH   <- "output/figures/"

# Set TRUE if running standalone (not after 3A in same session)
STANDALONE <- FALSE

# These must match 3A exactly
CRISIS_DATE      <- 2008.3
CRISIS_START     <- 2004.1
CRISIS_END       <- 2013.4
RBC_DATE         <- 2022.1
RBC_START        <- 2018.1
RBC_END          <- 2024.4
SIZE_THRESHOLD   <- 500e6
CRISIS_PRE_START <- 2007.3
CRISIS_PRE_END   <- 2008.2
RBC_PRE_START    <- 2021.1
RBC_PRE_END      <- 2021.4

EVENT_MIN <- -12
EVENT_MAX <-  10
EVENT_REF <-  -1

controls_base <- "ln_assets + loan_to_asset"

# Colors — consistent throughout project
COL_CRISIS   <- "#C94040"    # red
COL_RBC      <- "#1B3A6B"    # navy
COL_CI_CRISIS <- "#E8A0A0"   # light red for CI
COL_CI_RBC    <- "#8AAAD0"   # light navy for CI
COL_THIN      <- "#2E7D32"   # green — thin-buffer CUs
COL_THICK     <- "#F57F17"   # amber — thick-buffer CUs

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

dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 2. REBUILD DATA IF STANDALONE
# =============================================================================

if (STANDALONE) {

  message("── Standalone mode: rebuilding panels from raw data ──────────────")

  df_raw <- readRDS(RAW_DATA_PATH)

  merged_cus <- df_raw |>
    filter(outcome %in% c(2, 3)) |>
    pull(cu_number) |> unique()

  df_clean <- df_raw |>
    filter(!cu_number %in% merged_cus,
           is.na(acquiredcu) | acquiredcu == 0) |>
    mutate(
      networth_ratio = if_else(assets_tot > 0,
                               networth_tot / assets_tot * 100, NA_real_),
      cap_buffer     = networth_ratio - 10,
      lns_mbl_use    = coalesce(lns_mbl, lns_mbl_part723),
      mbl_shr        = if_else(lns_tot > 0,
                               lns_mbl_use / lns_tot * 100, NA_real_),
      re_shr         = if_else(lns_tot > 0,
                               lns_re / lns_tot * 100, NA_real_),
      auto_shr       = if_else(lns_tot > 0,
                               lns_auto / lns_tot * 100, NA_real_),
      ln_assets      = log(assets_tot),
      ln_loans       = log(lns_tot + 1),
      dq_rate_var    = dq_rate,
      chgoff_ratio   = chg_tot_ratio,
      roa_var        = roa,
      nim            = netintmrg,
      loan_to_asset  = if_else(assets_tot > 0,
                               lns_tot / assets_tot * 100, NA_real_),
      cecl_adopter   = as.integer(!is.na(cecl) & cecl == 1)
    ) |>
    arrange(cu_number, q_period_num) |>
    group_by(cu_number) |>
    mutate(
      asset_growth = ln_assets - lag(ln_assets),
      loan_growth  = ln_loans  - lag(ln_loans)
    ) |>
    ungroup()

  # Crisis panel
  crisis_tx <- df_clean |>
    filter(q_period_num >= CRISIS_PRE_START,
           q_period_num <= CRISIS_PRE_END) |>
    group_by(cu_number) |>
    summarise(avg_assets_pre_crisis = mean(assets_tot, na.rm = TRUE),
              .groups = "drop") |>
    mutate(complex_crisis = as.integer(avg_assets_pre_crisis >= SIZE_THRESHOLD))

  df_crisis <- df_clean |>
    filter(q_period_num >= CRISIS_START,
           q_period_num <= CRISIS_END) |>
    inner_join(crisis_tx, by = "cu_number") |>
    filter(!is.na(complex_crisis)) |>
    mutate(
      post_crisis = as.integer(q_period_num >= CRISIS_DATE),
      treat_post  = complex_crisis * post_crisis,
      event_time  = case_when(
        quarter == 1 ~ (year - 2008L) * 4 - 2L,
        quarter == 2 ~ (year - 2008L) * 4 - 1L,
        quarter == 3 ~ (year - 2008L) * 4 + 0L,
        quarter == 4 ~ (year - 2008L) * 4 + 1L
      )
    )

  # RBC panel
  rbc_tx <- df_clean |>
    filter(q_period_num >= RBC_PRE_START,
           q_period_num <= RBC_PRE_END) |>
    group_by(cu_number) |>
    summarise(avg_assets_pre_rbc = mean(assets_tot, na.rm = TRUE),
              .groups = "drop") |>
    mutate(complex_rbc = as.integer(avg_assets_pre_rbc >= SIZE_THRESHOLD))

  df_rbc <- df_clean |>
    filter(q_period_num >= RBC_START,
           q_period_num <= RBC_END) |>
    inner_join(rbc_tx, by = "cu_number") |>
    filter(!is.na(complex_rbc)) |>
    mutate(
      post_rbc   = as.integer(q_period_num >= RBC_DATE),
      treat_post = complex_rbc * post_rbc,
      event_time = case_when(
        quarter == 1 ~ (year - 2022L) * 4 + 0L,
        quarter == 2 ~ (year - 2022L) * 4 + 1L,
        quarter == 3 ~ (year - 2022L) * 4 + 2L,
        quarter == 4 ~ (year - 2022L) * 4 + 3L
      )
    )

  message("  Panels rebuilt.")

} else {
  message("── Using df_crisis and df_rbc from 3A session ────────────────────")
  message("  (Set STANDALONE <- TRUE if these are not in memory)")
}


# =============================================================================
# 3. EVENT STUDY FUNCTION
# =============================================================================

run_es <- function(outcome, data, complex_var, min_et = EVENT_MIN,
                   max_et = EVENT_MAX, ref = EVENT_REF) {
  fml <- as.formula(
    paste0(outcome,
           " ~ i(event_time, ", complex_var, ", ref = ", ref, ") + ",
           controls_base,
           " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml,
          data    = data |> filter(event_time >= min_et,
                                   event_time <= max_et),
          cluster = ~cu_number,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
}

# Extract tidy coefficients with window label
extract_es <- function(model, window_label) {
  if (is.null(model)) return(NULL)
  tryCatch({
    tidy(model, conf.int = TRUE) |>
      filter(str_detect(term, "event_time::")) |>
      mutate(
        event_time = as.integer(str_extract(term, "-?\\d+")),
        window     = window_label
      ) |>
      filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX) |>
      bind_rows(tibble(
        term = "ref", estimate = 0, std.error = 0,
        conf.low = 0, conf.high = 0, p.value = NA_real_,
        event_time = EVENT_REF, window = window_label
      )) |>
      arrange(event_time)
  }, error = function(e) NULL)
}


# =============================================================================
# 4. RUN EVENT STUDIES — ALL KEY OUTCOMES, BOTH WINDOWS
# =============================================================================

message("── Step 1: Running event studies — both windows ──────────────────────")

es_outcomes <- list(
  list(var = "networth_ratio", label = "Net Worth Ratio (%)",      np = TRUE),
  list(var = "cap_buffer",     label = "Capital Buffer vs 10% (pp)", np = TRUE),
  list(var = "loan_growth",    label = "Loan Growth (QoQ log)",    np = FALSE),
  list(var = "asset_growth",   label = "Asset Growth (QoQ log)",   np = FALSE),
  list(var = "dq_rate_var",    label = "Delinquency Rate (%)",     np = FALSE),
  list(var = "chgoff_ratio",   label = "Charge-Off Ratio (%)",     np = FALSE),
  list(var = "roa_var",        label = "ROA (%)",                  np = FALSE),
  list(var = "nim",            label = "Net Interest Margin (%)",  np = FALSE),
  list(var = "re_shr",         label = "RE Share (%)",             np = FALSE),
  list(var = "auto_shr",       label = "Auto Loan Share (%)",      np = FALSE),
  list(var = "mbl_shr",        label = "MBL Share (%)",            np = TRUE)
)

message("  Crisis window event studies...")
es_crisis_list <- map(es_outcomes, function(o) {
  run_es(o$var, df_crisis, "complex_crisis")
})

message("  RBC window event studies...")
es_rbc_list <- map(es_outcomes, function(o) {
  run_es(o$var, df_rbc, "complex_rbc")
})

# Combine into comparison datasets
es_combined <- map2(es_crisis_list, es_rbc_list, function(mc, mr) {
  bind_rows(
    extract_es(mc, "Crisis (2008q3)"),
    extract_es(mr, "RBC Rule (2022q1)")
  )
})

message("  Event studies complete.")


# =============================================================================
# 5. SHAPE ANALYSIS — V-SHAPE vs. STEP-CHANGE TEST
# =============================================================================
# Key hypothesis: Crisis produces a V-shaped response (shock then recovery).
# RBC produces a step-change (persistent new level).
# We test this by:
#   (a) Computing the ratio of post-period coefficients: late/early
#       - V-shape: late period coefs smaller than early (recovery)
#       - Step-change: late period coefs >= early (persistence)
#   (b) Testing whether post-period coefficients are declining (crisis)
#       or stable/increasing (RBC) using a simple slope regression

message("── Step 2: Shape analysis — V-shape vs. step-change ─────────────────")

analyze_shape <- function(coef_data, window_label, outcome_label) {
  if (is.null(coef_data)) return(NULL)

  d <- coef_data |>
    filter(window == window_label, !is.na(p.value))

  # Early post: Q+1 to Q+4 | Late post: Q+5 to Q+10
  early_post <- d |> filter(event_time >= 1, event_time <= 4)
  late_post  <- d |> filter(event_time >= 5, event_time <= EVENT_MAX)
  pre_period <- d |> filter(event_time >= EVENT_MIN, event_time <= -2)

  if (nrow(early_post) == 0 | nrow(late_post) == 0) return(NULL)

  early_mean <- mean(early_post$estimate, na.rm = TRUE)
  late_mean  <- mean(late_post$estimate,  na.rm = TRUE)
  pre_mean   <- mean(pre_period$estimate, na.rm = TRUE)

  # Persistence ratio: late/early (>1 = persistent, <1 = V-shape recovery)
  persistence <- if_else(abs(early_mean) > 0.001,
                         late_mean / early_mean, NA_real_)

  # Post-period slope (positive = growing, negative = recovery)
  post_data <- d |> filter(event_time >= 0)
  slope_fit  <- tryCatch(
    lm(estimate ~ event_time, data = post_data),
    error = function(e) NULL
  )
  post_slope <- if (!is.null(slope_fit)) coef(slope_fit)[2] else NA_real_

  tibble(
    Outcome        = outcome_label,
    Window         = window_label,
    Pre_mean       = round(pre_mean,   3),
    Early_post     = round(early_mean, 3),
    Late_post      = round(late_mean,  3),
    Persistence    = round(persistence, 2),
    Post_slope     = round(post_slope,  4),
    Shape          = case_when(
      is.na(persistence)            ~ "Undetermined",
      early_mean > 0 & late_mean < 0 ~ "Full reversal (V-shape)",
      persistence < 0.3             ~ "Strong recovery (V-shape)",
      persistence >= 0.3 & persistence < 0.7 ~ "Partial recovery",
      persistence >= 0.7            ~ "Persistent (step-change)",
      TRUE                          ~ "Undetermined"
    )
  )
}

shape_results <- map2_dfr(es_combined, es_outcomes, function(coefs, o) {
  bind_rows(
    analyze_shape(coefs, "Crisis (2008q3)",   o$label),
    analyze_shape(coefs, "RBC Rule (2022q1)", o$label)
  )
})

cat("\n=== SHAPE ANALYSIS: V-SHAPE vs. STEP-CHANGE ===\n")
cat("Persistence ratio > 0.7 = step-change (regulatory)\n")
cat("Persistence ratio < 0.3 = V-shape recovery (crisis)\n\n")
print(shape_results |> arrange(Outcome, Window), n = Inf)

write_csv(shape_results,
          file.path(TABLE_PATH, "3B_shape_analysis.csv"))
message("  Shape analysis saved.")


# =============================================================================
# 6. COMBINED EVENT STUDY OVERLAY PLOTS
# =============================================================================

message("── Step 3: Combined event study overlay plots ─────────────────────────")

plot_es_overlay <- function(coef_data, outcome_label,
                            non_parallel = FALSE,
                            shape_data   = NULL) {

  if (is.null(coef_data) | nrow(coef_data) == 0) return(NULL)

  # Subtitle
  sub <- paste0(
    "Event time = quarters relative to shock (Q0). ",
    "Reference = Q", EVENT_REF, ". 95% CI shaded."
  )
  if (non_parallel) {
    sub <- paste0(sub,
                  "\nNote: Non-parallel pre-trends in this outcome.")
  }

  # Add shape annotation if available
  shape_note <- ""
  if (!is.null(shape_data)) {
    crisis_shape <- shape_data |>
      filter(str_detect(Window, "Crisis")) |>
      pull(Shape)
    rbc_shape <- shape_data |>
      filter(str_detect(Window, "RBC")) |>
      pull(Shape)
    if (length(crisis_shape) > 0 & length(rbc_shape) > 0) {
      shape_note <- paste0(
        "\nCrisis: ", crisis_shape,
        " | RBC: ", rbc_shape
      )
    }
  }

  p <- ggplot(coef_data,
              aes(x = event_time, y = estimate,
                  color = window, fill = window)) +
    # Post-period shading
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "gray95", alpha = 0.4) +
    # CI ribbons — separate colors per window
    geom_ribbon(
      data = coef_data |> filter(window == "Crisis (2008q3)"),
      aes(ymin = conf.low, ymax = conf.high),
      fill = COL_CI_CRISIS, alpha = 0.25, color = NA
    ) +
    geom_ribbon(
      data = coef_data |> filter(window == "RBC Rule (2022q1)"),
      aes(ymin = conf.low, ymax = conf.high),
      fill = COL_CI_RBC, alpha = 0.25, color = NA
    ) +
    # Reference lines
    geom_hline(yintercept = 0, color = "gray50",
               linewidth = 0.5, linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.6, linetype = "dashed") +
    # Lines
    geom_line(linewidth = 1.0) +
    geom_point(aes(shape = window), size = 2.2) +
    # Significant post-period markers
    geom_point(
      data = coef_data |>
        filter(!is.na(p.value), p.value < 0.05, event_time >= 0),
      shape = 8, size = 2.8, show.legend = FALSE
    ) +
    scale_color_manual(values = c(
      "Crisis (2008q3)"   = COL_CRISIS,
      "RBC Rule (2022q1)" = COL_RBC
    )) +
    scale_fill_manual(values = c(
      "Crisis (2008q3)"   = COL_CI_CRISIS,
      "RBC Rule (2022q1)" = COL_CI_RBC
    )) +
    scale_shape_manual(values = c(
      "Crisis (2008q3)"   = 17,
      "RBC Rule (2022q1)" = 16
    )) +
    scale_x_continuous(
      breaks = seq(EVENT_MIN, EVENT_MAX, by = 4),
      labels = function(x) paste0("Q", x)
    ) +
    labs(
      title    = outcome_label,
      subtitle = paste0(sub, shape_note),
      x        = "Quarters Relative to Shock",
      y        = "DiD Coefficient",
      color = NULL, fill = NULL, shape = NULL,
      caption  = paste0(
        "★ = p<0.05 (post-period). Two-way FE. SE clustered at CU. ",
        "Red=Crisis 2008q3, Navy=RBC 2022q1."
      )
    ) +
    theme_rbc()

  p
}

# Save individual plots
message("  Saving individual overlay plots...")

for (i in seq_along(es_outcomes)) {
  o     <- es_outcomes[[i]]
  coefs <- es_combined[[i]]
  if (is.null(coefs) | nrow(coefs) == 0) next

  shape_d <- shape_results |> filter(Outcome == o$label)

  p <- plot_es_overlay(coefs, o$label,
                       non_parallel = o$np,
                       shape_data   = shape_d)
  if (is.null(p)) next

  fname <- paste0("3B_es_overlay_",
                  str_replace_all(o$var, "_", "-"), ".png")
  ggsave(file.path(FIGURE_PATH, fname),
         p, width = 10, height = 6, dpi = 300)
  message(sprintf("    Saved: %s", fname))
}


# =============================================================================
# 7. MAIN COMPARISON PANELS
# =============================================================================

message("── Step 4: Main comparison panels ───────────────────────────────────")

# Helper: compact panel version (no subtitle, smaller text)
make_compact <- function(idx, title_txt) {
  coefs   <- es_combined[[idx]]
  o       <- es_outcomes[[idx]]
  shape_d <- shape_results |> filter(Outcome == o$label)

  if (is.null(coefs) | nrow(coefs) == 0) return(plot_spacer())

  p <- plot_es_overlay(coefs, title_txt,
                       non_parallel = o$np,
                       shape_data   = shape_d)
  p + theme(
    legend.position = "none",
    plot.subtitle   = element_text(size = 7.5),
    plot.title      = element_text(size = 11)
  )
}

# Panel 1: Capital dynamics (2x2)
p_nw   <- make_compact(1, "A. Net Worth Ratio (%)")
p_buf  <- make_compact(2, "B. Capital Buffer vs 10% (pp)")
p_loan <- make_compact(3, "C. Loan Growth (QoQ log)")
p_dq   <- make_compact(5, "D. Delinquency Rate (%)")

panel_capital <- (p_nw + p_buf) / (p_loan + p_dq) +
  plot_annotation(
    title   = "Dynamic Responses: 2008 Crisis vs. 2022 RBC Rule — Capital & Lending",
    subtitle = paste0(
      "Red triangles = crisis shock (2008 Q3). ",
      "Navy circles = RBC rule (2022 Q1). ",
      "★ = p<0.05. Shape labels: V-shape = recovery, Step = persistence."
    ),
    caption = paste0(
      "Two-way FE (CU + quarter-year). SE clustered at CU. ",
      "Treatment = avg assets > $500M in pre-period. ",
      "Source: NCUA Call Report (5300)."
    )
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_capital_lending.png"),
       panel_capital, width = 14, height = 10, dpi = 300)

# Panel 2: Portfolio reallocation (2x2)
p_re   <- make_compact(9,  "A. Real Estate Share (%)")
p_auto <- make_compact(10, "B. Auto Loan Share (%)")
p_mbl  <- make_compact(11, "C. MBL Share (%)")
p_roa  <- make_compact(7,  "D. ROA (%)")

panel_portfolio <- (p_re + p_auto) / (p_mbl + p_roa) +
  plot_annotation(
    title   = "Dynamic Responses: 2008 Crisis vs. 2022 RBC Rule — Portfolio & Profitability",
    subtitle = paste0(
      "Red triangles = crisis shock (2008 Q3). ",
      "Navy circles = RBC rule (2022 Q1). ★ = p<0.05."
    ),
    caption = "Two-way FE (CU + quarter-year). SE clustered at CU. Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_portfolio_profitability.png"),
       panel_portfolio, width = 14, height = 10, dpi = 300)

# Panel 3: Credit quality (2x2)
p_chg  <- make_compact(6, "A. Charge-Off Ratio (%)")
p_nim  <- make_compact(8, "B. Net Interest Margin (%)")
p_ast  <- make_compact(4, "C. Asset Growth (QoQ log)")

panel_credit <- (p_dq + p_chg) / (p_nim + p_ast) +
  plot_annotation(
    title   = "Dynamic Responses: 2008 Crisis vs. 2022 RBC Rule — Credit Quality",
    subtitle = "Red triangles = crisis. Navy circles = RBC. ★ = p<0.05.",
    caption  = "Two-way FE (CU + quarter-year). SE clustered at CU. Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_credit_quality.png"),
       panel_credit, width = 14, height = 10, dpi = 300)

message("  All panels saved.")


# =============================================================================
# 8. BUFFER PROXIMITY ANALYSIS
# =============================================================================
# Tests the dose-response hypothesis:
# Complex CUs with THINNER capital buffers pre-shock should respond MORE.
# This applies in both windows:
#   - Crisis: CUs close to insolvency cut lending/raised capital more
#   - RBC: CUs close to 10% threshold faced more binding constraint
# If the dose-response is STRONGER under RBC than the crisis,
# that suggests the regulatory threshold creates additional pressure
# beyond what the capital level alone would predict.

message("── Step 5: Buffer proximity (dose-response) analysis ─────────────────")

# ── Pre-crisis buffer split ───────────────────────────────────────────────
crisis_buffers <- df_crisis |>
  filter(q_period_num >= CRISIS_PRE_START,
         q_period_num <= CRISIS_PRE_END,
         complex_crisis == 1) |>
  group_by(cu_number) |>
  summarise(
    pre_nw_crisis = mean(networth_ratio, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    # Thin: below 11% (within 1pp of 10% threshold)
    # Thick: above 12% (comfortable buffer)
    crisis_buffer_group = case_when(
      pre_nw_crisis < 11  ~ "Thin buffer (<11%)",
      pre_nw_crisis >= 12 ~ "Thick buffer (≥12%)",
      TRUE                ~ "Mid buffer (11-12%)"
    )
  )

df_crisis_buf <- df_crisis |>
  inner_join(crisis_buffers |>
               select(cu_number, pre_nw_crisis, crisis_buffer_group),
             by = "cu_number")

# ── Pre-RBC buffer split ──────────────────────────────────────────────────
rbc_buffers <- df_rbc |>
  filter(q_period_num >= RBC_PRE_START,
         q_period_num <= RBC_PRE_END,
         complex_rbc == 1) |>
  group_by(cu_number) |>
  summarise(
    pre_nw_rbc = mean(networth_ratio, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    rbc_buffer_group = case_when(
      pre_nw_rbc < 11  ~ "Thin buffer (<11%)",
      pre_nw_rbc >= 12 ~ "Thick buffer (≥12%)",
      TRUE             ~ "Mid buffer (11-12%)"
    )
  )

df_rbc_buf <- df_rbc |>
  inner_join(rbc_buffers |>
               select(cu_number, pre_nw_rbc, rbc_buffer_group),
             by = "cu_number")

message(sprintf("  Crisis buffer groups: Thin=%d, Mid=%d, Thick=%d",
                sum(crisis_buffers$crisis_buffer_group == "Thin buffer (<11%)"),
                sum(crisis_buffers$crisis_buffer_group == "Mid buffer (11-12%)"),
                sum(crisis_buffers$crisis_buffer_group == "Thick buffer (≥12%)")))

message(sprintf("  RBC buffer groups: Thin=%d, Mid=%d, Thick=%d",
                sum(rbc_buffers$rbc_buffer_group == "Thin buffer (<11%)"),
                sum(rbc_buffers$rbc_buffer_group == "Mid buffer (11-12%)"),
                sum(rbc_buffers$rbc_buffer_group == "Thick buffer (≥12%)")))

# ── DiD by buffer group ───────────────────────────────────────────────────
buffer_outcomes <- c("loan_growth", "networth_ratio",
                     "dq_rate_var", "auto_shr", "re_shr")
buffer_labels   <- c("Loan Growth", "Net Worth Ratio",
                     "DQ Rate", "Auto Share", "RE Share")

run_buffer_did <- function(outcome, data, treat_var, ctrl = controls_base) {
  fml <- as.formula(
    paste0(outcome, " ~ treat_post + ", ctrl,
           " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml, data = data, cluster = ~cu_number,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
}

extract_buf <- function(model, label, window, group) {
  if (is.null(model)) return(NULL)
  tryCatch({
    tidy_m <- tidy(model, conf.int = TRUE)
    row    <- tidy_m[tidy_m$term == "treat_post", ]
    tibble(
      Outcome = label, Window = window, Buffer_group = group,
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
      N = nobs(model)
    )
  }, error = function(e) NULL)
}

buffer_groups_crisis <- c("Thin buffer (<11%)", "Thick buffer (≥12%)")
buffer_groups_rbc    <- c("Thin buffer (<11%)", "Thick buffer (≥12%)")

message("  Running buffer-split DiDs...")

buffer_results <- bind_rows(
  # Crisis — thin buffer
  map_dfr(seq_along(buffer_outcomes), function(j) {
    d <- df_crisis_buf |>
      filter(crisis_buffer_group == "Thin buffer (<11%)" |
               complex_crisis == 0)
    m <- run_buffer_did(buffer_outcomes[j], d, "complex_crisis")
    extract_buf(m, buffer_labels[j], "Crisis (2008q3)", "Thin (<11%)")
  }),
  # Crisis — thick buffer
  map_dfr(seq_along(buffer_outcomes), function(j) {
    d <- df_crisis_buf |>
      filter(crisis_buffer_group == "Thick buffer (≥12%)" |
               complex_crisis == 0)
    m <- run_buffer_did(buffer_outcomes[j], d, "complex_crisis")
    extract_buf(m, buffer_labels[j], "Crisis (2008q3)", "Thick (≥12%)")
  }),
  # RBC — thin buffer
  map_dfr(seq_along(buffer_outcomes), function(j) {
    d <- df_rbc_buf |>
      filter(rbc_buffer_group == "Thin buffer (<11%)" |
               complex_rbc == 0)
    m <- run_buffer_did(buffer_outcomes[j], d, "complex_rbc")
    extract_buf(m, buffer_labels[j], "RBC Rule (2022q1)", "Thin (<11%)")
  }),
  # RBC — thick buffer
  map_dfr(seq_along(buffer_outcomes), function(j) {
    d <- df_rbc_buf |>
      filter(rbc_buffer_group == "Thick buffer (≥12%)" |
               complex_rbc == 0)
    m <- run_buffer_did(buffer_outcomes[j], d, "complex_rbc")
    extract_buf(m, buffer_labels[j], "RBC Rule (2022q1)", "Thick (≥12%)")
  })
)

cat("\n=== BUFFER PROXIMITY: DOSE-RESPONSE ANALYSIS ===\n")
cat("Thin buffer CUs should respond MORE than thick-buffer CUs.\n")
cat("If this is stronger under RBC than crisis → regulatory threshold matters.\n\n")
buffer_results |>
  mutate(Result = paste0(Beta, Stars, " (", SE, ")")) |>
  select(Outcome, Window, Buffer_group, Result, p_value) |>
  print(n = Inf)

write_csv(buffer_results,
          file.path(TABLE_PATH, "3B_buffer_proximity_results.csv"))


# ── Buffer proximity coefficient plot ────────────────────────────────────
p_buffer <- ggplot(
  buffer_results |> filter(!is.na(Beta)),
  aes(x = Beta, y = Outcome,
      xmin = CI_low, xmax = CI_high,
      color = interaction(Window, Buffer_group),
      shape = Buffer_group)
) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.6) +
  geom_errorbarh(height = 0.3, linewidth = 0.8,
                 position = position_dodge(width = 0.7)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.7)) +
  scale_color_manual(
    values = c(
      "Crisis (2008q3).Thin (<11%)"    = "#C94040",
      "Crisis (2008q3).Thick (≥12%)"   = "#FFAAAA",
      "RBC Rule (2022q1).Thin (<11%)"  = "#1B3A6B",
      "RBC Rule (2022q1).Thick (≥12%)" = "#7AAAD0"
    ),
    labels = c(
      "Crisis (2008q3).Thin (<11%)"    = "Crisis — Thin (<11%)",
      "Crisis (2008q3).Thick (≥12%)"   = "Crisis — Thick (≥12%)",
      "RBC Rule (2022q1).Thin (<11%)"  = "RBC — Thin (<11%)",
      "RBC Rule (2022q1).Thick (≥12%)" = "RBC — Thick (≥12%)"
    )
  ) +
  scale_shape_manual(values = c("Thin (<11%)" = 17, "Thick (≥12%)" = 16)) +
  facet_wrap(~ Window, ncol = 2) +
  labs(
    title    = "Dose-Response by Capital Buffer: Crisis vs. RBC",
    subtitle = paste0(
      "Thin = complex CUs with net worth ratio < 11% pre-shock. ",
      "Thick = ≥ 12%. 95% CI. SE clustered at CU."
    ),
    x       = "DiD Coefficient (Complex × Post-Shock)",
    y       = NULL,
    color   = NULL, shape = NULL,
    caption = paste0(
      "If thin-buffer gap is LARGER under RBC than crisis, ",
      "the regulatory threshold creates additional pressure beyond capital level alone."
    )
  ) +
  theme_rbc() +
  theme(legend.position = "bottom")

ggsave(file.path(FIGURE_PATH, "3B_buffer_proximity_plot.png"),
       p_buffer, width = 12, height = 8, dpi = 300)

message("  Buffer proximity plot saved.")


# =============================================================================
# 9. TIMING ANALYSIS — When Do Effects Peak?
# =============================================================================
# For each outcome and window, find the quarter of peak effect.
# Crisis: peak should be early (Q+1 or Q+2) then recover
# RBC: peak may be later (Q+3 to Q+6) as behavior adjusts gradually

message("── Step 6: Timing analysis — peak effect quarters ────────────────────")

timing_results <- map2_dfr(es_combined, es_outcomes, function(coefs, o) {
  if (is.null(coefs)) return(NULL)

  coefs |>
    filter(!is.na(p.value), event_time >= 0) |>
    group_by(window) |>
    summarise(
      Outcome          = o$label,
      Peak_quarter     = event_time[which.max(abs(estimate))],
      Peak_coef        = round(estimate[which.max(abs(estimate))], 3),
      Peak_significant = p.value[which.max(abs(estimate))] < 0.05,
      N_sig_quarters   = sum(p.value < 0.05, na.rm = TRUE),
      First_sig_q      = if_else(any(p.value < 0.05, na.rm = TRUE),
                                 min(event_time[p.value < 0.05],
                                     na.rm = TRUE),
                                 NA_integer_),
      .groups = "drop"
    )
})

cat("\n=== TIMING ANALYSIS: PEAK EFFECT QUARTERS ===\n")
cat("Crisis peaks should be earlier (Q+1/Q+2) than RBC peaks (Q+3/Q+6)\n\n")
timing_results |>
  select(Outcome, window, Peak_quarter, Peak_coef,
         Peak_significant, N_sig_quarters, First_sig_q) |>
  arrange(Outcome, window) |>
  print(n = Inf)

write_csv(timing_results,
          file.path(TABLE_PATH, "3B_timing_analysis.csv"))

message("  Timing analysis saved.")


# =============================================================================
# 10. FINAL SUMMARY TABLE
# =============================================================================

message("── Step 7: Final summary ─────────────────────────────────────────────")

summary_3b <- shape_results |>
  select(Outcome, Window, Shape, Persistence, Post_slope) |>
  left_join(
    timing_results |>
      select(Outcome, Window = window, Peak_quarter,
             N_sig_quarters, First_sig_q),
    by = c("Outcome", "Window")
  ) |>
  mutate(
    Mechanism = case_when(
      Shape == "Full reversal (V-shape)" & str_detect(Window, "RBC") ~
        "Crisis-like transitory shock",
      Shape == "Persistent (step-change)" & str_detect(Window, "RBC") ~
        "Regulatory permanent effect",
      Shape == "Partial recovery" & str_detect(Window, "RBC") ~
        "Regulatory medium-term effect",
      TRUE ~ Shape
    )
  )

cat("\n=== SUMMARY: SHAPE + TIMING ANALYSIS ===\n")
print(summary_3b, n = Inf)

write_csv(summary_3b,
          file.path(TABLE_PATH, "3B_summary_shape_timing.csv"))


# =============================================================================
# 11. CONSOLE OUTPUT SUMMARY
# =============================================================================

cat("\n=== 3B ANALYSIS COMPLETE ===\n\n")

cat("Tables saved (output/tables/):\n")
for (t in c("3B_shape_analysis.csv",
            "3B_buffer_proximity_results.csv",
            "3B_timing_analysis.csv",
            "3B_summary_shape_timing.csv")) {
  cat("  →", t, "\n")
}

cat("\nFigures saved (output/figures/):\n")
for (f in c(
  "3B_panel_capital_lending.png",
  "3B_panel_portfolio_profitability.png",
  "3B_panel_credit_quality.png",
  "3B_buffer_proximity_plot.png",
  "[+ individual 3B_es_overlay_*.png for each outcome]"
)) cat("  →", f, "\n")

cat("\nKey questions answered by 3B:\n")
cat("  1. 3B_shape_analysis.csv:\n")
cat("     Persistence > 0.7 = step-change (regulatory permanent effect)\n")
cat("     Persistence < 0.3 = V-shape (transitory crisis response)\n")
cat("  2. 3B_panel_capital_lending.png:\n")
cat("     Visual comparison of dynamic paths — crisis vs. RBC\n")
cat("  3. 3B_buffer_proximity_plot.png:\n")
cat("     Thin vs. thick buffer dose-response — is it stronger under RBC?\n")
cat("  4. 3B_timing_analysis.csv:\n")
cat("     Crisis peaks earlier, RBC builds more gradually?\n")

cat("\nPaper narrative:\n")
cat("  'While 3A showed average effects differ significantly for ROA,\n")
cat("   NIM, and auto share, 3B reveals that the DYNAMIC PATHS also differ.\n")
cat("   Crisis-induced capital increases show V-shaped recovery,\n")
cat("   while RBC-induced changes are persistent (step-change),\n")
cat("   consistent with a permanent regulatory equilibrium shift.'\n")

message("\n── 3B_2008_Crisis_EventStudy.R complete ✓ ───────────────────────────")
message("  Next: Run 3C_2008_Crisis_CapitalBuffer.R or proceed to 4_Paper.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
