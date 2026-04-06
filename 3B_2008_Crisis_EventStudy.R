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
#
#   Key questions:
#   (1) Do SHAPES of impulse responses differ?
#       Crisis: sharp V-shape (shock + recovery)?
#       RBC: step-change (permanent new equilibrium)?
#   (2) Are pre-trends flat in both windows?
#   (3) Which effects are front-loaded vs. back-loaded?
#   (4) Buffer proximity dose-response: thin-buffer CUs respond more?
#       Is this gap LARGER under RBC → threshold amplification?
#
# FIXES vs. prior version:
#   - window column forced to character throughout (fixes factor==string error)
#   - No lemon/ggforce dependencies (all base ggplot2)
#   - position_dodge replaces position_dodgev
#
# Input  : call_report.rds  (full raw data back to 2000)
#          Set STANDALONE <- TRUE if running without 3A in memory.
# Output : output/tables/3B_*  |  output/figures/3B_*
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

# TRUE  = rebuild panels from raw data (fresh R session)
# FALSE = reuse df_crisis and df_rbc already in memory from 3A
STANDALONE <- FALSE

# Must match 3A exactly
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

# Colors
COL_CRISIS    <- "#C94040"
COL_RBC       <- "#1B3A6B"
COL_CI_CRISIS <- "#E8A0A0"
COL_CI_RBC    <- "#8AAAD0"

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

  message("-- Standalone mode: rebuilding panels from raw data ----------------")

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
                               lns_tot / assets_tot * 100, NA_real_)
    ) |>
    arrange(cu_number, q_period_num) |>
    group_by(cu_number) |>
    mutate(
      asset_growth = ln_assets - lag(ln_assets),
      loan_growth  = ln_loans  - lag(ln_loans)
    ) |>
    ungroup()

  # ---- Crisis panel ----
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

  thin_c    <- df_crisis |> count(cu_number) |> filter(n < 8) |> pull(cu_number)
  df_crisis <- df_crisis |> filter(!cu_number %in% thin_c)

  # ---- RBC panel ----
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

  thin_r  <- df_rbc |> count(cu_number) |> filter(n < 8) |> pull(cu_number)
  df_rbc  <- df_rbc  |> filter(!cu_number %in% thin_r)

  message("  Panels rebuilt successfully.")

} else {
  message("-- Using df_crisis and df_rbc from 3A session ---------------------")
  message("   Set STANDALONE <- TRUE if these are not in memory.")
}


# =============================================================================
# 3. HELPER FUNCTIONS
# =============================================================================

# Run event study feols model
run_es <- function(outcome, data, complex_var) {
  fml <- as.formula(
    paste0(outcome,
           " ~ i(event_time, ", complex_var, ", ref = ", EVENT_REF, ") + ",
           controls_base,
           " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml,
          data    = data |> filter(event_time >= EVENT_MIN,
                                   event_time <= EVENT_MAX),
          cluster = ~cu_number,
          warn    = FALSE, notes = FALSE),
    error = function(e) NULL
  )
}

# Extract tidy event study coefficients
# window column is ALWAYS forced to character to avoid factor==string errors
extract_es <- function(model, window_label) {
  if (is.null(model)) return(NULL)
  wl <- as.character(window_label)
  tryCatch({
    tidy(model, conf.int = TRUE) |>
      filter(str_detect(term, "event_time::")) |>
      mutate(
        event_time = as.integer(str_extract(term, "-?\\d+")),
        window     = wl
      ) |>
      filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX) |>
      bind_rows(tibble(
        term       = "ref",
        estimate   = 0,
        std.error  = 0,
        conf.low   = 0,
        conf.high  = 0,
        p.value    = NA_real_,
        event_time = as.integer(EVENT_REF),
        window     = wl
      )) |>
      arrange(event_time)
  }, error = function(e) NULL)
}

# Shape analysis: V-shape vs step-change
# Persistence ratio = late-post mean / early-post mean
# >0.7 = persistent step-change | <0.3 = V-shape recovery
analyze_shape <- function(coef_data, window_label, outcome_label) {
  if (is.null(coef_data) | nrow(coef_data) == 0) return(NULL)

  # Force character — prevents any factor vs string comparison error
  wl <- as.character(window_label)
  d  <- coef_data |>
    mutate(window = as.character(window)) |>
    filter(window == wl, !is.na(p.value))

  if (nrow(d) == 0) return(NULL)

  early_post <- d |> filter(event_time >= 1,         event_time <= 4)
  late_post  <- d |> filter(event_time >= 5,         event_time <= EVENT_MAX)
  pre_period <- d |> filter(event_time >= EVENT_MIN,  event_time <= -2)

  if (nrow(early_post) == 0 | nrow(late_post) == 0) return(NULL)

  early_mean  <- mean(early_post$estimate, na.rm = TRUE)
  late_mean   <- mean(late_post$estimate,  na.rm = TRUE)
  pre_mean    <- mean(pre_period$estimate, na.rm = TRUE)
  persistence <- if_else(abs(early_mean) > 0.001,
                         late_mean / early_mean, NA_real_)

  post_d     <- d |> filter(event_time >= 0)
  slope_fit  <- tryCatch(lm(estimate ~ event_time, data = post_d),
                         error = function(e) NULL)
  post_slope <- if (!is.null(slope_fit)) coef(slope_fit)[2] else NA_real_

  tibble(
    Outcome     = outcome_label,
    Window      = wl,
    Pre_mean    = round(pre_mean,    3),
    Early_post  = round(early_mean,  3),
    Late_post   = round(late_mean,   3),
    Persistence = round(persistence, 2),
    Post_slope  = round(post_slope,  4),
    Shape       = case_when(
      is.na(persistence)                        ~ "Undetermined",
      early_mean > 0 & late_mean < 0            ~ "Full reversal (V-shape)",
      early_mean < 0 & late_mean > 0            ~ "Full reversal (V-shape)",
      !is.na(persistence) & persistence < 0.3  ~ "Strong recovery (V-shape)",
      !is.na(persistence) & persistence < 0.7  ~ "Partial recovery",
      !is.na(persistence) & persistence >= 0.7 ~ "Persistent (step-change)",
      TRUE                                      ~ "Undetermined"
    )
  )
}

# Extract buffer DiD coefficient
extract_buf_coef <- function(model, label, window, grp) {
  if (is.null(model)) return(NULL)
  tryCatch({
    tidy_m <- tidy(model, conf.int = TRUE)
    row    <- tidy_m[tidy_m$term == "treat_post", ]
    if (nrow(row) == 0) return(NULL)
    tibble(
      Outcome = label, Window = window, Buffer = grp,
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


# =============================================================================
# 4. RUN EVENT STUDIES — ALL KEY OUTCOMES, BOTH WINDOWS
# =============================================================================

message("-- Step 1: Running event studies both windows ----------------------")

es_outcomes <- list(
  list(var = "networth_ratio", label = "Net Worth Ratio (%)",        np = TRUE),
  list(var = "cap_buffer",     label = "Capital Buffer vs 10% (pp)", np = TRUE),
  list(var = "loan_growth",    label = "Loan Growth (QoQ log)",      np = FALSE),
  list(var = "asset_growth",   label = "Asset Growth (QoQ log)",     np = FALSE),
  list(var = "dq_rate_var",    label = "Delinquency Rate (%)",       np = FALSE),
  list(var = "chgoff_ratio",   label = "Charge-Off Ratio (%)",       np = FALSE),
  list(var = "roa_var",        label = "ROA (%)",                    np = FALSE),
  list(var = "nim",            label = "Net Interest Margin (%)",    np = FALSE),
  list(var = "re_shr",         label = "RE Share (%)",               np = FALSE),
  list(var = "auto_shr",       label = "Auto Loan Share (%)",        np = FALSE),
  list(var = "mbl_shr",        label = "MBL Share (%)",              np = TRUE)
)

message("  Crisis window...")
es_crisis_list <- map(es_outcomes, function(o) {
  run_es(o$var, df_crisis, "complex_crisis")
})

message("  RBC window...")
es_rbc_list <- map(es_outcomes, function(o) {
  run_es(o$var, df_rbc, "complex_rbc")
})

# Combine — window is character from extract_es
es_combined <- map2(es_crisis_list, es_rbc_list, function(mc, mr) {
  bind_rows(
    extract_es(mc, "Crisis (2008q3)"),
    extract_es(mr, "RBC Rule (2022q1)")
  )
})

message("  Event studies complete.")


# =============================================================================
# 5. SHAPE ANALYSIS
# =============================================================================

message("-- Step 2: Shape analysis V-shape vs step-change -------------------")

shape_results <- map2_dfr(es_combined, es_outcomes, function(coefs, o) {
  bind_rows(
    analyze_shape(coefs, "Crisis (2008q3)",   o$label),
    analyze_shape(coefs, "RBC Rule (2022q1)", o$label)
  )
})

cat("\n=== SHAPE ANALYSIS: V-SHAPE vs. STEP-CHANGE ===\n")
cat("Persistence > 0.7 = step-change | < 0.3 = V-shape recovery\n\n")
shape_results |> arrange(Outcome, Window) |> print(n = Inf)

write_csv(shape_results,
          file.path(TABLE_PATH, "3B_shape_analysis.csv"))
message("  Saved: 3B_shape_analysis.csv")


# =============================================================================
# 6. EVENT STUDY OVERLAY PLOTS
# =============================================================================

message("-- Step 3: Event study overlay plots --------------------------------")

plot_es_overlay <- function(coef_data, outcome_label,
                            non_parallel = FALSE,
                            shape_data   = NULL) {

  if (is.null(coef_data) | nrow(coef_data) == 0) return(NULL)

  # Force character for safe layer filtering
  coef_data <- coef_data |> mutate(window = as.character(window))

  sub <- paste0(
    "Event time = quarters relative to shock (Q0). ",
    "Reference = Q", EVENT_REF, ". 95% CI shaded."
  )
  if (non_parallel) {
    sub <- paste0(sub,
                  "\nNote: Non-parallel pre-trends -- interpret with caution.")
  }
  if (!is.null(shape_data) & nrow(shape_data) > 0) {
    cr  <- shape_data |> filter(str_detect(Window, "Crisis")) |> pull(Shape)
    rb  <- shape_data |> filter(str_detect(Window, "RBC"))   |> pull(Shape)
    if (length(cr) > 0 & length(rb) > 0) {
      sub <- paste0(sub, "\nCrisis: ", cr[1], "  |  RBC: ", rb[1])
    }
  }

  ggplot(coef_data,
         aes(x = event_time, y = estimate,
             color = window, fill = window)) +
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "gray95", alpha = 0.4) +
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
    geom_hline(yintercept = 0, color = "gray50",
               linewidth = 0.5, linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.6, linetype = "dashed") +
    geom_line(linewidth = 1.0) +
    geom_point(aes(shape = window), size = 2.2) +
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
      subtitle = sub,
      x        = "Quarters Relative to Shock",
      y        = "DiD Coefficient",
      color = NULL, fill = NULL, shape = NULL,
      caption  = paste0(
        "Red triangle = Crisis 2008q3.  Navy circle = RBC 2022q1.  ",
        "* = p<0.05 (post-period).  Two-way FE.  SE clustered at CU."
      )
    ) +
    theme_rbc()
}

# Save individual plots
for (i in seq_along(es_outcomes)) {
  o     <- es_outcomes[[i]]
  coefs <- es_combined[[i]]
  if (is.null(coefs) | nrow(coefs) == 0) next
  shape_d <- shape_results |> filter(Outcome == o$label)
  p <- plot_es_overlay(coefs, o$label,
                       non_parallel = o$np, shape_data = shape_d)
  if (is.null(p)) next
  fname <- paste0("3B_es_overlay_",
                  str_replace_all(o$var, "_", "-"), ".png")
  ggsave(file.path(FIGURE_PATH, fname), p, width = 10, height = 6, dpi = 300)
  message(sprintf("    Saved: %s", fname))
}


# =============================================================================
# 7. MAIN COMPARISON PANELS
# =============================================================================

message("-- Step 4: Main comparison panels ----------------------------------")

make_compact <- function(idx, title_txt) {
  coefs   <- es_combined[[idx]]
  o       <- es_outcomes[[idx]]
  shape_d <- shape_results |> filter(Outcome == o$label)
  if (is.null(coefs) | nrow(coefs) == 0) return(plot_spacer())
  p <- plot_es_overlay(coefs, title_txt,
                       non_parallel = o$np, shape_data = shape_d)
  p + theme(legend.position = "none",
            plot.subtitle   = element_text(size = 7.5),
            plot.title      = element_text(size = 11),
            plot.caption    = element_blank())
}

# Panel 1: Capital & Lending (idx: 1=NW, 2=cap buf, 3=loan growth, 5=DQ)
panel1 <- (make_compact(1, "A. Net Worth Ratio (%)") +
           make_compact(2, "B. Capital Buffer vs 10% (pp)")) /
          (make_compact(3, "C. Loan Growth (QoQ log)") +
           make_compact(5, "D. Delinquency Rate (%)")) +
  plot_annotation(
    title   = "Dynamic Responses: 2008 Crisis vs. 2022 RBC -- Capital & Lending",
    subtitle = paste0(
      "Red triangles = crisis shock (2008 Q3).  ",
      "Navy circles = RBC rule (2022 Q1).  * = p<0.05."
    ),
    caption = paste0(
      "Two-way FE (CU + quarter-year).  SE clustered at CU.  ",
      "Treatment = avg assets > $500M.  Source: NCUA Call Report (5300)."
    )
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_capital_lending.png"),
       panel1, width = 14, height = 10, dpi = 300)
message("  Saved: 3B_panel_capital_lending.png")

# Panel 2: Portfolio & Profitability (idx: 9=RE, 10=auto, 11=MBL, 7=ROA)
panel2 <- (make_compact(9,  "A. Real Estate Share (%)") +
           make_compact(10, "B. Auto Loan Share (%)")) /
          (make_compact(11, "C. MBL Share (%)") +
           make_compact(7,  "D. ROA (%)")) +
  plot_annotation(
    title   = "Dynamic Responses: 2008 Crisis vs. 2022 RBC -- Portfolio & Profitability",
    subtitle = "Red triangles = crisis (2008 Q3).  Navy circles = RBC (2022 Q1).  * = p<0.05.",
    caption  = "Two-way FE (CU + quarter-year).  SE clustered at CU.  Source: NCUA 5300."
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_portfolio_profitability.png"),
       panel2, width = 14, height = 10, dpi = 300)
message("  Saved: 3B_panel_portfolio_profitability.png")

# Panel 3: Credit Quality (idx: 5=DQ, 6=charge-off, 8=NIM, 4=asset growth)
panel3 <- (make_compact(5, "A. Delinquency Rate (%)") +
           make_compact(6, "B. Charge-Off Ratio (%)")) /
          (make_compact(8, "C. Net Interest Margin (%)") +
           make_compact(4, "D. Asset Growth (QoQ log)")) +
  plot_annotation(
    title   = "Dynamic Responses: 2008 Crisis vs. 2022 RBC -- Credit Quality",
    subtitle = "Red triangles = crisis (2008 Q3).  Navy circles = RBC (2022 Q1).  * = p<0.05.",
    caption  = "Two-way FE (CU + quarter-year).  SE clustered at CU.  Source: NCUA 5300."
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_credit_quality.png"),
       panel3, width = 14, height = 10, dpi = 300)
message("  Saved: 3B_panel_credit_quality.png")


# =============================================================================
# 8. BUFFER PROXIMITY ANALYSIS
# =============================================================================

message("-- Step 5: Buffer proximity dose-response ---------------------------")

# Pre-crisis buffer classification
crisis_buffers <- df_crisis |>
  filter(q_period_num >= CRISIS_PRE_START,
         q_period_num <= CRISIS_PRE_END,
         complex_crisis == 1) |>
  group_by(cu_number) |>
  summarise(pre_nw_crisis = mean(networth_ratio, na.rm = TRUE),
            .groups = "drop") |>
  mutate(buf_grp = case_when(
    pre_nw_crisis < 11  ~ "Thin (<11%)",
    pre_nw_crisis >= 12 ~ "Thick (>=12%)",
    TRUE                ~ "Mid (11-12%)"
  ))

# Pre-RBC buffer classification
rbc_buffers <- df_rbc |>
  filter(q_period_num >= RBC_PRE_START,
         q_period_num <= RBC_PRE_END,
         complex_rbc == 1) |>
  group_by(cu_number) |>
  summarise(pre_nw_rbc = mean(networth_ratio, na.rm = TRUE),
            .groups = "drop") |>
  mutate(buf_grp = case_when(
    pre_nw_rbc < 11  ~ "Thin (<11%)",
    pre_nw_rbc >= 12 ~ "Thick (>=12%)",
    TRUE             ~ "Mid (11-12%)"
  ))

message(sprintf(
  "  Crisis -- Thin: %d  Mid: %d  Thick: %d",
  sum(crisis_buffers$buf_grp == "Thin (<11%)",   na.rm = TRUE),
  sum(crisis_buffers$buf_grp == "Mid (11-12%)",  na.rm = TRUE),
  sum(crisis_buffers$buf_grp == "Thick (>=12%)", na.rm = TRUE)
))
message(sprintf(
  "  RBC    -- Thin: %d  Mid: %d  Thick: %d",
  sum(rbc_buffers$buf_grp == "Thin (<11%)",   na.rm = TRUE),
  sum(rbc_buffers$buf_grp == "Mid (11-12%)",  na.rm = TRUE),
  sum(rbc_buffers$buf_grp == "Thick (>=12%)", na.rm = TRUE)
))

df_crisis_buf <- df_crisis |>
  left_join(crisis_buffers |> select(cu_number, pre_nw_crisis, buf_grp),
            by = "cu_number")

df_rbc_buf <- df_rbc |>
  left_join(rbc_buffers |> select(cu_number, pre_nw_rbc, buf_grp),
            by = "cu_number")

buf_outcomes <- list(
  list("loan_growth",    "Loan Growth"),
  list("networth_ratio", "Net Worth Ratio"),
  list("dq_rate_var",    "DQ Rate"),
  list("auto_shr",       "Auto Share"),
  list("re_shr",         "RE Share"),
  list("roa_var",        "ROA")
)

run_buf_did <- function(outcome, data) {
  fml <- as.formula(
    paste0(outcome, " ~ treat_post + ", controls_base,
           " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml, data = data, cluster = ~cu_number,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
}

message("  Running buffer-split DiDs...")

buffer_results <- bind_rows(
  map_dfr(buf_outcomes, function(o) {
    d <- df_crisis_buf |>
      filter(buf_grp == "Thin (<11%)" | complex_crisis == 0)
    extract_buf_coef(run_buf_did(o[[1]], d),
                     o[[2]], "Crisis (2008q3)", "Thin (<11%)")
  }),
  map_dfr(buf_outcomes, function(o) {
    d <- df_crisis_buf |>
      filter(buf_grp == "Thick (>=12%)" | complex_crisis == 0)
    extract_buf_coef(run_buf_did(o[[1]], d),
                     o[[2]], "Crisis (2008q3)", "Thick (>=12%)")
  }),
  map_dfr(buf_outcomes, function(o) {
    d <- df_rbc_buf |>
      filter(buf_grp == "Thin (<11%)" | complex_rbc == 0)
    extract_buf_coef(run_buf_did(o[[1]], d),
                     o[[2]], "RBC Rule (2022q1)", "Thin (<11%)")
  }),
  map_dfr(buf_outcomes, function(o) {
    d <- df_rbc_buf |>
      filter(buf_grp == "Thick (>=12%)" | complex_rbc == 0)
    extract_buf_coef(run_buf_did(o[[1]], d),
                     o[[2]], "RBC Rule (2022q1)", "Thick (>=12%)")
  })
)

cat("\n=== BUFFER PROXIMITY: DOSE-RESPONSE ===\n")
cat("Thin = NW ratio < 11%.  Thick = >= 12%.\n")
cat("Larger thin/thick gap under RBC = threshold amplification.\n\n")
buffer_results |>
  mutate(Result = paste0(Beta, Stars, " (", SE, ")")) |>
  select(Outcome, Window, Buffer, Result, p_value) |>
  print(n = Inf)

write_csv(buffer_results,
          file.path(TABLE_PATH, "3B_buffer_proximity_results.csv"))

# Buffer proximity plot
p_buffer <- ggplot(
  buffer_results |> filter(!is.na(Beta)) |>
    mutate(Outcome = factor(Outcome, levels = rev(unique(Outcome)))),
  aes(x = Beta, y = Outcome,
      xmin = CI_low, xmax = CI_high,
      color = interaction(Window, Buffer, sep = "\n"),
      shape = Buffer)
) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.6) +
  geom_errorbarh(height = 0.3, linewidth = 0.8,
                 position = position_dodge(width = 0.8)) +
  geom_point(size = 3, position = position_dodge(width = 0.8)) +
  scale_color_manual(values = c(
    "Crisis (2008q3)\nThin (<11%)"     = "#C94040",
    "Crisis (2008q3)\nThick (>=12%)"   = "#FFAAAA",
    "RBC Rule (2022q1)\nThin (<11%)"   = "#1B3A6B",
    "RBC Rule (2022q1)\nThick (>=12%)" = "#7AAAD0"
  )) +
  scale_shape_manual(values = c("Thin (<11%)" = 17, "Thick (>=12%)" = 16)) +
  facet_wrap(~ Window, ncol = 2) +
  labs(
    title    = "Dose-Response by Capital Buffer: Crisis vs. RBC",
    subtitle = paste0(
      "Thin = pre-shock NW ratio < 11%.  Thick = >= 12%.  95% CI.  SE clustered at CU.\n",
      "Larger thin/thick gap under RBC = regulatory threshold amplifies beyond capital level."
    ),
    x       = "DiD Coefficient (Complex x Post-Shock)",
    y       = NULL,
    color   = NULL, shape = NULL,
    caption = "Two-way FE (CU + quarter-year).  Source: NCUA Call Report (5300)."
  ) +
  theme_rbc() +
  theme(legend.position = "right")

ggsave(file.path(FIGURE_PATH, "3B_buffer_proximity_plot.png"),
       p_buffer, width = 13, height = 8, dpi = 300)
message("  Saved: 3B_buffer_proximity_plot.png")


# =============================================================================
# 9. TIMING ANALYSIS
# =============================================================================

message("-- Step 6: Timing analysis peak effect quarters --------------------")

timing_results <- map2_dfr(es_combined, es_outcomes, function(coefs, o) {
  if (is.null(coefs) | nrow(coefs) == 0) return(NULL)

  coefs |>
    mutate(window = as.character(window)) |>
    filter(!is.na(p.value), event_time >= 0) |>
    group_by(window) |>
    summarise(
      Outcome          = o$label,
      Peak_quarter     = event_time[which.max(abs(estimate))],
      Peak_coef        = round(estimate[which.max(abs(estimate))], 3),
      Peak_significant = p.value[which.max(abs(estimate))] < 0.05,
      N_sig_quarters   = sum(p.value < 0.05, na.rm = TRUE),
      First_sig_q      = if_else(
        any(p.value < 0.05, na.rm = TRUE),
        min(event_time[p.value < 0.05], na.rm = TRUE),
        NA_integer_
      ),
      .groups = "drop"
    ) |>
    rename(Window = window)
})

cat("\n=== TIMING: PEAK EFFECT QUARTERS ===\n")
cat("Expected: Crisis peaks at Q+1/Q+2, RBC peaks at Q+3/Q+6\n\n")
timing_results |>
  select(Outcome, Window, Peak_quarter, Peak_coef,
         Peak_significant, N_sig_quarters, First_sig_q) |>
  arrange(Outcome, Window) |>
  print(n = Inf)

write_csv(timing_results,
          file.path(TABLE_PATH, "3B_timing_analysis.csv"))
message("  Saved: 3B_timing_analysis.csv")


# =============================================================================
# 10. SUMMARY TABLE
# =============================================================================

message("-- Step 7: Summary table -------------------------------------------")

summary_3b <- shape_results |>
  select(Outcome, Window, Shape, Persistence, Post_slope,
         Early_post, Late_post) |>
  left_join(
    timing_results |>
      select(Outcome, Window, Peak_quarter, N_sig_quarters, First_sig_q),
    by = c("Outcome", "Window")
  ) |>
  mutate(
    Interpretation = case_when(
      Shape == "Persistent (step-change)" & str_detect(Window, "RBC") ~
        "Regulatory permanent effect",
      Shape == "Full reversal (V-shape)" & str_detect(Window, "Crisis") ~
        "Classic transitory crisis shock",
      Shape == "Strong recovery (V-shape)" & str_detect(Window, "RBC") ~
        "RBC = crisis-like transitory (no regulatory permanence)",
      Shape == "Partial recovery" & str_detect(Window, "RBC") ~
        "Regulatory medium-term adjustment",
      TRUE ~ Shape
    )
  ) |>
  arrange(Outcome, Window)

cat("\n=== FINAL SUMMARY: SHAPE + TIMING ===\n")
print(summary_3b, n = Inf)

write_csv(summary_3b,
          file.path(TABLE_PATH, "3B_summary_shape_timing.csv"))
message("  Saved: 3B_summary_shape_timing.csv")


# =============================================================================
# 11. FINAL CONSOLE SUMMARY
# =============================================================================

cat("\n=== 3B ANALYSIS COMPLETE ===\n\n")

cat("Tables (output/tables/):\n")
for (t in c("3B_shape_analysis.csv",
            "3B_buffer_proximity_results.csv",
            "3B_timing_analysis.csv",
            "3B_summary_shape_timing.csv")) {
  cat("  ->", t, "\n")
}

cat("\nFigures (output/figures/):\n")
for (f in c(
  "3B_panel_capital_lending.png",
  "3B_panel_portfolio_profitability.png",
  "3B_panel_credit_quality.png",
  "3B_buffer_proximity_plot.png",
  "[+ 3B_es_overlay_*.png for each outcome]"
)) cat("  ->", f, "\n")

cat("\nKey questions answered:\n")
cat("  1. 3B_shape_analysis.csv\n")
cat("     Persistence > 0.7 -> step-change (regulatory permanent)\n")
cat("     Persistence < 0.3 -> V-shape (transitory crisis shock)\n\n")
cat("  2. 3B_panel_capital_lending.png\n")
cat("     Do red (crisis) and navy (RBC) lines have different shapes?\n\n")
cat("  3. 3B_buffer_proximity_plot.png\n")
cat("     Is thin/thick gap larger under RBC than crisis?\n")
cat("     If yes -> threshold creates pressure beyond capital level\n\n")
cat("  4. 3B_timing_analysis.csv\n")
cat("     Crisis peaks earlier (Q+1/Q+2) vs RBC (Q+3/Q+6)?\n")

message("\n-- 3B_2008_Crisis_EventStudy.R complete ----------------------------")

# =============================================================================
# END OF SCRIPT
# =============================================================================
