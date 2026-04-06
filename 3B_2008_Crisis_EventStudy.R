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
#   vs. the 2022 RBC rule. While 3A established average differences,
#   3B asks whether the SHAPES of responses differ:
#     - Crisis: V-shaped shock + recovery?
#     - RBC:    Step-change to new permanent equilibrium?
#
# REQUIRES: df_crisis and df_rbc in memory from 3A.
#   If running standalone, set STANDALONE <- TRUE.
#
# Input  : call_report.rds (if STANDALONE = TRUE)
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

STANDALONE <- FALSE   # set TRUE if df_crisis / df_rbc not in memory

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

  message("── Standalone: rebuilding panels from raw data ───────────────────")

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
  message("── Using df_crisis / df_rbc from 3A session ─────────────────────")
}


# =============================================================================
# 3. EVENT STUDY FUNCTIONS
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

# Extract tidy coefficients — returns a plain data.frame with character window
extract_es <- function(model, window_label) {
  if (is.null(model)) return(NULL)
  tryCatch({
    coefs <- tidy(model, conf.int = TRUE) |>
      filter(str_detect(term, "event_time::")) |>
      mutate(
        event_time = as.integer(str_extract(term, "-?\\d+")),
        window     = as.character(window_label)   # always character
      ) |>
      filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)

    # Add reference row (normalized to 0)
    ref_row <- data.frame(
      term       = "ref",
      estimate   = 0,
      std.error  = 0,
      statistic  = NA_real_,
      p.value    = NA_real_,
      conf.low   = 0,
      conf.high  = 0,
      event_time = as.integer(EVENT_REF),
      window     = as.character(window_label),
      stringsAsFactors = FALSE
    )

    bind_rows(coefs, ref_row) |>
      arrange(event_time) |>
      mutate(window = as.character(window))   # ensure character after bind

  }, error = function(e) NULL)
}


# =============================================================================
# 4. OUTCOMES TO ANALYSE
# =============================================================================

es_outcomes <- list(
  list(var = "networth_ratio", label = "Net Worth Ratio (%)",       np = TRUE),
  list(var = "cap_buffer",     label = "Capital Buffer (pp)",       np = TRUE),
  list(var = "loan_growth",    label = "Loan Growth (QoQ log)",     np = FALSE),
  list(var = "asset_growth",   label = "Asset Growth (QoQ log)",    np = FALSE),
  list(var = "dq_rate_var",    label = "Delinquency Rate (%)",      np = FALSE),
  list(var = "chgoff_ratio",   label = "Charge-Off Ratio (%)",      np = FALSE),
  list(var = "roa_var",        label = "ROA (%)",                   np = FALSE),
  list(var = "nim",            label = "Net Interest Margin (%)",   np = FALSE),
  list(var = "re_shr",         label = "RE Share (%)",              np = FALSE),
  list(var = "auto_shr",       label = "Auto Loan Share (%)",       np = FALSE),
  list(var = "mbl_shr",        label = "MBL Share (%)",             np = TRUE)
)

n_outcomes <- length(es_outcomes)


# =============================================================================
# 5. RUN EVENT STUDIES
# =============================================================================

message("── Step 1: Running event studies ────────────────────────────────────")

message("  Crisis window...")
es_crisis_list <- map(es_outcomes, function(o) {
  run_es(o$var, df_crisis, "complex_crisis")
})

message("  RBC window...")
es_rbc_list <- map(es_outcomes, function(o) {
  run_es(o$var, df_rbc, "complex_rbc")
})

# Combine into list of data frames — one per outcome
# Each df has rows for both windows, window column is character
es_combined <- map2(es_crisis_list, es_rbc_list, function(mc, mr) {
  parts <- list(
    extract_es(mc, "Crisis (2008q3)"),
    extract_es(mr, "RBC Rule (2022q1)")
  )
  parts <- parts[!sapply(parts, is.null)]
  if (length(parts) == 0) return(NULL)
  bind_rows(parts) |>
    mutate(window = as.character(window))
})

message("  Done.")


# =============================================================================
# 6. SHAPE ANALYSIS — V-SHAPE vs. STEP-CHANGE
# =============================================================================

message("── Step 2: Shape analysis ────────────────────────────────────────────")

# Safe helper: compute shape stats for ONE window within ONE outcome's coef df
analyze_one_shape <- function(coef_df, win_label, out_label) {

  # coef_df is a plain data.frame with character window column
  d <- coef_df[coef_df$window == win_label & !is.na(coef_df$p.value), ]

  if (nrow(d) == 0) return(NULL)

  early <- d[d$event_time >= 1  & d$event_time <= 4,  ]
  late  <- d[d$event_time >= 5  & d$event_time <= EVENT_MAX, ]
  pre   <- d[d$event_time >= EVENT_MIN & d$event_time <= -2, ]

  if (nrow(early) == 0 | nrow(late) == 0) return(NULL)

  em <- mean(early$estimate, na.rm = TRUE)
  lm <- mean(late$estimate,  na.rm = TRUE)
  pm <- mean(pre$estimate,   na.rm = TRUE)

  pers <- if (!is.na(em) && abs(em) > 0.001) lm / em else NA_real_

  post_d <- d[d$event_time >= 0, ]
  sl <- tryCatch(
    coef(lm(estimate ~ event_time, data = post_d))[2],
    error = function(e) NA_real_
  )

  shape <- dplyr::case_when(
    is.na(pers)                ~ "Undetermined",
    em > 0 & lm < 0           ~ "Full reversal (V-shape)",
    pers < 0.3                 ~ "Strong recovery (V-shape)",
    pers >= 0.3 & pers < 0.7  ~ "Partial recovery",
    pers >= 0.7                ~ "Persistent (step-change)",
    TRUE                       ~ "Undetermined"
  )

  data.frame(
    Outcome     = out_label,
    Window      = win_label,
    Pre_mean    = round(pm,   3),
    Early_post  = round(em,   3),
    Late_post   = round(lm,   3),
    Persistence = round(pers, 2),
    Post_slope  = round(sl,   4),
    Shape       = shape,
    stringsAsFactors = FALSE
  )
}

# Loop over all outcomes and both windows
shape_rows <- list()
for (i in seq_len(n_outcomes)) {
  coefs <- es_combined[[i]]
  lbl   <- es_outcomes[[i]]$label

  if (is.null(coefs) || nrow(coefs) == 0) next

  for (win in c("Crisis (2008q3)", "RBC Rule (2022q1)")) {
    row <- tryCatch(
      analyze_one_shape(coefs, win, lbl),
      error = function(e) {
        message(sprintf("    Shape error: %s / %s — %s", lbl, win, e$message))
        NULL
      }
    )
    if (!is.null(row)) shape_rows[[length(shape_rows) + 1]] <- row
  }
}

shape_results <- bind_rows(shape_rows)

cat("\n=== SHAPE ANALYSIS: V-SHAPE vs. STEP-CHANGE ===\n")
cat("Persistence > 0.7 = step-change | < 0.3 = V-shape recovery\n\n")
print(shape_results |> arrange(Outcome, Window), n = Inf)

write_csv(shape_results, file.path(TABLE_PATH, "3B_shape_analysis.csv"))
message("  Shape analysis saved.")


# =============================================================================
# 7. EVENT STUDY OVERLAY PLOT FUNCTION
# =============================================================================

plot_es_overlay <- function(coef_df, outcome_label, non_parallel = FALSE) {

  if (is.null(coef_df) || nrow(coef_df) == 0) return(NULL)

  coef_df <- coef_df |> mutate(window = as.character(window))

  sub <- paste0(
    "Event time = quarters relative to shock (Q0). ",
    "Ref = Q", EVENT_REF, ". 95% CI shaded."
  )
  if (non_parallel) sub <- paste0(sub, "\nNote: Non-parallel pre-trends.")

  crisis_df <- coef_df[coef_df$window == "Crisis (2008q3)", ]
  rbc_df    <- coef_df[coef_df$window == "RBC Rule (2022q1)", ]

  p <- ggplot(coef_df, aes(x = event_time, y = estimate,
                            color = window, shape = window)) +
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "gray95", alpha = 0.4) +
    # CI ribbons per window
    {if (nrow(crisis_df) > 0)
      geom_ribbon(data = crisis_df,
                  aes(ymin = conf.low, ymax = conf.high),
                  fill = COL_CI_CRISIS, alpha = 0.3, color = NA,
                  inherit.aes = FALSE,
                  mapping = aes(x = event_time, ymin = conf.low,
                                ymax = conf.high))
    } +
    {if (nrow(rbc_df) > 0)
      geom_ribbon(data = rbc_df,
                  aes(ymin = conf.low, ymax = conf.high),
                  fill = COL_CI_RBC, alpha = 0.3, color = NA,
                  inherit.aes = FALSE,
                  mapping = aes(x = event_time, ymin = conf.low,
                                ymax = conf.high))
    } +
    geom_hline(yintercept = 0, color = "gray50",
               linewidth = 0.5, linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.6, linetype = "dashed") +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    geom_point(
      data = coef_df |>
        filter(!is.na(p.value), p.value < 0.05, event_time >= 0),
      shape = 8, size = 2.8, show.legend = FALSE
    ) +
    scale_color_manual(values = c(
      "Crisis (2008q3)"   = COL_CRISIS,
      "RBC Rule (2022q1)" = COL_RBC
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
      title   = outcome_label,
      subtitle = sub,
      x       = "Quarters Relative to Shock",
      y       = "DiD Coefficient",
      color = NULL, shape = NULL,
      caption = "★ = p<0.05. Two-way FE. SE clustered at CU. Red=Crisis, Navy=RBC."
    ) +
    theme_rbc()

  p
}


# =============================================================================
# 8. SAVE INDIVIDUAL OVERLAY PLOTS
# =============================================================================

message("── Step 3: Saving individual overlay plots ───────────────────────────")

for (i in seq_len(n_outcomes)) {
  coefs <- es_combined[[i]]
  o     <- es_outcomes[[i]]
  if (is.null(coefs) || nrow(coefs) == 0) next

  p <- tryCatch(
    plot_es_overlay(coefs, o$label, o$np),
    error = function(e) {
      message(sprintf("  Plot error %s: %s", o$var, e$message)); NULL
    }
  )
  if (is.null(p)) next

  fname <- paste0("3B_es_overlay_", str_replace_all(o$var, "_", "-"), ".png")
  ggsave(file.path(FIGURE_PATH, fname), p, width = 10, height = 6, dpi = 300)
  message(sprintf("  Saved: %s", fname))
}


# =============================================================================
# 9. MAIN COMPARISON PANELS (2x2 grids)
# =============================================================================

message("── Step 4: Building main comparison panels ───────────────────────────")

# Helper: compact panel (no legend, smaller subtitle)
compact_panel <- function(idx, title_txt) {
  coefs <- es_combined[[idx]]
  o     <- es_outcomes[[idx]]
  if (is.null(coefs) || nrow(coefs) == 0) return(plot_spacer())
  p <- tryCatch(plot_es_overlay(coefs, title_txt, o$np),
                error = function(e) NULL)
  if (is.null(p)) return(plot_spacer())
  p + theme(legend.position = "none",
            plot.subtitle   = element_text(size = 7.5),
            plot.title      = element_text(size = 11))
}

# Indices: 1=NW ratio, 2=cap buffer, 3=loan growth, 4=asset growth
#          5=DQ, 6=chgoff, 7=ROA, 8=NIM, 9=RE share, 10=auto, 11=MBL

# Panel A: Capital & Lending
pA1 <- compact_panel(1, "A. Net Worth Ratio (%)")
pA2 <- compact_panel(2, "B. Capital Buffer (pp)")
pA3 <- compact_panel(3, "C. Loan Growth")
pA4 <- compact_panel(5, "D. Delinquency Rate (%)")

panel_A <- (pA1 + pA2) / (pA3 + pA4) +
  plot_annotation(
    title   = "Dynamic Responses: 2008 Crisis vs. 2022 RBC — Capital & Lending",
    subtitle = "Red triangles = crisis (2008 Q3). Navy circles = RBC (2022 Q1). ★ = p<0.05.",
    caption  = "Two-way FE (CU + quarter). SE clustered at CU. Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_A_capital_lending.png"),
       panel_A, width = 14, height = 10, dpi = 300)
message("  Panel A saved.")

# Panel B: Portfolio & Profitability
pB1 <- compact_panel(9,  "A. Real Estate Share (%)")
pB2 <- compact_panel(10, "B. Auto Loan Share (%)")
pB3 <- compact_panel(11, "C. MBL Share (%)")
pB4 <- compact_panel(7,  "D. ROA (%)")

panel_B <- (pB1 + pB2) / (pB3 + pB4) +
  plot_annotation(
    title   = "Dynamic Responses: 2008 Crisis vs. 2022 RBC — Portfolio & Profitability",
    subtitle = "Red triangles = crisis. Navy circles = RBC. ★ = p<0.05.",
    caption  = "Two-way FE (CU + quarter). SE clustered at CU. Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_B_portfolio_profitability.png"),
       panel_B, width = 14, height = 10, dpi = 300)
message("  Panel B saved.")

# Panel C: Credit Quality & NIM
pC1 <- compact_panel(6, "A. Charge-Off Ratio (%)")
pC2 <- compact_panel(8, "B. Net Interest Margin (%)")
pC3 <- compact_panel(4, "C. Asset Growth")
pC4 <- compact_panel(7, "D. ROA (%)")

panel_C <- (pC1 + pC2) / (pC3 + pC4) +
  plot_annotation(
    title   = "Dynamic Responses: 2008 Crisis vs. 2022 RBC — Credit Quality",
    subtitle = "Red triangles = crisis. Navy circles = RBC. ★ = p<0.05.",
    caption  = "Two-way FE (CU + quarter). SE clustered at CU. Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_C_credit_quality.png"),
       panel_C, width = 14, height = 10, dpi = 300)
message("  Panel C saved.")


# =============================================================================
# 10. BUFFER PROXIMITY ANALYSIS
# =============================================================================

message("── Step 5: Buffer proximity dose-response ────────────────────────────")

# Pre-shock capital buffer groups for complex CUs
get_buffer_groups <- function(df_panel, pre_start, pre_end,
                              complex_var, group_var_name) {
  df_panel |>
    filter(q_period_num >= pre_start,
           q_period_num <= pre_end,
           .data[[complex_var]] == 1) |>
    group_by(cu_number) |>
    summarise(pre_nw = mean(networth_ratio, na.rm = TRUE), .groups = "drop") |>
    mutate(
      !!group_var_name := case_when(
        pre_nw < 11  ~ "Thin (<11%)",
        pre_nw >= 12 ~ "Thick (>=12%)",
        TRUE         ~ "Mid (11-12%)"
      )
    )
}

crisis_buf <- get_buffer_groups(
  df_crisis, CRISIS_PRE_START, CRISIS_PRE_END,
  "complex_crisis", "buf_group"
)
rbc_buf <- get_buffer_groups(
  df_rbc, RBC_PRE_START, RBC_PRE_END,
  "complex_rbc", "buf_group"
)

message(sprintf("  Crisis buffer: Thin=%d, Mid=%d, Thick=%d",
                sum(crisis_buf$buf_group == "Thin (<11%)"),
                sum(crisis_buf$buf_group == "Mid (11-12%)"),
                sum(crisis_buf$buf_group == "Thick (>=12%)")))

message(sprintf("  RBC buffer: Thin=%d, Mid=%d, Thick=%d",
                sum(rbc_buf$buf_group == "Thin (<11%)"),
                sum(rbc_buf$buf_group == "Mid (11-12%)"),
                sum(rbc_buf$buf_group == "Thick (>=12%)")))

# DiD by buffer group
buf_outcomes <- c("loan_growth", "networth_ratio",
                  "dq_rate_var", "auto_shr", "re_shr")
buf_labels   <- c("Loan Growth", "Net Worth Ratio",
                  "DQ Rate", "Auto Share", "RE Share")

run_buf_did <- function(outcome, data, ctrl = controls_base) {
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

extract_buf_coef <- function(model, out_lbl, win_lbl, grp_lbl) {
  if (is.null(model)) return(NULL)
  tryCatch({
    tidy_m <- tidy(model, conf.int = TRUE)
    row    <- tidy_m[tidy_m$term == "treat_post", ]
    if (nrow(row) == 0) return(NULL)
    data.frame(
      Outcome      = out_lbl,
      Window       = win_lbl,
      Buffer_group = grp_lbl,
      Beta         = round(row$estimate,  3),
      SE           = round(row$std.error, 3),
      CI_low       = round(row$conf.low,  3),
      CI_high      = round(row$conf.high, 3),
      p_value      = round(row$p.value,   3),
      Stars        = dplyr::case_when(
        row$p.value < 0.01 ~ "***",
        row$p.value < 0.05 ~ "**",
        row$p.value < 0.10 ~ "*",
        TRUE               ~ ""
      ),
      N = nobs(model),
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
}

buf_rows <- list()
for (grp in c("Thin (<11%)", "Thick (>=12%)")) {

  # Crisis
  df_c_sub <- df_crisis |>
    inner_join(crisis_buf |> select(cu_number, buf_group),
               by = "cu_number") |>
    filter(buf_group == grp | complex_crisis == 0)

  # RBC
  df_r_sub <- df_rbc |>
    inner_join(rbc_buf |> select(cu_number, buf_group),
               by = "cu_number") |>
    filter(buf_group == grp | complex_rbc == 0)

  for (j in seq_along(buf_outcomes)) {
    mc <- run_buf_did(buf_outcomes[j], df_c_sub)
    mr <- run_buf_did(buf_outcomes[j], df_r_sub)

    buf_rows[[length(buf_rows) + 1]] <-
      extract_buf_coef(mc, buf_labels[j], "Crisis (2008q3)", grp)
    buf_rows[[length(buf_rows) + 1]] <-
      extract_buf_coef(mr, buf_labels[j], "RBC Rule (2022q1)", grp)
  }
}

buffer_results <- bind_rows(buf_rows)

cat("\n=== BUFFER PROXIMITY: DOSE-RESPONSE ===\n")
print(buffer_results |> arrange(Outcome, Window, Buffer_group), n = Inf)

write_csv(buffer_results, file.path(TABLE_PATH, "3B_buffer_proximity.csv"))

# Coefficient plot
p_buf <- ggplot(
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
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  scale_color_manual(
    values = c(
      "Crisis (2008q3).Thin (<11%)"     = "#C94040",
      "Crisis (2008q3).Thick (>=12%)"   = "#FFAAAA",
      "RBC Rule (2022q1).Thin (<11%)"   = "#1B3A6B",
      "RBC Rule (2022q1).Thick (>=12%)" = "#7AAAD0"
    ),
    labels = c(
      "Crisis (2008q3).Thin (<11%)"     = "Crisis — Thin (<11%)",
      "Crisis (2008q3).Thick (>=12%)"   = "Crisis — Thick (>=12%)",
      "RBC Rule (2022q1).Thin (<11%)"   = "RBC — Thin (<11%)",
      "RBC Rule (2022q1).Thick (>=12%)" = "RBC — Thick (>=12%)"
    )
  ) +
  scale_shape_manual(values = c("Thin (<11%)" = 17, "Thick (>=12%)" = 16)) +
  facet_wrap(~ Window, ncol = 2) +
  labs(
    title   = "Dose-Response by Capital Buffer: Crisis vs. RBC",
    subtitle = "Thin = net worth ratio < 11% pre-shock. Thick = >= 12%. 95% CI.",
    x       = "DiD Coefficient",
    y       = NULL,
    color   = NULL, shape = NULL,
    caption = "Two-way FE. SE clustered at CU. Source: NCUA Call Report (5300)."
  ) +
  theme_rbc() +
  theme(legend.position = "bottom")

ggsave(file.path(FIGURE_PATH, "3B_buffer_proximity_plot.png"),
       p_buf, width = 12, height = 8, dpi = 300)
message("  Buffer proximity plot saved.")


# =============================================================================
# 11. TIMING ANALYSIS
# =============================================================================

message("── Step 6: Timing analysis ───────────────────────────────────────────")

timing_rows <- list()

for (i in seq_len(n_outcomes)) {
  coefs <- es_combined[[i]]
  lbl   <- es_outcomes[[i]]$label
  if (is.null(coefs) || nrow(coefs) == 0) next

  for (win in c("Crisis (2008q3)", "RBC Rule (2022q1)")) {
    d <- coefs[coefs$window == win & !is.na(coefs$p.value) &
                 coefs$event_time >= 0, ]
    if (nrow(d) == 0) next

    peak_idx <- which.max(abs(d$estimate))
    timing_rows[[length(timing_rows) + 1]] <- data.frame(
      Outcome        = lbl,
      Window         = win,
      Peak_quarter   = d$event_time[peak_idx],
      Peak_coef      = round(d$estimate[peak_idx], 3),
      Peak_sig       = d$p.value[peak_idx] < 0.05,
      N_sig_quarters = sum(d$p.value < 0.05, na.rm = TRUE),
      First_sig_q    = if (any(d$p.value < 0.05, na.rm = TRUE))
        min(d$event_time[d$p.value < 0.05]) else NA_integer_,
      stringsAsFactors = FALSE
    )
  }
}

timing_results <- bind_rows(timing_rows)

cat("\n=== TIMING: WHEN DO EFFECTS PEAK? ===\n")
cat("Expect: Crisis peaks Q+1/Q+2, RBC peaks Q+3/Q+6\n\n")
print(timing_results |> arrange(Outcome, Window), n = Inf)

write_csv(timing_results, file.path(TABLE_PATH, "3B_timing_analysis.csv"))
message("  Timing analysis saved.")


# =============================================================================
# 12. COMBINED SUMMARY TABLE
# =============================================================================

message("── Step 7: Summary table ─────────────────────────────────────────────")

summary_3b <- shape_results |>
  left_join(
    timing_results |>
      select(Outcome, Window, Peak_quarter, N_sig_quarters, First_sig_q),
    by = c("Outcome", "Window")
  ) |>
  mutate(
    Mechanism = dplyr::case_when(
      Shape == "Persistent (step-change)" & str_detect(Window, "RBC") ~
        "Regulatory permanent effect",
      Shape %in% c("Strong recovery (V-shape)", "Full reversal (V-shape)") &
        str_detect(Window, "RBC") ~ "Crisis-like transitory response",
      Shape == "Partial recovery" & str_detect(Window, "RBC") ~
        "Regulatory medium-term effect",
      TRUE ~ Shape
    )
  ) |>
  arrange(Outcome, Window)

cat("\n=== COMBINED SUMMARY: SHAPE + TIMING ===\n")
print(summary_3b, n = Inf)

write_csv(summary_3b, file.path(TABLE_PATH, "3B_summary_shape_timing.csv"))


# =============================================================================
# 13. FINAL OUTPUT SUMMARY
# =============================================================================

cat("\n=== 3B COMPLETE ===\n\n")

cat("Tables (output/tables/):\n")
for (t in c("3B_shape_analysis.csv", "3B_buffer_proximity.csv",
            "3B_timing_analysis.csv", "3B_summary_shape_timing.csv")) {
  cat("  →", t, "\n")
}

cat("\nFigures (output/figures/):\n")
for (f in c("3B_panel_A_capital_lending.png",
            "3B_panel_B_portfolio_profitability.png",
            "3B_panel_C_credit_quality.png",
            "3B_buffer_proximity_plot.png",
            "[+ individual 3B_es_overlay_*.png]")) {
  cat("  →", f, "\n")
}

cat("\nKey interpretation guide:\n")
cat("  3B_shape_analysis.csv\n")
cat("    Persistence > 0.7 → step-change (regulatory permanent shift)\n")
cat("    Persistence < 0.3 → V-shape recovery (transitory crisis)\n")
cat("  3B_panel_A_capital_lending.png\n")
cat("    Visual comparison of dynamic paths\n")
cat("  3B_buffer_proximity_plot.png\n")
cat("    Thin vs thick buffer dose-response\n")
cat("    Larger thin/thick gap under RBC = threshold-specific pressure\n")

message("\n── 3B_2008_Crisis_EventStudy.R complete ✓ ───────────────────────────")
message("  Next: proceed to 4_Paper_Tables.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
