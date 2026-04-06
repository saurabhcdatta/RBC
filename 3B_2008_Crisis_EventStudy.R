# =============================================================================
# 3B_2008_Crisis_EventStudy.R
# RBC Rule Analysis — 2008 Crisis vs. RBC Event Study Comparison
# NCUA Call Report (5300) Data
#
# Author  : [Your Name]
# Created : 2026
#
# AUDIT LOG (all bugs fixed vs prior versions):
#   BUG 1: lm() masked by variable named 'lm' in analyze_one_shape → renamed
#   BUG 2: bind_rows() on list of NULLs returns 0-col tibble → use rbind loop
#   BUG 3: window column type inconsistency (factor vs char) → forced character
#          at every step with explicit as.character()
#   BUG 4: next inside tryCatch doesn't propagate → moved next outside
#   BUG 5: dplyr::case_when on scalar → replaced with plain if/else chain
#   BUG 6: geom_ribbon with duplicated aes() args → cleaned to single mapping
#   BUG 7: map2/sapply on NULL list elements → explicit for loops with guards
#
# REQUIRES: df_crisis, df_rbc from 3A session.
#           Set STANDALONE <- TRUE to rebuild from raw data.
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)   # ggplot2, dplyr, purrr, stringr, readr
library(fixest)      # feols()
library(patchwork)   # panel plots
library(scales)
library(broom)       # tidy()


# =============================================================================
# 1. SETTINGS
# =============================================================================

RAW_DATA_PATH    <- "call_report.rds"
TABLE_PATH       <- "output/tables/"
FIGURE_PATH      <- "output/figures/"

STANDALONE       <- FALSE   # TRUE = rebuild panels from raw data

# Match 3A exactly
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

EVENT_MIN        <- -12L
EVENT_MAX        <-  10L
EVENT_REF        <-  -1L

CONTROLS         <- "ln_assets + loan_to_asset"

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
# 2. REBUILD PANELS IF STANDALONE
# =============================================================================

if (STANDALONE) {

  message("── Standalone: rebuilding panels ─────────────────────────────────")

  df_raw <- readRDS(RAW_DATA_PATH)

  merged_cus <- df_raw |>
    filter(outcome %in% c(2, 3)) |>
    pull(cu_number) |>
    unique()

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

  # ── Crisis panel ───────────────────────────────────────────────────────
  crisis_tx <- df_clean |>
    filter(q_period_num >= CRISIS_PRE_START,
           q_period_num <= CRISIS_PRE_END) |>
    group_by(cu_number) |>
    summarise(avg_pre = mean(assets_tot, na.rm = TRUE), .groups = "drop") |>
    mutate(complex_crisis = as.integer(avg_pre >= SIZE_THRESHOLD))

  df_crisis <- df_clean |>
    filter(q_period_num >= CRISIS_START,
           q_period_num <= CRISIS_END) |>
    inner_join(crisis_tx |> select(cu_number, complex_crisis),
               by = "cu_number") |>
    filter(!is.na(complex_crisis)) |>
    mutate(
      post_crisis = as.integer(q_period_num >= CRISIS_DATE),
      treat_post  = complex_crisis * post_crisis,
      event_time  = as.integer(case_when(
        quarter == 1L ~ (year - 2008L) * 4L - 2L,
        quarter == 2L ~ (year - 2008L) * 4L - 1L,
        quarter == 3L ~ (year - 2008L) * 4L + 0L,
        quarter == 4L ~ (year - 2008L) * 4L + 1L
      ))
    )

  # ── RBC panel ──────────────────────────────────────────────────────────
  rbc_tx <- df_clean |>
    filter(q_period_num >= RBC_PRE_START,
           q_period_num <= RBC_PRE_END) |>
    group_by(cu_number) |>
    summarise(avg_pre = mean(assets_tot, na.rm = TRUE), .groups = "drop") |>
    mutate(complex_rbc = as.integer(avg_pre >= SIZE_THRESHOLD))

  df_rbc <- df_clean |>
    filter(q_period_num >= RBC_START,
           q_period_num <= RBC_END) |>
    inner_join(rbc_tx |> select(cu_number, complex_rbc),
               by = "cu_number") |>
    filter(!is.na(complex_rbc)) |>
    mutate(
      post_rbc   = as.integer(q_period_num >= RBC_DATE),
      treat_post = complex_rbc * post_rbc,
      event_time = as.integer(case_when(
        quarter == 1L ~ (year - 2022L) * 4L + 0L,
        quarter == 2L ~ (year - 2022L) * 4L + 1L,
        quarter == 3L ~ (year - 2022L) * 4L + 2L,
        quarter == 4L ~ (year - 2022L) * 4L + 3L
      ))
    )

  message("  Panels rebuilt.")

} else {
  message("── Using df_crisis / df_rbc from 3A session ─────────────────────")
  stopifnot(exists("df_crisis"), exists("df_rbc"))
}


# =============================================================================
# 3. OUTCOME DEFINITIONS
# =============================================================================

# Each entry: var = column name, label = display label, np = non-parallel flag
ES_OUTCOMES <- list(
  list(var = "networth_ratio", label = "Net Worth Ratio (%)",     np = TRUE),
  list(var = "cap_buffer",     label = "Capital Buffer (pp)",     np = TRUE),
  list(var = "loan_growth",    label = "Loan Growth (QoQ log)",   np = FALSE),
  list(var = "asset_growth",   label = "Asset Growth (QoQ log)",  np = FALSE),
  list(var = "dq_rate_var",    label = "Delinquency Rate (%)",    np = FALSE),
  list(var = "chgoff_ratio",   label = "Charge-Off Ratio (%)",    np = FALSE),
  list(var = "roa_var",        label = "ROA (%)",                 np = FALSE),
  list(var = "nim",            label = "NIM (%)",                 np = FALSE),
  list(var = "re_shr",         label = "RE Share (%)",            np = FALSE),
  list(var = "auto_shr",       label = "Auto Loan Share (%)",     np = FALSE),
  list(var = "mbl_shr",        label = "MBL Share (%)",           np = TRUE)
)

N_OUT <- length(ES_OUTCOMES)


# =============================================================================
# 4. EVENT STUDY MODEL FUNCTION
# =============================================================================

run_es_model <- function(outcome_var, panel_df, complex_col) {
  # Filter to event window first
  panel_sub <- panel_df[
    !is.na(panel_df$event_time) &
      panel_df$event_time >= EVENT_MIN &
      panel_df$event_time <= EVENT_MAX, ]

  fml_str <- paste0(
    outcome_var,
    " ~ i(event_time, ", complex_col, ", ref = ", EVENT_REF, ") + ",
    CONTROLS,
    " | cu_number + q_period_num"
  )

  tryCatch(
    feols(as.formula(fml_str), data = panel_sub,
          cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) {
      message("  Model failed: ", outcome_var, " / ", complex_col,
              " — ", e$message)
      NULL
    }
  )
}


# =============================================================================
# 5. COEFFICIENT EXTRACTION FUNCTION
# =============================================================================
# AUDIT: Returns a plain data.frame (not tibble) with character window column.
# No bind_rows on potentially-NULL inputs — builds incrementally with rbind().

extract_es_coefs <- function(model_obj, win_str) {
  # win_str: "Crisis (2008q3)" or "RBC Rule (2022q1)"
  if (is.null(model_obj)) return(NULL)

  raw <- tryCatch(
    tidy(model_obj, conf.int = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  # Keep only event_time interaction rows
  raw <- raw[grepl("event_time::", raw$term, fixed = TRUE), ]
  if (nrow(raw) == 0) return(NULL)

  # Extract integer event time from term string
  # e.g. "event_time::-4:complex_crisis" → -4
  raw$event_time <- as.integer(
    regmatches(raw$term, regexpr("-?[0-9]+", raw$term))
  )

  # Keep within window
  raw <- raw[raw$event_time >= EVENT_MIN & raw$event_time <= EVENT_MAX, ]
  if (nrow(raw) == 0) return(NULL)

  # Build output data.frame — all columns explicit, no tibble magic
  out <- data.frame(
    event_time = raw$event_time,
    estimate   = raw$estimate,
    std_error  = raw$std.error,
    p_value    = raw$p.value,
    conf_low   = raw$conf.low,
    conf_high  = raw$conf.high,
    window     = as.character(win_str),
    stringsAsFactors = FALSE
  )

  # Add reference row (event_time = EVENT_REF, all zeros)
  ref_row <- data.frame(
    event_time = as.integer(EVENT_REF),
    estimate   = 0,
    std_error  = 0,
    p_value    = NA_real_,
    conf_low   = 0,
    conf_high  = 0,
    window     = as.character(win_str),
    stringsAsFactors = FALSE
  )

  out <- rbind(out, ref_row)
  out <- out[order(out$event_time), ]
  rownames(out) <- NULL
  out
}


# =============================================================================
# 6. RUN EVENT STUDIES — BOTH WINDOWS
# =============================================================================

message("── Step 1: Running event studies ────────────────────────────────────")

# Lists to hold feols model objects
crisis_models <- vector("list", N_OUT)
rbc_models    <- vector("list", N_OUT)

for (i in seq_len(N_OUT)) {
  vname <- ES_OUTCOMES[[i]]$var
  message("  ", vname, "...")
  crisis_models[[i]] <- run_es_model(vname, df_crisis, "complex_crisis")
  rbc_models[[i]]    <- run_es_model(vname, df_rbc,    "complex_rbc")
}

# Build es_combined: list of N_OUT data.frames, one per outcome
# Each df has rows for both windows with character 'window' column
es_combined <- vector("list", N_OUT)

for (i in seq_len(N_OUT)) {
  parts <- NULL

  c_coefs <- extract_es_coefs(crisis_models[[i]], "Crisis (2008q3)")
  r_coefs <- extract_es_coefs(rbc_models[[i]],    "RBC Rule (2022q1)")

  if (!is.null(c_coefs)) parts <- c_coefs
  if (!is.null(r_coefs)) {
    if (is.null(parts)) parts <- r_coefs else parts <- rbind(parts, r_coefs)
  }

  if (!is.null(parts)) {
    parts$window <- as.character(parts$window)   # guarantee character
    es_combined[[i]] <- parts
  }
}

# Diagnostic
n_good <- sum(!sapply(es_combined, is.null))
message(sprintf("  Event studies complete. %d/%d outcomes have data.",
                n_good, N_OUT))

# Spot-check first good element
first_idx <- which(!sapply(es_combined, is.null))[1]
if (!is.na(first_idx)) {
  message("  First outcome with data: ", ES_OUTCOMES[[first_idx]]$label)
  message("  Window values: ",
          paste(unique(es_combined[[first_idx]]$window), collapse = " | "))
  message("  Column class (window): ",
          class(es_combined[[first_idx]]$window))
}


# =============================================================================
# 7. SHAPE ANALYSIS — V-SHAPE vs. STEP-CHANGE
# =============================================================================
# AUDIT: Uses base R only. No dplyr inside function. No naming conflict with
# lm() — the linear model function. Variable renamed to late_mean throughout.

message("── Step 2: Shape analysis ────────────────────────────────────────────")

# Pre-allocate result data.frame
shape_results <- data.frame(
  Outcome     = character(0),
  Window      = character(0),
  Pre_mean    = numeric(0),
  Early_post  = numeric(0),
  Late_post   = numeric(0),
  Persistence = numeric(0),
  Post_slope  = numeric(0),
  Shape       = character(0),
  stringsAsFactors = FALSE
)

for (i in seq_len(N_OUT)) {
  lbl   <- ES_OUTCOMES[[i]]$label
  coefs <- es_combined[[i]]

  if (is.null(coefs) || !is.data.frame(coefs) || nrow(coefs) == 0) {
    message("  Skipping ", lbl, " — no coefficient data")
    next
  }

  # Ensure window is character
  coefs$window <- as.character(coefs$window)

  for (win in c("Crisis (2008q3)", "RBC Rule (2022q1)")) {

    # Subset to this window, exclude reference row (p_value is NA)
    d <- coefs[coefs$window == win & !is.na(coefs$p_value), ]

    if (nrow(d) == 0) {
      message("  Skipping ", lbl, " / ", win, " — no rows after filter")
      next
    }

    # Period subsets
    d_early <- d[d$event_time >= 1L  & d$event_time <= 4L,  ]
    d_late  <- d[d$event_time >= 5L  & d$event_time <= EVENT_MAX, ]
    d_pre   <- d[d$event_time >= EVENT_MIN & d$event_time <= -2L, ]
    d_post  <- d[d$event_time >= 0L, ]

    if (nrow(d_early) == 0 || nrow(d_late) == 0) {
      message("  Skipping ", lbl, " / ", win,
              " — insufficient early/late post data (",
              nrow(d_early), " early, ", nrow(d_late), " late)")
      next
    }

    early_mean <- mean(d_early$estimate, na.rm = TRUE)
    late_mean  <- mean(d_late$estimate,  na.rm = TRUE)  # NOT named 'lm'
    pre_mean   <- if (nrow(d_pre) > 0)
      mean(d_pre$estimate, na.rm = TRUE) else NA_real_

    # Persistence ratio: late / early
    # >0.7 = persistent step-change, <0.3 = V-shape recovery
    persistence <- if (!is.na(early_mean) && abs(early_mean) > 0.001)
      late_mean / early_mean else NA_real_

    # Post-period slope using stats::lm (explicit namespace)
    post_slope <- tryCatch({
      fit <- stats::lm(estimate ~ event_time, data = d_post)
      as.numeric(coef(fit)[["event_time"]])
    }, error = function(e) NA_real_)

    # Shape classification — plain if/else, no case_when
    shape_label <- if (is.na(persistence)) {
      "Undetermined"
    } else if (early_mean > 0 && late_mean < 0) {
      "Full reversal (V-shape)"
    } else if (persistence < 0.3) {
      "Strong recovery (V-shape)"
    } else if (persistence < 0.7) {
      "Partial recovery"
    } else {
      "Persistent (step-change)"
    }

    new_row <- data.frame(
      Outcome     = lbl,
      Window      = win,
      Pre_mean    = round(pre_mean,    3),
      Early_post  = round(early_mean,  3),
      Late_post   = round(late_mean,   3),
      Persistence = round(persistence, 2),
      Post_slope  = round(post_slope,  4),
      Shape       = shape_label,
      stringsAsFactors = FALSE
    )

    shape_results <- rbind(shape_results, new_row)

    message(sprintf("  %-35s | %-22s | Persist=%-5s | %s",
                    lbl, win,
                    ifelse(is.na(persistence), "NA",
                           as.character(round(persistence, 2))),
                    shape_label))
  }
}

cat("\n=== SHAPE ANALYSIS: V-SHAPE vs. STEP-CHANGE ===\n")
cat("Persistence > 0.7 = step-change (regulatory)\n")
cat("Persistence < 0.3 = V-shape recovery (crisis)\n\n")
print(shape_results[order(shape_results$Outcome, shape_results$Window), ],
      row.names = FALSE)

write.csv(shape_results,
          file.path(TABLE_PATH, "3B_shape_analysis.csv"),
          row.names = FALSE)
message("Saved: 3B_shape_analysis.csv")


# =============================================================================
# 8. EVENT STUDY OVERLAY PLOT FUNCTION
# =============================================================================
# AUDIT: CI ribbons use separate geom_ribbon() calls with explicit data=
# argument. No inherit.aes conflicts. No duplicate aes() mappings.

plot_es_overlay <- function(coef_df, outcome_label,
                            non_parallel = FALSE) {

  if (is.null(coef_df) || !is.data.frame(coef_df) || nrow(coef_df) == 0) {
    return(NULL)
  }

  coef_df$window <- as.character(coef_df$window)

  sub_txt <- paste0(
    "Event time = quarters relative to shock (Q0 = treatment date). ",
    "Reference = Q", EVENT_REF, ". 95% CI shaded."
  )
  if (non_parallel) {
    sub_txt <- paste0(sub_txt, "\nNote: Non-parallel pre-trends present.")
  }

  # Split by window for separate CI ribbons
  df_c <- coef_df[coef_df$window == "Crisis (2008q3)", ]
  df_r <- coef_df[coef_df$window == "RBC Rule (2022q1)", ]

  # Significant post-period points
  df_sig <- coef_df[
    !is.na(coef_df$p_value) &
      coef_df$p_value < 0.05 &
      coef_df$event_time >= 0L, ]

  p <- ggplot(coef_df,
              aes(x = event_time, y = estimate,
                  color = window, shape = window)) +
    # Post-period background shading
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "gray93", alpha = 0.5) +
    # Crisis CI ribbon
    geom_ribbon(
      data    = df_c,
      mapping = aes(x = event_time,
                    ymin = conf_low, ymax = conf_high),
      fill    = COL_CI_CRISIS,
      alpha   = 0.3,
      color   = NA,
      inherit.aes = FALSE
    ) +
    # RBC CI ribbon
    geom_ribbon(
      data    = df_r,
      mapping = aes(x = event_time,
                    ymin = conf_low, ymax = conf_high),
      fill    = COL_CI_RBC,
      alpha   = 0.3,
      color   = NA,
      inherit.aes = FALSE
    ) +
    # Reference lines
    geom_hline(yintercept = 0, color = "gray50",
               linewidth  = 0.5, linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth  = 0.6, linetype = "dashed") +
    # Main lines + points
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.2) +
    # Significant post-period markers (★)
    geom_point(
      data        = df_sig,
      mapping     = aes(x = event_time, y = estimate),
      shape       = 8,
      size        = 2.8,
      color       = "darkred",
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = c("Crisis (2008q3)"   = COL_CRISIS,
                 "RBC Rule (2022q1)" = COL_RBC)
    ) +
    scale_shape_manual(
      values = c("Crisis (2008q3)"   = 17L,
                 "RBC Rule (2022q1)" = 16L)
    ) +
    scale_x_continuous(
      breaks = seq(EVENT_MIN, EVENT_MAX, by = 4L),
      labels = function(x) paste0("Q", x)
    ) +
    labs(
      title    = outcome_label,
      subtitle = sub_txt,
      x        = "Quarters Relative to Shock",
      y        = "DiD Coefficient",
      color    = NULL,
      shape    = NULL,
      caption  = paste0(
        "★ = p<0.05 (post-period). Two-way FE (CU + quarter-year). ",
        "SE clustered at CU. Red = Crisis 2008q3, Navy = RBC 2022q1."
      )
    ) +
    theme_rbc()

  p
}


# =============================================================================
# 9. SAVE INDIVIDUAL OVERLAY PLOTS
# =============================================================================

message("── Step 3: Individual overlay plots ─────────────────────────────────")

for (i in seq_len(N_OUT)) {
  o     <- ES_OUTCOMES[[i]]
  coefs <- es_combined[[i]]

  if (is.null(coefs) || nrow(coefs) == 0) {
    message("  Skipping plot: ", o$var, " — no data")
    next
  }

  p <- tryCatch(
    plot_es_overlay(coefs, o$label, o$np),
    error = function(e) {
      message("  Plot error (", o$var, "): ", e$message)
      NULL
    }
  )
  if (is.null(p)) next

  fname <- paste0("3B_es_overlay_",
                  gsub("_", "-", o$var, fixed = TRUE), ".png")
  ggsave(file.path(FIGURE_PATH, fname),
         p, width = 10, height = 6, dpi = 300)
  message("  Saved: ", fname)
}


# =============================================================================
# 10. MAIN COMPARISON PANELS
# =============================================================================

message("── Step 4: Main comparison panels ───────────────────────────────────")

# Compact version: no legend, smaller text
compact_panel <- function(idx, title_txt) {
  coefs <- es_combined[[idx]]
  o     <- ES_OUTCOMES[[idx]]
  if (is.null(coefs) || nrow(coefs) == 0) return(patchwork::plot_spacer())

  p <- tryCatch(
    plot_es_overlay(coefs, title_txt, o$np),
    error = function(e) NULL
  )
  if (is.null(p)) return(patchwork::plot_spacer())

  p + theme(
    legend.position = "none",
    plot.subtitle   = element_text(size = 7.5),
    plot.title      = element_text(size = 11)
  )
}

# Outcome index reference:
# 1=networth_ratio  2=cap_buffer    3=loan_growth   4=asset_growth
# 5=dq_rate_var     6=chgoff_ratio  7=roa_var       8=nim
# 9=re_shr         10=auto_shr     11=mbl_shr

# ── Panel A: Capital & Lending ────────────────────────────────────────────
pA1 <- compact_panel(1, "A. Net Worth Ratio (%)")
pA2 <- compact_panel(2, "B. Capital Buffer (pp)")
pA3 <- compact_panel(3, "C. Loan Growth")
pA4 <- compact_panel(5, "D. Delinquency Rate (%)")

panel_A <- (pA1 + pA2) / (pA3 + pA4) +
  plot_annotation(
    title    = "Dynamic Responses: 2008 Crisis vs. 2022 RBC — Capital & Lending",
    subtitle = paste0("Red triangles = crisis (2008 Q3). ",
                      "Navy circles = RBC (2022 Q1). ★ = p<0.05."),
    caption  = paste0("Two-way FE (CU + quarter-year). SE clustered at CU. ",
                      "Source: NCUA Call Report (5300).")
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_A_capital_lending.png"),
       panel_A, width = 14, height = 10, dpi = 300)
message("  Panel A saved.")

# ── Panel B: Portfolio & Profitability ────────────────────────────────────
pB1 <- compact_panel(9,  "A. Real Estate Share (%)")
pB2 <- compact_panel(10, "B. Auto Loan Share (%)")
pB3 <- compact_panel(11, "C. MBL Share (%)")
pB4 <- compact_panel(7,  "D. ROA (%)")

panel_B <- (pB1 + pB2) / (pB3 + pB4) +
  plot_annotation(
    title    = "Dynamic Responses: 2008 Crisis vs. 2022 RBC — Portfolio & Profitability",
    subtitle = "Red triangles = crisis. Navy circles = RBC. ★ = p<0.05.",
    caption  = paste0("Two-way FE (CU + quarter-year). SE clustered at CU. ",
                      "Source: NCUA Call Report (5300).")
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_B_portfolio_profitability.png"),
       panel_B, width = 14, height = 10, dpi = 300)
message("  Panel B saved.")

# ── Panel C: Credit Quality ───────────────────────────────────────────────
pC1 <- compact_panel(6, "A. Charge-Off Ratio (%)")
pC2 <- compact_panel(8, "B. Net Interest Margin (%)")
pC3 <- compact_panel(4, "C. Asset Growth")
pC4 <- compact_panel(7, "D. ROA (%)")

panel_C <- (pC1 + pC2) / (pC3 + pC4) +
  plot_annotation(
    title    = "Dynamic Responses: 2008 Crisis vs. 2022 RBC — Credit Quality",
    subtitle = "Red triangles = crisis. Navy circles = RBC. ★ = p<0.05.",
    caption  = paste0("Two-way FE (CU + quarter-year). SE clustered at CU. ",
                      "Source: NCUA Call Report (5300).")
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_C_credit_quality.png"),
       panel_C, width = 14, height = 10, dpi = 300)
message("  Panel C saved.")


# =============================================================================
# 11. BUFFER PROXIMITY ANALYSIS
# =============================================================================
# Tests dose-response: thin-buffer complex CUs should respond more.
# If gap is larger under RBC than crisis → regulatory threshold matters.

message("── Step 5: Buffer proximity analysis ────────────────────────────────")

# Classify complex CUs by pre-shock net worth ratio
classify_buffers <- function(panel_df, pre_start, pre_end, complex_col) {
  sub <- panel_df[
    panel_df$q_period_num >= pre_start &
      panel_df$q_period_num <= pre_end &
      panel_df[[complex_col]] == 1L, ]

  if (nrow(sub) == 0) return(data.frame(cu_number = integer(0),
                                         buf_group = character(0)))

  agg <- aggregate(networth_ratio ~ cu_number, data = sub,
                   FUN = function(x) mean(x, na.rm = TRUE))
  names(agg)[2] <- "pre_nw"

  agg$buf_group <- ifelse(agg$pre_nw < 11,  "Thin (<11%)",
                   ifelse(agg$pre_nw >= 12, "Thick (>=12%)",
                                            "Mid (11-12%)"))
  agg[, c("cu_number", "buf_group")]
}

crisis_buf <- classify_buffers(df_crisis, CRISIS_PRE_START, CRISIS_PRE_END,
                                "complex_crisis")
rbc_buf    <- classify_buffers(df_rbc,    RBC_PRE_START,    RBC_PRE_END,
                                "complex_rbc")

message(sprintf("  Crisis: Thin=%d, Mid=%d, Thick=%d",
                sum(crisis_buf$buf_group == "Thin (<11%)"),
                sum(crisis_buf$buf_group == "Mid (11-12%)"),
                sum(crisis_buf$buf_group == "Thick (>=12%)")))
message(sprintf("  RBC:    Thin=%d, Mid=%d, Thick=%d",
                sum(rbc_buf$buf_group    == "Thin (<11%)"),
                sum(rbc_buf$buf_group    == "Mid (11-12%)"),
                sum(rbc_buf$buf_group    == "Thick (>=12%)")))

# DiD function
run_buf_did <- function(outcome_var, data_df) {
  fml_str <- paste0(outcome_var, " ~ treat_post + ", CONTROLS,
                    " | cu_number + q_period_num")
  tryCatch(
    feols(as.formula(fml_str), data = data_df,
          cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
}

# Extract coefficient from model
extract_buf_coef <- function(model_obj, out_lbl, win_lbl, grp_lbl) {
  if (is.null(model_obj)) return(NULL)
  tryCatch({
    td   <- tidy(model_obj, conf.int = TRUE)
    row  <- td[td$term == "treat_post", ]
    if (nrow(row) == 0) return(NULL)
    stars <- if (row$p.value < 0.01) "***" else
             if (row$p.value < 0.05) "**"  else
             if (row$p.value < 0.10) "*"   else ""
    data.frame(
      Outcome      = out_lbl,
      Window       = win_lbl,
      Buffer_group = grp_lbl,
      Beta         = round(row$estimate,  3),
      SE           = round(row$std.error, 3),
      CI_low       = round(row$conf.low,  3),
      CI_high      = round(row$conf.high, 3),
      p_value      = round(row$p.value,   3),
      Stars        = stars,
      N            = as.integer(nobs(model_obj)),
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
}

buf_outcome_vars  <- c("loan_growth", "networth_ratio",
                       "dq_rate_var", "auto_shr", "re_shr")
buf_outcome_lbls  <- c("Loan Growth", "Net Worth Ratio",
                       "DQ Rate", "Auto Share", "RE Share")

buffer_results <- data.frame(
  Outcome = character(0), Window = character(0), Buffer_group = character(0),
  Beta = numeric(0), SE = numeric(0), CI_low = numeric(0),
  CI_high = numeric(0), p_value = numeric(0), Stars = character(0),
  N = integer(0), stringsAsFactors = FALSE
)

for (grp in c("Thin (<11%)", "Thick (>=12%)")) {
  # Merge buffer group into panels
  df_c_g <- merge(df_crisis, crisis_buf, by = "cu_number", all.x = FALSE)
  df_c_g <- df_c_g[df_c_g$buf_group == grp | df_c_g$complex_crisis == 0L, ]

  df_r_g <- merge(df_rbc, rbc_buf, by = "cu_number", all.x = FALSE)
  df_r_g <- df_r_g[df_r_g$buf_group == grp | df_r_g$complex_rbc == 0L, ]

  for (j in seq_along(buf_outcome_vars)) {
    mc <- run_buf_did(buf_outcome_vars[j], df_c_g)
    mr <- run_buf_did(buf_outcome_vars[j], df_r_g)

    rc <- extract_buf_coef(mc, buf_outcome_lbls[j], "Crisis (2008q3)", grp)
    rr <- extract_buf_coef(mr, buf_outcome_lbls[j], "RBC Rule (2022q1)", grp)

    if (!is.null(rc)) buffer_results <- rbind(buffer_results, rc)
    if (!is.null(rr)) buffer_results <- rbind(buffer_results, rr)
  }
}

cat("\n=== BUFFER PROXIMITY: DOSE-RESPONSE ===\n")
print(buffer_results[order(buffer_results$Outcome,
                           buffer_results$Window,
                           buffer_results$Buffer_group), ],
      row.names = FALSE)

write.csv(buffer_results,
          file.path(TABLE_PATH, "3B_buffer_proximity.csv"),
          row.names = FALSE)

# Buffer proximity plot
if (nrow(buffer_results) > 0) {
  buffer_results$group_id <- interaction(buffer_results$Window,
                                          buffer_results$Buffer_group,
                                          sep = ".")
  col_vals <- c(
    "Crisis (2008q3).Thin (<11%)"     = "#C94040",
    "Crisis (2008q3).Thick (>=12%)"   = "#FFAAAA",
    "RBC Rule (2022q1).Thin (<11%)"   = "#1B3A6B",
    "RBC Rule (2022q1).Thick (>=12%)" = "#7AAAD0"
  )

  p_buf <- ggplot(
    buffer_results[!is.na(buffer_results$Beta), ],
    aes(x = Beta, y = Outcome,
        xmin = CI_low, xmax = CI_high,
        color = group_id, shape = Buffer_group)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray50", linewidth = 0.6) +
    geom_errorbarh(height = 0.3, linewidth = 0.8,
                   position = position_dodge(width = 0.7)) +
    geom_point(size = 3,
               position = position_dodge(width = 0.7)) +
    scale_color_manual(values = col_vals) +
    scale_shape_manual(values = c("Thin (<11%)" = 17L,
                                  "Thick (>=12%)" = 16L)) +
    facet_wrap(~ Window, ncol = 2) +
    labs(
      title    = "Dose-Response by Capital Buffer: Crisis vs. RBC",
      subtitle = "Thin = net worth ratio < 11% pre-shock. Thick = >= 12%. 95% CI.",
      x        = "DiD Coefficient (Complex × Post-Shock)",
      y        = NULL,
      color    = NULL, shape = NULL,
      caption  = paste0("Two-way FE. SE clustered at CU. ",
                        "Source: NCUA Call Report (5300).")
    ) +
    theme_rbc() +
    theme(legend.position = "bottom")

  ggsave(file.path(FIGURE_PATH, "3B_buffer_proximity_plot.png"),
         p_buf, width = 12, height = 8, dpi = 300)
  message("  Buffer proximity plot saved.")
} else {
  message("  No buffer results to plot.")
}


# =============================================================================
# 12. TIMING ANALYSIS
# =============================================================================

message("── Step 6: Timing analysis ───────────────────────────────────────────")

timing_results <- data.frame(
  Outcome        = character(0),
  Window         = character(0),
  Peak_quarter   = integer(0),
  Peak_coef      = numeric(0),
  Peak_sig       = logical(0),
  N_sig_quarters = integer(0),
  First_sig_q    = integer(0),
  stringsAsFactors = FALSE
)

for (i in seq_len(N_OUT)) {
  lbl   <- ES_OUTCOMES[[i]]$label
  coefs <- es_combined[[i]]
  if (is.null(coefs) || nrow(coefs) == 0) next

  coefs$window <- as.character(coefs$window)

  for (win in c("Crisis (2008q3)", "RBC Rule (2022q1)")) {
    d <- coefs[coefs$window == win &
                 !is.na(coefs$p_value) &
                 coefs$event_time >= 0L, ]
    if (nrow(d) == 0) next

    pk <- which.max(abs(d$estimate))
    sig_q <- d$event_time[!is.na(d$p_value) & d$p_value < 0.05]

    timing_results <- rbind(timing_results, data.frame(
      Outcome        = lbl,
      Window         = win,
      Peak_quarter   = as.integer(d$event_time[pk]),
      Peak_coef      = round(d$estimate[pk], 3),
      Peak_sig       = (!is.na(d$p_value[pk]) && d$p_value[pk] < 0.05),
      N_sig_quarters = as.integer(length(sig_q)),
      First_sig_q    = as.integer(if (length(sig_q) > 0) min(sig_q) else NA_integer_),
      stringsAsFactors = FALSE
    ))
  }
}

cat("\n=== TIMING: WHEN DO EFFECTS PEAK? ===\n")
cat("Expect: Crisis peaks Q+1/Q+2, RBC builds to Q+3/Q+6\n\n")
print(timing_results[order(timing_results$Outcome,
                            timing_results$Window), ],
      row.names = FALSE)

write.csv(timing_results,
          file.path(TABLE_PATH, "3B_timing_analysis.csv"),
          row.names = FALSE)
message("  Timing analysis saved.")


# =============================================================================
# 13. COMBINED SUMMARY TABLE
# =============================================================================

message("── Step 7: Combined summary table ───────────────────────────────────")

if (nrow(shape_results) > 0 && nrow(timing_results) > 0) {

  summary_3b <- merge(
    shape_results,
    timing_results[, c("Outcome", "Window", "Peak_quarter",
                       "N_sig_quarters", "First_sig_q")],
    by = c("Outcome", "Window"),
    all.x = TRUE
  )

  summary_3b$Mechanism <- ifelse(
    summary_3b$Shape == "Persistent (step-change)" &
      grepl("RBC", summary_3b$Window),
    "Regulatory permanent effect",
    ifelse(
      summary_3b$Shape %in% c("Strong recovery (V-shape)",
                               "Full reversal (V-shape)") &
        grepl("RBC", summary_3b$Window),
      "Crisis-like transitory response",
      ifelse(
        summary_3b$Shape == "Partial recovery" &
          grepl("RBC", summary_3b$Window),
        "Regulatory medium-term effect",
        summary_3b$Shape
      )
    )
  )

  summary_3b <- summary_3b[order(summary_3b$Outcome,
                                  summary_3b$Window), ]

  cat("\n=== COMBINED SUMMARY: SHAPE + TIMING ===\n")
  print(summary_3b, row.names = FALSE)

  write.csv(summary_3b,
            file.path(TABLE_PATH, "3B_summary_shape_timing.csv"),
            row.names = FALSE)
  message("  Summary table saved.")

} else {
  message("  Skipping summary — shape or timing results are empty.")
}


# =============================================================================
# 14. FINAL OUTPUT SUMMARY
# =============================================================================

cat("\n=== 3B COMPLETE ===\n\n")

cat("Tables (output/tables/):\n")
for (t in c("3B_shape_analysis.csv",
            "3B_buffer_proximity.csv",
            "3B_timing_analysis.csv",
            "3B_summary_shape_timing.csv")) {
  exists_flag <- if (file.exists(file.path(TABLE_PATH, t))) "✓" else "missing"
  cat(sprintf("  %s %s\n", exists_flag, t))
}

cat("\nFigures (output/figures/):\n")
for (f in c("3B_panel_A_capital_lending.png",
            "3B_panel_B_portfolio_profitability.png",
            "3B_panel_C_credit_quality.png",
            "3B_buffer_proximity_plot.png")) {
  exists_flag <- if (file.exists(file.path(FIGURE_PATH, f))) "✓" else "missing"
  cat(sprintf("  %s %s\n", exists_flag, f))
}

cat("\nInterpretation:\n")
cat("  Persistence > 0.7 → step-change (regulatory permanent shift)\n")
cat("  Persistence < 0.3 → V-shape recovery (transitory crisis shock)\n")
cat("  Larger thin/thick buffer gap under RBC = threshold-specific pressure\n")

message("\n── 3B complete ✓ ────────────────────────────────────────────────────")
message("  Next: 4_Paper_Tables.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
