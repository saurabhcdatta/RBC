# =============================================================================
# 3B_2008_Crisis_EventStudy.R
# RBC Rule Analysis — 2008 Crisis vs. RBC Event Study Comparison
# NCUA Call Report (5300) Data
#
# Author  : Saurabh C. Datta, Ph.D.
# Created : 2026
#
# PURPOSE:
#   Characterises the DYNAMIC SHAPE of crisis vs. RBC treatment effects.
#   3A established WHETHER effects differ; 3B establishes HOW they differ —
#   specifically whether effects are transitory (V-shape) or permanent
#   (step-change), and how quickly and strongly they materialise.
#
# ORIGINAL AUDIT LOG (bugs fixed in first corrected version):
#   BUG 1: lm() masked by variable named 'lm' → renamed to late_mean
#   BUG 2: bind_rows() on list of NULLs → replaced with rbind loop
#   BUG 3: window column type inconsistency → forced as.character() everywhere
#   BUG 4: next inside tryCatch doesn't propagate → moved next outside
#   BUG 5: dplyr::case_when on scalar → replaced with plain if/else chain
#   BUG 6: geom_ribbon with duplicated aes() → cleaned to single mapping
#   BUG 7: map2/sapply on NULL list elements → explicit for loops with guards
#
# CHANGE LOG vs prior version:
#   [FIX 1] RBC_END updated 2024.4 → 2025.4 (matches 0_Data_Prep & 3A)
#   [FIX 2] Standalone growth vars scaled ×100 (matches 0_Data_Prep FIX 2)
#   [FIX 3] ES_OUTCOMES labels updated to "QoQ log×100" for growth vars
#   [FIX 4] buf_outcome_lbls updated for growth label consistency
#   [FIX 5] compact_panel labels updated to "QoQ log×100" for growth panels
#   [NEW]   Section 14: Four policy charts from 3B shape analysis:
#             (a) Shape classification summary heatmap
#             (b) Persistence ratio comparison (all outcomes)
#             (c) Peak timing comparison (crisis vs RBC)
#             (d) Thin vs thick buffer dose-response gap chart
#
# REQUIRES: df_crisis, df_rbc from 3A session.
#           Set STANDALONE <- TRUE to rebuild from raw data.
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(fixest)
library(patchwork)
library(scales)
library(broom)


# =============================================================================
# 1. SETTINGS
# =============================================================================

RAW_DATA_PATH <- "call_report.rds"
TABLE_PATH    <- "output/tables/"
FIGURE_PATH   <- "output/figures/"

STANDALONE    <- FALSE   # TRUE = rebuild panels from raw; FALSE = use 3A session

# Window settings — MUST match 3A exactly
CRISIS_DATE      <- 2008.3
CRISIS_START     <- 2004.1
CRISIS_END       <- 2013.4
CRISIS_PRE_START <- 2007.3
CRISIS_PRE_END   <- 2008.2

RBC_DATE         <- 2022.1
RBC_START        <- 2018.1
RBC_END          <- 2025.4    # [FIX 1]: was 2024.4 — updated to match 3A & 0_Data_Prep
RBC_PRE_START    <- 2021.1
RBC_PRE_END      <- 2021.4

SIZE_THRESHOLD   <- 500e6

EVENT_MIN        <- -12L
EVENT_MAX        <-  10L
EVENT_REF        <-  -1L

CONTROLS         <- "ln_assets + loan_to_asset"

# [FIX 3]: Canonical growth label suffix — must match 2_DiD & 3A
GROWTH_LABEL_SUFFIX <- "\u00d7100"

# Colors
COL_CRISIS    <- "#C94040"
COL_RBC       <- "#1B3A6B"
COL_CI_CRISIS <- "#E8A0A0"
COL_CI_RBC    <- "#8AAAD0"
COL_NEUTRAL   <- "#E8A838"

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
      mbl_shr        = if_else(lns_tot > 0, lns_mbl_use / lns_tot * 100, NA_real_),
      re_shr         = if_else(lns_tot > 0, lns_re   / lns_tot * 100, NA_real_),
      auto_shr       = if_else(lns_tot > 0, lns_auto / lns_tot * 100, NA_real_),
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
      # [FIX 2]: ×100 scaling matches 0_Data_Prep.R FIX 2
      asset_growth = (ln_assets - lag(ln_assets)) * 100,
      loan_growth  = (ln_loans  - lag(ln_loans))  * 100
    ) |>
    ungroup()

  # Crisis panel
  crisis_tx <- df_clean |>
    filter(q_period_num >= CRISIS_PRE_START,
           q_period_num <= CRISIS_PRE_END) |>
    group_by(cu_number) |>
    summarise(avg_pre = mean(assets_tot, na.rm = TRUE), .groups = "drop") |>
    mutate(complex_crisis = as.integer(avg_pre >= SIZE_THRESHOLD))

  df_crisis <- df_clean |>
    filter(q_period_num >= CRISIS_START, q_period_num <= CRISIS_END) |>
    inner_join(crisis_tx |> select(cu_number, complex_crisis), by = "cu_number") |>
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

  # RBC panel — [FIX 1]: uses RBC_END = 2025.4
  rbc_tx <- df_clean |>
    filter(q_period_num >= RBC_PRE_START, q_period_num <= RBC_PRE_END) |>
    group_by(cu_number) |>
    summarise(avg_pre = mean(assets_tot, na.rm = TRUE), .groups = "drop") |>
    mutate(complex_rbc = as.integer(avg_pre >= SIZE_THRESHOLD))

  df_rbc <- df_clean |>
    filter(q_period_num >= RBC_START, q_period_num <= RBC_END) |>
    inner_join(rbc_tx |> select(cu_number, complex_rbc), by = "cu_number") |>
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

  message("  Standalone panels rebuilt. RBC window: ", RBC_START, " to ", RBC_END)

} else {
  message("── Using df_crisis / df_rbc from 3A session ─────────────────────")
  stopifnot(exists("df_crisis"), exists("df_rbc"))
  message(sprintf("  df_crisis: %s obs | df_rbc: %s obs",
                  scales::comma(nrow(df_crisis)),
                  scales::comma(nrow(df_rbc))))
}


# =============================================================================
# 3. OUTCOME DEFINITIONS
# =============================================================================
# [FIX 3]: Growth variable labels updated to "QoQ log×100"

ES_OUTCOMES <- list(
  list(var = "networth_ratio", label = "Net Worth Ratio (%)",                             np = TRUE),
  list(var = "cap_buffer",     label = "Capital Buffer (pp)",                             np = TRUE),
  list(var = "loan_growth",    label = paste0("Loan Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"),  np = FALSE),
  list(var = "asset_growth",   label = paste0("Asset Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"), np = FALSE),
  list(var = "dq_rate_var",    label = "Delinquency Rate (%)",                            np = FALSE),
  list(var = "chgoff_ratio",   label = "Charge-Off Ratio (%)",                            np = FALSE),
  list(var = "roa_var",        label = "ROA (%)",                                         np = FALSE),
  list(var = "nim",            label = "NIM (%)",                                         np = FALSE),
  list(var = "re_shr",         label = "RE Share (%)",                                    np = FALSE),
  list(var = "auto_shr",       label = "Auto Loan Share (%)",                             np = FALSE),
  list(var = "mbl_shr",        label = "MBL Share (%)",                                   np = TRUE)
)

N_OUT <- length(ES_OUTCOMES)

# Index reference (used in compact_panel calls below):
# 1=networth_ratio  2=cap_buffer    3=loan_growth   4=asset_growth
# 5=dq_rate_var     6=chgoff_ratio  7=roa_var       8=nim
# 9=re_shr         10=auto_shr     11=mbl_shr


# =============================================================================
# 4. EVENT STUDY MODEL FUNCTION
# =============================================================================

run_es_model <- function(outcome_var, panel_df, complex_col) {
  panel_sub <- panel_df[
    !is.na(panel_df$event_time) &
      panel_df$event_time >= EVENT_MIN &
      panel_df$event_time <= EVENT_MAX, ]

  fml_str <- paste0(
    outcome_var,
    " ~ i(event_time, ", complex_col, ", ref = ", EVENT_REF, ") + ",
    CONTROLS, " | cu_number + q_period_num"
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
# Returns a plain data.frame with character window column.
# Builds incrementally with rbind() — no bind_rows on potentially-NULL inputs.

extract_es_coefs <- function(model_obj, win_str) {
  if (is.null(model_obj)) return(NULL)

  raw <- tryCatch(tidy(model_obj, conf.int = TRUE), error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(NULL)

  raw <- raw[grepl("event_time::", raw$term, fixed = TRUE), ]
  if (nrow(raw) == 0) return(NULL)

  raw$event_time <- as.integer(
    regmatches(raw$term, regexpr("-?[0-9]+", raw$term))
  )
  raw <- raw[raw$event_time >= EVENT_MIN & raw$event_time <= EVENT_MAX, ]
  if (nrow(raw) == 0) return(NULL)

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

  # Reference row
  ref_row <- data.frame(
    event_time = as.integer(EVENT_REF),
    estimate   = 0, std_error = 0, p_value = NA_real_,
    conf_low   = 0, conf_high = 0,
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

crisis_models <- vector("list", N_OUT)
rbc_models    <- vector("list", N_OUT)

for (i in seq_len(N_OUT)) {
  vname <- ES_OUTCOMES[[i]]$var
  message("  ", vname, "...")
  crisis_models[[i]] <- run_es_model(vname, df_crisis, "complex_crisis")
  rbc_models[[i]]    <- run_es_model(vname, df_rbc,    "complex_rbc")
}

# Build es_combined list
es_combined <- vector("list", N_OUT)

for (i in seq_len(N_OUT)) {
  parts   <- NULL
  c_coefs <- extract_es_coefs(crisis_models[[i]], "Crisis (2008q3)")
  r_coefs <- extract_es_coefs(rbc_models[[i]],    "RBC Rule (2022q1)")

  if (!is.null(c_coefs)) parts <- c_coefs
  if (!is.null(r_coefs)) {
    if (is.null(parts)) parts <- r_coefs else parts <- rbind(parts, r_coefs)
  }

  if (!is.null(parts)) {
    parts$window   <- as.character(parts$window)
    es_combined[[i]] <- parts
  }
}

n_good <- sum(!sapply(es_combined, is.null))
message(sprintf("  Event studies complete. %d/%d outcomes have data.", n_good, N_OUT))

# Spot-check first good element
first_idx <- which(!sapply(es_combined, is.null))[1]
if (!is.na(first_idx)) {
  message("  First outcome with data: ", ES_OUTCOMES[[first_idx]]$label)
  message("  Windows: ",
          paste(unique(es_combined[[first_idx]]$window), collapse = " | "))
}


# =============================================================================
# 7. SHAPE ANALYSIS — V-SHAPE vs. STEP-CHANGE
# =============================================================================
# Persistence ratio = late_post_mean / early_post_mean
#   > 0.7 → Persistent (step-change) — regulatory permanent effect
#   < 0.3 → Strong recovery (V-shape) — transitory crisis shock

message("── Step 2: Shape analysis ────────────────────────────────────────────")

shape_results <- data.frame(
  Outcome     = character(0), Window = character(0),
  Pre_mean    = numeric(0),   Early_post  = numeric(0),
  Late_post   = numeric(0),   Persistence = numeric(0),
  Post_slope  = numeric(0),   Shape       = character(0),
  stringsAsFactors = FALSE
)

for (i in seq_len(N_OUT)) {
  lbl   <- ES_OUTCOMES[[i]]$label
  coefs <- es_combined[[i]]

  if (is.null(coefs) || !is.data.frame(coefs) || nrow(coefs) == 0) {
    message("  Skipping ", lbl, " — no coefficient data"); next
  }
  coefs$window <- as.character(coefs$window)

  for (win in c("Crisis (2008q3)", "RBC Rule (2022q1)")) {
    d <- coefs[coefs$window == win & !is.na(coefs$p_value), ]
    if (nrow(d) == 0) {
      message("  Skipping ", lbl, " / ", win, " — no rows after filter"); next
    }

    d_early <- d[d$event_time >= 1L  & d$event_time <= 4L,  ]
    d_late  <- d[d$event_time >= 5L  & d$event_time <= EVENT_MAX, ]
    d_pre   <- d[d$event_time >= EVENT_MIN & d$event_time <= -2L, ]
    d_post  <- d[d$event_time >= 0L, ]

    if (nrow(d_early) == 0 || nrow(d_late) == 0) {
      message("  Skipping ", lbl, " / ", win,
              " — insufficient early/late data (", nrow(d_early), "/", nrow(d_late), ")"); next
    }

    early_mean <- mean(d_early$estimate, na.rm = TRUE)
    late_mean  <- mean(d_late$estimate,  na.rm = TRUE)
    pre_mean   <- if (nrow(d_pre) > 0) mean(d_pre$estimate, na.rm = TRUE) else NA_real_

    persistence <- if (!is.na(early_mean) && abs(early_mean) > 0.001)
      late_mean / early_mean else NA_real_

    # Post-period slope (stats::lm — explicit namespace avoids masking)
    post_slope <- tryCatch({
      fit <- stats::lm(estimate ~ event_time, data = d_post)
      as.numeric(coef(fit)[["event_time"]])
    }, error = function(e) NA_real_)

    # Shape classification — plain if/else, no dplyr::case_when on scalar
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

    shape_results <- rbind(shape_results, data.frame(
      Outcome     = lbl,
      Window      = win,
      Pre_mean    = round(pre_mean,    3),
      Early_post  = round(early_mean,  3),
      Late_post   = round(late_mean,   3),
      Persistence = round(persistence, 2),
      Post_slope  = round(post_slope,  4),
      Shape       = shape_label,
      stringsAsFactors = FALSE
    ))

    message(sprintf("  %-42s | %-22s | Persist=%-5s | %s",
                    lbl, win,
                    ifelse(is.na(persistence), "NA",
                           as.character(round(persistence, 2))),
                    shape_label))
  }
}

cat("\n=== SHAPE ANALYSIS: V-SHAPE vs. STEP-CHANGE ===\n")
cat("Persistence > 0.7 = step-change (regulatory permanent)\n")
cat("Persistence < 0.3 = V-shape (transitory crisis shock)\n\n")
print(shape_results[order(shape_results$Outcome, shape_results$Window), ],
      row.names = FALSE)

write.csv(shape_results,
          file.path(TABLE_PATH, "3B_shape_analysis.csv"),
          row.names = FALSE)
message("Saved: 3B_shape_analysis.csv")


# =============================================================================
# 8. EVENT STUDY OVERLAY PLOT FUNCTION
# =============================================================================
# CI ribbons use separate geom_ribbon() calls with explicit data= argument.
# No inherit.aes conflicts. No duplicate aes() mappings.

plot_es_overlay <- function(coef_df, outcome_label, non_parallel = FALSE) {

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

  df_c   <- coef_df[coef_df$window == "Crisis (2008q3)", ]
  df_r   <- coef_df[coef_df$window == "RBC Rule (2022q1)", ]
  df_sig <- coef_df[!is.na(coef_df$p_value) &
                      coef_df$p_value < 0.05 &
                      coef_df$event_time >= 0L, ]

  p <- ggplot(coef_df,
              aes(x = event_time, y = estimate,
                  color = window, shape = window)) +
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf, fill = "gray93", alpha = 0.5) +
    geom_ribbon(
      data = df_c,
      mapping = aes(x = event_time, ymin = conf_low, ymax = conf_high),
      fill = COL_CI_CRISIS, alpha = 0.3, color = NA, inherit.aes = FALSE
    ) +
    geom_ribbon(
      data = df_r,
      mapping = aes(x = event_time, ymin = conf_low, ymax = conf_high),
      fill = COL_CI_RBC, alpha = 0.3, color = NA, inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, color = "gray50",
               linewidth = 0.5, linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.6, linetype = "dashed") +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.2) +
    geom_point(
      data = df_sig,
      mapping = aes(x = event_time, y = estimate),
      shape = 8, size = 2.8, color = "darkred",
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    scale_color_manual(values = c("Crisis (2008q3)"   = COL_CRISIS,
                                  "RBC Rule (2022q1)" = COL_RBC)) +
    scale_shape_manual(values = c("Crisis (2008q3)"   = 17L,
                                  "RBC Rule (2022q1)" = 16L)) +
    scale_x_continuous(
      breaks = seq(EVENT_MIN, EVENT_MAX, by = 4L),
      labels = function(x) paste0("Q", x)
    ) +
    labs(
      title    = outcome_label,
      subtitle = sub_txt,
      x        = "Quarters Relative to Shock",
      y        = "DiD Coefficient",
      color    = NULL, shape = NULL,
      caption  = paste0(
        "\u2605 = p<0.05 (post-period). Two-way FE (CU + quarter-year). ",
        "SE clustered at CU. Red \u25b2 = Crisis 2008q3, Navy \u25cf = RBC 2022q1."
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
    message("  Skipping plot: ", o$var, " — no data"); next
  }

  p <- tryCatch(
    plot_es_overlay(coefs, o$label, o$np),
    error = function(e) {
      message("  Plot error (", o$var, "): ", e$message); NULL
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

# Panel A: Capital & Lending
# [FIX 5]: "C. Loan Growth" label updated to include ×100 suffix
pA1 <- compact_panel(1, "A. Net Worth Ratio (%)")
pA2 <- compact_panel(2, "B. Capital Buffer (pp)")
pA3 <- compact_panel(3, paste0("C. Loan Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"))
pA4 <- compact_panel(5, "D. Delinquency Rate (%)")

panel_A <- (pA1 + pA2) / (pA3 + pA4) +
  plot_annotation(
    title    = "Dynamic Responses: 2008 Crisis vs. 2022 RBC \u2014 Capital & Lending",
    subtitle = paste0("Red triangles = crisis (2008 Q3). ",
                      "Navy circles = RBC (2022 Q1). \u2605 = p<0.05."),
    caption  = paste0("Two-way FE (CU + quarter-year). SE clustered at CU. ",
                      "Source: NCUA Call Report (5300).")
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
    title    = "Dynamic Responses: 2008 Crisis vs. 2022 RBC \u2014 Portfolio & Profitability",
    subtitle = "Red triangles = crisis. Navy circles = RBC. \u2605 = p<0.05.",
    caption  = paste0("Two-way FE (CU + quarter-year). SE clustered at CU. ",
                      "Source: NCUA Call Report (5300).")
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_B_portfolio_profitability.png"),
       panel_B, width = 14, height = 10, dpi = 300)
message("  Panel B saved.")

# Panel C: Credit Quality & Growth
# [FIX 5]: "C. Asset Growth" updated to include ×100 suffix
pC1 <- compact_panel(6, "A. Charge-Off Ratio (%)")
pC2 <- compact_panel(8, "B. Net Interest Margin (%)")
pC3 <- compact_panel(4, paste0("C. Asset Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"))
pC4 <- compact_panel(7, "D. ROA (%)")

panel_C <- (pC1 + pC2) / (pC3 + pC4) +
  plot_annotation(
    title    = "Dynamic Responses: 2008 Crisis vs. 2022 RBC \u2014 Credit Quality & Growth",
    subtitle = "Red triangles = crisis. Navy circles = RBC. \u2605 = p<0.05.",
    caption  = paste0("Two-way FE (CU + quarter-year). SE clustered at CU. ",
                      "Source: NCUA Call Report (5300).")
  )

ggsave(file.path(FIGURE_PATH, "3B_panel_C_credit_quality.png"),
       panel_C, width = 14, height = 10, dpi = 300)
message("  Panel C saved.")


# =============================================================================
# 11. BUFFER PROXIMITY ANALYSIS
# =============================================================================
# Dose-response by pre-shock capital buffer proximity.
# If gap between thin/thick is larger under RBC → threshold-specific pressure.

message("── Step 5: Buffer proximity analysis ────────────────────────────────")

classify_buffers <- function(panel_df, pre_start, pre_end, complex_col) {
  sub <- panel_df[
    panel_df$q_period_num >= pre_start &
      panel_df$q_period_num <= pre_end &
      panel_df[[complex_col]] == 1L, ]

  if (nrow(sub) == 0) return(data.frame(cu_number = integer(0),
                                         buf_group = character(0)))

  agg        <- aggregate(networth_ratio ~ cu_number, data = sub,
                          FUN = function(x) mean(x, na.rm = TRUE))
  names(agg)[2] <- "pre_nw"
  agg$buf_group <- ifelse(agg$pre_nw < 11,  "Thin (<11%)",
                   ifelse(agg$pre_nw >= 12, "Thick (>=12%)", "Mid (11-12%)"))
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

run_buf_did <- function(outcome_var, data_df) {
  fml_str <- paste0(outcome_var, " ~ treat_post + ", CONTROLS,
                    " | cu_number + q_period_num")
  tryCatch(
    feols(as.formula(fml_str), data = data_df,
          cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
}

extract_buf_coef <- function(model_obj, out_lbl, win_lbl, grp_lbl) {
  if (is.null(model_obj)) return(NULL)
  tryCatch({
    td  <- tidy(model_obj, conf.int = TRUE)
    row <- td[td$term == "treat_post", ]
    if (nrow(row) == 0) return(NULL)
    stars <- if (row$p.value < 0.01) "***" else
             if (row$p.value < 0.05) "**"  else
             if (row$p.value < 0.10) "*"   else ""
    data.frame(
      Outcome      = out_lbl, Window = win_lbl, Buffer_group = grp_lbl,
      Beta    = round(row$estimate,  3), SE    = round(row$std.error, 3),
      CI_low  = round(row$conf.low,  3), CI_high = round(row$conf.high, 3),
      p_value = round(row$p.value,   3), Stars = stars,
      N       = as.integer(nobs(model_obj)),
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
}

# [FIX 4]: buf_outcome_lbls updated for growth label consistency
buf_outcome_vars <- c("loan_growth", "networth_ratio",
                      "dq_rate_var", "auto_shr", "re_shr")
buf_outcome_lbls <- c(paste0("Loan Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"),
                      "Net Worth Ratio",
                      "DQ Rate", "Auto Share", "RE Share")

buffer_results <- data.frame(
  Outcome = character(0), Window = character(0), Buffer_group = character(0),
  Beta = numeric(0), SE = numeric(0), CI_low = numeric(0),
  CI_high = numeric(0), p_value = numeric(0), Stars = character(0),
  N = integer(0), stringsAsFactors = FALSE
)

for (grp in c("Thin (<11%)", "Thick (>=12%)")) {
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
                                          buffer_results$Buffer_group, sep = ".")
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
    geom_point(size = 3, position = position_dodge(width = 0.7)) +
    scale_color_manual(values = col_vals) +
    scale_shape_manual(values = c("Thin (<11%)" = 17L, "Thick (>=12%)" = 16L)) +
    facet_wrap(~ Window, ncol = 2) +
    labs(
      title    = "Dose-Response by Capital Buffer: Crisis vs. RBC",
      subtitle = "Thin = net worth ratio < 11% pre-shock. Thick = \u2265 12%. 95% CI.",
      x = "DiD Coefficient (Complex \u00d7 Post-Shock)",
      y = NULL, color = NULL, shape = NULL,
      caption = "Two-way FE. SE clustered at CU. Source: NCUA Call Report (5300)."
    ) +
    theme_rbc() + theme(legend.position = "bottom")

  ggsave(file.path(FIGURE_PATH, "3B_buffer_proximity_plot.png"),
         p_buf, width = 12, height = 8, dpi = 300)
  message("  Buffer proximity plot saved.")
}


# =============================================================================
# 12. TIMING ANALYSIS
# =============================================================================

message("── Step 6: Timing analysis ───────────────────────────────────────────")

timing_results <- data.frame(
  Outcome        = character(0), Window = character(0),
  Peak_quarter   = integer(0),   Peak_coef = numeric(0),
  Peak_sig       = logical(0),   N_sig_quarters = integer(0),
  First_sig_q    = integer(0),   stringsAsFactors = FALSE
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

    pk    <- which.max(abs(d$estimate))
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
print(timing_results[order(timing_results$Outcome, timing_results$Window), ],
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
    by = c("Outcome", "Window"), all.x = TRUE
  )

  summary_3b$Mechanism <- ifelse(
    summary_3b$Shape == "Persistent (step-change)" & grepl("RBC", summary_3b$Window),
    "Regulatory permanent effect",
    ifelse(
      summary_3b$Shape %in% c("Strong recovery (V-shape)", "Full reversal (V-shape)") &
        grepl("RBC", summary_3b$Window),
      "Crisis-like transitory response",
      ifelse(
        summary_3b$Shape == "Partial recovery" & grepl("RBC", summary_3b$Window),
        "Regulatory medium-term effect",
        summary_3b$Shape
      )
    )
  )

  summary_3b <- summary_3b[order(summary_3b$Outcome, summary_3b$Window), ]
  cat("\n=== COMBINED SUMMARY: SHAPE + TIMING ===\n")
  print(summary_3b, row.names = FALSE)

  write.csv(summary_3b,
            file.path(TABLE_PATH, "3B_summary_shape_timing.csv"),
            row.names = FALSE)
  message("  Summary table saved.")
}


# =============================================================================
# 14. POLICY CHARTS
# =============================================================================
# Four policy charts derived from 3B shape analysis results.
# All use data computed above — no additional regressions required.
# =============================================================================

message("── Step 8: Policy charts ─────────────────────────────────────────────")


# ── Policy Chart 3B-1: Shape Classification Summary Heatmap ──────────────────
# Tile grid: outcomes × window, coloured by shape classification

message("  Policy Chart 3B-1: Shape classification heatmap...")

if (nrow(shape_results) > 0) {

  shape_plot_data <- shape_results |>
    mutate(
      Shape_group = case_when(
        grepl("step-change", Shape)      ~ "Permanent\n(Step-Change)",
        grepl("V-shape|reversal", Shape) ~ "Transitory\n(V-Shape)",
        Shape == "Partial recovery"      ~ "Partial\nRecovery",
        TRUE                             ~ "Undetermined"
      ),
      Shape_group = factor(Shape_group,
                           levels = c("Transitory\n(V-Shape)",
                                      "Partial\nRecovery",
                                      "Permanent\n(Step-Change)",
                                      "Undetermined")),
      Window_short = if_else(grepl("Crisis", Window),
                             "2008 Crisis", "2022 RBC Rule"),
      # Wrap long outcome labels
      Outcome_wrap = str_wrap(Outcome, width = 22)
    )

  p_shape_heat <- ggplot(shape_plot_data,
                          aes(x = Window_short, y = Outcome_wrap,
                              fill = Shape_group)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(
      aes(label = paste0(ifelse(is.na(Persistence), "?",
                                as.character(Persistence)),
                         "\n", Shape_group)),
      size = 2.7, color = "white", fontface = "bold", lineheight = 1.1
    ) +
    scale_fill_manual(values = c(
      "Transitory\n(V-Shape)"    = COL_CRISIS,
      "Partial\nRecovery"        = "#D4884A",
      "Permanent\n(Step-Change)" = COL_RBC,
      "Undetermined"             = "gray70"
    )) +
    labs(
      title    = "Policy Chart 3B-1. Dynamic Response Shape: Crisis vs. RBC Rule",
      subtitle = paste0(
        "Persistence ratio = late post-period mean / early post-period mean. ",
        ">0.7 = step-change; <0.3 = V-shape."
      ),
      x     = NULL,
      y     = NULL,
      fill  = "Dynamic Shape",
      caption = paste0(
        "Persistence = (mean coef Q+5 to Q+10) / (mean coef Q+1 to Q+4). ",
        "Two-way FE event studies. Source: NCUA Call Report (5300)."
      )
    ) +
    theme_rbc() +
    theme(legend.position = "right",
          axis.text.y     = element_text(size = 9),
          axis.text.x     = element_text(size = 11, face = "bold"))

  ggsave(file.path(FIGURE_PATH, "policy_3b1_shape_classification_heatmap.png"),
         p_shape_heat, width = 11, height = 9, dpi = 300)
  message("    Saved: policy_3b1_shape_classification_heatmap.png")
}


# ── Policy Chart 3B-2: Persistence Ratio Comparison ─────────────────────────
# Bar chart comparing persistence ratios across outcomes and windows

message("  Policy Chart 3B-2: Persistence ratio comparison...")

if (nrow(shape_results) > 0) {
  persist_data <- shape_results |>
    filter(!is.na(Persistence)) |>
    mutate(
      Window_short = if_else(grepl("Crisis", Window),
                             "2008 Crisis", "2022 RBC Rule"),
      Outcome_wrap = str_wrap(Outcome, width = 20),
      # Flag regulatory-specific: RBC persistent but crisis not
      Reg_specific = Persistence > 0.7 & grepl("RBC", Window)
    )

  p_persist <- ggplot(persist_data,
                       aes(x = Persistence, y = Outcome_wrap,
                           fill = Window_short, alpha = Reg_specific)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_vline(xintercept = 0.3, linetype = "dashed",
               color = "gray50", linewidth = 0.7) +
    geom_vline(xintercept = 0.7, linetype = "dashed",
               color = COL_RBC, linewidth = 0.7) +
    annotate("text", x = 0.15, y = 0.4, label = "V-shape\nzone",
             color = "gray50", size = 3, hjust = 0.5) +
    annotate("text", x = 0.85, y = 0.4, label = "Step-change\nzone",
             color = COL_RBC, size = 3, hjust = 0.5) +
    scale_fill_manual(values = c("2008 Crisis"   = COL_CRISIS,
                                 "2022 RBC Rule" = COL_RBC)) +
    scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.65),
                       guide = "none") +
    scale_x_continuous(limits = c(NA, 1.5),
                       labels = function(x) paste0(x, "x")) +
    labs(
      title    = "Policy Chart 3B-2. Persistence Ratios: How Lasting Are the Effects?",
      subtitle = paste0(
        "Persistence = (Q+5 to Q+10 mean) / (Q+1 to Q+4 mean). ",
        "Opaque bars = RBC step-changes (persistence > 0.7)."
      ),
      x     = "Persistence Ratio (late / early post-period coefficients)",
      y     = NULL,
      fill  = NULL,
      caption = paste0(
        "Dashed grey line = 0.3 (V-shape threshold). ",
        "Dashed navy line = 0.7 (step-change threshold). ",
        "Source: NCUA Call Report (5300)."
      )
    ) +
    theme_rbc() + theme(legend.position = "top")

  ggsave(file.path(FIGURE_PATH, "policy_3b2_persistence_ratio_comparison.png"),
         p_persist, width = 12, height = 8, dpi = 300)
  message("    Saved: policy_3b2_persistence_ratio_comparison.png")
}


# ── Policy Chart 3B-3: Peak Timing Comparison ────────────────────────────────
# When do effects peak under the crisis vs RBC? Later peaks under RBC
# suggest a slow-building, structural regulatory mechanism.

message("  Policy Chart 3B-3: Peak timing comparison...")

if (nrow(timing_results) > 0) {
  timing_plot <- timing_results |>
    filter(!is.na(Peak_quarter)) |>
    mutate(
      Window_short = if_else(grepl("Crisis", Window),
                             "2008 Crisis", "2022 RBC Rule"),
      Outcome_wrap = str_wrap(Outcome, width = 22),
      Sig_label    = if_else(Peak_sig, paste0("Q+", Peak_quarter, "*"),
                             paste0("Q+", Peak_quarter))
    )

  p_timing <- ggplot(timing_plot,
                      aes(x = Peak_quarter, y = Outcome_wrap,
                          color = Window_short, shape = Window_short)) +
    geom_vline(xintercept = c(2, 6), linetype = "dashed",
               color = "gray70", linewidth = 0.5) +
    geom_point(size = 4, alpha = 0.85) +
    geom_text(aes(label = Sig_label), hjust = -0.25, size = 2.8,
              show.legend = FALSE) +
    annotate("text", x = 2, y = 0.3,
             label = "Typical\ncrisis peak",
             color = COL_CRISIS, size = 2.8, vjust = 0, hjust = 0.5) +
    annotate("text", x = 6, y = 0.3,
             label = "Typical\nRBC peak",
             color = COL_RBC, size = 2.8, vjust = 0, hjust = 0.5) +
    scale_color_manual(values = c("2008 Crisis"   = COL_CRISIS,
                                  "2022 RBC Rule" = COL_RBC)) +
    scale_shape_manual(values = c("2008 Crisis"   = 17,
                                  "2022 RBC Rule" = 16)) +
    scale_x_continuous(
      breaks = seq(0, EVENT_MAX, 2),
      labels = function(x) paste0("Q+", x),
      limits = c(-0.5, EVENT_MAX + 1.5)
    ) +
    labs(
      title    = "Policy Chart 3B-3. When Do Effects Peak? Crisis vs. RBC Timing",
      subtitle = paste0(
        "Peak quarter = post-treatment quarter with largest absolute DiD coefficient. ",
        "* = peak is statistically significant (p<0.05)."
      ),
      x     = "Quarter of Peak Effect (relative to treatment date)",
      y     = NULL,
      color = NULL, shape = NULL,
      caption = paste0(
        "Later peaks under RBC suggest slow-building structural mechanisms ",
        "rather than acute crisis responses. Source: NCUA Call Report (5300)."
      )
    ) +
    theme_rbc() + theme(legend.position = "top")

  ggsave(file.path(FIGURE_PATH, "policy_3b3_peak_timing_comparison.png"),
         p_timing, width = 12, height = 8, dpi = 300)
  message("    Saved: policy_3b3_peak_timing_comparison.png")
}


# ── Policy Chart 3B-4: Thin vs. Thick Buffer Dose-Response Gap ───────────────
# Shows the additional "regulatory penalty" borne by thin-buffer CUs.
# A larger thin/thick gap under RBC than crisis confirms the threshold matters.

message("  Policy Chart 3B-4: Buffer dose-response gap...")

if (nrow(buffer_results) > 0) {
  # Compute gap: Thin beta minus Thick beta (positive = thin more affected)
  buf_wide <- buffer_results |>
    filter(Buffer_group %in% c("Thin (<11%)", "Thick (>=12%)")) |>
    select(Outcome, Window, Buffer_group, Beta) |>
    pivot_wider(names_from = Buffer_group,
                values_from = Beta,
                names_prefix = "Beta_") |>
    rename(Beta_Thin  = `Beta_Thin (<11%)`,
           Beta_Thick = `Beta_Thick (>=12%)`) |>
    mutate(
      Gap          = Beta_Thin - Beta_Thick,
      Window_short = if_else(grepl("Crisis", Window),
                             "2008 Crisis", "2022 RBC Rule"),
      Outcome_wrap = str_wrap(Outcome, width = 20)
    )

  p_gap <- ggplot(buf_wide |> filter(!is.na(Gap)),
                   aes(x = Gap, y = Outcome_wrap,
                       fill = Window_short,
                       color = Window_short)) +
    geom_col(position = position_dodge(width = 0.7),
             width = 0.6, alpha = 0.85) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    scale_fill_manual(values  = c("2008 Crisis"   = COL_CRISIS,
                                  "2022 RBC Rule" = COL_RBC)) +
    scale_color_manual(values = c("2008 Crisis"   = COL_CRISIS,
                                  "2022 RBC Rule" = COL_RBC)) +
    labs(
      title    = "Policy Chart 3B-4. Buffer Dose-Response Gap: Thin vs. Thick Capital",
      subtitle = paste0(
        "Gap = DiD coefficient (thin-buffer CUs) minus DiD coefficient (thick-buffer CUs). ",
        "Positive = thin-buffer CUs bear a larger regulatory burden."
      ),
      x     = "Thin\u2212Thick Buffer DiD Gap",
      y     = NULL,
      fill  = NULL, color = NULL,
      caption = paste0(
        "Thin = pre-shock net worth ratio < 11%. Thick = \u2265 12%. ",
        "A larger positive gap under RBC than crisis confirms the 10% threshold ",
        "is creating disproportionate pressure on near-threshold CUs. ",
        "Source: NCUA Call Report (5300)."
      )
    ) +
    theme_rbc() + theme(legend.position = "top")

  ggsave(file.path(FIGURE_PATH, "policy_3b4_buffer_gap_chart.png"),
         p_gap, width = 11, height = 7, dpi = 300)
  message("    Saved: policy_3b4_buffer_gap_chart.png")
}

message("  All policy charts complete.")


# =============================================================================
# 15. FINAL OUTPUT SUMMARY
# =============================================================================

cat("\n=== 3B COMPLETE ===\n\n")

cat("Tables (output/tables/):\n")
for (t in c("3B_shape_analysis.csv", "3B_buffer_proximity.csv",
            "3B_timing_analysis.csv", "3B_summary_shape_timing.csv")) {
  flag <- if (file.exists(file.path(TABLE_PATH, t))) "\u2713" else "MISSING"
  cat(sprintf("  %s %s\n", flag, t))
}

cat("\nMain figures (output/figures/):\n")
for (f in c("3B_panel_A_capital_lending.png",
            "3B_panel_B_portfolio_profitability.png",
            "3B_panel_C_credit_quality.png",
            "3B_buffer_proximity_plot.png")) {
  flag <- if (file.exists(file.path(FIGURE_PATH, f))) "\u2713" else "MISSING"
  cat(sprintf("  %s %s\n", flag, f))
}

cat("\nPolicy charts (output/figures/):\n")
for (f in c("policy_3b1_shape_classification_heatmap.png",
            "policy_3b2_persistence_ratio_comparison.png",
            "policy_3b3_peak_timing_comparison.png",
            "policy_3b4_buffer_gap_chart.png")) {
  flag <- if (file.exists(file.path(FIGURE_PATH, f))) "\u2713" else "MISSING"
  cat(sprintf("  %s %s\n", flag, f))
}

cat("\nFIX SUMMARY:\n")
cat("  [FIX 1] RBC_END: 2024.4 \u2192 2025.4\n")
cat("  [FIX 2] Standalone growth vars: \u00d7100 scaling applied\n")
cat("  [FIX 3] ES_OUTCOMES labels: 'QoQ log\u00d7100'\n")
cat("  [FIX 4] buf_outcome_lbls: loan_growth label updated\n")
cat("  [FIX 5] compact_panel titles: growth labels updated\n")

cat("\nInterpretation:\n")
cat("  Persistence > 0.7 \u2192 step-change (regulatory permanent shift)\n")
cat("  Persistence < 0.3 \u2192 V-shape recovery (transitory crisis shock)\n")
cat("  Later peak quarter under RBC \u2192 slow-building structural mechanism\n")
cat("  Larger thin/thick gap under RBC \u2192 threshold pressure specific to RBC\n")

message("\n── 3B complete \u2713 ────────────────────────────────────────────────────")
message("  Next: 3C_CCULR_Adoption.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
