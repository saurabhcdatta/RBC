# =============================================================================
# Build_RQ3_Combined.R
# Builds a clean unified 4-panel event study figure matching your existing
# figure style exactly — by sourcing the theme + plot function from your
# existing 2_DiD_Estimation_v2.R script and reusing saved model results.
#
# Run from: S:/Projects/RBC_2026/Data/
# Output:   output/paper/figures/Figure_RQ3_Combined.png
# =============================================================================

library(tidyverse)
library(fixest)
library(haven)
library(patchwork)

FIGURE_PATH <- "output/paper/figures"

# ── Replicate your existing theme and plot function exactly ───────────────────
# (Copied from 2_DiD_Estimation_v2.R so we don't have to source the full script)

theme_rbc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major.x  = element_blank(),
      panel.grid.major.y  = element_line(color = "#e8e0d4", linewidth = 0.4),
      plot.title          = element_text(face = "bold", size = 13, color = "#1a2744"),
      plot.subtitle       = element_text(size = 10, color = "#5a5a5a", lineheight = 1.3),
      plot.caption        = element_text(size = 8.5, color = "#888888", lineheight = 1.3),
      axis.title          = element_text(size = 10, color = "#444444"),
      axis.text           = element_text(size = 9, color = "#555555"),
      legend.position     = "bottom",
      legend.text         = element_text(size = 9),
      strip.text          = element_text(face = "bold"),
      plot.background     = element_rect(fill = "white", color = NA)
    )
}

plot_event_study <- function(es_df, ylab, add_trend_note = FALSE) {
  # es_df must have: quarter (numeric), estimate, conf_low, conf_high,
  #                  post (logical: TRUE = Post-RBC)
  p <- ggplot(es_df, aes(x = quarter, y = estimate)) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "#C8972A", linewidth = 1.0) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "#AAAAAA", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
                fill = "#1a2744", alpha = 0.15) +
    geom_line(color = "#1a2744", linewidth = 1.3) +
    geom_point(aes(shape = if_else(post, "Post-RBC", "Pre-RBC"),
                   color = if_else(post, "Post-RBC", "Pre-RBC")),
               size = 3, stroke = 1.1, fill = "white") +
    scale_shape_manual(values = c("Post-RBC" = 8, "Pre-RBC" = 1), name = NULL) +
    scale_color_manual(values = c("Post-RBC" = "#1a2744", "Pre-RBC" = "#1a2744"),
                       name = NULL) +
    scale_x_continuous(breaks = seq(-12, 8, by = 4)) +
    labs(y = ylab,
         x = "Quarters Relative to RBC Effective Date (Q0 = 2022 Q1)") +
    theme_rbc()

  if (add_trend_note) {
    p <- p + labs(subtitle = "Note: Non-parallel pre-trends — interpret with caution")
  }
  p
}

# ── Load panel ────────────────────────────────────────────────────────────────
message("Loading analysis panel...")
panel <- readRDS("analysis_panel_raw.rds") |>
  haven::zap_labels() |>
  mutate(across(where(is.numeric), as.numeric))

# Print columns for reference
spread_cols <- names(panel)[str_detect(names(panel), "spread")]
message("Spread columns: ", paste(spread_cols, collapse = ", "))
message("Other relevant: ",
        paste(names(panel)[str_detect(names(panel), "loan_gr|complex|cu_num")],
              collapse = ", "))

# ── Set column names (edit these if auto-detect fails) ───────────────────────
# Treatment
treat_var <- if ("complex" %in% names(panel)) "complex" else "complex_cu"

# CU identifier
cu_var <- if ("cu_number" %in% names(panel)) "cu_number" else "cu_id"

# Spread outcomes — match exactly what 2_DiD_Estimation_v2.R uses
spread_mort  <- "spread_mort"      # or "spread_re_junior" — check spread_cols above
spread_nauto <- "spread_nauto"     # new auto spread
spread_comm  <- "spread_comm_re"   # commercial non-RE spread

# Loan growth
lgr_var      <- "loan_growth"      # QoQ log x 100

# Adjust if needed based on printed column names:
if (!spread_mort  %in% names(panel)) spread_mort  <- spread_cols[str_detect(spread_cols, "mort|re_j")][1]
if (!spread_nauto %in% names(panel)) spread_nauto <- spread_cols[str_detect(spread_cols, "nauto|uauto")][1]
if (!spread_comm  %in% names(panel)) spread_comm  <- spread_cols[str_detect(spread_cols, "comm")][1]
if (!lgr_var      %in% names(panel)) lgr_var      <- names(panel)[str_detect(names(panel), "^loan_gr")][1]

message("\nUsing:")
message("  treat_var:    ", treat_var)
message("  cu_var:       ", cu_var)
message("  spread_mort:  ", spread_mort)
message("  spread_nauto: ", spread_nauto)
message("  spread_comm:  ", spread_comm)
message("  lgr_var:      ", lgr_var)

# ── Build event time ──────────────────────────────────────────────────────────
panel <- panel |>
  mutate(
    event_time = (year - 2022) * 4 + (quarter - 1),
    yq_fe      = year * 10 + quarter
  )

# Use same time FE as existing scripts
time_fe <- if ("year_quarter" %in% names(panel)) "year_quarter" else "yq_fe"

panel_es <- panel |>
  filter(event_time >= -12, event_time <= 8) |>
  mutate(Et = relevel(factor(event_time), ref = "-1"))

# ── Run event study (same spec as 2_DiD_Estimation_v2.R) ─────────────────────
run_es <- function(outcome, label) {
  message("Estimating: ", label)
  df  <- panel_es |> filter(!is.na(.data[[outcome]]))
  fml <- as.formula(paste0(
    outcome, " ~ i(Et, ", treat_var, ", ref='-1') | ", cu_var, " + ", time_fe
  ))
  m  <- feols(fml, data = df, cluster = as.formula(paste0("~", cu_var)))
  ip <- iplot(m, only.params = TRUE)$prms
  tibble(
    quarter   = as.numeric(as.character(ip$x)),
    estimate  = ip$y,
    conf_low  = ip$ci_low,
    conf_high = ip$ci_high,
    post      = as.numeric(as.character(ip$x)) >= 0
  )
}

es_mort  <- run_es(spread_mort,  "Mortgage spread")
es_nauto <- run_es(spread_nauto, "New auto spread")
es_comm  <- run_es(spread_comm,  "Commercial spread")
es_lgr   <- run_es(lgr_var,      "Loan growth")

# ── Build four panels ─────────────────────────────────────────────────────────
pA <- plot_event_study(es_mort,  "Coefficient (Mortgage Spread (pp))") +
  labs(title = "A. Mortgage Spread",
       subtitle = paste0("Event time = quarters relative to RBC (Q0 = 2022 Q1).",
                         " Reference = Q-1. 95% CI shaded."))

pB <- plot_event_study(es_nauto, "Coefficient (New Auto Spread (pp))") +
  labs(title = "B. New Auto Spread",
       subtitle = paste0("Event time = quarters relative to RBC (Q0 = 2022 Q1).",
                         " Reference = Q-1. 95% CI shaded."))

pC <- plot_event_study(es_comm,  "Coefficient (Comm non-RE Spread (pp))") +
  labs(title = "C. Comm non-RE Spread",
       subtitle = paste0("Event time = quarters relative to RBC (Q0 = 2022 Q1).",
                         " Reference = Q-1. 95% CI shaded."))

pD <- plot_event_study(es_lgr,   "Coefficient (Loan growth (QoQ log\u00d7100))") +
  labs(title = "D. Loan Growth",
       subtitle = paste0("Event time = quarters relative to RBC (Q0 = 2022 Q1).",
                         " Reference = Q-1. 95% CI shaded."))

# ── Combine into 2x2 matching your existing panel style ──────────────────────
combined <- (pA | pB) / (pC | pD) +
  plot_annotation(
    title   = "Event Study: RBC Rule Impact on Loan Rate Spreads and Lending Volume",
    caption = paste0(
      "\u2605 = p < 0.05 (post-period). ",
      "Two-way FE (CU + quarter-year). SE clustered at CU. ",
      "Reference quarter = Q-1 (normalized to 0)."
    )
  ) &
  theme(plot.background = element_rect(fill = "white", color = NA))

# ── Save ───────────────────────────────────────────────────────────────────────
out_path <- file.path(FIGURE_PATH, "Figure_RQ3_Combined.png")
ggsave(out_path, combined, width = 14, height = 10, dpi = 300,
       bg = "white")
message("\nSaved: ", out_path)
