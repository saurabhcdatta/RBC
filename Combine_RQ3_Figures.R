# =============================================================================
# Combine_RQ3_Figures.R
# Combines Figure4_EventStudy_Spreads.png (3 panels: mortgage, new auto,
# commercial) with Figure3b_EventStudy_LoanGrowth.png (1 panel: loan growth)
# into a single 2x2 grid: Figure_RQ3_Combined.png
#
# Run from: S:/Projects/RBC_2026/Data/
# Output:   output/paper/figures/Figure_RQ3_Combined.png
# =============================================================================

library(tidyverse)
library(patchwork)

FIGURE_IN  <- "output/paper/figures"
FIGURE_OUT <- "output/paper/figures"

# ── Theme matching existing RBC style ─────────────────────────────────────────
theme_rbc <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#e8e0d4", linewidth = 0.4),
      plot.title         = element_text(face = "bold", size = base_size + 1,
                                        color = "#1a2744"),
      plot.subtitle      = element_text(size = base_size - 1,
                                        color = "#5a5a5a", lineheight = 1.2),
      axis.title         = element_text(size = base_size - 1, color = "#444444"),
      axis.text          = element_text(size = base_size - 2, color = "#555555"),
      legend.position    = "bottom",
      legend.text        = element_text(size = base_size - 2),
      plot.background    = element_rect(fill = "white", color = NA),
      plot.caption       = element_text(size = 7.5, color = "#888888",
                                        lineheight = 1.2)
    )
}

COL_POST  <- "#1a2744"   # navy  — Post-RBC
COL_PRE   <- "#8FA4C8"   # steel — Pre-RBC
RULE_LINE <- "#C8972A"   # gold  — rule effective date

# ── Helper: event study plot from a data frame ────────────────────────────────
# Expects columns: quarter, estimate, conf_low, conf_high, period ("Pre"/"Post")
plot_es <- function(df, title, ylab, y_breaks = NULL) {
  p <- ggplot(df, aes(x = quarter, y = estimate)) +
    geom_vline(xintercept = 0, color = RULE_LINE,
               linetype = "dashed", linewidth = 1.0) +
    geom_hline(yintercept = 0, color = "#AAAAAA",
               linetype = "dashed", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
                fill = COL_POST, alpha = 0.12) +
    geom_line(color = COL_POST, linewidth = 1.4) +
    geom_point(aes(shape = period), color = COL_POST, size = 2.8, fill = "white") +
    scale_shape_manual(
      values = c("Pre" = 1, "Post" = 19),
      labels = c("Pre" = "Pre-RBC", "Post" = "Post-RBC"),
      name   = NULL
    ) +
    labs(title = title, y = ylab, x = "Quarters Relative to RBC Effective Date") +
    theme_rbc()

  if (!is.null(y_breaks)) {
    p <- p + scale_y_continuous(breaks = y_breaks)
  }
  p
}

# ── Load event study results ───────────────────────────────────────────────────
# The event study estimates are saved by 2_DiD_Estimation_v2.R as RDS or CSV.
# Try RDS first (preferred), fall back to reading from analysis_panel_raw.rds
# and re-running the minimal event study extraction.

es_file <- "output/tables/es_results_all.rds"

if (file.exists(es_file)) {

  es_all <- readRDS(es_file)

  get_es <- function(outcome_name) {
    es_all |>
      filter(outcome == outcome_name) |>
      mutate(
        period   = if_else(quarter < 0, "Pre", "Post"),
        conf_low  = estimate - 1.96 * std_error,
        conf_high = estimate + 1.96 * std_error
      )
  }

  df_mort <- get_es("spread_mortgage")
  df_nauto <- get_es("spread_nauto")
  df_comm <- get_es("spread_commercial")
  df_lgr  <- get_es("loan_growth")

} else {

  # ── Fallback: re-run minimal event study from panel data ─────────────────────
  message("es_results_all.rds not found — re-running from panel data")

  library(fixest)
  library(haven)

  panel <- readRDS("analysis_panel_raw.rds") |>
    haven::zap_labels() |>
    mutate(across(everything(), \(x) {
      if (inherits(x, "haven_labelled")) as.numeric(x) else x
    }))

  # Define relative quarter (event time)
  RULE_Q <- as.numeric(as.Date("2022-01-01"))

  panel <- panel |>
    mutate(
      date_num   = as.numeric(as.Date(paste0(year, "-", (quarter-1)*3+1, "-01"))),
      event_time = round((date_num - RULE_Q) / (365.25/4))
    ) |>
    filter(event_time >= -12, event_time <= 8) |>
    mutate(
      Et = factor(event_time),
      Et = relevel(Et, ref = "-1")
    )

  run_es <- function(outcome) {
    fml <- as.formula(paste0(outcome, " ~ i(Et, complex_cu, ref = '-1') | cu_id + year_quarter"))
    tryCatch({
      m   <- feols(fml, data = panel, cluster = ~cu_id)
      iplot_data <- iplot(m, only.params = TRUE)
      tibble(
        outcome   = outcome,
        quarter   = as.numeric(as.character(iplot_data$prms$x)),
        estimate  = iplot_data$prms$y,
        std_error = (iplot_data$prms$ci_high - iplot_data$prms$ci_low) / (2 * 1.96),
        conf_low  = iplot_data$prms$ci_low,
        conf_high = iplot_data$prms$ci_high,
        period    = if_else(as.numeric(as.character(iplot_data$prms$x)) < 0, "Pre", "Post")
      )
    }, error = function(e) {
      message("  Skipping ", outcome, ": ", e$message)
      NULL
    })
  }

  outcomes <- c("spread_mortgage", "spread_nauto", "spread_commercial", "loan_growth_pp")
  es_list  <- map(outcomes, run_es) |> compact()

  get_es <- function(outcome_name) {
    bind_rows(es_list) |>
      filter(outcome == outcome_name)
  }

  df_mort  <- get_es("spread_mortgage")
  df_nauto <- get_es("spread_nauto")
  df_comm  <- get_es("spread_commercial")
  df_lgr   <- get_es("loan_growth_pp")
}

# ── Build four panels ─────────────────────────────────────────────────────────
pA <- plot_es(df_mort,  "A.  Mortgage Rate Spread",    "Basis points (pp)")
pB <- plot_es(df_nauto, "B.  New Auto Rate Spread",    "Basis points (pp)")
pC <- plot_es(df_comm,  "C.  Commercial Rate Spread",  "Basis points (pp)")
pD <- plot_es(df_lgr,   "D.  Loan Growth",             "Quarterly growth (pp)")

# ── Combine into 2×2 grid ─────────────────────────────────────────────────────
combined <- (pA | pB) / (pC | pD) +
  plot_annotation(
    title    = "Event Study: RBC Rule Impact on Loan Rate Spreads and Lending Volume",
    subtitle = paste0(
      "Event time = quarters relative to RBC effective date (2022 Q1). Reference = Q\u22121. Shaded band = 95% CI.\n",
      "Panels A\u2013C: loan rate spreads by type. Panel D: total loan growth rate.\n",
      "Pre-period dots near zero confirm parallel trends. Post-period divergence is the rule\u2019s estimated effect."
    ),
    caption  = paste0(
      "Two-way FE (CU + quarter-year), SE clustered at CU. Complex CUs = avg assets \u2265 $500M (2021 classification).\n",
      "N \u2248 150,000 CU-quarters, 2000\u20132025. Mortgage and commercial spreads semi-annual (Q2/Q4 only)."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, color = "#1a2744"),
      plot.subtitle = element_text(size = 10, color = "#555555", lineheight = 1.3),
      plot.caption  = element_text(size = 8, color = "#888888")
    )
  )

# ── Save ───────────────────────────────────────────────────────────────────────
out_path <- file.path(FIGURE_OUT, "Figure_RQ3_Combined.png")
ggsave(out_path, combined, width = 14, height = 10, dpi = 300)
message("Saved: ", out_path)
