# =============================================================================
# Combine_RQ3_Figures.R  —  CLEAN AUDITED VERSION
#
# Strategy: mirror exactly what 2_DiD_Estimation_v2.R does. Use i() with ref
# matching the actual values in event_time. Diagnose first, then regress.
#
# Run from: S:/Projects/RBC_2026/Data/
# =============================================================================

rm(list = ls())
library(tidyverse)
library(fixest)
library(haven)
library(patchwork)

PANEL_FILE <- "analysis_panel_raw.rds"
FIGURE_OUT <- "output/paper/figures/Figure_RQ3_Combined.png"

NAVY <- "#1a2744"; GOLD <- "#C8972A"

# =============================================================================
# 1. LOAD AND DIAGNOSE
# =============================================================================
message("Loading panel...")
panel <- readRDS(PANEL_FILE) |> haven::zap_labels()

# Cast numeric columns explicitly
num_cols <- c("event_time", "complex", "cu_number", "q_period_num",
              "ln_assets", "loan_to_asset", "cecl_adopter",
              "spread_mortgage", "spread_nauto", "spread_uauto", "loan_growth")
for (nm in intersect(num_cols, names(panel))) {
  panel[[nm]] <- as.numeric(panel[[nm]])
}

# ── Diagnostic: confirm key columns exist with correct types ─────────────────
message("\n=== DIAGNOSTIC ===")
for (col in c("event_time","complex","cu_number","q_period_num",
              "ln_assets","loan_to_asset","cecl_adopter",
              "spread_mortgage","spread_nauto","spread_uauto","loan_growth")) {
  if (!(col %in% names(panel))) {
    message("  MISSING: ", col)
  } else {
    x <- panel[[col]]
    message(sprintf("  %-20s  class=%-10s  non-NA=%s  range=[%s, %s]",
                    col, class(x)[1], sum(!is.na(x)),
                    if (is.numeric(x)) round(min(x, na.rm=TRUE), 2) else "-",
                    if (is.numeric(x)) round(max(x, na.rm=TRUE), 2) else "-"))
  }
}

# Confirm event_time has -1 in it
et_vals <- sort(unique(panel$event_time[!is.na(panel$event_time)]))
message("\n  event_time unique values: ", paste(head(et_vals, 10), collapse=","),
        " ... ", paste(tail(et_vals, 5), collapse=","))
message("  -1 in event_time? ", -1 %in% et_vals)

if (!(-1 %in% et_vals)) {
  stop("event_time does not contain -1; cannot use as reference quarter")
}

# =============================================================================
# 2. FILTER
# =============================================================================
panel_es <- panel |> filter(event_time >= -12, event_time <= 8)
message(sprintf("\nPanel_es: %d rows", nrow(panel_es)))

# =============================================================================
# 3. EVENT STUDY (mirrors 2_DiD_Estimation_v2.R line for line)
# =============================================================================
run_es <- function(outcome_var, outcome_label, outcome_ylab) {
  message("\n-- ", outcome_label, " [", outcome_var, "]")

  df <- panel_es |> filter(!is.na(.data[[outcome_var]]))

  if (nrow(df) == 0) {
    stop("No non-NA rows for ", outcome_var)
  }
  message("  Rows: ", nrow(df))

  # Formula matches paper spec exactly
  fml <- as.formula(paste0(
    outcome_var,
    " ~ i(event_time, complex, ref = -1)",
    " + ln_assets + loan_to_asset + cecl_adopter",
    " | cu_number + q_period_num"
  ))

  m <- feols(fml, data = df, cluster = ~cu_number,
             warn = FALSE, notes = FALSE)

  # iplot gives the event-time coefficients directly
  ip <- iplot(m, only.params = TRUE)$prms

  tibble(
    label     = outcome_label,
    ylab      = outcome_ylab,
    quarter   = as.numeric(as.character(ip$x)),
    estimate  = ip$y,
    conf_low  = ip$ci_low,
    conf_high = ip$ci_high,
    period    = if_else(as.numeric(as.character(ip$x)) < 0, "Pre", "Post"),
    sig       = ip$ci_low > 0 | ip$ci_high < 0
  ) |>
    arrange(quarter)
}

message("\nRunning event studies...")
r_mort  <- run_es("spread_mortgage", "A.  Mortgage Rate Spread",  "Spread (pp)")
r_nauto <- run_es("spread_nauto",    "B.  New Auto Rate Spread",  "Spread (pp)")
r_uauto <- run_es("spread_uauto",    "C.  Used Auto Rate Spread", "Spread (pp)")
r_lgr   <- run_es("loan_growth",     "D.  Loan Growth (QoQ log\u00d7100)", "Growth (pp)")

# =============================================================================
# 4. PLOT
# =============================================================================
theme_rbc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E8E4DC", linewidth = 0.4),
      plot.title         = element_text(face = "bold", size = 13, color = NAVY),
      plot.subtitle      = element_text(size = 9, color = "#777", lineheight = 1.2),
      axis.title         = element_text(size = 10, color = "#555"),
      axis.text          = element_text(size = 9, color = "#555"),
      legend.position    = "bottom",
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "#FAFAF8", color = NA),
      plot.caption       = element_text(size = 7.5, color = "#999", lineheight = 1.2),
      plot.margin        = margin(10, 14, 8, 10)
    )
}

make_panel <- function(df) {
  pad <- diff(range(c(df$conf_low, df$conf_high), na.rm = TRUE)) * 0.12
  yl  <- min(df$conf_low,  na.rm = TRUE) - pad
  yh  <- max(df$conf_high, na.rm = TRUE) + pad

  ggplot(df, aes(x = quarter, y = estimate)) +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
                fill = NAVY, alpha = 0.13) +
    geom_vline(xintercept = 0, color = GOLD,
               linetype = "dashed", linewidth = 1.1) +
    geom_hline(yintercept = 0, color = "#AAA",
               linetype = "dashed", linewidth = 0.6) +
    geom_line(color = NAVY, linewidth = 1.5) +
    geom_point(data = ~filter(.x, period == "Pre"),
               shape = 1, color = NAVY, size = 3, stroke = 1.2) +
    geom_point(data = ~filter(.x, period == "Post", !sig),
               shape = 1, color = NAVY, size = 3, stroke = 1.2) +
    geom_point(data = ~filter(.x, period == "Post", sig),
               shape = 19, color = "#C8292C", size = 3.2) +
    annotate("text", x = 0.3, y = yh * 0.88, label = "Rule\neffective",
             hjust = 0, size = 3, color = GOLD, fontface = "bold",
             lineheight = 0.9) +
    scale_x_continuous(breaks = c(-12, -8, -4, 0, 4, 8),
                       labels = c("Q-12","Q-8","Q-4","Q0","Q+4","Q+8")) +
    coord_cartesian(ylim = c(yl, yh)) +
    labs(
      title    = unique(df$label),
      subtitle = "Event time = quarters relative to RBC (Q0 = 2022 Q1). Reference = Q\u22121. 95% CI shaded.",
      x        = "Quarters Relative to RBC Effective Date",
      y        = unique(df$ylab),
      caption  = "Two-way FE (CU + quarter-year). SE clustered at CU."
    ) +
    theme_rbc()
}

message("\nBuilding panels...")
p1 <- make_panel(r_mort)
p2 <- make_panel(r_nauto)
p3 <- make_panel(r_uauto)
p4 <- make_panel(r_lgr)

combined <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title   = "Event Study: RBC Rule Impact on Loan Rate Spreads and Lending Volume",
    caption = "Two-way FE (CU \u00d7 quarter-year). SE clustered at CU. Complex CUs = avg assets \u2265 $500M (2021 Q4). N \u2248 150,000 CU-quarters.",
    theme   = theme(
      plot.title   = element_text(face = "bold", size = 15, color = NAVY),
      plot.caption = element_text(size = 8.5, color = "#888", lineheight = 1.3)
    )
  )

dir.create(dirname(FIGURE_OUT), showWarnings = FALSE, recursive = TRUE)
message("\nSaving: ", FIGURE_OUT)
ggsave(FIGURE_OUT, combined, width = 16, height = 12, dpi = 300, bg = "white")
message("Done. Size: ", round(file.size(FIGURE_OUT) / 1024), " KB")
