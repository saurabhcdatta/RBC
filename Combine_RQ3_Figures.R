# =============================================================================
# Combine_RQ3_Figures.R  —  FINAL VERSION
# Run from: S:/Projects/RBC_2026/Data/
# Output:   output/paper/figures/Figure_RQ3_Combined.png
# =============================================================================

library(tidyverse)
library(fixest)
library(haven)
library(patchwork)

PANEL_FILE <- "analysis_panel_raw.rds"
FIGURE_OUT <- "output/paper/figures/Figure_RQ3_Combined.png"

NAVY  <- "#1a2744"
GOLD  <- "#C8972A"
MUTED <- "#777777"

theme_rbc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#E8E4DC", linewidth = 0.4),
      plot.title         = element_text(face = "bold", size = 13, color = NAVY,
                                        margin = margin(b = 4)),
      plot.subtitle      = element_text(size = 9, color = MUTED,
                                        lineheight = 1.2, margin = margin(b = 6)),
      axis.title.x       = element_text(size = 10, color = "#555555", margin = margin(t = 6)),
      axis.title.y       = element_text(size = 9,  color = "#555555", margin = margin(r = 4)),
      axis.text          = element_text(size = 9,  color = "#555555"),
      legend.position    = "bottom",
      legend.text        = element_text(size = 9),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "#FAFAF8", color = NA),
      plot.caption       = element_text(size = 7.5, color = "#999999",
                                        lineheight = 1.2, margin = margin(t = 6)),
      plot.margin        = margin(10, 14, 8, 10)
    )
}

# ── Load panel ────────────────────────────────────────────────────────────────
message("Loading panel...")
panel <- readRDS(PANEL_FILE) |>
  haven::zap_labels() |>
  mutate(across(where(is.numeric), as.numeric))

message(sprintf("  %d rows x %d cols", nrow(panel), ncol(panel)))
message("  event_time range: ", min(panel$event_time, na.rm=TRUE),
        " to ", max(panel$event_time, na.rm=TRUE))
message("  event_time class: ", class(panel$event_time))

# ── Filter event window ───────────────────────────────────────────────────────
panel_es <- panel |>
  filter(event_time >= -12, event_time <= 8)

message(sprintf("  Event window rows: %d", nrow(panel_es)))

# ── Event study function ──────────────────────────────────────────────────────
# KEY FIX: use sunab() or i() with ref as character matching actual values
# Safest approach: manually dummy out event_time and use standard lm-style FE

run_es <- function(outcome_var, outcome_label, ylab) {
  message("  Running: ", outcome_label)

  df <- panel_es |>
    filter(!is.na(.data[[outcome_var]])) |>
    # Create event time factor with explicit reference = -1
    mutate(
      et_fac = factor(event_time),
      et_fac = relevel(et_fac, ref = "-1")
    )

  # Formula using factor with relevel (avoids i() ref mismatch)
  fml <- as.formula(paste0(
    outcome_var,
    " ~ et_fac:complex",
    " + ln_assets + loan_to_asset + cecl_adopter",
    " | cu_number + q_period_num"
  ))

  m <- tryCatch(
    feols(fml, data = df,
          cluster = ~cu_number,
          warn    = FALSE,
          notes   = FALSE),
    error = function(e) stop("feols failed for '", outcome_var, "': ", e$message)
  )

  # Extract coefficients for complex == 1 interactions (post and pre)
  coefs <- coef(m)
  ses   <- se(m)
  cis   <- confint(m)

  # Keep only the et_fac:complex interaction terms
  idx <- str_detect(names(coefs), "^et_fac.*:complex$|^et_fac.*complex$")
  if (sum(idx) == 0) {
    # Try alternate naming
    idx <- str_detect(names(coefs), "complex.*et_fac|et_fac.*complex")
  }

  coef_names <- names(coefs)[idx]

  # Extract quarter from coefficient name
  qtrs <- as.numeric(str_extract(coef_names, "-?\\d+"))

  # Reference quarter = -1 contributes 0 by construction
  all_qtrs <- sort(unique(c(qtrs, -1L)))

  result <- tibble(
    label     = outcome_label,
    quarter   = all_qtrs,
    estimate  = case_when(
      all_qtrs == -1L ~ 0,
      TRUE ~ coefs[coef_names][match(all_qtrs[all_qtrs != -1L], qtrs)]
    ),
    conf_low  = case_when(
      all_qtrs == -1L ~ 0,
      TRUE ~ cis[coef_names, 1][match(all_qtrs[all_qtrs != -1L], qtrs)]
    ),
    conf_high = case_when(
      all_qtrs == -1L ~ 0,
      TRUE ~ cis[coef_names, 2][match(all_qtrs[all_qtrs != -1L], qtrs)]
    ),
    period    = if_else(all_qtrs < 0, "Pre", "Post"),
    sig       = (conf_low > 0 | conf_high < 0) & all_qtrs != -1L,
    ylab      = ylab
  )

  result
}

# ── Run all four ──────────────────────────────────────────────────────────────
message("\nRunning event studies...")

SPECS <- list(
  list(var="spread_mortgage", label="A.  Mortgage Rate Spread",
       ylab="Coefficient (Mortgage Spread, pp)"),
  list(var="spread_nauto",    label="B.  New Auto Rate Spread",
       ylab="Coefficient (New Auto Spread, pp)"),
  list(var="spread_uauto",    label="C.  Used Auto Rate Spread",
       ylab="Coefficient (Used Auto Spread, pp)"),
  list(var="loan_growth",     label="D.  Loan Growth (QoQ log\u00d7100)",
       ylab="Coefficient (Loan Growth, pp)")
)

results <- map(SPECS, ~ run_es(.x$var, .x$label, .x$ylab))

# ── Plot function ─────────────────────────────────────────────────────────────
make_panel <- function(df) {
  ylo <- min(df$conf_low,  na.rm=TRUE) - abs(diff(range(df$estimate, na.rm=TRUE)))*0.1
  yhi <- max(df$conf_high, na.rm=TRUE) + abs(diff(range(df$estimate, na.rm=TRUE)))*0.1

  ggplot(df, aes(x = quarter, y = estimate)) +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
                fill = NAVY, alpha = 0.13) +
    geom_vline(xintercept = 0, color = GOLD,
               linetype = "dashed", linewidth = 1.1) +
    geom_hline(yintercept = 0, color = "#AAAAAA",
               linetype = "dashed", linewidth = 0.6) +
    geom_line(color = NAVY, linewidth = 1.5) +
    geom_point(data = ~filter(.x, period == "Pre"),
               shape = 1, color = NAVY, size = 3.0, stroke = 1.2) +
    geom_point(data = ~filter(.x, period == "Post", !sig),
               shape = 1, color = NAVY, size = 3.0, stroke = 1.2) +
    geom_point(data = ~filter(.x, period == "Post", sig),
               shape = 19, color = "#C8292C", size = 3.2) +
    annotate("text", x = 0.3, y = yhi * 0.88,
             label = "Rule\neffective", hjust = 0,
             size = 3.0, color = GOLD, fontface = "bold", lineheight = 0.9) +
    scale_x_continuous(breaks = c(-12,-8,-4,0,4,8),
                       labels = c("Q-12","Q-8","Q-4","Q0","Q+4","Q+8")) +
    coord_cartesian(ylim = c(ylo, yhi)) +
    labs(
      title    = unique(df$label),
      subtitle = paste0("Event time = quarters relative to RBC (Q0 = 2022 Q1). ",
                        "Reference = Q\u22121. 95% CI shaded.\n",
                        "\u25cf Post-RBC significant (red)  \u25cb Post-RBC not significant  \u25cb Pre-RBC"),
      x        = "Quarters Relative to RBC Effective Date",
      y        = unique(df$ylab),
      caption  = "\u2605 p < 0.05 (post-period). Two-way FE (CU + quarter-year). SE clustered at CU."
    ) +
    theme_rbc()
}

message("\nBuilding panels...")
plots <- map(results, make_panel)

# ── Combine 2x2 ───────────────────────────────────────────────────────────────
combined <- (plots[[1]] | plots[[2]]) / (plots[[3]] | plots[[4]]) +
  plot_annotation(
    title   = "Event Study: RBC Rule Impact on Loan Rate Spreads and Lending Volume",
    caption = paste0(
      "Two-way FE (CU \u00d7 quarter-year). SE clustered at CU. ",
      "Complex CUs = avg assets \u2265 $500M (2021 Q4). ",
      "N \u2248 150,000 CU-quarters, 2000\u20132025."
    ),
    theme = theme(
      plot.title   = element_text(face="bold", size=15, color=NAVY, margin=margin(b=6)),
      plot.caption = element_text(size=8.5, color="#888888", lineheight=1.3)
    )
  )

# ── Save ──────────────────────────────────────────────────────────────────────
dir.create(dirname(FIGURE_OUT), showWarnings=FALSE, recursive=TRUE)
message("\nSaving: ", FIGURE_OUT)
ggsave(FIGURE_OUT, combined, width=16, height=12, dpi=300, bg="white")
message("Done. Size: ", round(file.size(FIGURE_OUT)/1024), " KB")
