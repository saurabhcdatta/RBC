# =============================================================================
# Combine_RQ3_Figures.R  —  FINAL VERSION
# Produces a publication-quality 2x2 event study grid for RQ3:
#   A. Mortgage rate spread   B. New auto rate spread
#   C. Used auto rate spread  D. Loan growth
#
# Run from: S:/Projects/RBC_2026/Data/
# Output:   output/paper/figures/Figure_RQ3_Combined.png
# =============================================================================

library(tidyverse)
library(fixest)
library(haven)
library(patchwork)

# ── Paths ─────────────────────────────────────────────────────────────────────
PANEL_FILE <- "analysis_panel_raw.rds"
FIGURE_OUT <- "output/paper/figures/Figure_RQ3_Combined.png"

# ── Confirmed column names from 2_DiD_Estimation_v2.R ────────────────────────
CU_FE      <- "cu_number"      # entity fixed effect
TIME_FE    <- "q_period_num"   # time fixed effect
TREAT      <- "complex"        # treatment indicator (1 = complex CU)
EVENT_TIME <- "event_time"     # quarters relative to 2022 Q1, already in panel
EVENT_REF  <- -1L              # reference quarter

OUTCOMES <- list(
  list(var = "spread_mortgage", label = "A.  Mortgage Rate Spread",    ylab = "Coefficient (Mortgage Spread, pp)"),
  list(var = "spread_nauto",    label = "B.  New Auto Rate Spread",     ylab = "Coefficient (New Auto Spread, pp)"),
  list(var = "spread_uauto",    label = "C.  Used Auto Rate Spread",    ylab = "Coefficient (Used Auto Spread, pp)"),
  list(var = "loan_growth",     label = "D.  Loan Growth (QoQ log×100)", ylab = "Coefficient (Loan Growth, pp)")
)

# ── Controls (matching main paper — 2_DiD_Estimation_v2.R) ───────────────────
CONTROLS <- "ln_assets + loan_to_asset + cecl_adopter"

# ── Event window ──────────────────────────────────────────────────────────────
EVENT_MIN <- -12L
EVENT_MAX <-  8L

# ── Theme ─────────────────────────────────────────────────────────────────────
NAVY      <- "#1a2744"
GOLD      <- "#C8972A"
LGOLD     <- "#E8C36A"
MUTED     <- "#777777"

theme_rbc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major.x  = element_blank(),
      panel.grid.major.y  = element_line(color = "#E8E4DC", linewidth = 0.4),
      plot.title          = element_text(face = "bold", size = 13,
                                         color = NAVY, margin = margin(b = 4)),
      plot.subtitle       = element_text(size = 9, color = MUTED,
                                         lineheight = 1.2,
                                         margin = margin(b = 6)),
      axis.title.x        = element_text(size = 10, color = "#555555",
                                         margin = margin(t = 6)),
      axis.title.y        = element_text(size = 9,  color = "#555555",
                                         margin = margin(r = 4)),
      axis.text           = element_text(size = 9,  color = "#555555"),
      legend.position     = "bottom",
      legend.text         = element_text(size = 9),
      legend.key.size     = unit(0.5, "cm"),
      plot.background     = element_rect(fill = "white", color = NA),
      panel.background    = element_rect(fill = "#FAFAF8", color = NA),
      plot.caption        = element_text(size = 7.5, color = "#999999",
                                         lineheight = 1.2,
                                         margin = margin(t = 6)),
      plot.margin         = margin(10, 14, 8, 10)
    )
}

# ── Load panel ────────────────────────────────────────────────────────────────
message("Loading panel: ", PANEL_FILE)
panel <- readRDS(PANEL_FILE) |>
  haven::zap_labels() |>
  mutate(across(where(is.numeric), as.numeric))

message(sprintf("  Panel: %s rows x %s cols", nrow(panel), ncol(panel)))

# Verify key columns exist
required <- c(CU_FE, TIME_FE, TREAT, EVENT_TIME, CONTROLS |>
                str_split("\\s*\\+\\s*") |> unlist() |> str_trim())
missing  <- setdiff(required, names(panel))
if (length(missing) > 0) {
  stop("Missing columns: ", paste(missing, collapse = ", "),
       "\nAvailable: ", paste(sort(names(panel)), collapse = ", "))
}

# ── Filter to event window ────────────────────────────────────────────────────
panel_es <- panel |>
  filter(.data[[EVENT_TIME]] >= EVENT_MIN,
         .data[[EVENT_TIME]] <= EVENT_MAX)

message(sprintf("  Event window: %d rows", nrow(panel_es)))

# ── Event study runner ────────────────────────────────────────────────────────
run_es <- function(outcome_var, outcome_label) {

  message("  Running: ", outcome_label, " [", outcome_var, "]")

  df <- panel_es |> filter(!is.na(.data[[outcome_var]]))

  fml <- as.formula(paste0(
    outcome_var,
    " ~ i(", EVENT_TIME, ", ", TREAT, ", ref = ", EVENT_REF, ")",
    " + ", CONTROLS,
    " | ", CU_FE, " + ", TIME_FE
  ))

  m <- tryCatch(
    feols(fml, data = df,
          cluster  = as.formula(paste0("~", CU_FE)),
          warn     = FALSE,
          notes    = FALSE),
    error = function(e) {
      stop("feols failed for '", outcome_var, "': ", e$message)
    }
  )

  # Extract iplot parameters
  ip <- iplot(m, only.params = TRUE)$prms

  tibble(
    label     = outcome_label,
    quarter   = as.numeric(as.character(ip$x)),
    estimate  = ip$y,
    conf_low  = ip$ci_low,
    conf_high = ip$ci_high,
    sig       = ip$ci_low > 0 | ip$ci_high < 0,  # significant at 95%
    period    = if_else(as.numeric(as.character(ip$x)) < 0, "Pre", "Post")
  )
}

# ── Run all four event studies ────────────────────────────────────────────────
message("\nRunning event studies...")
results <- map(OUTCOMES, ~ run_es(.x$var, .x$label))
names(results) <- map_chr(OUTCOMES, "var")

# ── Plot function ─────────────────────────────────────────────────────────────
make_panel <- function(df, ylab) {

  # Determine y-axis range with padding
  y_lo  <- min(df$conf_low,  na.rm = TRUE)
  y_hi  <- max(df$conf_high, na.rm = TRUE)
  pad   <- (y_hi - y_lo) * 0.12
  y_lo  <- y_lo - pad
  y_hi  <- y_hi + pad

  ggplot(df, aes(x = quarter, y = estimate)) +

    # Confidence band
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
                fill = NAVY, alpha = 0.13) +

    # Rule effective date line
    geom_vline(xintercept = 0,
               color = GOLD, linetype = "dashed", linewidth = 1.1) +

    # Zero line
    geom_hline(yintercept = 0,
               color = "#AAAAAA", linetype = "dashed", linewidth = 0.6) +

    # Main line
    geom_line(color = NAVY, linewidth = 1.5) +

    # Points — filled = significant, open = not significant
    geom_point(data = filter(df, period == "Pre"),
               shape = 1, color = NAVY, size = 3.0, stroke = 1.2) +
    geom_point(data = filter(df, period == "Post",  sig == FALSE),
               shape = 1, color = NAVY, size = 3.0, stroke = 1.2) +
    geom_point(data = filter(df, period == "Post",  sig == TRUE),
               shape = 19, color = "#C8292C", size = 3.2) +

    # Rule label
    annotate("text", x = 0.25, y = y_hi * 0.92,
             label = "Rule\neffective", hjust = 0,
             size = 3.0, color = GOLD, fontface = "bold",
             lineheight = 0.9) +

    scale_x_continuous(
      breaks = seq(EVENT_MIN, EVENT_MAX, by = 4),
      labels = function(x) paste0("Q", ifelse(x >= 0, paste0("+", x), x))
    ) +
    coord_cartesian(ylim = c(y_lo, y_hi)) +
    labs(
      title    = unique(df$label),
      subtitle = paste0(
        "Event time = quarters relative to RBC (Q0 = 2022 Q1). ",
        "Reference = Q\u22121. 95% CI shaded.\n",
        "\u2022 Post-RBC (red filled) = p < 0.05  \u25cb Post-RBC (open) = not significant  \u25cb Pre-RBC (open)"
      ),
      x     = "Quarters Relative to RBC Effective Date",
      y     = ylab,
      caption = paste0(
        "\u2605 p < 0.05 (post-period). Two-way FE (CU + quarter-year). ",
        "SE clustered at CU. Reference quarter = Q\u22121 (normalised to 0)."
      )
    ) +
    theme_rbc()
}

# ── Build four panels ─────────────────────────────────────────────────────────
message("\nBuilding panels...")
plots <- map2(results, OUTCOMES, ~ make_panel(.x, .y$ylab))

pA <- plots[["spread_mortgage"]]
pB <- plots[["spread_nauto"]]
pC <- plots[["spread_uauto"]]
pD <- plots[["loan_growth"]]

# ── Combine into 2x2 grid ─────────────────────────────────────────────────────
combined <- (pA | pB) / (pC | pD) +
  plot_annotation(
    title   = "Event Study: RBC Rule Impact on Loan Rate Spreads and Lending Volume",
    caption = paste0(
      "Two-way FE (CU \u00d7 quarter-year). SE clustered at CU level. ",
      "Complex CUs defined as avg assets \u2265 $500M using 2021 Q4 data (fixed classification). ",
      "N \u2248 150,000 CU-quarters, 2000\u20132025. ",
      "Mortgage and commercial spreads semi-annual (Q2/Q4 only)."
    ),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 15,
                                  color = NAVY, margin = margin(b = 4)),
      plot.caption = element_text(size = 8.5, color = "#888888",
                                  lineheight = 1.3)
    )
  )

# ── Save ───────────────────────────────────────────────────────────────────────
dir.create(dirname(FIGURE_OUT), showWarnings = FALSE, recursive = TRUE)
message("\nSaving: ", FIGURE_OUT)
ggsave(FIGURE_OUT, combined,
       width  = 16,
       height = 12,
       dpi    = 300,
       bg     = "white")
message("Done. Size: ", round(file.size(FIGURE_OUT) / 1024), " KB")
