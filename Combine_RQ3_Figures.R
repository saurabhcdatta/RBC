# =============================================================================
# Combine_RQ3_Figures.R  —  FIXED v3
# Run from: S:/Projects/RBC_2026/Data/
# Output:   output/paper/figures/Figure_RQ3_Combined.png
# =============================================================================

library(tidyverse)
library(fixest)
library(haven)
library(patchwork)

FIGURE_OUT <- "output/paper/figures"

theme_rbc <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#e8e0d4", linewidth = 0.4),
      plot.title         = element_text(face = "bold", size = base_size+1, color = "#1a2744"),
      plot.subtitle      = element_text(size = base_size-2, color = "#5a5a5a", lineheight = 1.2),
      axis.title         = element_text(size = base_size-1, color = "#444444"),
      axis.text          = element_text(size = base_size-2, color = "#555555"),
      legend.position    = "bottom",
      legend.text        = element_text(size = base_size-2),
      plot.background    = element_rect(fill = "white", color = NA),
      plot.caption       = element_text(size = 7.5, color = "#888888", lineheight = 1.2)
    )
}

COL_POST  <- "#1a2744"
RULE_LINE <- "#C8972A"

# ── Load panel ────────────────────────────────────────────────────────────────
message("Loading panel...")
panel <- readRDS("analysis_panel_raw.rds") |>
  haven::zap_labels() |>
  mutate(across(where(is.numeric), as.numeric))

# ── DIAGNOSTIC: print ALL column names so we can see everything ───────────────
message("\n========== ALL COLUMN NAMES IN PANEL ==========")
print(names(panel))
message("================================================\n")

# ── Auto-detect CU identifier (entity FE) ────────────────────────────────────
cu_id_var <- case_when(
  "cu_id"     %in% names(panel) ~ "cu_id",
  "cu_number" %in% names(panel) ~ "cu_number",
  "rssd_id"   %in% names(panel) ~ "rssd_id",
  "id"        %in% names(panel) ~ "id",
  TRUE ~ NA_character_
)
if (is.na(cu_id_var)) stop("Cannot find CU identifier column — check column names above")
message("CU identifier: ", cu_id_var)

# ── Auto-detect time FE ───────────────────────────────────────────────────────
time_var <- case_when(
  "year_quarter"  %in% names(panel) ~ "year_quarter",
  "yyyyqq"        %in% names(panel) ~ "yyyyqq",
  "date"          %in% names(panel) ~ "date",
  "period"        %in% names(panel) ~ "period",
  "time"          %in% names(panel) ~ "time",
  TRUE ~ NA_character_
)
# If no dedicated time column, construct one from year + quarter
if (is.na(time_var)) {
  if (all(c("year","quarter") %in% names(panel))) {
    panel <- panel |> mutate(yq_fe = year * 10 + quarter)
    time_var <- "yq_fe"
    message("Constructed time FE: yq_fe = year*10 + quarter")
  } else {
    stop("Cannot find time fixed effect column — check column names above")
  }
}
message("Time FE: ", time_var)

# ── Auto-detect treatment variable ───────────────────────────────────────────
treat_var <- case_when(
  "complex"    %in% names(panel) ~ "complex",
  "complex_cu" %in% names(panel) ~ "complex_cu",
  "treated"    %in% names(panel) ~ "treated",
  TRUE ~ NA_character_
)
if (is.na(treat_var)) stop("Cannot find treatment indicator — check column names above")
message("Treatment: ", treat_var)

# ── Auto-detect outcome variables ─────────────────────────────────────────────
all_cols <- names(panel)

# Loan growth
lgr_var <- all_cols[str_detect(all_cols, "loan_gr")][1]
if (is.na(lgr_var)) lgr_var <- all_cols[str_detect(all_cols, "^lgr|^loan_g")][1]
message("Loan growth: ", lgr_var)

# Mortgage spread
spread_mortgage <- all_cols[str_detect(all_cols, "spread") & str_detect(all_cols, "mort|re_j|re_30|re_fix")][1]
message("Mortgage spread: ", spread_mortgage)

# Auto spread
spread_nauto <- all_cols[str_detect(all_cols, "spread") & str_detect(all_cols, "nauto|uauto|auto")][1]
message("New/used auto spread: ", spread_nauto)

# Commercial spread — prefer non-RE commercial
spread_commercial <- all_cols[str_detect(all_cols, "spread") & str_detect(all_cols, "comm|bus|mbl")][1]
message("Commercial spread: ", spread_commercial)

# Check
missing <- c(lgr_var, spread_mortgage, spread_nauto, spread_commercial)
if (any(is.na(missing))) {
  message("\nCould not auto-detect some columns. Available spread/loan columns:")
  print(all_cols[str_detect(all_cols, "spread|loan_gr|irate")])
  message("\nSet these manually below and re-run:")
  message("  spread_mortgage   <- 'YOUR_COLUMN'")
  message("  spread_nauto      <- 'YOUR_COLUMN'")
  message("  spread_commercial <- 'YOUR_COLUMN'")
  message("  lgr_var           <- 'YOUR_COLUMN'")
  stop("Manual column assignment required")
}

# ══ MANUAL OVERRIDE (uncomment if auto-detect picks wrong columns) ════════════
# spread_mortgage   <- "spread_re_junior"
# spread_nauto      <- "spread_uauto"
# spread_commercial <- "spread_comm_re"
# lgr_var           <- "loan_growth"
# cu_id_var         <- "cu_number"
# time_var          <- "yq_fe"   # or whatever you construct below
# ══════════════════════════════════════════════════════════════════════════════

# ── Build event time ──────────────────────────────────────────────────────────
if (all(c("year","quarter") %in% names(panel))) {
  panel <- panel |> mutate(event_time = (year - 2022) * 4 + (quarter - 1))
} else {
  stop("Need 'year' and 'quarter' columns to build event time")
}

panel_es <- panel |>
  filter(event_time >= -12, event_time <= 8) |>
  mutate(Et = relevel(factor(event_time), ref = "-1"))

# ── Event study runner ────────────────────────────────────────────────────────
run_es <- function(outcome_col, panel_label) {
  message("Running: ", panel_label, " [", outcome_col, "]")
  df  <- panel_es |> filter(!is.na(.data[[outcome_col]]))
  fml <- as.formula(paste0(
    outcome_col, " ~ i(Et, ", treat_var, ", ref='-1') | ",
    cu_id_var, " + ", time_var
  ))
  tryCatch({
    m  <- feols(fml, data = df, cluster = as.formula(paste0("~", cu_id_var)))
    ip <- iplot(m, only.params = TRUE)$prms
    tibble(
      label     = panel_label,
      quarter   = as.numeric(as.character(ip$x)),
      estimate  = ip$y,
      conf_low  = ip$ci_low,
      conf_high = ip$ci_high,
      period    = if_else(as.numeric(as.character(ip$x)) < 0, "Pre", "Post")
    )
  }, error = function(e) { message("  ERROR: ", e$message); NULL })
}

es_mort  <- run_es(spread_mortgage,   "A.  Mortgage Rate Spread")
es_nauto <- run_es(spread_nauto,      "B.  New/Used Auto Rate Spread")
es_comm  <- run_es(spread_commercial, "C.  Commercial Rate Spread")
es_lgr   <- run_es(lgr_var,           "D.  Loan Growth")

if (any(sapply(list(es_mort, es_nauto, es_comm, es_lgr), is.null)))
  stop("One or more event studies failed — see errors above")

# ── Plot ──────────────────────────────────────────────────────────────────────
plot_es <- function(df) {
  ggplot(df, aes(x = quarter, y = estimate)) +
    geom_vline(xintercept = 0, color = RULE_LINE, linetype = "dashed", linewidth = 1.0) +
    geom_hline(yintercept = 0, color = "#AAAAAA", linetype = "dashed", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high), fill = COL_POST, alpha = 0.15) +
    geom_line(color = COL_POST, linewidth = 1.4) +
    geom_point(aes(shape = period), color = COL_POST, size = 2.8,
               fill = "white", stroke = 1.2) +
    scale_shape_manual(
      values = c("Pre" = 1, "Post" = 19),
      labels = c("Pre" = "Pre-RBC", "Post" = "Post-RBC"),
      name   = NULL
    ) +
    scale_x_continuous(breaks = seq(-12, 8, by = 2)) +
    labs(title = unique(df$label),
         x = "Quarters Relative to RBC Effective Date",
         y = "Estimated effect (pp)") +
    theme_rbc()
}

combined <- (plot_es(es_mort) | plot_es(es_nauto)) /
            (plot_es(es_comm) | plot_es(es_lgr))  +
  plot_annotation(
    title    = "Event Study: RBC Rule Impact on Loan Rate Spreads and Lending Volume",
    subtitle = "Event time = quarters relative to RBC effective date (2022 Q1). Reference = Q\u22121. Shaded band = 95% CI.",
    caption  = paste0(
      "Two-way FE (", cu_id_var, " + ", time_var, "). ",
      "SE clustered at CU. Complex CUs = avg assets \u2265 $500M (2021)."
    ),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, color = "#1a2744"),
      plot.subtitle = element_text(size = 10, color = "#555555"),
      plot.caption  = element_text(size = 8, color = "#888888")
    )
  )

dir.create(FIGURE_OUT, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(FIGURE_OUT, "Figure_RQ3_Combined.png")
ggsave(out_path, combined, width = 14, height = 10, dpi = 300)
message("\nDone. Saved: ", out_path)
