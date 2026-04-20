# =============================================================================
# Combine_RQ3_Figures.R  —  FIXED VERSION
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

# ── Diagnostic: print relevant column names ───────────────────────────────────
message("\n--- COLUMN NAMES CONTAINING 'spread', 'loan_gr', or 'complex' ---")
print(names(panel)[str_detect(names(panel), "spread|loan_gr|complex")])
message("--------------------------------------------------------------\n")

# ── Auto-detect correct variable names ────────────────────────────────────────
treat_var <- case_when(
  "complex"    %in% names(panel) ~ "complex",
  "complex_cu" %in% names(panel) ~ "complex_cu",
  TRUE ~ NA_character_
)
if (is.na(treat_var)) stop("No treatment variable found — check column names above")
message("Treatment: ", treat_var)

lgr_var <- names(panel)[str_detect(names(panel), "^loan_gr")][1]
if (is.na(lgr_var)) stop("No loan growth variable found")
message("Loan growth: ", lgr_var)

spread_cols <- names(panel)[str_detect(names(panel), "^spread_")]
spread_mortgage   <- spread_cols[str_detect(spread_cols, "mort")][1]
spread_nauto      <- spread_cols[str_detect(spread_cols, "nauto|new_auto")][1]
spread_commercial <- spread_cols[str_detect(spread_cols, "comm")][1]

message("Mortgage spread:   ", spread_mortgage)
message("New auto spread:   ", spread_nauto)
message("Commercial spread: ", spread_commercial)

if (any(is.na(c(spread_mortgage, spread_nauto, spread_commercial)))) {
  message("\nCould not auto-detect all spread columns.")
  message("Please set them manually below and re-run.\n")
  # MANUAL OVERRIDE — uncomment and set if auto-detect fails:
  # spread_mortgage   <- "spread_mort"
  # spread_nauto      <- "spread_nauto"
  # spread_commercial <- "spread_comm_re"
  stop("Fix spread column names above")
}

# ── Build event time ──────────────────────────────────────────────────────────
if (all(c("year","quarter") %in% names(panel))) {
  panel <- panel |> mutate(event_time = (year - 2022) * 4 + (quarter - 1))
} else if ("year_quarter" %in% names(panel)) {
  panel <- panel |> mutate(
    yr  = as.integer(str_extract(as.character(year_quarter), "^\\d{4}")),
    qtr = as.integer(str_extract(as.character(year_quarter), "\\d$")),
    event_time = (yr - 2022) * 4 + (qtr - 1)
  )
} else stop("Cannot build event time — need year+quarter columns")

panel_es <- panel |>
  filter(event_time >= -12, event_time <= 8) |>
  mutate(Et = relevel(factor(event_time), ref = "-1"))

# ── Event study runner ────────────────────────────────────────────────────────
run_es <- function(outcome_col, panel_label) {
  message("Running: ", panel_label)
  df  <- panel_es |> filter(!is.na(.data[[outcome_col]]))
  fml <- as.formula(paste0(
    outcome_col, " ~ i(Et, ", treat_var, ", ref='-1') | cu_id + year_quarter"
  ))
  tryCatch({
    m  <- feols(fml, data = df, cluster = ~cu_id)
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
es_nauto <- run_es(spread_nauto,      "B.  New Auto Rate Spread")
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
    caption  = "Two-way FE (CU + quarter-year). SE clustered at CU. Complex CUs = avg assets \u2265 $500M (2021 classification).",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 13, color = "#1a2744"),
      plot.subtitle = element_text(size = 10, color = "#555555"),
      plot.caption  = element_text(size = 8, color = "#888888")
    )
  )

dir.create(FIGURE_OUT, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(FIGURE_OUT, "Figure_RQ3_Combined.png")
ggsave(out_path, combined, width = 14, height = 10, dpi = 300)
message("\nDone. Saved: ", out_path)
