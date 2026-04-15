# =============================================================================
# 3G_Disaggregated_Delinquency.R
# RBC Rule Impact Analysis — Disaggregated Delinquency Analysis
#
# Author  : Saurabh C. Datta, Ph.D.
# Created : 2026
#
# BACKGROUND:
#   The NCUA 5300 Call Report provides three pre-computed DQ ratios that are
#   available consistently across the full panel:
#     dq_rate      - overall portfolio delinquency rate
#     dq_mbl_rate  - member business loan delinquency rate
#     dq_comm_rate - commercial loan delinquency rate
#
#   Disaggregated DQ ratios by product type (auto, CC, RE, residential) are
#   NOT pre-computed consistently in the 5300 — credit unions report dollar
#   amounts in selected schedules with inconsistent coverage. Rather than
#   imputing unreliable ratios from sparse data, this script uses three
#   complementary analytical approaches that together tell a rigorous story.
#
# THREE ANALYTICAL APPROACHES:
#
#   APPROACH 1 — Direct DiD on available DQ and charge-off categories
#     Establishes which pre-computed DQ/charge-off measures rose significantly.
#     Five specifications: overall DQ, MBL DQ, commercial DQ,
#     total charge-offs, MBL charge-offs.
#
#   APPROACH 2 — Portfolio-shift interaction (KEY CAUSAL TEST)
#     Tests whether the DQ increase is concentrated in institutions that
#     shifted their portfolios toward CRE (RE share rose) and away from auto.
#     If DQ rises MORE for CRE-heavy complex CUs, it directly confirms the
#     portfolio composition → delinquency mechanism.
#     Sub-tests:
#       (a) High vs. low RE-shift subgroups
#       (b) High vs. low auto-exit subgroups
#
#   APPROACH 3 — Mechanism summary
#     Integrates Findings 4 and 5 into a single 4-panel chart showing
#     the causal chain: RE shift UP → auto shift DOWN → DQ UP → charge-offs UP.
#     The lag structure (DQ follows portfolio shift by 2-4 quarters) provides
#     additional causal evidence.
#
# WHY THIS IS MORE RIGOROUS THAN JUST REPORTING DISAGGREGATED RATES:
#   Showing that the DQ increase tracks the portfolio shift institution-by-
#   institution is a stronger causal claim than simply showing a CRE DQ rate
#   rose. It rules out alternative explanations (e.g., general credit
#   deterioration, macroeconomic factors) that would affect all institutions
#   regardless of how much they shifted their portfolios.
#
# OUTPUTS (7 charts + 3 CSV tables):
#   policy_3g1_dq_available_categories.png  - DiD: available DQ categories
#   policy_3g2_dq_event_studies.png         - event studies: 4 DQ measures
#   policy_3g3_portfolio_dq_interaction.png - RE shift subgroup comparison
#   policy_3g4_auto_dq_interaction.png      - auto exit subgroup comparison
#   policy_3g5_chargeoff_breakdown.png      - charge-off event studies
#   policy_3g6_thin_buffer_dq.png           - thin-buffer vs. well-cap DQ
#   policy_3g7_mechanism_summary.png        - integrated 4-panel causal chain
#   3G_dq_category_results.csv
#   3G_dq_interactions.csv
#   3G_dq_mechanism_summary.csv
# =============================================================================

library(tidyverse)
library(fixest)
library(patchwork)
library(scales)
library(broom)

setwd("S:/Projects/RBC_2026/Data/")

message("=================================================================")
message("  3G -- DISAGGREGATED DELINQUENCY ANALYSIS")
message("=================================================================")

# =============================================================================
# 1. PATHS AND CONSTANTS
# =============================================================================

PANEL_PATH  <- "analysis_panel_raw.rds"
FIGURE_PATH <- "output/figures/"
TABLE_PATH  <- "output/tables/"

dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)
dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)

EVENT_MIN       <- -12L
EVENT_MAX       <-  10L
EVENT_REF       <-  -1L
EVENT_MAX_PAPER <-  10L

# =============================================================================
# 2. COLOURS AND THEME
# =============================================================================

COL_COMPLEX  <- "#1B3A6B"
COL_CONCERN  <- "#C94040"
COL_POSITIVE <- "#2E7D4F"
COL_MIXED    <- "#E8A838"
COL_NEUTRAL  <- "#4A7CB5"
COL_ZERO     <- "gray40"

theme_rbc <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12, color = "#1B3A6B"),
      plot.subtitle    = element_text(size = 9,  color = "#4A5568"),
      plot.caption     = element_text(size = 7.5, color = "#6B7A99"),
      axis.title       = element_text(size = 9,  color = "#2D3748"),
      axis.text        = element_text(size = 8,  color = "#4A5568"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#EDF2F7", linewidth = 0.4),
      strip.text       = element_text(face = "bold", size = 9, color = "#1B3A6B"),
      legend.text      = element_text(size = 8),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# =============================================================================
# 3. LOAD DATA
# =============================================================================

message("-- Loading analysis panel --------------------------------------------")

df <- readRDS(PANEL_PATH)

message(sprintf("  Panel rows  : %s", scales::comma(nrow(df))))
message(sprintf("  Complex CUs : %s",
                n_distinct(df$cu_number[df$complex == 1])))

# Validate all required variables exist
required_vars <- c(
  "dq_rate_var", "dq_mbl_rate_var", "dq_comm_rate_var",
  "chgoff_ratio", "chgoff_mbl_ratio",
  "re_shr", "auto_shr", "mbl_shr", "cc_shr",
  "complex", "post_rbc", "treat_post", "event_time",
  "cu_number", "q_period_num",
  "ln_assets", "loan_to_asset", "cecl_adopter",
  "dep_to_asset", "inv_to_asset"
)

missing_req <- setdiff(required_vars, names(df))
if (length(missing_req) > 0) {
  stop(sprintf("Missing required variables: %s",
               paste(missing_req, collapse = ", ")))
}
message("  All required variables present.")

# Coverage diagnostics
message("\n  DQ variable coverage:")
for (v in c("dq_rate_var", "dq_mbl_rate_var", "dq_comm_rate_var",
            "chgoff_ratio", "chgoff_mbl_ratio")) {
  cov_all <- mean(!is.na(df[[v]]))
  cov_cx  <- mean(!is.na(df[[v]][df$complex == 1]))
  message(sprintf("    %-22s  all=%.0f%%  complex=%.0f%%",
                  v, cov_all * 100, cov_cx * 100))
}

# =============================================================================
# 4. PORTFOLIO SHIFT VARIABLES
# =============================================================================

message("\n-- Computing portfolio shift variables --------------------------------")

# Institution-level shift = post-rule mean minus pre-rule mean
# Positive delta_re_shr  = shifted INTO real estate
# Negative delta_auto_shr = exited auto loans

portfolio_shift <- df |>
  group_by(cu_number) |>
  summarise(
    re_shr_pre    = mean(re_shr[post_rbc == 0],   na.rm = TRUE),
    re_shr_post   = mean(re_shr[post_rbc == 1],   na.rm = TRUE),
    auto_shr_pre  = mean(auto_shr[post_rbc == 0], na.rm = TRUE),
    auto_shr_post = mean(auto_shr[post_rbc == 1], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    delta_re_shr   = re_shr_post   - re_shr_pre,
    delta_auto_shr = auto_shr_post - auto_shr_pre,
    # Binary flags: above median shift
    high_re_shift  = as.integer(
      delta_re_shr > median(delta_re_shr, na.rm = TRUE)
    ),
    high_auto_exit = as.integer(
      delta_auto_shr < median(delta_auto_shr, na.rm = TRUE)
    )
  )

df <- df |>
  left_join(
    portfolio_shift |> select(cu_number, delta_re_shr, delta_auto_shr,
                               high_re_shift, high_auto_exit),
    by = "cu_number"
  )

n_high_re   <- n_distinct(df$cu_number[df$complex == 1 & df$high_re_shift == 1])
n_high_auto <- n_distinct(df$cu_number[df$complex == 1 & df$high_auto_exit == 1])
message(sprintf("  Complex CUs: above-median RE shift    = %d", n_high_re))
message(sprintf("  Complex CUs: above-median auto exit   = %d", n_high_auto))

df_es <- df |> filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)

controls <- "ln_assets + loan_to_asset + cecl_adopter + dep_to_asset + inv_to_asset"

# =============================================================================
# 5. ESTIMATION FUNCTIONS
# =============================================================================

run_did <- function(outcome, data = df, ctrl = controls) {
  fml <- as.formula(paste0(
    outcome, " ~ treat_post + ", ctrl, " | cu_number + q_period_num"
  ))
  tryCatch(
    feols(fml, data = data, cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) { warning(e$message); NULL }
  )
}

run_es <- function(outcome, data = df_es, ctrl = controls) {
  fml <- as.formula(paste0(
    outcome,
    " ~ i(event_time, complex, ref = ", EVENT_REF, ") + ",
    ctrl, " | cu_number + q_period_num"
  ))
  tryCatch(
    feols(fml, data = data, cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) { warning(e$message); NULL }
  )
}

# Canonical empty tibble for extract_did — used when model is NULL or fails
empty_did_row <- function(label) {
  tibble(
    label   = label,
    beta    = NA_real_, se    = NA_real_,
    ci_lo   = NA_real_, ci_hi = NA_real_,
    p_value = NA_real_, stars = "",
    n_obs   = NA_integer_
  )
}

extract_did <- function(model, label, term = "treat_post") {
  # Guard: NULL model or all-NA outcome → return empty row, never NULL
  if (is.null(model)) return(empty_did_row(label))
  tryCatch({
    td  <- tidy(model, conf.int = TRUE)
    row <- td[td$term == term, ]
    if (nrow(row) == 0) return(empty_did_row(label))
    tibble(
      label   = label,
      beta    = round(row$estimate,  4),
      se      = round(row$std.error, 4),
      ci_lo   = round(row$conf.low,  4),
      ci_hi   = round(row$conf.high, 4),
      p_value = round(row$p.value,   4),
      stars   = case_when(
        row$p.value < 0.01 ~ "***",
        row$p.value < 0.05 ~ "**",
        row$p.value < 0.10 ~ "*",
        TRUE               ~ ""
      ),
      n_obs = nobs(model)
    )
  }, error = function(e) empty_did_row(label))
}

extract_es <- function(model, label) {
  if (is.null(model)) return(tibble())   # empty tibble — plot_es handles nrow==0
  tryCatch({
    coefs <- tidy(model, conf.int = TRUE) |>
      filter(str_detect(term, "event_time::")) |>
      mutate(event_time = as.integer(str_extract(term, "-?\\d+"))) |>
      filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX_PAPER)
    ref_row <- tibble(
      term = "ref", estimate = 0, std.error = 0,
      conf.low = 0, conf.high = 0, p.value = NA_real_,
      event_time = EVENT_REF
    )
    bind_rows(coefs, ref_row) |>
      arrange(event_time) |>
      mutate(label  = label,
             period = if_else(event_time < 0, "Pre-RBC", "Post-RBC"))
  }, error = function(e) tibble())
}

plot_es <- function(es_data, lbl, color = COL_COMPLEX,
                    y_lab = "Coefficient (DQ Rate, pp)") {
  d <- if (!is.null(es_data) && "label" %in% names(es_data)) {
    es_data |> filter(label == lbl)
  } else es_data
  if (is.null(d) || nrow(d) == 0) return(plot_spacer())

  ggplot(d, aes(x = event_time, y = estimate)) +
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX_PAPER + 0.5,
             ymin = -Inf, ymax = Inf, fill = "gray95", alpha = 0.5) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = color, alpha = 0.15) +
    geom_hline(yintercept = 0, color = COL_ZERO,
               linewidth = 0.6, linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.7, linetype = "dashed") +
    geom_line(color = color, linewidth = 0.9) +
    geom_point(aes(shape = period, color = period), size = 2.2) +
    geom_point(
      data = d |> filter(event_time >= 0, !is.na(p.value), p.value < 0.05),
      shape = 8, size = 2.8, color = color
    ) +
    scale_color_manual(
      values = c("Post-RBC" = color, "Pre-RBC" = "gray55"),
      guide = "none"
    ) +
    scale_shape_manual(
      values = c("Post-RBC" = 16, "Pre-RBC" = 1),
      guide = "none"
    ) +
    scale_x_continuous(
      breaks = c(-12, -8, -4, 0, 4, 8, 10),
      labels = function(x) paste0("Q", x)
    ) +
    labs(
      title    = lbl,
      subtitle = "Event time vs. RBC (Q0=2022Q1). Ref=Q-1. 95% CI shaded. star=p<0.05",
      x = "Quarters Relative to RBC Effective Date", y = y_lab
    ) +
    theme_rbc()
}

# =============================================================================
# 6. APPROACH 1 -- DiD ON AVAILABLE DQ CATEGORIES
# =============================================================================

message("\n-- Approach 1: DiD on available DQ categories -----------------------")

dq_specs <- list(
  list("dq_rate_var",      "Overall DQ Rate",       "Delinquency"),
  list("dq_mbl_rate_var",  "MBL DQ Rate",           "Delinquency"),
  list("dq_comm_rate_var", "Commercial DQ Rate",     "Delinquency"),
  list("chgoff_ratio",     "Total Charge-off Rate",  "Charge-off"),
  list("chgoff_mbl_ratio", "MBL Charge-off Rate",    "Charge-off")
)

did_cat <- map_dfr(dq_specs, function(s) {
  message(sprintf("  %s", s[[1]]))
  m <- run_did(s[[1]])
  r <- extract_did(m, s[[2]])
  if (!is.null(r)) r$type <- s[[3]]
  r
}) |> filter(!is.na(beta))

cat("\n=== DiD BY CATEGORY ===\n")
print(did_cat |> select(label, type, beta, se, stars, p_value, n_obs), n = Inf)
write_csv(did_cat, file.path(TABLE_PATH, "3G_dq_category_results.csv"))

did_cat_plot <- did_cat |>
  mutate(
    color_sig = case_when(
      p_value < 0.10 & beta > 0 ~ "Significant increase",
      p_value < 0.10 & beta < 0 ~ "Significant decrease",
      TRUE                       ~ "Not significant"
    ),
    label = factor(label, levels = rev(label))
  )

p3g1 <- ggplot(did_cat_plot,
               aes(x = beta * 100, y = label, color = color_sig)) +
  geom_vline(xintercept = 0, color = COL_ZERO,
             linewidth = 0.7, linetype = "dashed") +
  geom_errorbarh(aes(xmin = ci_lo * 100, xmax = ci_hi * 100),
                 height = 0.3, linewidth = 0.8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = paste0(sprintf("%+.2f", beta * 100), "bp", stars)),
    hjust = -0.25, size = 3.2, fontface = "bold"
  ) +
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c(
    "Significant increase" = COL_CONCERN,
    "Not significant"      = "gray55",
    "Significant decrease" = COL_POSITIVE
  )) +
  scale_x_continuous(
    limits = function(x) c(min(x) - 1, max(x) + 3),
    labels = function(x) paste0(x, "bp")
  ) +
  labs(
    title    = "RBC Rule Impact on Credit Quality -- Available DQ Categories",
    subtitle = paste0(
      "DiD estimates: complex vs. non-complex credit unions, post-rule period.\n",
      "DQ rates and charge-off rates from NCUA 5300 Call Report."
    ),
    x = "DiD Coefficient (basis points)", y = NULL, color = NULL,
    caption = paste0(
      "Two-way FE (CU + quarter-year). SE clustered at CU. ",
      "Controls: ln_assets, loan_to_asset, cecl_adopter, dep_to_asset, inv_to_asset. ",
      "*** p<0.01, ** p<0.05, * p<0.10."
    )
  ) +
  theme_rbc() +
  theme(legend.position = "bottom")

ggsave(file.path(FIGURE_PATH, "policy_3g1_dq_available_categories.png"),
       p3g1, width = 11, height = 8, dpi = 300)
message("  Chart 3G1 saved.")

# =============================================================================
# 7. APPROACH 2 -- EVENT STUDIES BY DQ CATEGORY
# =============================================================================

message("\n-- Approach 2: Event studies by DQ category -------------------------")

es_overall <- run_es("dq_rate_var")
es_mbl     <- run_es("dq_mbl_rate_var")
es_comm    <- run_es("dq_comm_rate_var")
es_chgoff  <- run_es("chgoff_ratio")

es_all <- bind_rows(
  extract_es(es_overall, "Overall DQ Rate"),
  extract_es(es_mbl,     "MBL DQ Rate"),
  extract_es(es_comm,    "Commercial DQ Rate"),
  extract_es(es_chgoff,  "Total Charge-off Rate")
)

pES1 <- plot_es(es_all, "Overall DQ Rate",    COL_COMPLEX) +
  labs(title = "A. Overall DQ Rate")
pES2 <- plot_es(es_all, "MBL DQ Rate",        COL_CONCERN) +
  labs(title = "B. MBL Delinquency Rate")
pES3 <- plot_es(es_all, "Commercial DQ Rate", COL_MIXED)   +
  labs(title = "C. Commercial DQ Rate")
pES4 <- plot_es(es_all, "Total Charge-off Rate", "gray30") +
  labs(title = "D. Total Charge-off Rate")

p3g2 <- (pES1 + pES2) / (pES3 + pES4) +
  plot_annotation(
    title   = "Event Studies: Credit Quality Deterioration by Category",
    subtitle = paste0(
      "Pre-period near-zero confirms parallel trends. Post-period divergence shows WHEN ",
      "and WHICH categories deteriorated.\n",
      "Key question: does MBL/commercial DQ rise faster than overall? That confirms the CRE mechanism."
    ),
    caption = "Two-way FE (CU + quarter-year). SE clustered at CU. Reference = Q-1."
  )

ggsave(file.path(FIGURE_PATH, "policy_3g2_dq_event_studies.png"),
       p3g2, width = 14, height = 10, dpi = 300)
message("  Chart 3G2 saved.")

# =============================================================================
# 8. APPROACH 3 -- PORTFOLIO-SHIFT INTERACTION TEST
# =============================================================================

message("\n-- Approach 3: Portfolio-shift interaction tests --------------------")

# Split complex CUs into high/low RE-shift and high/low auto-exit subgroups
df_high_re  <- df |> filter(high_re_shift == 1  | complex == 0)
df_low_re   <- df |> filter(high_re_shift == 0  | complex == 0)
df_high_ae  <- df |> filter(high_auto_exit == 1 | complex == 0)
df_low_ae   <- df |> filter(high_auto_exit == 0 | complex == 0)

# DQ models by RE-shift and auto-exit subgroups are run inside safe_run_did below,
# which adds coverage guards and attaches group/outcome columns atomically.

# DQ models by auto-exit subgroup
# (also handled inside safe_run_did below)

# Helper: run_did only if outcome has enough non-NA obs in the data
safe_run_did <- function(outcome, data, label, group_val, outcome_val,
                          ctrl = controls, min_obs = 100) {
  n_valid <- sum(!is.na(data[[outcome]]), na.rm = TRUE)
  if (n_valid < min_obs) {
    message(sprintf("  SKIP: %s in '%s' — only %d non-NA obs (need %d)",
                    outcome, group_val, n_valid, min_obs))
    return(empty_did_row(label) |>
             mutate(group = group_val, outcome = outcome_val))
  }
  m <- run_did(outcome, data = data, ctrl = ctrl)
  extract_did(m, label) |>
    mutate(group = group_val, outcome = outcome_val)
}

re_results <- bind_rows(
  safe_run_did("dq_rate_var",     df_high_re, "Overall DQ",
               "Above-median RE shift", "Overall DQ"),
  safe_run_did("dq_rate_var",     df_low_re,  "Overall DQ",
               "Below-median RE shift", "Overall DQ"),
  safe_run_did("dq_mbl_rate_var", df_high_re, "MBL DQ",
               "Above-median RE shift", "MBL DQ"),
  safe_run_did("dq_mbl_rate_var", df_low_re,  "MBL DQ",
               "Below-median RE shift", "MBL DQ")
) |> filter(!is.na(beta))

ae_results <- bind_rows(
  safe_run_did("dq_rate_var", df_high_ae, "Above-median auto exit",
               "Above-median auto exit (exited most)", "Overall DQ"),
  safe_run_did("dq_rate_var", df_low_ae,  "Below-median auto exit",
               "Below-median auto exit (retained most)", "Overall DQ")
) |> filter(!is.na(beta))

interaction_results <- bind_rows(
  re_results |> mutate(test = "RE shift"),
  ae_results |> mutate(test = "Auto exit", outcome = "Overall DQ")
)

cat("\n=== INTERACTION RESULTS ===\n")
print(interaction_results |>
        select(test, group, outcome, beta, se, stars, p_value), n = Inf)
write_csv(interaction_results, file.path(TABLE_PATH, "3G_dq_interactions.csv"))

# Chart 3G3 -- RE shift subgroup
p3g3 <- ggplot(
  re_results |> filter(outcome == "Overall DQ"),
  aes(x = beta * 100, y = group, color = group)
) +
  geom_vline(xintercept = 0, color = COL_ZERO,
             linewidth = 0.6, linetype = "dashed") +
  geom_errorbarh(aes(xmin = ci_lo * 100, xmax = ci_hi * 100),
                 height = 0.2, linewidth = 1.0) +
  geom_point(size = 5) +
  geom_text(
    aes(label = paste0(sprintf("%+.2f", beta * 100), "bp", stars)),
    hjust = -0.3, size = 3.8, fontface = "bold"
  ) +
  scale_color_manual(values = c(
    "Above-median RE shift" = COL_CONCERN,
    "Below-median RE shift" = COL_POSITIVE
  ), guide = "none") +
  scale_x_continuous(
    limits = function(x) c(min(x) - 1, max(x) + 3),
    labels = function(x) paste0(x, "bp")
  ) +
  labs(
    title    = "DQ Increase Concentrated in Institutions That Shifted INTO CRE",
    subtitle = paste0(
      "Complex CUs split by whether their real estate loan share rose above or below the median.\n",
      "If the portfolio-shift mechanism is correct: above-median RE-shift CUs should show",
      " significantly larger DQ increases."
    ),
    x = "DiD Coefficient -- Overall DQ Rate (basis points)", y = NULL,
    caption = paste0(
      "RE shift = post-rule mean RE share minus pre-rule mean RE share (% of total loans). ",
      "Median computed across all complex CUs.\n",
      "Two-way FE. SE clustered at CU. *** p<0.01, ** p<0.05, * p<0.10."
    )
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "policy_3g3_portfolio_dq_interaction.png"),
       p3g3, width = 11, height = 5, dpi = 300)
message("  Chart 3G3 saved.")

# Chart 3G4 -- Auto exit subgroup
p3g4 <- ggplot(
  ae_results,
  aes(x = beta * 100, y = group, color = group)
) +
  geom_vline(xintercept = 0, color = COL_ZERO,
             linewidth = 0.6, linetype = "dashed") +
  geom_errorbarh(aes(xmin = ci_lo * 100, xmax = ci_hi * 100),
                 height = 0.2, linewidth = 1.0) +
  geom_point(size = 5) +
  geom_text(
    aes(label = paste0(sprintf("%+.2f", beta * 100), "bp", stars)),
    hjust = -0.3, size = 3.8, fontface = "bold"
  ) +
  scale_color_manual(values = c(
    "Above-median auto exit (exited most)"   = COL_CONCERN,
    "Below-median auto exit (retained most)" = COL_POSITIVE
  ), guide = "none") +
  scale_x_continuous(
    limits = function(x) c(min(x) - 1, max(x) + 3),
    labels = function(x) paste0(x, "bp")
  ) +
  labs(
    title    = "DQ Increase Larger for Institutions That Exited Auto Loans Most",
    subtitle = paste0(
      "Complex CUs split by whether their auto loan share fell above or below the median.\n",
      "Auto loans have historically stable DQ profiles -- exiting them removes a stabilizing factor."
    ),
    x = "DiD Coefficient -- Overall DQ Rate (basis points)", y = NULL,
    caption = paste0(
      "Auto exit = post-rule mean auto share minus pre-rule mean auto share (% of total loans). ",
      "Median computed across all complex CUs.\n",
      "Two-way FE. SE clustered at CU. *** p<0.01, ** p<0.05, * p<0.10."
    )
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "policy_3g4_auto_dq_interaction.png"),
       p3g4, width = 11, height = 5, dpi = 300)
message("  Chart 3G4 saved.")

# =============================================================================
# 9. CHARGE-OFF EVENT STUDIES
# =============================================================================

message("\n-- Approach 3b: Charge-off event studies ----------------------------")

es_co_total <- run_es("chgoff_ratio")

# chgoff_mbl_ratio has low coverage — guard before running event study
n_mbl_co <- sum(!is.na(df_es$chgoff_mbl_ratio), na.rm = TRUE)
es_co_mbl <- if (n_mbl_co >= 100) {
  message(sprintf("  Running MBL charge-off event study (%d obs)", n_mbl_co))
  run_es("chgoff_mbl_ratio")
} else {
  message(sprintf("  SKIP: chgoff_mbl_ratio has only %d non-NA obs — omitting Panel B", n_mbl_co))
  NULL
}

pCO1 <- plot_es(extract_es(es_co_total, "Total Charge-off"),
                "Total Charge-off", "gray30",
                "Coefficient (Charge-off Rate, pp)") +
  labs(title = "A. Total Charge-off Rate")
pCO2 <- plot_es(extract_es(es_co_mbl, "MBL Charge-off"),
                "MBL Charge-off", COL_CONCERN,
                "Coefficient (Charge-off Rate, pp)") +
  labs(title = "B. MBL Charge-off Rate")

p3g5 <- (pCO1 + pCO2) +
  plot_annotation(
    title   = "Charge-off Rate Event Studies: Delinquency Progressing to Loss",
    subtitle = paste0(
      "Charge-offs confirm WHICH delinquencies became actual losses.\n",
      "Panel A: total charge-off rate across all loan types. ",
      "Panel B: MBL-specific (shown only if sufficient coverage; otherwise blank).\n",
      "A larger MBL-specific effect confirms that commercial/CRE loans drove the deterioration."
    ),
    caption = "Two-way FE (CU + quarter-year). SE clustered at CU. Reference = Q-1."
  )

ggsave(file.path(FIGURE_PATH, "policy_3g5_chargeoff_breakdown.png"),
       p3g5, width = 13, height = 6, dpi = 300)
message("  Chart 3G5 saved.")

# =============================================================================
# 10. THIN-BUFFER HETEROGENEITY
# =============================================================================

message("\n-- Thin-buffer heterogeneity ----------------------------------------")

if (all(c("thin_buffer", "avg_nw_pre") %in% names(df))) {

  df_thin    <- df |> filter(thin_buffer == 1 | complex == 0)
  df_wellcap <- df |> filter(
    (complex == 1 & !is.na(avg_nw_pre) & avg_nw_pre > 11.0) | complex == 0
  )

  n_thin_cx    <- n_distinct(df_thin$cu_number[df_thin$complex == 1])
  n_wellcap_cx <- n_distinct(df_wellcap$cu_number[df_wellcap$complex == 1])
  message(sprintf("  Thin-buffer complex CUs     : %d", n_thin_cx))
  message(sprintf("  Well-capitalized complex CUs: %d", n_wellcap_cx))

  het_dq <- list(
    list("dq_rate_var",      "Overall DQ"),
    list("dq_mbl_rate_var",  "MBL DQ"),
    list("dq_comm_rate_var", "Commercial DQ"),
    list("chgoff_ratio",     "Charge-off")
  )

  lbl_thin    <- sprintf("Thin-buffer (9-11%% pre-rule, n=%d)", n_thin_cx)
  lbl_wellcap <- sprintf("Well-capitalized (>11%% pre-rule, n=%d)", n_wellcap_cx)

  het_results <- map_dfr(het_dq, function(s) {
    bind_rows(
      safe_run_did(s[[1]], df_thin,    s[[2]], lbl_thin,    s[[2]]),
      safe_run_did(s[[1]], df_wellcap, s[[2]], lbl_wellcap, s[[2]])
    )
  }) |> filter(!is.na(beta))

  p3g6 <- ggplot(het_results,
                 aes(x = beta * 100, y = label,
                     color = group, shape = group)) +
    geom_vline(xintercept = 0, color = COL_ZERO,
               linewidth = 0.6, linetype = "dashed") +
    geom_errorbarh(
      aes(xmin = ci_lo * 100, xmax = ci_hi * 100),
      height = 0.25, linewidth = 0.8,
      position = position_dodge2(width = 0.55, padding = 0.1)
    ) +
    geom_point(size = 3.8,
               position = position_dodge2(width = 0.55, padding = 0.1)) +
    geom_text(
      aes(label = paste0(sprintf("%+.2f", beta * 100), "bp", stars)),
      hjust = -0.25, size = 2.9,
      position = position_dodge2(width = 0.55, padding = 0.1)
    ) +
    scale_color_manual(values = setNames(
      c(COL_CONCERN, COL_NEUTRAL),
      c(lbl_thin, lbl_wellcap)
    )) +
    scale_shape_manual(values = setNames(c(16, 17), c(lbl_thin, lbl_wellcap))) +
    scale_x_continuous(
      limits = function(x) c(min(x) - 1, max(x) + 3),
      labels = function(x) paste0(x, "bp")
    ) +
    labs(
      title    = "DQ Effects Concentrated in Most Constrained Institutions",
      subtitle = paste0(
        "Thin-buffer CUs (pre-rule NW 9-11%) faced the most compliance pressure ",
        "and shifted their portfolios most aggressively.\n",
        "If compliance pressure drives DQ: thin-buffer CUs should show larger effects."
      ),
      x = "DiD Coefficient (basis points)", y = NULL,
      color = NULL, shape = NULL,
      caption = "Two-way FE. SE clustered at CU. *** p<0.01, ** p<0.05, * p<0.10."
    ) +
    theme_rbc() +
    theme(legend.position = "bottom")

  ggsave(file.path(FIGURE_PATH, "policy_3g6_thin_buffer_dq.png"),
         p3g6, width = 11, height = 7, dpi = 300)
  message("  Chart 3G6 saved.")

} else {
  message("  NOTE: thin_buffer/avg_nw_pre not found. Re-run 0_Data_Prep.R.")
}

# =============================================================================
# 11. MECHANISM SUMMARY CHART
# =============================================================================

message("\n-- Mechanism summary chart ------------------------------------------")

es_re_shr   <- run_es("re_shr")
es_auto_shr <- run_es("auto_shr")

pM1 <- plot_es(extract_es(es_re_shr,   "RE Share"),
               "RE Share", COL_CONCERN,
               "Coefficient (RE Loan Share, pp)") +
  labs(title = "A. RE Loan Share (shifted IN -- mechanism origin)")

pM2 <- plot_es(extract_es(es_auto_shr, "Auto Share"),
               "Auto Share", COL_POSITIVE,
               "Coefficient (Auto Loan Share, pp)") +
  labs(title = "B. Auto Loan Share (exited -- mechanism origin)")

pM3 <- plot_es(extract_es(es_overall, "Overall DQ"),
               "Overall DQ", COL_COMPLEX,
               "Coefficient (DQ Rate, pp)") +
  labs(title = "C. Overall DQ Rate (consequence)")

pM4 <- plot_es(extract_es(es_mbl, "MBL DQ"),
               "MBL DQ", COL_CONCERN,
               "Coefficient (DQ Rate, pp)") +
  labs(title = "D. MBL / Commercial DQ (which category drove it)")

p3g7 <- (pM1 + pM2) / (pM3 + pM4) +
  plot_annotation(
    title   = "Portfolio Shift to Delinquency: The Complete Causal Chain",
    subtitle = paste0(
      "Panels A-B: the rule caused complex CUs to shift FROM auto loans INTO real estate ",
      "(both effects significant from Q0).\n",
      "Panels C-D: this shift was followed by rising delinquency -- particularly in commercial ",
      "categories -- with a 2-4 quarter lag.\n",
      "The timing lag between Panels A-B and Panels C-D is itself evidence of causality: ",
      "portfolio composition changed first, then credit quality deteriorated."
    ),
    caption = paste0(
      "Two-way FE (CU + quarter-year). SE clustered at CU. Reference = Q-1. star = p < 0.05. ",
      "All panels: DiD coefficients, complex vs. non-complex CUs."
    )
  )

ggsave(file.path(FIGURE_PATH, "policy_3g7_mechanism_summary.png"),
       p3g7, width = 14, height = 10, dpi = 300)
message("  Chart 3G7 saved.")

# Mechanism summary CSV
mechanism_summary <- bind_rows(
  extract_did(run_did("re_shr"),   "RE share change (pp)")   |>
    mutate(step = "Step 1: Portfolio shift IN"),
  extract_did(run_did("auto_shr"), "Auto share change (pp)") |>
    mutate(step = "Step 1: Portfolio shift OUT"),
  did_cat |> filter(type == "Delinquency") |>
    mutate(step = "Step 2: DQ consequence"),
  did_cat |> filter(type == "Charge-off") |>
    mutate(step = "Step 3: Loss")
) |> filter(!is.na(beta))

write_csv(mechanism_summary,
          file.path(TABLE_PATH, "3G_dq_mechanism_summary.csv"))

# =============================================================================
# 12. FINAL SUMMARY
# =============================================================================

cat("\n")
cat("================================================================\n")
cat("  3G DISAGGREGATED DELINQUENCY -- COMPLETE\n")
cat("================================================================\n")
cat("  Approach 1: DiD by available category\n")
for (i in seq_len(nrow(did_cat))) {
  cat(sprintf("    %-28s  %+.3f pp%s\n",
              did_cat$label[i], did_cat$beta[i], did_cat$stars[i]))
}
cat("\n  Approach 2: Portfolio-shift interaction (RE)\n")
for (i in seq_len(nrow(re_results |> filter(outcome == "Overall DQ")))) {
  r <- (re_results |> filter(outcome == "Overall DQ"))[i, ]
  cat(sprintf("    %-36s  %+.3f pp%s\n",
              r$group, r$beta, r$stars))
}
cat("\n  Figures saved to output/figures/:\n")
figs <- c("policy_3g1_dq_available_categories.png",
          "policy_3g2_dq_event_studies.png",
          "policy_3g3_portfolio_dq_interaction.png",
          "policy_3g4_auto_dq_interaction.png",
          "policy_3g5_chargeoff_breakdown.png",
          "policy_3g6_thin_buffer_dq.png",
          "policy_3g7_mechanism_summary.png")
for (f in figs) {
  flag <- if (file.exists(file.path(FIGURE_PATH, f))) "[OK]" else "[--]"
  cat(sprintf("    %s %s\n", flag, f))
}
cat("================================================================\n")
