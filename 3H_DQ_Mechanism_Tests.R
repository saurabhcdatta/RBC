# =============================================================================
# 3H_DQ_Mechanism_Tests.R
# RBC Rule Impact Analysis — Why Did Overall Delinquency Increase?
#
# Author  : Saurabh C. Datta, Ph.D.
# Created : 2026
#
# MOTIVATION:
#   Finding 5 establishes the overall DQ rate rose +8bp (p<0.001) after the
#   RBC rule. 3G showed the increase is real (charge-offs also rose) but did
#   not isolate the mechanism. This script runs four targeted tests to
#   determine WHETHER and HOW MUCH of the DQ rate increase is driven by:
#
#   TEST 1 — DENOMINATOR SHRINKAGE (your hypothesis)
#     DQ rate = delinquent $ / total loan $
#     Finding 3 shows loan growth contracted -0.44pp/quarter post-rule.
#     If delinquent dollar amounts stayed flat while the denominator shrank,
#     the rate rises mechanically — NOT because credit quality deteriorated.
#     Method: (a) Run DiD on DOLLAR-LEVEL delinquency proxy (dq_rate × lns_tot)
#              (b) Add loan_growth as a control in the rate DiD — if the
#                  coefficient drops substantially, the denominator effect
#                  explains part of the rate increase.
#
#   TEST 2 — PORTFOLIO COMPOSITION CONTROL
#     Adding re_shr and auto_shr to the DQ DiD tests whether the rate increase
#     is explained by the portfolio shift (Finding 4) after controlling for
#     composition. If the coefficient drops: CRE shift explains it.
#     If it stays: the mechanism is something else (rates paid by borrowers).
#
#   TEST 3 — RATE CHANNEL (borrower stress from higher loan rates)
#     Finding 2 shows loan rates rose 58-78bp across all products.
#     Higher rates mechanically stress existing borrowers. We can't directly
#     regress DQ on rate increases (both are post-rule outcomes), but we can:
#     (a) Test whether DQ rose MORE at institutions that raised rates the most
#         (high-spread-increase CUs vs. low-spread-increase CUs)
#     (b) This is an institution-level intensity-of-treatment test.
#
#   TEST 4 — CAPACITY CHANNEL (reduced loan-workout capacity under capital stress)
#     When CUs are under capital pressure they have less bandwidth to work
#     proactively with struggling borrowers. We proxy this by:
#     (a) Thin-buffer interaction: did DQ rise more where capital was most
#         binding? (thin_buffer already defined)
#     (b) ROA decline interaction: did DQ rise more where profitability fell
#         most? (earnings capacity = workout capacity)
#
# KEY OUTPUT: A summary table showing how much the DQ DiD coefficient changes
#   under each additional control — this tells you which mechanisms matter.
#
# OUTPUTS:
#   policy_3h1_dq_mechanism_decomp.png  — coefficient comparison across specs
#   policy_3h2_dollar_dq_test.png       — event study: dollar DQ vs. rate DQ
#   policy_3h3_rate_channel.png         — high/low rate-increase subgroups
#   policy_3h4_capacity_channel.png     — ROA decline interaction
#   3H_dq_mechanism_tests.csv
# =============================================================================

library(tidyverse)
library(fixest)
library(patchwork)
library(scales)
library(broom)

setwd("S:/Projects/RBC_2026/Data/")

message("=================================================================")
message("  3H -- DQ MECHANISM TESTS: WHY DID DELINQUENCY INCREASE?")
message("=================================================================")

# =============================================================================
# 1. SETUP
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
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# =============================================================================
# 2. LOAD DATA
# =============================================================================

message("-- Loading panel -----------------------------------------------------")
df <- readRDS(PANEL_PATH)
message(sprintf("  Rows: %s | Complex CUs: %s",
                scales::comma(nrow(df)),
                n_distinct(df$cu_number[df$complex == 1])))

# Check required variables
req <- c("dq_rate_var", "loan_growth", "lns_tot", "re_shr", "auto_shr",
         "roa_var", "loan_to_asset", "treat_post", "complex",
         "cu_number", "q_period_num", "ln_assets", "cecl_adopter",
         "dep_to_asset", "inv_to_asset", "event_time", "post_rbc")
missing_req <- setdiff(req, names(df))
if (length(missing_req) > 0) stop("Missing: ", paste(missing_req, collapse=", "))
message("  All required variables present.")

# =============================================================================
# 3. CONSTRUCT ADDITIONAL VARIABLES
# =============================================================================

message("\n-- Constructing test variables ---------------------------------------")

df <- df |>
  arrange(cu_number, q_period_num) |>
  group_by(cu_number) |>
  mutate(
    # TEST 1: Dollar-level delinquency proxy
    # dq_rate is already delinquent$/total_loans$ * 100
    # So implied delinquent dollar amount = dq_rate_var/100 * lns_tot
    # Log-transform for DiD (like loan_growth)
    dq_dollar_proxy = if_else(
      !is.na(dq_rate_var) & lns_tot > 0,
      dq_rate_var / 100 * lns_tot,
      NA_real_
    ),
    ln_dq_dollar = if_else(
      !is.na(dq_dollar_proxy) & dq_dollar_proxy > 0,
      log(dq_dollar_proxy),
      NA_real_
    ),
    # Change in loan-to-asset ratio (denominator shrinkage proxy)
    delta_loan_to_asset = loan_to_asset - lag(loan_to_asset),

    # TEST 4: ROA decline — pre vs post
    roa_pre  = mean(roa_var[post_rbc == 0], na.rm = TRUE),
    roa_post = mean(roa_var[post_rbc == 1], na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    # Large ROA decline: fell more than median among complex CUs
    roa_decline = roa_post - roa_pre,
    # Computed after ungroup for cross-sectional median
  )

# Cross-sectional median ROA decline (among complex CUs only)
median_roa_decline <- median(
  df$roa_decline[df$complex == 1],
  na.rm = TRUE
)
df <- df |>
  mutate(
    high_roa_decline = as.integer(
      complex == 1 & roa_decline < median_roa_decline
    )
  )

# TEST 3: Institutions that raised rates the most
# Proxy: post-rule mean loan_to_asset vs pre-rule (higher lending concentration
# with rising rates = more rate-stressed borrowers)
# Better proxy: use spread data if available
spread_vars <- c("spread_mortgage", "spread_nauto", "spread_uauto")
spread_available <- intersect(spread_vars, names(df))

if (length(spread_available) > 0) {
  rate_intensity <- df |>
    filter(complex == 1) |>
    group_by(cu_number) |>
    summarise(
      avg_spread_post = mean(
        rowMeans(across(all_of(spread_available)),
                 na.rm = TRUE)[post_rbc == 1],
        na.rm = TRUE
      ),
      avg_spread_pre = mean(
        rowMeans(across(all_of(spread_available)),
                 na.rm = TRUE)[post_rbc == 0],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    mutate(delta_spread = avg_spread_post - avg_spread_pre)

  med_spread <- median(rate_intensity$delta_spread, na.rm = TRUE)

  df <- df |>
    left_join(
      rate_intensity |> select(cu_number, delta_spread),
      by = "cu_number"
    ) |>
    mutate(
      high_rate_increase = as.integer(
        complex == 1 & !is.na(delta_spread) & delta_spread > med_spread
      )
    )
  message(sprintf("  High rate-increase complex CUs: %d",
                  n_distinct(df$cu_number[df$high_rate_increase == 1])))
} else {
  df$delta_spread <- NA_real_
  df$high_rate_increase <- 0L
  message("  NOTE: Spread variables not available for rate-channel test")
}

df_es <- df |> filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX)

# =============================================================================
# 4. ESTIMATION FUNCTIONS
# =============================================================================

# Base controls (matching main paper)
ctrl_base     <- "ln_assets + loan_to_asset + cecl_adopter + dep_to_asset + inv_to_asset"
# With loan growth (denominator test)
ctrl_with_growth <- paste(ctrl_base, "loan_growth", sep = " + ")
# With portfolio composition (CRE mechanism test)
ctrl_with_portfolio <- paste(ctrl_base, "re_shr + auto_shr", sep = " + ")
# Full controls
ctrl_full <- paste(ctrl_base, "loan_growth + re_shr + auto_shr", sep = " + ")

run_did <- function(outcome, data = df, ctrl = ctrl_base) {
  fml <- as.formula(paste0(
    outcome, " ~ treat_post + ", ctrl, " | cu_number + q_period_num"
  ))
  tryCatch(
    feols(fml, data = data, cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) { warning(e$message); NULL }
  )
}

run_es <- function(outcome, data = df_es, ctrl = ctrl_base) {
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

safe_extract <- function(model, label, spec = "Baseline") {
  if (is.null(model)) {
    return(tibble(label = label, spec = spec,
                  beta = NA_real_, se = NA_real_,
                  ci_lo = NA_real_, ci_hi = NA_real_,
                  p_value = NA_real_, stars = "", n_obs = NA_integer_))
  }
  tryCatch({
    td  <- tidy(model, conf.int = TRUE)
    row <- td[td$term == "treat_post", ]
    if (nrow(row) == 0) return(tibble(label = label, spec = spec,
                                       beta = NA_real_, se = NA_real_,
                                       ci_lo = NA_real_, ci_hi = NA_real_,
                                       p_value = NA_real_, stars = "",
                                       n_obs = nobs(model)))
    tibble(
      label   = label, spec = spec,
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
  }, error = function(e) tibble(label = label, spec = spec,
                                 beta = NA_real_, se = NA_real_,
                                 ci_lo = NA_real_, ci_hi = NA_real_,
                                 p_value = NA_real_, stars = "",
                                 n_obs = NA_integer_))
}

extract_es_coefs <- function(model, label) {
  if (is.null(model)) return(tibble())
  tryCatch({
    coefs <- tidy(model, conf.int = TRUE) |>
      filter(str_detect(term, "event_time::")) |>
      mutate(event_time = as.integer(str_extract(term, "-?\\d+"))) |>
      filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX_PAPER)
    ref <- tibble(term="ref", estimate=0, std.error=0,
                  conf.low=0, conf.high=0, p.value=NA_real_,
                  event_time=EVENT_REF)
    bind_rows(coefs, ref) |>
      arrange(event_time) |>
      mutate(label = label,
             period = if_else(event_time < 0, "Pre-RBC", "Post-RBC"))
  }, error = function(e) tibble())
}

# =============================================================================
# 5. TEST 1 — DENOMINATOR SHRINKAGE
# =============================================================================

message("\n-- TEST 1: Denominator shrinkage ------------------------------------")

# 1a: DQ rate DiD — baseline (already known: +8bp)
m1_rate_base  <- run_did("dq_rate_var", ctrl = ctrl_base)

# 1b: DQ rate DiD — controlling for loan growth
#     If beta drops when we add loan_growth: denominator shrinkage explains part
m1_rate_ctrl  <- run_did("dq_rate_var", ctrl = ctrl_with_growth)

# 1c: Dollar-level DQ DiD (log delinquent dollar amount)
#     If beta is NEGATIVE or ZERO: the rate increase is PURELY a denominator artifact
#     If beta is POSITIVE: delinquent dollar amounts genuinely rose
m1_dollar     <- if (sum(!is.na(df$ln_dq_dollar)) > 100) {
  run_did("ln_dq_dollar", ctrl = ctrl_base)
} else { NULL }

# 1d: Dollar-level DQ event study
es1_rate   <- run_es("dq_rate_var")
es1_dollar <- if (!is.null(m1_dollar)) run_es("ln_dq_dollar") else NULL

r1 <- bind_rows(
  safe_extract(m1_rate_base,  "DQ Rate",         "Baseline"),
  safe_extract(m1_rate_ctrl,  "DQ Rate",         "+Loan growth ctrl"),
  safe_extract(m1_dollar,     "log(DQ dollars)", "Baseline")
)

cat("\n=== TEST 1: DENOMINATOR SHRINKAGE ===\n")
cat("Key question: does DQ rate rise because denominator (loans) shrank?\n")
cat("If dollar-level DiD is near zero while rate DiD is positive => denominator effect.\n")
cat("If controlling for loan_growth kills the rate DiD => confirmed denominator effect.\n\n")
print(r1 |> select(label, spec, beta, se, stars, p_value), n = Inf)

# Pct change in coefficient when adding loan growth control
if (!is.na(r1$beta[1]) && !is.na(r1$beta[2]) && r1$beta[1] != 0) {
  pct_change <- (r1$beta[2] - r1$beta[1]) / abs(r1$beta[1]) * 100
  cat(sprintf(
    "\n  DQ rate coefficient change when controlling for loan growth: %+.1f%%\n",
    pct_change
  ))
  if (abs(pct_change) < 20) {
    cat("  INTERPRETATION: Denominator shrinkage explains LITTLE of the DQ increase.\n")
  } else if (abs(pct_change) < 50) {
    cat("  INTERPRETATION: Denominator shrinkage explains SOME of the DQ increase.\n")
  } else {
    cat("  INTERPRETATION: Denominator shrinkage explains MUCH of the DQ increase.\n")
  }
}

# =============================================================================
# 6. TEST 2 — PORTFOLIO COMPOSITION CONTROL
# =============================================================================

message("\n-- TEST 2: Portfolio composition control ----------------------------")

m2_base      <- run_did("dq_rate_var", ctrl = ctrl_base)
m2_portfolio <- run_did("dq_rate_var", ctrl = ctrl_with_portfolio)
m2_full      <- run_did("dq_rate_var", ctrl = ctrl_full)

r2 <- bind_rows(
  safe_extract(m2_base,      "DQ Rate", "Baseline"),
  safe_extract(m2_portfolio, "DQ Rate", "+RE & auto share ctrl"),
  safe_extract(m2_full,      "DQ Rate", "+Loan growth + portfolio")
)

cat("\n=== TEST 2: PORTFOLIO COMPOSITION ===\n")
cat("Key question: does DQ rise disappear after controlling for RE/auto shares?\n\n")
print(r2 |> select(label, spec, beta, se, stars, p_value), n = Inf)

if (!is.na(r2$beta[1]) && !is.na(r2$beta[2]) && r2$beta[1] != 0) {
  pct2 <- (r2$beta[2] - r2$beta[1]) / abs(r2$beta[1]) * 100
  cat(sprintf(
    "\n  DQ rate coefficient change when controlling for portfolio: %+.1f%%\n",
    pct2
  ))
}

# =============================================================================
# 7. TEST 3 — RATE CHANNEL (borrower stress from higher rates)
# =============================================================================

message("\n-- TEST 3: Rate channel ---------------------------------------------")

if (any(df$high_rate_increase == 1, na.rm = TRUE)) {
  df_hi_rate <- df |> filter(high_rate_increase == 1 | complex == 0)
  df_lo_rate <- df |> filter((complex == 1 & high_rate_increase == 0) | complex == 0)

  m3_hi <- run_did("dq_rate_var", data = df_hi_rate)
  m3_lo <- run_did("dq_rate_var", data = df_lo_rate)

  r3 <- bind_rows(
    safe_extract(m3_hi, "DQ Rate", "High rate-increase CUs"),
    safe_extract(m3_lo, "DQ Rate", "Low rate-increase CUs")
  )

  cat("\n=== TEST 3: RATE CHANNEL ===\n")
  cat("Key question: did DQ rise more for CUs that raised rates the most?\n\n")
  print(r3 |> select(spec, beta, se, stars, p_value), n = Inf)
} else {
  r3 <- tibble()
  cat("\n=== TEST 3: Rate channel — insufficient spread data. Skipping.\n")
}

# =============================================================================
# 8. TEST 4 — CAPACITY CHANNEL (ROA decline = reduced workout capacity)
# =============================================================================

message("\n-- TEST 4: Capacity channel -----------------------------------------")

if (sum(df$high_roa_decline == 1, na.rm = TRUE) > 500) {
  df_hi_roa_dec <- df |> filter(high_roa_decline == 1 | complex == 0)
  df_lo_roa_dec <- df |> filter((complex == 1 & high_roa_decline == 0) | complex == 0)

  m4_hi <- run_did("dq_rate_var", data = df_hi_roa_dec)
  m4_lo <- run_did("dq_rate_var", data = df_lo_roa_dec)

  r4 <- bind_rows(
    safe_extract(m4_hi, "DQ Rate", "High ROA decline (most squeezed)"),
    safe_extract(m4_lo, "DQ Rate", "Low ROA decline (least squeezed)")
  )

  cat("\n=== TEST 4: CAPACITY CHANNEL ===\n")
  cat("Key question: did DQ rise more for CUs whose profitability fell most?\n")
  cat("Higher ROA decline = less capacity to work out troubled loans.\n\n")
  print(r4 |> select(spec, beta, se, stars, p_value), n = Inf)
} else {
  r4 <- tibble()
  cat("\n=== TEST 4: Capacity channel — insufficient data. Skipping.\n")
}

# =============================================================================
# 9. CHART 3H1 — COEFFICIENT COMPARISON ACROSS SPECIFICATIONS
# =============================================================================

message("\n-- Building charts --------------------------------------------------")

# Compile all specification results
all_specs <- bind_rows(
  # Baseline
  safe_extract(m1_rate_base, "DQ Rate", "1. Baseline DiD"),
  # Denominator tests
  safe_extract(m1_rate_ctrl,  "DQ Rate", "2. +Loan growth control"),
  safe_extract(m1_dollar,     "log(DQ $)", "3. Dollar-level DQ (log)"),
  # Portfolio test
  safe_extract(m2_portfolio,  "DQ Rate", "4. +Portfolio composition ctrl"),
  safe_extract(m2_full,       "DQ Rate", "5. +Loan growth + portfolio"),
  # Subgroup tests (high intensity only)
  if (nrow(r3) > 0)
    r3 |> filter(str_detect(spec, "High rate")) |>
      mutate(label = "DQ Rate", spec = "6. High rate-increase CUs only")
  else NULL,
  if (nrow(r4) > 0)
    r4 |> filter(str_detect(spec, "High ROA")) |>
      mutate(label = "DQ Rate", spec = "7. High ROA-decline CUs only")
  else NULL
) |>
  filter(!is.na(beta)) |>
  mutate(
    spec   = factor(spec, levels = rev(unique(spec))),
    sig    = p_value < 0.10,
    color  = case_when(
      beta > 0 & sig  ~ "Significant increase",
      beta < 0 & sig  ~ "Significant decrease",
      TRUE            ~ "Not significant"
    )
  )

write_csv(all_specs, file.path(TABLE_PATH, "3H_dq_mechanism_tests.csv"))

# Reference line = baseline estimate
baseline_beta <- all_specs$beta[all_specs$spec == "1. Baseline DiD"][1]

p3h1 <- ggplot(all_specs,
               aes(x = beta * 100, y = spec, color = color)) +
  # Baseline reference band
  annotate("rect",
           xmin = (baseline_beta - all_specs$se[1]) * 100,
           xmax = (baseline_beta + all_specs$se[1]) * 100,
           ymin = -Inf, ymax = Inf,
           fill = "gray85", alpha = 0.4) +
  geom_vline(xintercept = 0, color = COL_ZERO,
             linewidth = 0.7, linetype = "dashed") +
  geom_vline(xintercept = baseline_beta * 100,
             color = COL_COMPLEX, linewidth = 0.9,
             linetype = "dotted") +
  geom_errorbarh(aes(xmin = ci_lo * 100, xmax = ci_hi * 100),
                 height = 0.3, linewidth = 0.8) +
  geom_point(size = 4) +
  geom_text(
    aes(label = paste0(sprintf("%+.2f", beta * 100), "bp", stars)),
    hjust = -0.25, size = 3.2, fontface = "bold"
  ) +
  facet_wrap(~label, scales = "free_x", ncol = 2) +
  scale_color_manual(values = c(
    "Significant increase" = COL_CONCERN,
    "Significant decrease" = COL_POSITIVE,
    "Not significant"      = "gray50"
  )) +
  scale_x_continuous(labels = function(x) paste0(x, "bp")) +
  labs(
    title    = "Why Did Delinquency Increase? Coefficient Stability Across Specifications",
    subtitle = paste0(
      "Each row adds or changes a control to isolate the mechanism.\n",
      "Dotted blue line = baseline estimate. Gray band = baseline +-1 SE.\n",
      "If the coefficient is stable across specs: the mechanism is NOT that control variable.\n",
      "If it drops substantially: that control variable explains the DQ increase."
    ),
    x = "DiD Coefficient (basis points)", y = NULL, color = NULL,
    caption = paste0(
      "Two-way FE (CU + quarter-year). SE clustered at CU. ",
      "Baseline controls: ln_assets + loan_to_asset + cecl_adopter + dep_to_asset + inv_to_asset.\n",
      "Dollar-level DQ = log(dq_rate/100 * lns_tot). *** p<0.01, ** p<0.05, * p<0.10."
    )
  ) +
  theme_rbc() +
  theme(legend.position = "bottom")

ggsave(file.path(FIGURE_PATH, "policy_3h1_dq_mechanism_decomp.png"),
       p3h1, width = 14, height = 9, dpi = 300)
message("  Chart 3H1 saved.")

# =============================================================================
# 10. CHART 3H2 — EVENT STUDIES: RATE vs. DOLLAR-LEVEL DQ
# =============================================================================

es_rate_coefs   <- extract_es_coefs(es1_rate,   "DQ Rate (pp)")
es_dollar_coefs <- extract_es_coefs(es1_dollar, "log(DQ Dollar Amount)")

plot_es_panel <- function(coefs, label, color, y_lab) {
  if (is.null(coefs) || nrow(coefs) == 0) return(plot_spacer())
  d <- coefs |> filter(label == !!label)
  if (nrow(d) == 0) return(plot_spacer())

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
      title    = label,
      subtitle = "Event time vs. RBC (Q0=2022Q1). Ref=Q-1. 95% CI shaded.",
      x = "Quarters Relative to RBC", y = y_lab
    ) +
    theme_rbc()
}

pA <- plot_es_panel(es_rate_coefs,   "DQ Rate (pp)",          COL_COMPLEX,
                    "Coefficient (DQ Rate, pp)")
pB <- plot_es_panel(es_dollar_coefs, "log(DQ Dollar Amount)", COL_CONCERN,
                    "Coefficient (log DQ dollars)")

p3h2 <- (pA + pB) +
  plot_annotation(
    title   = "DQ Rate vs. Dollar-Level Delinquency: Is the Rate Rise Real?",
    subtitle = paste0(
      "Panel A: DQ rate (delinquent $ / total loan $) — what Finding 5 measures.\n",
      "Panel B: log(delinquent dollar amount) — removes the denominator entirely.\n",
      "If Panel B also rises post-rule: credit quality genuinely deteriorated (not a denominator artifact).\n",
      "If Panel B is flat while Panel A rises: the DQ rate increase is driven by the shrinking loan book."
    ),
    caption = "Two-way FE (CU + quarter-year). SE clustered at CU. Reference = Q-1."
  )

ggsave(file.path(FIGURE_PATH, "policy_3h2_dollar_dq_test.png"),
       p3h2, width = 14, height = 6, dpi = 300)
message("  Chart 3H2 saved.")

# =============================================================================
# 11. CHART 3H3 — RATE CHANNEL
# =============================================================================

if (nrow(r3) > 0) {
  p3h3 <- ggplot(r3, aes(x = beta * 100, y = spec, color = spec)) +
    geom_vline(xintercept = 0, color = COL_ZERO,
               linewidth = 0.7, linetype = "dashed") +
    geom_errorbarh(aes(xmin = ci_lo * 100, xmax = ci_hi * 100),
                   height = 0.2, linewidth = 1.0) +
    geom_point(size = 5) +
    geom_text(
      aes(label = paste0(sprintf("%+.2f", beta * 100), "bp", stars)),
      hjust = -0.3, size = 3.8, fontface = "bold"
    ) +
    scale_color_manual(
      values = c(
        "High rate-increase CUs" = COL_CONCERN,
        "Low rate-increase CUs"  = COL_POSITIVE
      ), guide = "none"
    ) +
    scale_x_continuous(
      limits = function(x) c(min(x) - 1, max(x) + 3),
      labels = function(x) paste0(x, "bp")
    ) +
    labs(
      title    = "Rate Channel: DQ Rose More Where Loan Rates Rose Most",
      subtitle = paste0(
        "Complex CUs split by whether their loan rate spreads rose above or below the median.\n",
        "If the rate channel is operative: institutions that raised rates most should show",
        " the largest DQ increases.\n",
        "This would confirm the mechanism: rule -> higher rates -> borrower stress -> delinquency."
      ),
      x = "DiD Coefficient -- DQ Rate (basis points)", y = NULL,
      caption = "Two-way FE. SE clustered at CU. *** p<0.01, ** p<0.05, * p<0.10."
    ) +
    theme_rbc()

  ggsave(file.path(FIGURE_PATH, "policy_3h3_rate_channel.png"),
         p3h3, width = 11, height = 5, dpi = 300)
  message("  Chart 3H3 saved.")
}

# =============================================================================
# 12. CHART 3H4 — CAPACITY CHANNEL
# =============================================================================

if (nrow(r4) > 0) {
  p3h4 <- ggplot(r4, aes(x = beta * 100, y = spec, color = spec)) +
    geom_vline(xintercept = 0, color = COL_ZERO,
               linewidth = 0.7, linetype = "dashed") +
    geom_errorbarh(aes(xmin = ci_lo * 100, xmax = ci_hi * 100),
                   height = 0.2, linewidth = 1.0) +
    geom_point(size = 5) +
    geom_text(
      aes(label = paste0(sprintf("%+.2f", beta * 100), "bp", stars)),
      hjust = -0.3, size = 3.8, fontface = "bold"
    ) +
    scale_color_manual(
      values = setNames(
        c(COL_CONCERN, COL_POSITIVE),
        r4$spec
      ), guide = "none"
    ) +
    scale_x_continuous(
      limits = function(x) c(min(x) - 1, max(x) + 3),
      labels = function(x) paste0(x, "bp")
    ) +
    labs(
      title    = "Capacity Channel: DQ Rose More Where Profitability Fell Most",
      subtitle = paste0(
        "Complex CUs split by ROA decline (post minus pre-rule ROA).\n",
        "Institutions whose earnings fell most had the least capacity to work out",
        " troubled loans proactively.\n",
        "If capacity channel is operative: high-ROA-decline CUs should show larger DQ increases."
      ),
      x = "DiD Coefficient -- DQ Rate (basis points)", y = NULL,
      caption = "Two-way FE. SE clustered at CU. *** p<0.01, ** p<0.05, * p<0.10."
    ) +
    theme_rbc()

  ggsave(file.path(FIGURE_PATH, "policy_3h4_capacity_channel.png"),
         p3h4, width = 11, height = 5, dpi = 300)
  message("  Chart 3H4 saved.")
}

# =============================================================================
# 13. MECHANISM INTERPRETATION SUMMARY
# =============================================================================

cat("\n")
cat("================================================================\n")
cat("  3H: DQ MECHANISM TESTS -- RESULTS SUMMARY\n")
cat("================================================================\n\n")

cat("BASELINE: DQ rate DiD = ")
cat(sprintf("%+.3fpp%s\n\n", baseline_beta, all_specs$stars[1]))

cat("TEST 1 -- DENOMINATOR SHRINKAGE (size/loan-contraction hypothesis):\n")
if (!is.na(r1$beta[2])) {
  pct <- (r1$beta[2] - r1$beta[1]) / abs(r1$beta[1]) * 100
  cat(sprintf("  DQ rate with loan_growth control: %+.3fpp%s (%+.0f%% vs baseline)\n",
              r1$beta[2], r1$stars[2], pct))
}
if (!is.null(m1_dollar) && nrow(r1) >= 3 && !is.na(r1$beta[3])) {
  cat(sprintf("  Dollar-level DQ DiD: %+.3f log-points%s\n",
              r1$beta[3], r1$stars[3]))
  if (r1$beta[3] > 0 && !is.na(r1$p_value[3]) && r1$p_value[3] < 0.10) {
    cat("  => Delinquent dollar amounts ROSE => DQ increase is REAL, not denominator\n")
  } else {
    cat("  => Delinquent dollar amounts FLAT/FELL => DQ rate increase is a DENOMINATOR ARTIFACT\n")
  }
}

cat("\nTEST 2 -- PORTFOLIO COMPOSITION CHANNEL:\n")
if (nrow(r2) >= 2 && !is.na(r2$beta[2])) {
  pct2 <- (r2$beta[2] - r2$beta[1]) / abs(r2$beta[1]) * 100
  cat(sprintf("  DQ rate with portfolio controls: %+.3fpp%s (%+.0f%% vs baseline)\n",
              r2$beta[2], r2$stars[2], pct2))
}

if (nrow(r3) > 0 && !is.na(r3$beta[1])) {
  cat("\nTEST 3 -- RATE CHANNEL:\n")
  cat(sprintf("  High rate-increase CUs: %+.3fpp%s\n",
              r3$beta[1], r3$stars[1]))
  cat(sprintf("  Low  rate-increase CUs: %+.3fpp%s\n",
              r3$beta[2], r3$stars[2]))
}

if (nrow(r4) > 0 && !is.na(r4$beta[1])) {
  cat("\nTEST 4 -- CAPACITY CHANNEL:\n")
  cat(sprintf("  High ROA decline CUs: %+.3fpp%s\n",
              r4$beta[1], r4$stars[1]))
  cat(sprintf("  Low  ROA decline CUs: %+.3fpp%s\n",
              r4$beta[2], r4$stars[2]))
}

cat("\nFigures saved to output/figures/:\n")
for (f in c("policy_3h1_dq_mechanism_decomp.png",
            "policy_3h2_dollar_dq_test.png",
            "policy_3h3_rate_channel.png",
            "policy_3h4_capacity_channel.png")) {
  flag <- if (file.exists(file.path(FIGURE_PATH, f))) "[OK]" else "[--]"
  cat(sprintf("  %s %s\n", flag, f))
}
cat("================================================================\n")
