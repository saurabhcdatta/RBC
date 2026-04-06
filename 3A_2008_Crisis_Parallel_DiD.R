# =============================================================================
# 3A_2008_Crisis_Parallel_DiD.R
# RBC Rule Analysis — 2008 Crisis Parallel DiD Comparison
# NCUA Call Report (5300) Data
#
# Author  : [Your Name]
# Created : 2026
#
# PURPOSE:
#   Tests whether the behavioral responses observed post-RBC (2022) reflect
#   regulatory-specific mechanisms or simply how large CUs respond to ANY
#   capital stress event. Uses the 2008 financial crisis as a comparator.
#
# CENTRAL QUESTION:
#   Are the post-RBC DiD coefficients statistically different from the
#   equivalent DiD coefficients estimated around the 2008 crisis?
#   - If NOT different → RBC effects = generic capital stress response
#   - If DIFFERENT     → RBC has regulatory-specific behavioral channels
#
# DESIGN:
#   Identical 2x2 DiD specification applied to two separate windows:
#     Window 1 (Crisis)  : 2004q1 – 2013q4, treatment = 2008q3 (Lehman)
#     Window 2 (RBC)     : 2018q1 – 2024q4, treatment = 2022q1
#   Treatment group: CUs with avg assets > SIZE_THRESHOLD in pre-period
#   Control group  : CUs with avg assets < SIZE_THRESHOLD in pre-period
#
# KEY OUTPUT:
#   - Side-by-side coefficient tables: crisis vs. RBC
#   - Wald test: H0: beta_crisis = beta_rbc
#   - Combined event study plots (crisis vs. RBC on same axes)
#   - Interpretation guide
#
# Input  : call_report.rds  (full raw data, back to 2000)
# Output : output/tables/3A_*
#          output/figures/3A_*
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(fixest)
library(modelsummary)
library(patchwork)
library(scales)
library(broom)
library(lubridate)

# install.packages(c("fixest", "modelsummary", "broom"))


# =============================================================================
# 1. USER SETTINGS
# =============================================================================

RAW_DATA_PATH <- "call_report.rds"     # full raw panel back to 2000
TABLE_PATH    <- "output/tables/"
FIGURE_PATH   <- "output/figures/"

# ── Crisis window settings ───────────────────────────────────────────────────
# Treatment date: 2008q3 (Lehman Brothers collapse, Sep 2008)
# Alternative candidates: 2008q4 (TARP), 2009q1 (peak stress)
CRISIS_DATE      <- 2008.3             # 2008 Q3
CRISIS_START     <- 2004.1             # 4 years pre-crisis
CRISIS_END       <- 2013.4             # 5 years post-crisis

# ── RBC window settings (must match 2_DiD_Estimation.R) ─────────────────────
RBC_DATE         <- 2022.1
RBC_START        <- 2018.1
RBC_END          <- 2024.4

# ── Size threshold ───────────────────────────────────────────────────────────
# We apply the same $500M threshold in both windows.
# NOTE: $500M in 2008 ≈ $700M in 2022 dollars (CPI adjustment).
# We run a sensitivity check with inflation-adjusted threshold.
SIZE_THRESHOLD        <- 500e6         # nominal $500M (primary)
SIZE_THRESHOLD_ADJ    <- 350e6         # ~$500M in 2022 dollars, deflated to 2008

# Pre-period for treatment classification (4 quarters before each event)
CRISIS_PRE_START <- 2007.3
CRISIS_PRE_END   <- 2008.2
RBC_PRE_START    <- 2021.1
RBC_PRE_END      <- 2021.4

# Event study window (quarters relative to treatment)
EVENT_MIN <- -12
EVENT_MAX <-  10
EVENT_REF <-  -1

# Colors
COL_CRISIS  <- "#C94040"    # red — crisis
COL_RBC     <- "#1B3A6B"    # navy — RBC
COL_CI      <- "#AAAAAA"    # CI band

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


# =============================================================================
# 2. LOAD & PREPARE DATA
# =============================================================================

message("── Step 1: Loading raw data ──────────────────────────────────────────")

df_raw <- readRDS(RAW_DATA_PATH)

dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)

message(sprintf("  Raw data: %s obs, period %s to %s",
                scales::comma(nrow(df_raw)),
                min(df_raw$q_period_num, na.rm = TRUE),
                max(df_raw$q_period_num, na.rm = TRUE)))

# Drop merged/acquired CUs (same logic as 0_Data_Prep.R)
merged_cus <- df_raw |>
  filter(outcome %in% c(2, 3)) |>
  pull(cu_number) |> unique()

df_clean <- df_raw |>
  filter(
    !cu_number %in% merged_cus,
    is.na(acquiredcu) | acquiredcu == 0
  ) |>
  mutate(
    # Net worth ratio (primary capital measure — available back to 2000)
    networth_ratio = if_else(
      assets_tot > 0,
      networth_tot / assets_tot * 100,
      NA_real_
    ),
    cap_buffer    = networth_ratio - 10,

    # Loan portfolio shares
    lns_mbl_use   = coalesce(lns_mbl, lns_mbl_part723),
    mbl_shr       = if_else(lns_tot > 0,
                            lns_mbl_use / lns_tot * 100, NA_real_),
    re_shr        = if_else(lns_tot > 0,
                            lns_re / lns_tot * 100, NA_real_),
    auto_shr      = if_else(lns_tot > 0,
                            lns_auto / lns_tot * 100, NA_real_),

    # Growth (computed after sort)
    ln_assets     = log(assets_tot),
    ln_loans      = log(lns_tot + 1),

    # Credit quality
    dq_rate_var   = dq_rate,
    chgoff_ratio  = chg_tot_ratio,
    pll_assets    = if_else(assets_tot > 0,
                            pll_pcl / assets_tot * 100, NA_real_),

    # Profitability
    roa_var       = roa,
    nim           = netintmrg,

    # Balance sheet
    loan_to_asset = if_else(assets_tot > 0,
                            lns_tot / assets_tot * 100, NA_real_),
    cecl_adopter  = as.integer(!is.na(cecl) & cecl == 1)
  ) |>
  arrange(cu_number, q_period_num) |>
  group_by(cu_number) |>
  mutate(
    asset_growth = ln_assets - lag(ln_assets),
    loan_growth  = ln_loans  - lag(ln_loans)
  ) |>
  ungroup()

message(sprintf("  Clean data: %s obs, %s CUs",
                scales::comma(nrow(df_clean)),
                scales::comma(n_distinct(df_clean$cu_number))))


# =============================================================================
# 3. BUILD CRISIS WINDOW PANEL
# =============================================================================

message("── Step 2: Building crisis window panel ──────────────────────────────")

# ── Classify treatment group for 2008 crisis ────────────────────────────────
crisis_treatment <- df_clean |>
  filter(q_period_num >= CRISIS_PRE_START,
         q_period_num <= CRISIS_PRE_END) |>
  group_by(cu_number) |>
  summarise(
    avg_assets_pre_crisis = mean(assets_tot, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    complex_crisis     = as.integer(avg_assets_pre_crisis >= SIZE_THRESHOLD),
    complex_crisis_adj = as.integer(avg_assets_pre_crisis >= SIZE_THRESHOLD_ADJ)
  )

message(sprintf("  Complex CUs 2008 (nominal $500M): %d",
                sum(crisis_treatment$complex_crisis, na.rm = TRUE)))
message(sprintf("  Complex CUs 2008 (adj. $350M)   : %d",
                sum(crisis_treatment$complex_crisis_adj, na.rm = TRUE)))

# ── Build crisis panel ───────────────────────────────────────────────────────
df_crisis <- df_clean |>
  filter(q_period_num >= CRISIS_START,
         q_period_num <= CRISIS_END) |>
  inner_join(crisis_treatment |>
               select(cu_number, avg_assets_pre_crisis,
                      complex_crisis, complex_crisis_adj),
             by = "cu_number") |>
  filter(!is.na(complex_crisis)) |>
  mutate(
    post_crisis  = as.integer(q_period_num >= CRISIS_DATE),
    treat_post   = complex_crisis * post_crisis,

    # Event time relative to 2008q3
    event_time = case_when(
      quarter == 1 ~ (year - 2008L) * 4 - 2L,
      quarter == 2 ~ (year - 2008L) * 4 - 1L,
      quarter == 3 ~ (year - 2008L) * 4 + 0L,
      quarter == 4 ~ (year - 2008L) * 4 + 1L
    ),
    window = "Crisis (2008)"
  )

# ── Require minimum observations ────────────────────────────────────────────
cu_obs_crisis <- df_crisis |>
  count(cu_number, name = "n_q")
thin_crisis <- cu_obs_crisis |>
  filter(n_q < 8) |> pull(cu_number)
df_crisis <- df_crisis |>
  filter(!cu_number %in% thin_crisis)

message(sprintf("  Crisis panel: %s obs, %s CUs (%d complex, %d control)",
                scales::comma(nrow(df_crisis)),
                scales::comma(n_distinct(df_crisis$cu_number)),
                sum(df_crisis$complex_crisis[
                  !duplicated(df_crisis$cu_number)], na.rm = TRUE),
                sum(df_crisis$complex_crisis[
                  !duplicated(df_crisis$cu_number)] == 0, na.rm = TRUE)))


# =============================================================================
# 4. BUILD RBC WINDOW PANEL (re-constructed from raw for consistency)
# =============================================================================

message("── Step 3: Building RBC window panel (consistent spec) ───────────────")

rbc_treatment <- df_clean |>
  filter(q_period_num >= RBC_PRE_START,
         q_period_num <= RBC_PRE_END) |>
  group_by(cu_number) |>
  summarise(
    avg_assets_pre_rbc = mean(assets_tot, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    complex_rbc = as.integer(avg_assets_pre_rbc >= SIZE_THRESHOLD)
  )

df_rbc <- df_clean |>
  filter(q_period_num >= RBC_START,
         q_period_num <= RBC_END) |>
  inner_join(rbc_treatment |>
               select(cu_number, avg_assets_pre_rbc, complex_rbc),
             by = "cu_number") |>
  filter(!is.na(complex_rbc)) |>
  mutate(
    post_rbc   = as.integer(q_period_num >= RBC_DATE),
    treat_post = complex_rbc * post_rbc,

    # Event time relative to 2022q1
    event_time = case_when(
      quarter == 1 ~ (year - 2022L) * 4 + 0L,
      quarter == 2 ~ (year - 2022L) * 4 + 1L,
      quarter == 3 ~ (year - 2022L) * 4 + 2L,
      quarter == 4 ~ (year - 2022L) * 4 + 3L
    ),
    window = "RBC Rule (2022)"
  )

cu_obs_rbc <- df_rbc |> count(cu_number, name = "n_q")
thin_rbc   <- cu_obs_rbc |> filter(n_q < 8) |> pull(cu_number)
df_rbc     <- df_rbc |> filter(!cu_number %in% thin_rbc)

message(sprintf("  RBC panel: %s obs, %s CUs (%d complex, %d control)",
                scales::comma(nrow(df_rbc)),
                scales::comma(n_distinct(df_rbc$cu_number)),
                sum(df_rbc$complex_rbc[
                  !duplicated(df_rbc$cu_number)], na.rm = TRUE),
                sum(df_rbc$complex_rbc[
                  !duplicated(df_rbc$cu_number)] == 0, na.rm = TRUE)))


# =============================================================================
# 5. PARALLEL DiD ESTIMATION — IDENTICAL SPEC ON BOTH WINDOWS
# =============================================================================

message("── Step 4: Parallel DiD estimation ──────────────────────────────────")

# Outcomes available in both windows (pre-2022 variables only)
outcomes <- list(
  list("networth_ratio", "Net Worth Ratio (%)"),
  list("cap_buffer",     "Capital Buffer vs 10% (pp)"),
  list("loan_growth",    "Loan Growth (QoQ log)"),
  list("asset_growth",   "Asset Growth (QoQ log)"),
  list("mbl_shr",        "MBL Share (%)"),
  list("re_shr",         "RE Share (%)"),
  list("auto_shr",       "Auto Share (%)"),
  list("dq_rate_var",    "Delinquency Rate (%)"),
  list("chgoff_ratio",   "Charge-Off Ratio (%)"),
  list("pll_assets",     "PLL / Assets (%)"),
  list("roa_var",        "ROA (%)"),
  list("nim",            "Net Interest Margin (%)")
)

controls_base <- "ln_assets + loan_to_asset"
# NOTE: cecl_adopter not used for crisis window (CECL didn't exist in 2008)
# This makes the comparison cleaner — same controls in both windows

run_did_window <- function(outcome, data, complex_var) {
  fml <- as.formula(
    paste0(outcome,
           " ~ treat_post + ", controls_base,
           " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml, data = data, cluster = ~cu_number,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
}

# Extract coefficient helper
extract_coef <- function(model, label, window, note = "") {
  if (is.null(model)) {
    return(tibble(Outcome = label, Window = window,
                  Beta = NA, SE = NA, CI_low = NA, CI_high = NA,
                  p_value = NA, Stars = "", N_obs = NA, Note = note))
  }
  tryCatch({
    tidy_m <- tidy(model, conf.int = TRUE)
    row    <- tidy_m[tidy_m$term == "treat_post", ]
    tibble(
      Outcome = label, Window = window,
      Beta    = round(row$estimate,  3),
      SE      = round(row$std.error, 3),
      CI_low  = round(row$conf.low,  3),
      CI_high = round(row$conf.high, 3),
      p_value = round(row$p.value,   3),
      Stars   = case_when(
        row$p.value < 0.01 ~ "***",
        row$p.value < 0.05 ~ "**",
        row$p.value < 0.10 ~ "*",
        TRUE               ~ ""
      ),
      N_obs   = nobs(model),
      Note    = note
    )
  }, error = function(e) {
    tibble(Outcome = label, Window = window,
           Beta = NA, SE = NA, CI_low = NA, CI_high = NA,
           p_value = NA, Stars = "", N_obs = NA, Note = note)
  })
}

# Run both windows for all outcomes
message("  Estimating crisis DiD models...")
crisis_models <- map(outcomes, function(o) {
  run_did_window(o[[1]], df_crisis, "complex_crisis")
})

message("  Estimating RBC DiD models...")
rbc_models <- map(outcomes, function(o) {
  run_did_window(o[[1]], df_rbc, "complex_rbc")
})

# Build comparison table
parallel_results <- bind_rows(
  map2_dfr(crisis_models, outcomes, function(m, o) {
    extract_coef(m, o[[2]], "Crisis (2008q3)")
  }),
  map2_dfr(rbc_models, outcomes, function(m, o) {
    extract_coef(m, o[[2]], "RBC Rule (2022q1)")
  })
)

cat("\n=== PARALLEL DiD: CRISIS vs. RBC ===\n")
parallel_results |>
  mutate(Result = paste0(
    ifelse(is.na(Beta), "NA", paste0(Beta, Stars)),
    " (", ifelse(is.na(SE), "NA", SE), ")"
  )) |>
  select(Outcome, Window, Result, p_value, N_obs) |>
  pivot_wider(names_from = Window,
              values_from = c(Result, p_value, N_obs)) |>
  print(n = Inf)

write_csv(parallel_results,
          file.path(TABLE_PATH, "3A_parallel_did_crisis_vs_rbc.csv"))
message("  Saved → output/tables/3A_parallel_did_crisis_vs_rbc.csv")


# =============================================================================
# 6. WALD TEST — H0: beta_crisis = beta_rbc
# =============================================================================
# Tests whether the treatment effects are statistically distinguishable.
# Method: Stack both windows with a window indicator and test interaction.

message("── Step 5: Wald test for equality of coefficients ────────────────────")

# Stack the two panels with a window flag
# Rename complex and treat_post to common names for stacking
df_crisis_stack <- df_crisis |>
  mutate(
    complex    = complex_crisis,
    post       = post_crisis,
    treat_post = complex_crisis * post_crisis,
    is_rbc     = 0L
  ) |>
  select(cu_number, q_period_num, year, quarter,
         complex, post, treat_post, is_rbc,
         all_of(map_chr(outcomes, 1)),
         ln_assets, loan_to_asset)

df_rbc_stack <- df_rbc |>
  mutate(
    complex    = complex_rbc,
    post       = post_rbc,
    treat_post = complex_rbc * post_rbc,
    is_rbc     = 1L
  ) |>
  select(cu_number, q_period_num, year, quarter,
         complex, post, treat_post, is_rbc,
         all_of(map_chr(outcomes, 1)),
         ln_assets, loan_to_asset)

# To avoid CU fixed effect collision between windows,
# create unique CU-window IDs
df_stacked <- bind_rows(
  df_crisis_stack |> mutate(cu_window = paste0(cu_number, "_crisis")),
  df_rbc_stack    |> mutate(cu_window = paste0(cu_number, "_rbc"))
)

# Stacked model with interaction:
# Y = alpha_i + gamma_t + beta1*treat_post +
#     beta2*(treat_post x is_rbc) + ...
# beta2 = difference in DiD coefficients (crisis vs RBC)

wald_results <- map_dfr(outcomes, function(o) {
  fml <- as.formula(
    paste0(o[[1]],
           " ~ treat_post + treat_post:is_rbc + is_rbc + ",
           controls_base,
           " | cu_window + q_period_num")
  )
  m <- tryCatch(
    feols(fml, data = df_stacked, cluster = ~cu_window,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(m)) return(NULL)

  tidy_m <- tidy(m, conf.int = TRUE)

  # Base effect (crisis)
  base_row  <- tidy_m[tidy_m$term == "treat_post", ]
  # Difference: RBC - crisis
  diff_row  <- tidy_m[tidy_m$term == "treat_post:is_rbc", ]

  if (nrow(base_row) == 0 | nrow(diff_row) == 0) return(NULL)

  tibble(
    Outcome          = o[[2]],
    Beta_crisis      = round(base_row$estimate, 3),
    SE_crisis        = round(base_row$std.error, 3),
    Stars_crisis     = case_when(
      base_row$p.value < 0.01 ~ "***",
      base_row$p.value < 0.05 ~ "**",
      base_row$p.value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    Beta_rbc_minus_crisis = round(diff_row$estimate, 3),
    SE_diff          = round(diff_row$std.error, 3),
    p_diff           = round(diff_row$p.value, 3),
    Stars_diff       = case_when(
      diff_row$p.value < 0.01 ~ "***",
      diff_row$p.value < 0.05 ~ "**",
      diff_row$p.value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    # Implied RBC beta
    Beta_rbc_implied = round(base_row$estimate + diff_row$estimate, 3),
    N_stacked        = nobs(m)
  )
})

cat("\n=== WALD TEST: H0: beta_crisis = beta_rbc ===\n")
cat("(Stars on 'Diff' column = RBC effect is significantly DIFFERENT from crisis)\n\n")
print(wald_results, n = Inf)

write_csv(wald_results,
          file.path(TABLE_PATH, "3A_wald_test_crisis_vs_rbc.csv"))
message("  Wald tests saved → output/tables/3A_wald_test_crisis_vs_rbc.csv")


# =============================================================================
# 7. EVENT STUDY — BOTH WINDOWS
# =============================================================================

message("── Step 6: Event study estimation — both windows ─────────────────────")

run_es <- function(outcome, data, complex_var) {
  fml <- as.formula(
    paste0(outcome,
           " ~ i(event_time, ", complex_var, ", ref = ", EVENT_REF, ") + ",
           controls_base,
           " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml, data = data |>
            filter(event_time >= EVENT_MIN,
                   event_time <= EVENT_MAX),
          cluster = ~cu_number,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
}

# Key outcomes for event study comparison
es_outcomes <- list(
  list("networth_ratio", "Net Worth Ratio (%)",    TRUE),
  list("cap_buffer",     "Capital Buffer (pp)",    TRUE),
  list("loan_growth",    "Loan Growth (QoQ log)",  FALSE),
  list("dq_rate_var",    "Delinquency Rate (%)",   FALSE),
  list("re_shr",         "RE Share (%)",           FALSE),
  list("auto_shr",       "Auto Share (%)",         FALSE),
  list("roa_var",        "ROA (%)",                FALSE),
  list("mbl_shr",        "MBL Share (%)",          TRUE)
)

message("  Running crisis event studies...")
es_crisis <- map(es_outcomes, function(o) {
  run_es(o[[1]], df_crisis, "complex_crisis")
})

message("  Running RBC event studies...")
es_rbc <- map(es_outcomes, function(o) {
  run_es(o[[1]], df_rbc, "complex_rbc")
})

message("  Event studies complete.")


# =============================================================================
# 8. EXTRACT EVENT STUDY COEFFICIENTS
# =============================================================================

extract_es_coefs <- function(model, window_label, complex_var) {
  if (is.null(model)) return(NULL)
  tryCatch({
    tidy(model, conf.int = TRUE) |>
      filter(str_detect(term, "event_time::")) |>
      mutate(
        event_time = as.integer(str_extract(term, "-?\\d+")),
        window     = window_label
      ) |>
      filter(event_time >= EVENT_MIN, event_time <= EVENT_MAX) |>
      bind_rows(
        tibble(term = "ref", estimate = 0, std.error = 0,
               conf.low = 0, conf.high = 0, p.value = NA_real_,
               event_time = EVENT_REF, window = window_label)
      ) |>
      arrange(event_time)
  }, error = function(e) NULL)
}

# Combine crisis and RBC coefs for each outcome
es_combined <- map2(es_crisis, es_rbc, function(mc, mr) {
  bind_rows(
    extract_es_coefs(mc, "Crisis (2008q3)", "complex_crisis"),
    extract_es_coefs(mr, "RBC Rule (2022q1)", "complex_rbc")
  )
})


# =============================================================================
# 9. COMBINED EVENT STUDY PLOTS — CRISIS vs. RBC
# =============================================================================

message("── Step 7: Combined event study plots ────────────────────────────────")

plot_es_comparison <- function(coef_data, outcome_label,
                               non_parallel_note = FALSE) {
  if (is.null(coef_data) | nrow(coef_data) == 0) return(NULL)

  subtitle_txt <- paste0(
    "Event time = quarters relative to shock (Q0). ",
    "Reference = Q", EVENT_REF, ". 95% CI shaded."
  )
  if (non_parallel_note) {
    subtitle_txt <- paste0(subtitle_txt,
                           "\nNote: Non-parallel pre-trends in capital — interpret with caution.")
  }

  ggplot(coef_data,
         aes(x = event_time, y = estimate,
             color = window, fill = window)) +
    # Post-period shading
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "gray95", alpha = 0.4) +
    # CI ribbons
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, color = NA) +
    # Zero line
    geom_hline(yintercept = 0, color = "gray50",
               linewidth = 0.5, linetype = "dashed") +
    # Treatment line
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.6, linetype = "dashed") +
    # Lines and points
    geom_line(linewidth = 0.9) +
    geom_point(aes(shape = window), size = 2) +
    # Significant post-period
    geom_point(
      data = coef_data |>
        filter(!is.na(p.value), p.value < 0.05, event_time >= 0),
      shape = 8, size = 2.5, show.legend = FALSE
    ) +
    scale_color_manual(values = c(
      "Crisis (2008q3)"   = COL_CRISIS,
      "RBC Rule (2022q1)" = COL_RBC
    )) +
    scale_fill_manual(values = c(
      "Crisis (2008q3)"   = COL_CRISIS,
      "RBC Rule (2022q1)" = COL_RBC
    )) +
    scale_shape_manual(values = c(
      "Crisis (2008q3)"   = 17,    # triangle
      "RBC Rule (2022q1)" = 16     # circle
    )) +
    scale_x_continuous(
      breaks = seq(EVENT_MIN, EVENT_MAX, by = 4),
      labels = function(x) paste0("Q", x)
    ) +
    labs(
      title    = paste0("Event Study Comparison: ", outcome_label),
      subtitle = subtitle_txt,
      x        = "Quarters Relative to Shock (Q0 = Treatment Date)",
      y        = paste0("DiD Coefficient (", outcome_label, ")"),
      color = NULL, fill = NULL, shape = NULL,
      caption  = paste0(
        "Red triangle = 2008 crisis (Lehman, 2008q3). ",
        "Navy circle = RBC rule (2022q1). ★ = p<0.05. ",
        "Two-way FE (CU + quarter). SE clustered at CU."
      )
    ) +
    theme_rbc()
}

# Generate and save individual plots
message("  Saving individual comparison plots...")

for (i in seq_along(es_outcomes)) {
  if (is.null(es_combined[[i]])) next
  p <- plot_es_comparison(
    es_combined[[i]],
    es_outcomes[[i]][[2]],
    non_parallel_note = es_outcomes[[i]][[3]]
  )
  if (!is.null(p)) {
    fname <- paste0("3A_es_comparison_",
                    str_replace_all(tolower(es_outcomes[[i]][[1]]),
                                    "_", "-"), ".png")
    ggsave(file.path(FIGURE_PATH, fname),
           p, width = 10, height = 6, dpi = 300)
    message(sprintf("    Saved: %s", fname))
  }
}


# =============================================================================
# 10. MAIN COMPARISON PANEL — 2x4 GRID
# =============================================================================

message("── Step 8: Main comparison panel ────────────────────────────────────")

# Build 4 key panels: Net worth ratio, Loan growth, DQ rate, RE share
make_panel <- function(idx, title_override = NULL) {
  d <- es_combined[[idx]]
  lbl <- es_outcomes[[idx]][[2]]
  np  <- es_outcomes[[idx]][[3]]
  if (is.null(d) | nrow(d) == 0) return(plot_spacer())
  p <- plot_es_comparison(d, lbl, np)
  if (!is.null(title_override)) {
    p <- p + labs(title = title_override)
  }
  p + theme(legend.position = "none",
            plot.subtitle = element_text(size = 8))
}

# Indices: 1=NW ratio, 3=loan growth, 4=DQ rate, 5=RE share
# 6=auto share, 7=ROA, 8=MBL share
p1 <- make_panel(1, "A. Net Worth Ratio")
p2 <- make_panel(3, "B. Loan Growth")
p3 <- make_panel(4, "C. Delinquency Rate")
p4 <- make_panel(5, "D. Real Estate Share")
p5 <- make_panel(6, "E. Auto Loan Share")
p6 <- make_panel(7, "F. Return on Assets")

# Shared legend
legend_plot <- plot_es_comparison(
  es_combined[[1]], "", FALSE
) +
  theme(legend.position = "bottom") +
  labs(title = NULL, subtitle = NULL, x = NULL, y = NULL, caption = NULL)

# Extract legend as grob
get_legend <- function(p) {
  gt <- ggplot_gtable(ggplot_build(p))
  leg_idx <- which(sapply(gt$grobs, function(x) x$name) == "guide-box")
  if (length(leg_idx) == 0) return(NULL)
  gt$grobs[[leg_idx]]
}

p_main_grid <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
  plot_annotation(
    title   = "2008 Financial Crisis vs. 2022 RBC Rule: Parallel DiD Comparison",
    subtitle = paste0(
      "Red triangles = crisis shock (2008 Q3). Navy circles = RBC rule (2022 Q1). ",
      "Both use identical two-way FE DiD specification. ★ = p<0.05."
    ),
    caption = paste0(
      "Treatment = CUs with avg assets > $500M in pre-period. ",
      "Control = CUs with avg assets < $500M. ",
      "SE clustered at CU level. Source: NCUA Call Report (5300)."
    )
  )

ggsave(file.path(FIGURE_PATH, "3A_main_comparison_panel.png"),
       p_main_grid, width = 14, height = 14, dpi = 300)

message("  Main comparison panel saved.")


# =============================================================================
# 11. COEFFICIENT COMPARISON FOREST PLOT
# =============================================================================

message("── Step 9: Coefficient comparison forest plot ────────────────────────")

forest_data <- parallel_results |>
  filter(!is.na(Beta)) |>
  mutate(
    Sig      = !is.na(p_value) & p_value < 0.05,
    Outcome  = factor(Outcome, levels = rev(unique(Outcome)))
  )

p_forest <- ggplot(
  forest_data,
  aes(x = Beta, y = Outcome,
      xmin = CI_low, xmax = CI_high,
      color = Window, shape = Window)
) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "gray50", linewidth = 0.6) +
  geom_errorbarh(height = 0.25, linewidth = 0.8,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 3,
             position = position_dodge(width = 0.6)) +
  scale_color_manual(values = c(
    "Crisis (2008q3)"   = COL_CRISIS,
    "RBC Rule (2022q1)" = COL_RBC
  )) +
  scale_shape_manual(values = c(
    "Crisis (2008q3)"   = 17,
    "RBC Rule (2022q1)" = 16
  )) +
  labs(
    title    = "DiD Coefficients: 2008 Crisis vs. 2022 RBC Rule",
    subtitle = paste0(
      "Identical specification: two-way FE (CU + quarter), ",
      "treat_post = complex × post. 95% CI. SE clustered at CU."
    ),
    x       = "DiD Coefficient (Complex × Post-Shock)",
    y       = NULL,
    color   = NULL, shape = NULL,
    caption = paste0(
      "Treatment = avg assets > $500M pre-period. ",
      "Control = avg assets < $500M. Source: NCUA Call Report (5300)."
    )
  ) +
  theme_rbc() +
  theme(legend.position = "top")

ggsave(file.path(FIGURE_PATH, "3A_forest_plot_crisis_vs_rbc.png"),
       p_forest, width = 11, height = 9, dpi = 300)

message("  Forest plot saved.")


# =============================================================================
# 12. SENSITIVITY: INFLATION-ADJUSTED THRESHOLD
# =============================================================================

message("── Step 10: Sensitivity — inflation-adjusted threshold ───────────────")

# Re-run key outcomes with inflation-adjusted 2008 threshold ($350M)
df_crisis_adj <- df_crisis |>
  mutate(
    complex    = complex_crisis_adj,
    treat_post = complex_crisis_adj * post_crisis
  )

adj_results <- map_dfr(outcomes[c(1, 2, 3, 8, 9)], function(o) {
  m_nom <- run_did_window(o[[1]], df_crisis, "complex_crisis")
  m_adj <- run_did_window(o[[1]], df_crisis_adj, "complex_crisis_adj")
  bind_rows(
    extract_coef(m_nom, o[[2]], "Crisis: nominal $500M"),
    extract_coef(m_adj, o[[2]], "Crisis: adj. $350M (~$500M 2022)")
  )
})

cat("\n=== SENSITIVITY: INFLATION-ADJUSTED THRESHOLD ===\n")
adj_results |>
  mutate(Result = paste0(Beta, Stars, " (", SE, ")")) |>
  select(Outcome, Window, Result, p_value) |>
  print(n = Inf)

write_csv(adj_results,
          file.path(TABLE_PATH, "3A_sensitivity_adjusted_threshold.csv"))

message("  Sensitivity check saved.")


# =============================================================================
# 13. SUMMARY TABLE — KEY COMPARISON
# =============================================================================

message("── Step 11: Summary comparison table ────────────────────────────────")

summary_comparison <- wald_results |>
  select(
    Outcome,
    `Crisis Beta`     = Beta_crisis,
    `Crisis SE`       = SE_crisis,
    `Crisis Stars`    = Stars_crisis,
    `RBC Beta`        = Beta_rbc_implied,
    `Diff (RBC-Crisis)` = Beta_rbc_minus_crisis,
    `Diff SE`         = SE_diff,
    `Diff p-value`    = p_diff,
    `Diff Stars`      = Stars_diff,
    N                 = N_stacked
  ) |>
  mutate(
    Interpretation = case_when(
      is.na(`Diff Stars`) | `Diff Stars` == "" ~
        "No significant difference — generic stress response",
      `Diff Stars` != "" & `RBC Beta` > `Crisis Beta` ~
        "RBC effect LARGER than crisis — regulatory amplification",
      `Diff Stars` != "" & `RBC Beta` < `Crisis Beta` ~
        "RBC effect SMALLER than crisis — regulatory mitigation",
      TRUE ~ "Significant difference"
    )
  )

cat("\n=== SUMMARY: ARE RBC EFFECTS DIFFERENT FROM CRISIS EFFECTS? ===\n")
print(summary_comparison, n = Inf)

write_csv(summary_comparison,
          file.path(TABLE_PATH, "3A_summary_comparison.csv"))

message("  Summary comparison saved.")


# =============================================================================
# 14. FINAL CONSOLE SUMMARY
# =============================================================================

message("\n── Step 12: Final summary ────────────────────────────────────────────")

cat("\n=== 3A ANALYSIS COMPLETE ===\n\n")

cat("Tables saved (output/tables/):\n")
for (t in c(
  "3A_parallel_did_crisis_vs_rbc.csv",
  "3A_wald_test_crisis_vs_rbc.csv",
  "3A_sensitivity_adjusted_threshold.csv",
  "3A_summary_comparison.csv"
)) cat("  →", t, "\n")

cat("\nFigures saved (output/figures/):\n")
for (f in c(
  "3A_main_comparison_panel.png",
  "3A_forest_plot_crisis_vs_rbc.png",
  "3A_es_comparison_networth-ratio.png",
  "3A_es_comparison_loan-growth.png",
  "3A_es_comparison_dq-rate-var.png",
  "3A_es_comparison_re-shr.png",
  "3A_es_comparison_auto-shr.png",
  "3A_es_comparison_roa-var.png",
  "3A_es_comparison_mbl-shr.png"
)) cat("  →", f, "\n")

cat("\nKey questions answered by 3A:\n")
cat("  1. Check 3A_summary_comparison.csv:\n")
cat("     - 'No significant difference' → RBC = generic stress response\n")
cat("     - 'RBC effect LARGER'         → Regulatory amplification channel\n")
cat("     - 'RBC effect SMALLER'        → Regulation provided stability\n")
cat("  2. Check 3A_main_comparison_panel.png:\n")
cat("     - Do event study SHAPES differ between red and navy lines?\n")
cat("     - Portfolio reallocation (RE↑, auto↓) is RBC-specific if\n")
cat("       it appears in navy (RBC) but NOT in red (crisis)\n")
cat("  3. Check 3A_sensitivity_adjusted_threshold.csv:\n")
cat("     - Are crisis results robust to inflation-adjusted threshold?\n")

cat("\nNext step: Run 3B_2008_Crisis_EventStudy.R\n")

message("\n── 3A_2008_Crisis_Parallel_DiD.R complete ✓ ─────────────────────────")


# =============================================================================
# END OF SCRIPT
# =============================================================================
