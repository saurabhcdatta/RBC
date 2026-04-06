# =============================================================================
# 3C_CCULR_Adoption.R
# RBC Rule Analysis — CCULR Adoption as Secondary Treatment
# NCUA Call Report (5300) Data
#
# Author  : [Your Name]
# Created : 2026
#
# PURPOSE:
#   The Complex Credit Union Leverage Ratio (CCULR) framework allows complex
#   CUs to opt into a simplified 9% net worth threshold instead of the full
#   RBC 10% requirement. This creates a natural experiment WITHIN the treated
#   group (complex CUs):
#
#   Treatment A: Complex CU stayed under full RBC (10% threshold)
#   Treatment B: Complex CU opted into CCULR    (9%  threshold)
#   Control    : Non-complex CU (not subject to either rule)
#
#   KEY QUESTIONS:
#   (1) Did CCULR adopters behave differently from full-RBC CUs post-2022?
#       → Estimates the "relief value" of CCULR opt-in
#   (2) Which CUs opted into CCULR? (selection analysis)
#       → Were it thin-buffer CUs seeking relief, or thick-buffer CUs?
#   (3) Did CCULR adoption restore lending to pre-RBC levels?
#       → Tests whether the regulatory relief mechanism worked as intended
#   (4) Is CCULR adoption itself endogenous?
#       → Instruments and timing tests
#
# DESIGN:
#   Three-group DiD:
#     Y_it = alpha_i + gamma_t
#            + beta1*(RBC_only_i  x post_t)   [RBC without CCULR]
#            + beta2*(CCULR_i     x post_t)   [CCULR adopters]
#            + X_it + e_it
#
#   beta1 = effect of full RBC on non-CCULR complex CUs
#   beta2 = effect on CCULR adopters
#   beta2 - beta1 = relief value of CCULR opt-in
#
# IDENTIFICATION:
#   CCULR adoption timing varies across CUs → staggered adoption design
#   We use Callaway-Sant'Anna (2021) for the staggered DiD
#   and supplement with a simple three-group 2x2 DiD
#
# Input  : data/analysis_panel.rds  (from 0_Data_Prep.R)
#          call_report.rds          (for cecl and CCULR flags)
# Output : output/tables/3C_*
#          output/figures/3C_*
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(fixest)
library(patchwork)
library(scales)
library(broom)
library(lubridate)

# For Callaway-Sant'Anna staggered DiD
# install.packages("did")
library(did)


# =============================================================================
# 1. SETTINGS
# =============================================================================

PANEL_PATH  <- "data/analysis_panel.rds"
RAW_PATH    <- "call_report.rds"
TABLE_PATH  <- "output/tables/"
FIGURE_PATH <- "output/figures/"

RBC_DATE         <- 2022.1
RBC_START        <- 2018.1
RBC_END          <- 2024.4
SIZE_THRESHOLD   <- 500e6
RBC_PRE_START    <- 2021.1
RBC_PRE_END      <- 2021.4

# Capital thresholds
NW_RBC_THRESHOLD   <- 10    # full RBC well-capitalized
NW_CCULR_THRESHOLD <-  9    # CCULR well-capitalized

EVENT_MIN <- -12L
EVENT_MAX <-  10L
EVENT_REF <-  -1L

CONTROLS  <- "ln_assets + loan_to_asset + cecl_adopter"

COL_RBC_ONLY <- "#C94040"    # red  — full RBC, no CCULR
COL_CCULR    <- "#1B3A6B"    # navy — CCULR adopters
COL_CONTROL  <- "#888888"    # gray — non-complex control
COL_CI_RBC   <- "#E8A0A0"
COL_CI_CCULR <- "#8AAAD0"

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

dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# 2. LOAD DATA
# =============================================================================

message("── Step 1: Loading data ──────────────────────────────────────────────")

# Load analysis panel (from 0_Data_Prep.R)
df <- readRDS(PANEL_PATH)

message(sprintf("  Panel: %s obs, %s CUs",
                scales::comma(nrow(df)),
                scales::comma(n_distinct(df$cu_number))))

# Load raw data to get CCULR adoption flag
# The 'cecl' variable tracks CECL adoption; we need CCULR separately
# CCULR is identified via the pcanetworth variable:
#   - CUs under full RBC report pcanetworth based on 10% threshold
#   - CCULR adopters are flagged in NCUA data
# We use the approach: CCULR adopter = complex CU where
#   reported net worth ratio is benchmarked to 9% threshold
# The most reliable flag comes from NCUA's CCULR field if available,
# otherwise we infer from reporting patterns.

df_raw <- readRDS(RAW_PATH)

message(sprintf("  Raw data: %s obs", scales::comma(nrow(df_raw))))

# Check available CCULR-related variables
cculr_vars <- names(df_raw)[grepl("cculr|CCULR|leverage", names(df_raw),
                                   ignore.case = TRUE)]
message(sprintf("  CCULR-related variables found: %s",
                if (length(cculr_vars) > 0)
                  paste(cculr_vars, collapse = ", ")
                else "none — will use inference approach"))


# =============================================================================
# 3. IDENTIFY CCULR ADOPTERS
# =============================================================================
# Strategy: CCULR was available from 2022q1 onward.
# We identify CCULR adopters using the pcanetworth variable:
#   - Non-CCULR complex CUs: pcanetworth benchmarks to RBC (10%)
#   - CCULR adopters: pcanetworth benchmarks to CCULR (9%)
#   - A CU that "opts in" to CCULR will show pcanetworth near 9% threshold
#     rather than 10% threshold as the binding constraint
#
# Primary approach: Use NCUA CCULR flag if present
# Fallback: Classify based on capital ratio behavior post-2022
#   CUs that maintain NW ratio between 9-10% post-RBC (would be
#   undercapitalized under RBC but compliant under CCULR) → inferred CCULR

message("── Step 2: Identifying CCULR adopters ───────────────────────────────")

# Check if direct CCULR flag exists in raw data
has_cculr_flag <- any(c("cculr", "cculr_adopter", "leverage_ratio") %in%
                        tolower(names(df_raw)))

if (has_cculr_flag) {

  # Use direct flag
  cculr_flag_var <- names(df_raw)[tolower(names(df_raw)) %in%
                                    c("cculr", "cculr_adopter",
                                      "leverage_ratio")][1]
  message(sprintf("  Using direct CCULR flag: %s", cculr_flag_var))

  cculr_adopters <- df_raw |>
    filter(q_period_num >= 2022.1) |>
    group_by(cu_number) |>
    summarise(
      ever_cculr = max(.data[[cculr_flag_var]], na.rm = TRUE),
      first_cculr_period = if_else(
        any(!is.na(.data[[cculr_flag_var]]) &
              .data[[cculr_flag_var]] == 1),
        min(q_period_num[!is.na(.data[[cculr_flag_var]]) &
                           .data[[cculr_flag_var]] == 1]),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    mutate(cculr_adopter = as.integer(ever_cculr == 1))

} else {

  # Inference approach using capital ratio behavior
  # CCULR adopters are complex CUs that:
  #   1. Had NW ratio between 9-10.5% in 2021 (near threshold)
  #   2. Post-2022 their NW ratio stabilizes around 9-10% range
  #   3. Did not show the capital buildup pattern of full-RBC CUs
  message("  No direct CCULR flag found. Using capital ratio inference.")
  message("  NOTE: Verify this classification with NCUA data documentation.")

  # Pre-RBC capital position of complex CUs
  pre_rbc_cap <- df |>
    filter(complex == 1,
           q_period_num >= RBC_PRE_START,
           q_period_num <= RBC_PRE_END) |>
    group_by(cu_number) |>
    summarise(
      pre_nw_ratio   = mean(networth_ratio, na.rm = TRUE),
      pre_cap_buffer = mean(cap_buffer,     na.rm = TRUE),
      .groups = "drop"
    )

  # Post-RBC capital trajectory
  post_rbc_cap <- df |>
    filter(complex == 1,
           q_period_num >= 2022.1,
           q_period_num <= 2023.4) |>
    group_by(cu_number) |>
    summarise(
      post_nw_ratio  = mean(networth_ratio, na.rm = TRUE),
      post_cap_chg   = mean(cap_buffer,     na.rm = TRUE),
      .groups = "drop"
    )

  # Inferred CCULR adopters:
  # Complex CUs with pre-RBC NW ratio 9-11% that maintained
  # ratio in 9-10.5% band post-RBC (didn't build above 10%)
  cculr_adopters <- pre_rbc_cap |>
    left_join(post_rbc_cap, by = "cu_number") |>
    mutate(
      # Thin buffer pre-RBC AND maintained near CCULR threshold post-RBC
      inferred_cculr = as.integer(
        pre_nw_ratio >= 9 & pre_nw_ratio < 11 &
          !is.na(post_nw_ratio) &
          post_nw_ratio >= 9 & post_nw_ratio < 10.5
      ),
      cculr_adopter      = inferred_cculr,
      first_cculr_period = if_else(inferred_cculr == 1, 2022.1, NA_real_),
      ever_cculr         = inferred_cculr
    ) |>
    select(cu_number, cculr_adopter, first_cculr_period, ever_cculr)

  message("  Using inferred CCULR classification based on capital ratio behavior.")
}

message(sprintf("  CCULR adopters identified: %d",
                sum(cculr_adopters$cculr_adopter, na.rm = TRUE)))
message(sprintf("  Non-CCULR complex CUs    : %d",
                sum(cculr_adopters$cculr_adopter == 0, na.rm = TRUE)))


# =============================================================================
# 4. BUILD THREE-GROUP PANEL
# =============================================================================

message("── Step 3: Building three-group panel ───────────────────────────────")

# Merge CCULR flag into main panel
df_3g <- df |>
  left_join(cculr_adopters |>
              select(cu_number, cculr_adopter, first_cculr_period),
            by = "cu_number") |>
  mutate(
    # For non-complex CUs, CCULR flag = 0
    cculr_adopter = if_else(complex == 0, 0L,
                            coalesce(as.integer(cculr_adopter), 0L)),

    # Three-group indicator
    # 0 = non-complex (control)
    # 1 = complex, full RBC (no CCULR)
    # 2 = complex, CCULR adopter
    group_3 = case_when(
      complex == 0              ~ 0L,
      complex == 1 & cculr_adopter == 0 ~ 1L,
      complex == 1 & cculr_adopter == 1 ~ 2L,
      TRUE                      ~ NA_integer_
    ),

    # Treatment indicators for three-group DiD
    rbc_only  = as.integer(group_3 == 1L),  # full RBC, no CCULR
    cculr_grp = as.integer(group_3 == 2L),  # CCULR adopters

    # Post × treatment interactions
    rbc_post   = rbc_only  * post_rbc,
    cculr_post = cculr_grp * post_rbc,

    # Event time (matches main analysis)
    event_time = case_when(
      quarter == 1L ~ (year - 2022L) * 4L + 0L,
      quarter == 2L ~ (year - 2022L) * 4L + 1L,
      quarter == 3L ~ (year - 2022L) * 4L + 2L,
      quarter == 4L ~ (year - 2022L) * 4L + 3L
    )
  )

# Sample sizes
cat("\n=== THREE-GROUP SAMPLE ===\n")
group_summary <- df_3g |>
  filter(!is.na(group_3)) |>
  distinct(cu_number, group_3, complex, cculr_adopter) |>
  count(group_3, name = "n_cus") |>
  mutate(
    Label = case_when(
      group_3 == 0L ~ "Non-complex (control)",
      group_3 == 1L ~ "Complex — Full RBC (no CCULR)",
      group_3 == 2L ~ "Complex — CCULR adopter"
    )
  )
print(group_summary, row.names = FALSE)

cat(sprintf("\nTotal observations: %s\n",
            scales::comma(nrow(df_3g[!is.na(df_3g$group_3), ]))))


# =============================================================================
# 5. SELECTION ANALYSIS — WHO ADOPTS CCULR?
# =============================================================================
# Tests whether CCULR adoption is random within the complex CU group
# or driven by observable pre-treatment characteristics.
# Key question: Did thin-buffer CUs seek CCULR relief, or thick-buffer ones?

message("── Step 4: Selection analysis ───────────────────────────────────────")

# Compare CCULR vs non-CCULR complex CUs on pre-RBC characteristics
selection_vars <- c(
  "networth_ratio", "cap_buffer", "loan_to_asset",
  "mbl_shr", "re_shr", "auto_shr", "roa_var",
  "dq_rate_var", "nim", "assets_tot", "members"
)

selection_labels <- c(
  "Net worth ratio (%)", "Capital buffer vs 10% (pp)",
  "Loans / assets (%)", "MBL share (%)", "RE share (%)",
  "Auto share (%)", "ROA (%)", "DQ rate (%)",
  "NIM (%)", "Total assets ($)", "Members"
)

pre_rbc_chars <- df_3g |>
  filter(complex == 1,
         q_period_num >= RBC_PRE_START,
         q_period_num <= RBC_PRE_END,
         !is.na(group_3)) |>
  group_by(cu_number, cculr_adopter) |>
  summarise(across(all_of(selection_vars),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

selection_table <- map2_dfr(selection_vars, selection_labels,
                             function(v, lbl) {
  x_cculr    <- pre_rbc_chars[[v]][pre_rbc_chars$cculr_adopter == 1]
  x_non_cculr <- pre_rbc_chars[[v]][pre_rbc_chars$cculr_adopter == 0]

  m_cculr    <- mean(x_cculr,    na.rm = TRUE)
  m_non_cculr <- mean(x_non_cculr, na.rm = TRUE)
  sd_cculr   <- sd(x_cculr,      na.rm = TRUE)
  sd_non     <- sd(x_non_cculr,  na.rm = TRUE)

  tt <- tryCatch(
    t.test(x_cculr, x_non_cculr),
    error = function(e) list(p.value = NA_real_)
  )

  tibble(
    Variable          = lbl,
    `CCULR adopters`  = sprintf("%.3f (%.3f)", m_cculr, sd_cculr),
    `Full RBC`        = sprintf("%.3f (%.3f)", m_non_cculr, sd_non),
    Difference        = round(m_cculr - m_non_cculr, 3),
    `p-value`         = round(tt$p.value, 3)
  )
})

cat("\n=== SELECTION: WHO ADOPTS CCULR? ===\n")
cat("(Pre-RBC characteristics: CCULR adopters vs full-RBC complex CUs)\n\n")
print(selection_table, n = Inf)

write_csv(selection_table,
          file.path(TABLE_PATH, "3C_selection_analysis.csv"))
message("  Selection table saved.")

# Logit model: predict CCULR adoption from pre-RBC characteristics
logit_data <- pre_rbc_chars |>
  filter(!is.na(cculr_adopter))

if (nrow(logit_data) > 0 && sum(logit_data$cculr_adopter) > 5) {
  logit_fml <- as.formula(
    paste("cculr_adopter ~",
          paste(selection_vars[1:8], collapse = " + "))
  )
  logit_m <- tryCatch(
    glm(logit_fml, data = logit_data, family = binomial(link = "logit")),
    error = function(e) NULL
  )
  if (!is.null(logit_m)) {
    cat("\n=== LOGIT: PREDICTORS OF CCULR ADOPTION ===\n")
    logit_tidy <- tidy(logit_m, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    print(logit_tidy)
    write_csv(logit_tidy,
              file.path(TABLE_PATH, "3C_cculr_adoption_logit.csv"))
    message("  Logit results saved.")
  }
}


# =============================================================================
# 6. THREE-GROUP DiD ESTIMATION
# =============================================================================
# Model:
# Y_it = alpha_i + gamma_t
#        + beta1*(rbc_only_i  x post_t)   ← full RBC effect
#        + beta2*(cculr_grp_i x post_t)   ← CCULR adopter effect
#        + X_it + e_it
#
# beta1         = effect of full RBC rule (non-CCULR complex CUs)
# beta2         = effect on CCULR adopters
# beta2 - beta1 = relief value of CCULR opt-in
# H0: beta1 = beta2 → CCULR provides no differential relief

message("── Step 5: Three-group DiD estimation ───────────────────────────────")

outcomes_3g <- list(
  list(var = "networth_ratio", label = "Net Worth Ratio (%)"),
  list(var = "cap_buffer",     label = "Capital Buffer (pp)"),
  list(var = "well_capitalized", label = "Well-Capitalized (LPM)"),
  list(var = "loan_growth",    label = "Loan Growth (QoQ log)"),
  list(var = "asset_growth",   label = "Asset Growth (QoQ log)"),
  list(var = "mbl_shr",        label = "MBL Share (%)"),
  list(var = "re_shr",         label = "RE Share (%)"),
  list(var = "auto_shr",       label = "Auto Share (%)"),
  list(var = "dq_rate_var",    label = "DQ Rate (%)"),
  list(var = "chgoff_ratio",   label = "Charge-Off Ratio (%)"),
  list(var = "roa_var",        label = "ROA (%)"),
  list(var = "nim",            label = "NIM (%)")
)

# Helper: run three-group DiD
run_3g_did <- function(outcome_var, data = df_3g) {
  fml <- as.formula(
    paste0(outcome_var,
           " ~ rbc_post + cculr_post + ",
           CONTROLS,
           " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml, data = data[!is.na(data$group_3), ],
          cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) {
      message("  3g DiD failed: ", outcome_var, " — ", e$message)
      NULL
    }
  )
}

# Extract both coefficients + compute difference
extract_3g <- function(model_obj, out_lbl) {
  if (is.null(model_obj)) return(NULL)
  tryCatch({
    td <- tidy(model_obj, conf.int = TRUE)

    rbc_row  <- td[td$term == "rbc_post",  ]
    cculr_row <- td[td$term == "cculr_post", ]

    if (nrow(rbc_row) == 0 || nrow(cculr_row) == 0) return(NULL)

    # Relief = CCULR effect minus RBC effect
    relief      <- cculr_row$estimate - rbc_row$estimate
    # SE of difference (approximate, assuming independence)
    relief_se   <- sqrt(cculr_row$std.error^2 + rbc_row$std.error^2)
    relief_t    <- relief / relief_se
    relief_p    <- 2 * (1 - pnorm(abs(relief_t)))

    star_fn <- function(p) {
      if (is.na(p)) "" else
      if (p < 0.01) "***" else
      if (p < 0.05) "**"  else
      if (p < 0.10) "*"   else ""
    }

    data.frame(
      Outcome          = out_lbl,
      Beta_RBC         = round(rbc_row$estimate,   3),
      SE_RBC           = round(rbc_row$std.error,   3),
      Stars_RBC        = star_fn(rbc_row$p.value),
      Beta_CCULR       = round(cculr_row$estimate,  3),
      SE_CCULR         = round(cculr_row$std.error,  3),
      Stars_CCULR      = star_fn(cculr_row$p.value),
      Relief           = round(relief,    3),
      Relief_SE        = round(relief_se, 3),
      Relief_p         = round(relief_p,  3),
      Stars_Relief     = star_fn(relief_p),
      N                = as.integer(nobs(model_obj)),
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
}

message("  Running three-group DiD models...")
models_3g <- vector("list", length(outcomes_3g))
results_3g <- data.frame(
  Outcome = character(0), Beta_RBC = numeric(0), SE_RBC = numeric(0),
  Stars_RBC = character(0), Beta_CCULR = numeric(0), SE_CCULR = numeric(0),
  Stars_CCULR = character(0), Relief = numeric(0), Relief_SE = numeric(0),
  Relief_p = numeric(0), Stars_Relief = character(0), N = integer(0),
  stringsAsFactors = FALSE
)

for (i in seq_along(outcomes_3g)) {
  o <- outcomes_3g[[i]]
  models_3g[[i]] <- run_3g_did(o$var)
  row <- extract_3g(models_3g[[i]], o$label)
  if (!is.null(row)) results_3g <- rbind(results_3g, row)
  message(sprintf("  %s: done", o$var))
}

cat("\n=== THREE-GROUP DiD RESULTS ===\n")
cat("beta_RBC  = effect on full-RBC complex CUs (vs non-complex)\n")
cat("beta_CCULR = effect on CCULR adopters (vs non-complex)\n")
cat("Relief    = beta_CCULR - beta_RBC (positive = CCULR mitigated the effect)\n\n")
print(results_3g, row.names = FALSE)

write.csv(results_3g,
          file.path(TABLE_PATH, "3C_three_group_did.csv"),
          row.names = FALSE)
message("  Three-group DiD saved.")


# =============================================================================
# 7. EVENT STUDY — THREE GROUPS
# =============================================================================

message("── Step 6: Event study — three groups ───────────────────────────────")

run_3g_es <- function(outcome_var, data = df_3g) {
  data_es <- data[
    !is.na(data$group_3) &
      !is.na(data$event_time) &
      data$event_time >= EVENT_MIN &
      data$event_time <= EVENT_MAX, ]

  fml <- as.formula(
    paste0(outcome_var,
           " ~ i(event_time, rbc_only,  ref = ", EVENT_REF, ") +",
           " i(event_time, cculr_grp, ref = ", EVENT_REF, ") +",
           CONTROLS,
           " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml, data = data_es,
          cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) {
      message("  ES failed: ", outcome_var, " — ", e$message)
      NULL
    }
  )
}

extract_3g_es <- function(model_obj) {
  if (is.null(model_obj)) return(NULL)
  tryCatch({
    td <- tidy(model_obj, conf.int = TRUE)

    # RBC-only coefficients
    rbc_rows <- td[grepl("event_time::.*:rbc_only", td$term) |
                     grepl("rbc_only.*event_time::", td$term), ]

    # CCULR coefficients
    cculr_rows <- td[grepl("event_time::.*:cculr_grp", td$term) |
                       grepl("cculr_grp.*event_time::", td$term), ]

    parse_et <- function(rows, grp_label) {
      if (nrow(rows) == 0) return(NULL)
      rows$event_time <- as.integer(
        regmatches(rows$term, regexpr("-?[0-9]+", rows$term))
      )
      rows <- rows[rows$event_time >= EVENT_MIN &
                     rows$event_time <= EVENT_MAX, ]

      out <- data.frame(
        event_time = rows$event_time,
        estimate   = rows$estimate,
        std_error  = rows$std.error,
        p_value    = rows$p.value,
        conf_low   = rows$conf.low,
        conf_high  = rows$conf.high,
        group      = as.character(grp_label),
        stringsAsFactors = FALSE
      )

      # Add reference row
      ref <- data.frame(
        event_time = as.integer(EVENT_REF),
        estimate = 0, std_error = 0, p_value = NA_real_,
        conf_low = 0, conf_high = 0,
        group = as.character(grp_label),
        stringsAsFactors = FALSE
      )
      rbind(out, ref)
    }

    rbc_coefs  <- parse_et(rbc_rows,  "Full RBC (no CCULR)")
    cculr_coefs <- parse_et(cculr_rows, "CCULR adopters")

    if (is.null(rbc_coefs) && is.null(cculr_coefs)) return(NULL)

    parts <- NULL
    if (!is.null(rbc_coefs))  parts <- rbc_coefs
    if (!is.null(cculr_coefs)) {
      if (is.null(parts)) parts <- cculr_coefs
      else parts <- rbind(parts, cculr_coefs)
    }
    parts[order(parts$group, parts$event_time), ]

  }, error = function(e) NULL)
}

# Run event studies for key outcomes
es_outcomes_3g <- list(
  list(var = "networth_ratio", label = "Net Worth Ratio (%)"),
  list(var = "cap_buffer",     label = "Capital Buffer (pp)"),
  list(var = "loan_growth",    label = "Loan Growth (QoQ log)"),
  list(var = "dq_rate_var",    label = "Delinquency Rate (%)"),
  list(var = "roa_var",        label = "ROA (%)"),
  list(var = "auto_shr",       label = "Auto Loan Share (%)")
)

message("  Running three-group event studies...")
es_3g_models  <- vector("list", length(es_outcomes_3g))
es_3g_coefs   <- vector("list", length(es_outcomes_3g))

for (i in seq_along(es_outcomes_3g)) {
  o <- es_outcomes_3g[[i]]
  es_3g_models[[i]] <- run_3g_es(o$var)
  es_3g_coefs[[i]]  <- extract_3g_es(es_3g_models[[i]])
  if (!is.null(es_3g_coefs[[i]])) {
    message(sprintf("  %s: %d coef rows", o$var, nrow(es_3g_coefs[[i]])))
  } else {
    message(sprintf("  %s: NULL", o$var))
  }
}


# =============================================================================
# 8. THREE-GROUP EVENT STUDY PLOTS
# =============================================================================

message("── Step 7: Three-group event study plots ────────────────────────────")

plot_3g_es <- function(coef_df, outcome_label) {
  if (is.null(coef_df) || nrow(coef_df) == 0) return(NULL)

  coef_df$group <- as.character(coef_df$group)

  df_rbc_plot  <- coef_df[coef_df$group == "Full RBC (no CCULR)", ]
  df_cculr_plot <- coef_df[coef_df$group == "CCULR adopters", ]

  df_sig <- coef_df[
    !is.na(coef_df$p_value) &
      coef_df$p_value < 0.05 &
      coef_df$event_time >= 0L, ]

  p <- ggplot(coef_df, aes(x = event_time, y = estimate,
                            color = group, shape = group)) +
    annotate("rect",
             xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "gray93", alpha = 0.5) +
    geom_ribbon(
      data    = df_rbc_plot,
      mapping = aes(x = event_time,
                    ymin = conf_low, ymax = conf_high),
      fill = COL_CI_RBC, alpha = 0.25, color = NA,
      inherit.aes = FALSE
    ) +
    geom_ribbon(
      data    = df_cculr_plot,
      mapping = aes(x = event_time,
                    ymin = conf_low, ymax = conf_high),
      fill = COL_CI_CCULR, alpha = 0.25, color = NA,
      inherit.aes = FALSE
    ) +
    geom_hline(yintercept = 0, color = "gray50",
               linewidth = 0.5, linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.6, linetype = "dashed") +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.2) +
    geom_point(
      data        = df_sig,
      mapping     = aes(x = event_time, y = estimate),
      shape       = 8, size = 2.8, color = "darkred",
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    scale_color_manual(
      values = c("Full RBC (no CCULR)" = COL_RBC_ONLY,
                 "CCULR adopters"      = COL_CCULR)
    ) +
    scale_shape_manual(
      values = c("Full RBC (no CCULR)" = 17L,
                 "CCULR adopters"      = 16L)
    ) +
    scale_x_continuous(
      breaks = seq(EVENT_MIN, EVENT_MAX, by = 4L),
      labels = function(x) paste0("Q", x)
    ) +
    labs(
      title    = outcome_label,
      subtitle = paste0(
        "Event time = quarters relative to RBC (Q0 = 2022 Q1). ",
        "Ref = Q", EVENT_REF, ". 95% CI shaded.\n",
        "Red = Full RBC (no CCULR). Navy = CCULR adopters. ★ = p<0.05."
      ),
      x       = "Quarters Relative to RBC",
      y       = "DiD Coefficient (vs non-complex control)",
      color   = NULL, shape = NULL,
      caption = paste0(
        "Two-way FE (CU + quarter-year). SE clustered at CU. ",
        "Source: NCUA Call Report (5300)."
      )
    ) +
    theme_rbc()

  p
}

# Save individual plots
for (i in seq_along(es_outcomes_3g)) {
  o     <- es_outcomes_3g[[i]]
  coefs <- es_3g_coefs[[i]]
  if (is.null(coefs) || nrow(coefs) == 0) next

  p <- tryCatch(
    plot_3g_es(coefs, o$label),
    error = function(e) {
      message("  Plot error (", o$var, "): ", e$message); NULL
    }
  )
  if (is.null(p)) next

  fname <- paste0("3C_es_3group_",
                  gsub("_", "-", o$var, fixed = TRUE), ".png")
  ggsave(file.path(FIGURE_PATH, fname),
         p, width = 10, height = 6, dpi = 300)
  message("  Saved: ", fname)
}


# =============================================================================
# 9. MAIN PANEL — 2x3 GRID
# =============================================================================

message("── Step 8: Main three-group panel ───────────────────────────────────")

compact_3g <- function(idx, title_txt) {
  coefs <- es_3g_coefs[[idx]]
  if (is.null(coefs) || nrow(coefs) == 0) return(patchwork::plot_spacer())
  p <- tryCatch(plot_3g_es(coefs, title_txt), error = function(e) NULL)
  if (is.null(p)) return(patchwork::plot_spacer())
  p + theme(legend.position = "none",
            plot.subtitle   = element_text(size = 7.5),
            plot.title      = element_text(size = 11))
}

# Indices: 1=NW ratio, 2=cap buffer, 3=loan growth,
#          4=DQ rate, 5=ROA, 6=auto share
p1 <- compact_3g(1, "A. Net Worth Ratio (%)")
p2 <- compact_3g(2, "B. Capital Buffer (pp)")
p3 <- compact_3g(3, "C. Loan Growth")
p4 <- compact_3g(4, "D. Delinquency Rate (%)")
p5 <- compact_3g(5, "E. ROA (%)")
p6 <- compact_3g(6, "F. Auto Loan Share (%)")

panel_3g <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
  plot_annotation(
    title   = "CCULR Relief Value: Full RBC vs. CCULR Adopters",
    subtitle = paste0(
      "Red triangles = complex CUs under full RBC (10% threshold). ",
      "Navy circles = CCULR adopters (9% threshold).\n",
      "Both vs. non-complex control. ★ = p<0.05. ",
      "Vertical gap = estimated relief value of CCULR opt-in."
    ),
    caption = paste0(
      "Three-group DiD: Y ~ i(event_time, rbc_only) + ",
      "i(event_time, cculr_grp) + FE(CU + quarter). ",
      "SE clustered at CU. Source: NCUA Call Report (5300)."
    )
  )

ggsave(file.path(FIGURE_PATH, "3C_main_panel_3group.png"),
       panel_3g, width = 14, height = 14, dpi = 300)
message("  Main panel saved.")


# =============================================================================
# 10. COEFFICIENT FOREST PLOT — RELIEF VALUES
# =============================================================================

message("── Step 9: Relief value forest plot ─────────────────────────────────")

if (nrow(results_3g) > 0) {

  # Reshape for forest plot
  forest_3g <- bind_rows(
    results_3g |>
      transmute(
        Outcome,
        Group   = "Full RBC (no CCULR)",
        Beta    = Beta_RBC,
        CI_low  = Beta_RBC - 1.96 * SE_RBC,
        CI_high = Beta_RBC + 1.96 * SE_RBC,
        Stars   = Stars_RBC
      ),
    results_3g |>
      transmute(
        Outcome,
        Group   = "CCULR adopters",
        Beta    = Beta_CCULR,
        CI_low  = Beta_CCULR - 1.96 * SE_CCULR,
        CI_high = Beta_CCULR + 1.96 * SE_CCULR,
        Stars   = Stars_CCULR
      )
  ) |>
    mutate(Outcome = factor(Outcome, levels = rev(unique(Outcome))))

  p_forest <- ggplot(
    forest_3g[!is.na(forest_3g$Beta), ],
    aes(x = Beta, y = Outcome,
        xmin = CI_low, xmax = CI_high,
        color = Group, shape = Group)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray50", linewidth = 0.6) +
    geom_errorbarh(height = 0.3, linewidth = 0.8,
                   position = position_dodge(width = 0.6)) +
    geom_point(size = 3,
               position = position_dodge(width = 0.6)) +
    scale_color_manual(
      values = c("Full RBC (no CCULR)" = COL_RBC_ONLY,
                 "CCULR adopters"      = COL_CCULR)
    ) +
    scale_shape_manual(
      values = c("Full RBC (no CCULR)" = 17L,
                 "CCULR adopters"      = 16L)
    ) +
    labs(
      title    = "CCULR Relief Value: Three-Group DiD Coefficients",
      subtitle = paste0(
        "Both groups vs. non-complex control. 95% CI. ",
        "Horizontal gap between red and navy = estimated relief value."
      ),
      x       = "DiD Coefficient (vs non-complex control)",
      y       = NULL,
      color   = NULL, shape = NULL,
      caption = paste0(
        "Two-way FE (CU + quarter-year). SE clustered at CU. ",
        "Source: NCUA Call Report (5300)."
      )
    ) +
    theme_rbc() +
    theme(legend.position = "top")

  ggsave(file.path(FIGURE_PATH, "3C_forest_relief_value.png"),
         p_forest, width = 11, height = 9, dpi = 300)
  message("  Forest plot saved.")

  # Print relief value summary
  cat("\n=== RELIEF VALUE SUMMARY ===\n")
  cat("Positive Relief = CCULR dampened the negative RBC effect\n")
  cat("Negative Relief = CCULR worsened relative to full RBC\n\n")
  results_3g |>
    select(Outcome, Beta_RBC, Stars_RBC, Beta_CCULR, Stars_CCULR,
           Relief, Stars_Relief) |>
    mutate(
      Interpretation = case_when(
        Relief > 0 & Relief_p < 0.10 ~ "CCULR provided relief",
        Relief < 0 & Relief_p < 0.10 ~ "CCULR worsened outcome",
        TRUE                          ~ "No significant difference"
      )
    ) |>
    print(row.names = FALSE)
}


# =============================================================================
# 11. STAGGERED ADOPTION — CALLAWAY-SANT'ANNA (if variation exists)
# =============================================================================

message("── Step 10: Staggered adoption DiD (Callaway-Sant'Anna) ─────────────")

# Check if there is variation in CCULR adoption timing
timing_variation <- cculr_adopters |>
  filter(cculr_adopter == 1, !is.na(first_cculr_period)) |>
  count(first_cculr_period)

cat("\n=== CCULR ADOPTION TIMING DISTRIBUTION ===\n")
print(timing_variation)

n_distinct_periods <- nrow(timing_variation)

if (n_distinct_periods >= 2) {

  message("  Multiple adoption periods detected — running C&S staggered DiD")

  # Prepare data for did package
  # did requires: id (unit), period (numeric time), G (first treated period)
  #               G = 0 for never-treated (control group)

  # Create cohort variable G
  cu_cohorts <- df_3g |>
    filter(complex == 1) |>
    distinct(cu_number) |>
    left_join(
      cculr_adopters |> select(cu_number, first_cculr_period),
      by = "cu_number"
    ) |>
    mutate(
      # Convert q_period_num to integer period (quarters since 2000q1)
      G_period = if_else(
        !is.na(first_cculr_period),
        as.integer((floor(first_cculr_period) - 2000) * 4 +
                     round((first_cculr_period %% 1) / 0.1)),
        0L   # 0 = never treated (full RBC, no CCULR)
      )
    )

  # Also need control group (non-complex) with G = 0
  control_cohorts <- df_3g |>
    filter(complex == 0) |>
    distinct(cu_number) |>
    mutate(G_period = 0L)

  all_cohorts <- bind_rows(cu_cohorts, control_cohorts)

  # Build C&S dataset
  cs_data <- df_3g |>
    left_join(all_cohorts |> select(cu_number, G_period),
              by = "cu_number") |>
    filter(!is.na(G_period)) |>
    mutate(
      period_int = as.integer(
        (floor(q_period_num) - 2000) * 4 +
          round((q_period_num %% 1) / 0.1)
      )
    )

  # Run C&S DiD for loan growth (primary outcome)
  cs_result <- tryCatch({
    att_gt(
      yname         = "loan_growth",
      tname         = "period_int",
      idname        = "cu_number",
      gname         = "G_period",
      xformla       = ~ ln_assets + loan_to_asset,
      data          = cs_data,
      control_group = "nevertreated",
      est_method    = "reg"
    )
  }, error = function(e) {
    message("  C&S DiD error: ", e$message)
    NULL
  })

  if (!is.null(cs_result)) {
    cat("\n=== CALLAWAY-SANT'ANNA: CCULR ADOPTION EFFECT ON LOAN GROWTH ===\n")
    summary(cs_result)

    # Aggregate to simple ATT
    cs_agg <- tryCatch(
      aggte(cs_result, type = "simple"),
      error = function(e) NULL
    )
    if (!is.null(cs_agg)) {
      cat("\nAggregate ATT (simple):\n")
      summary(cs_agg)
    }

    # Save
    cs_tidy <- tryCatch(
      tidy(cs_result),
      error = function(e) NULL
    )
    if (!is.null(cs_tidy)) {
      write_csv(cs_tidy,
                file.path(TABLE_PATH, "3C_callaway_santanna.csv"))
      message("  C&S results saved.")
    }
  }

} else {
  message("  Only one CCULR adoption period detected.")
  message("  Staggered DiD requires variation in adoption timing.")
  message("  Using standard three-group DiD only (Step 5 results).")
}


# =============================================================================
# 12. PRE-TREND TEST — CCULR vs. FULL RBC (PRE-2022)
# =============================================================================
# Tests whether CCULR adopters and full-RBC CUs had parallel trends
# before 2022. Non-parallel pre-trends would indicate selection bias.

message("── Step 11: Pre-trend test — CCULR vs Full RBC ──────────────────────")

# Pre-period event study on complex CUs only
df_pre_test <- df_3g |>
  filter(
    complex == 1,
    !is.na(group_3),
    event_time >= -12L,
    event_time <= -1L
  ) |>
  mutate(
    # Binary: CCULR adopter vs full RBC (within complex group)
    cculr_vs_rbc = as.integer(cculr_adopter == 1L)
  )

pretrend_results <- data.frame(
  Outcome = character(0),
  F_stat  = numeric(0),
  p_value = numeric(0),
  stringsAsFactors = FALSE
)

pretrend_outcomes <- c("networth_ratio", "loan_growth", "dq_rate_var", "roa_var")
pretrend_labels   <- c("Net Worth Ratio", "Loan Growth", "DQ Rate", "ROA")

for (j in seq_along(pretrend_outcomes)) {
  fml_pre <- as.formula(
    paste0(pretrend_outcomes[j],
           " ~ i(event_time, cculr_vs_rbc, ref = -1L) + ",
           CONTROLS,
           " | cu_number + q_period_num")
  )
  m_pre <- tryCatch(
    feols(fml_pre, data = df_pre_test,
          cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (!is.null(m_pre)) {
    # Joint F-test on pre-period interaction terms
    f_test <- tryCatch({
      ht <- tidy(m_pre)
      pre_terms <- ht[grepl("event_time::", ht$term) &
                        grepl("cculr", ht$term), ]
      if (nrow(pre_terms) > 0) {
        f_val <- mean(pre_terms$statistic^2, na.rm = TRUE)
        f_p   <- 1 - pchisq(f_val * nrow(pre_terms), nrow(pre_terms))
        list(f = round(f_val, 3), p = round(f_p, 3))
      } else list(f = NA_real_, p = NA_real_)
    }, error = function(e) list(f = NA_real_, p = NA_real_))

    pretrend_results <- rbind(pretrend_results, data.frame(
      Outcome = pretrend_labels[j],
      F_stat  = f_test$f,
      p_value = f_test$p,
      stringsAsFactors = FALSE
    ))
  }
}

cat("\n=== PRE-TREND TEST: CCULR vs FULL RBC (pre-2022 only) ===\n")
cat("H0: parallel pre-trends between CCULR and full-RBC complex CUs\n")
cat("p > 0.10 = no evidence of pre-trend violation\n\n")
print(pretrend_results, row.names = FALSE)

write.csv(pretrend_results,
          file.path(TABLE_PATH, "3C_pretrend_test.csv"),
          row.names = FALSE)


# =============================================================================
# 13. FINAL SUMMARY
# =============================================================================

cat("\n=== 3C COMPLETE ===\n\n")

cat("Tables (output/tables/):\n")
for (t in c("3C_selection_analysis.csv",
            "3C_cculr_adoption_logit.csv",
            "3C_three_group_did.csv",
            "3C_callaway_santanna.csv",
            "3C_pretrend_test.csv")) {
  flag <- if (file.exists(file.path(TABLE_PATH, t))) "✓" else "–"
  cat(sprintf("  %s %s\n", flag, t))
}

cat("\nFigures (output/figures/):\n")
for (f in c("3C_main_panel_3group.png",
            "3C_forest_relief_value.png",
            paste0("3C_es_3group_",
                   gsub("_", "-",
                        sapply(es_outcomes_3g, `[[`, "var"),
                        fixed = TRUE), ".png"))) {
  flag <- if (file.exists(file.path(FIGURE_PATH, f))) "✓" else "–"
  cat(sprintf("  %s %s\n", flag, f))
}

cat("\nKey findings to look for:\n")
cat("  3C_three_group_did.csv — 'Relief' column:\n")
cat("    Positive + significant = CCULR provided measurable relief\n")
cat("    Near zero              = CCULR had no differential effect\n")
cat("  3C_main_panel_3group.png:\n")
cat("    Gap between red (RBC) and navy (CCULR) lines = relief value\n")
cat("    Converging lines post-RBC = CCULR successfully equalized outcomes\n")
cat("  3C_selection_analysis.csv:\n")
cat("    Were thin-buffer or thick-buffer CUs more likely to adopt?\n")
cat("    Significant differences = endogenous selection concern\n")

message("\n── 3C_CCULR_Adoption.R complete ✓ ───────────────────────────────────")
message("  Next: 4_Paper_Tables.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
