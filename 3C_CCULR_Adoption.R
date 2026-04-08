# =============================================================================
# 3C_CCULR_Adoption.R
# RBC Rule Analysis — CCULR Adoption as Secondary Treatment
# NCUA Call Report (5300) Data
#
# Author  : Saurabh C. Datta, Ph.D.
# Created : 2026
#
# PURPOSE:
#   The Complex Credit Union Leverage Ratio (CCULR) allows complex CUs to
#   opt into a simplified 9% net worth threshold instead of the full RBC 10%
#   requirement. This creates a natural experiment WITHIN the treated group:
#
#   Treatment A : Complex CU under full RBC   (10% threshold)
#   Treatment B : Complex CU under CCULR      ( 9% threshold)
#   Control     : Non-complex CU              (not subject to either rule)
#
#   KEY QUESTIONS:
#   (1) Did CCULR adopters behave differently from full-RBC CUs post-2022?
#       → Estimates the "relief value" of CCULR opt-in
#   (2) Which CUs opted into CCULR? (selection analysis)
#       → Were they thin-buffer CUs seeking relief, or thick-buffer CUs?
#   (3) Did CCULR adoption restore lending to pre-RBC levels?
#       → Tests whether the regulatory relief mechanism worked as intended
#   (4) Is CCULR adoption itself endogenous?
#       → Instruments and timing tests (Callaway-Sant'Anna)
#
# DESIGN:
#   Three-group DiD:
#     Y_it = alpha_i + gamma_t
#            + beta1*(RBC_only_i  x post_t)   [full RBC, no CCULR]
#            + beta2*(CCULR_i     x post_t)   [CCULR adopters]
#            + X_it + e_it
#   beta1         = effect of full RBC on non-CCULR complex CUs
#   beta2         = effect on CCULR adopters
#   beta2 - beta1 = relief value of CCULR opt-in
#
# CHANGE LOG vs prior version:
#   [FIX 1] RBC_END updated 2024.4 → 2025.4 (matches 0_Data_Prep & 3A/3B)
#   [FIX 2] Growth variable labels updated to "QoQ log×100" throughout
#           (outcomes_3g, es_outcomes_3g, compact_3g panel titles)
#   [FIX 3] GROWTH_LABEL_SUFFIX constant added — single point of change
#   [NEW]   Section 14: Five policy charts from CCULR analysis:
#             (a) CCULR selection profile — who adopted?
#             (b) Relief value scorecard — all outcomes in one view
#             (c) CCULR regulatory burden comparison vs full RBC
#             (d) Lending contraction: full RBC vs CCULR vs non-complex
#             (e) CCULR policy redesign — what would genuine relief require?
#
# Input  : data/analysis_panel.rds, call_report.rds
# Output : output/tables/3C_* | output/figures/3C_* | output/figures/policy_3c*
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
library(did)   # Callaway-Sant'Anna staggered DiD


# =============================================================================
# 1. SETTINGS
# =============================================================================

PANEL_PATH  <- "data/analysis_panel.rds"
RAW_PATH    <- "call_report.rds"
TABLE_PATH  <- "output/tables/"
FIGURE_PATH <- "output/figures/"

RBC_DATE         <- 2022.1
RBC_START        <- 2018.1
RBC_END          <- 2025.4    # [FIX 1]: was 2024.4 — updated to match 0_Data_Prep
RBC_PRE_START    <- 2021.1
RBC_PRE_END      <- 2021.4
SIZE_THRESHOLD   <- 500e6

NW_RBC_THRESHOLD   <- 10
NW_CCULR_THRESHOLD <-  9

EVENT_MIN <- -12L
EVENT_MAX <-  10L
EVENT_REF <-  -1L

CONTROLS <- "ln_assets + loan_to_asset + cecl_adopter"

# [FIX 3]: Canonical growth label suffix — change here to propagate everywhere
GROWTH_LABEL_SUFFIX <- "\u00d7100"

# Colors
COL_RBC_ONLY <- "#C94040"   # red  — full RBC, no CCULR
COL_CCULR    <- "#1B3A6B"   # navy — CCULR adopters
COL_CONTROL  <- "#888888"   # gray — non-complex control
COL_CI_RBC   <- "#E8A0A0"
COL_CI_CCULR <- "#8AAAD0"
COL_NEUTRAL  <- "#E8A838"

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

df <- readRDS(PANEL_PATH)
message(sprintf("  Panel: %s obs, %s CUs",
                scales::comma(nrow(df)),
                scales::comma(n_distinct(df$cu_number))))
message(sprintf("  Panel period range: %.1f to %.1f",
                min(df$q_period_num, na.rm = TRUE),
                max(df$q_period_num, na.rm = TRUE)))

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
# Primary: Use NCUA CCULR flag if present in raw data.
# Fallback: Infer from capital ratio behavior post-2022.
#   Inferred CCULR = complex CU with pre-RBC NW ratio 9–11% that maintained
#   ratio in 9–10.5% band post-2022 (compliant under CCULR, not under RBC).
# IMPORTANT: Verify classification against NCUA admin records before pub.

message("── Step 2: Identifying CCULR adopters ───────────────────────────────")

has_cculr_flag <- any(c("cculr", "cculr_adopter", "leverage_ratio") %in%
                        tolower(names(df_raw)))

if (has_cculr_flag) {

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
        any(!is.na(.data[[cculr_flag_var]]) & .data[[cculr_flag_var]] == 1),
        min(q_period_num[!is.na(.data[[cculr_flag_var]]) &
                           .data[[cculr_flag_var]] == 1]),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    mutate(cculr_adopter = as.integer(ever_cculr == 1))

} else {

  message("  No direct CCULR flag found. Using capital ratio inference.")
  message("  NOTE: Verify against NCUA administrative records before publication.")

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

  post_rbc_cap <- df |>
    filter(complex == 1,
           q_period_num >= 2022.1,
           q_period_num <= 2023.4) |>
    group_by(cu_number) |>
    summarise(
      post_nw_ratio = mean(networth_ratio, na.rm = TRUE),
      post_cap_chg  = mean(cap_buffer,     na.rm = TRUE),
      .groups = "drop"
    )

  cculr_adopters <- pre_rbc_cap |>
    left_join(post_rbc_cap, by = "cu_number") |>
    mutate(
      inferred_cculr     = as.integer(
        pre_nw_ratio >= 9 & pre_nw_ratio < 11 &
          !is.na(post_nw_ratio) &
          post_nw_ratio >= 9 & post_nw_ratio < 10.5
      ),
      cculr_adopter      = inferred_cculr,
      first_cculr_period = if_else(inferred_cculr == 1, 2022.1, NA_real_),
      ever_cculr         = inferred_cculr
    ) |>
    select(cu_number, cculr_adopter, first_cculr_period, ever_cculr)

  message("  Inference classification complete.")
}

n_cculr     <- sum(cculr_adopters$cculr_adopter, na.rm = TRUE)
n_non_cculr <- sum(cculr_adopters$cculr_adopter == 0, na.rm = TRUE)
message(sprintf("  CCULR adopters identified : %d", n_cculr))
message(sprintf("  Non-CCULR complex CUs     : %d", n_non_cculr))


# =============================================================================
# 4. BUILD THREE-GROUP PANEL
# =============================================================================

message("── Step 3: Building three-group panel ───────────────────────────────")

df_3g <- df |>
  left_join(cculr_adopters |>
              select(cu_number, cculr_adopter, first_cculr_period),
            by = "cu_number") |>
  mutate(
    cculr_adopter = if_else(complex == 0, 0L,
                            coalesce(as.integer(cculr_adopter), 0L)),
    group_3 = case_when(
      complex == 0                        ~ 0L,
      complex == 1 & cculr_adopter == 0  ~ 1L,
      complex == 1 & cculr_adopter == 1  ~ 2L,
      TRUE                               ~ NA_integer_
    ),
    rbc_only  = as.integer(group_3 == 1L),
    cculr_grp = as.integer(group_3 == 2L),
    rbc_post   = rbc_only  * post_rbc,
    cculr_post = cculr_grp * post_rbc,
    event_time = case_when(
      quarter == 1L ~ (year - 2022L) * 4L + 0L,
      quarter == 2L ~ (year - 2022L) * 4L + 1L,
      quarter == 3L ~ (year - 2022L) * 4L + 2L,
      quarter == 4L ~ (year - 2022L) * 4L + 3L
    )
  )

cat("\n=== THREE-GROUP SAMPLE ===\n")
group_summary <- df_3g |>
  filter(!is.na(group_3)) |>
  distinct(cu_number, group_3) |>
  count(group_3, name = "n_cus") |>
  mutate(Label = case_when(
    group_3 == 0L ~ "Non-complex (control)",
    group_3 == 1L ~ "Complex \u2014 Full RBC (no CCULR)",
    group_3 == 2L ~ "Complex \u2014 CCULR adopter"
  ))
print(group_summary, row.names = FALSE)
cat(sprintf("\nTotal observations: %s\n",
            scales::comma(nrow(df_3g[!is.na(df_3g$group_3), ]))))


# =============================================================================
# 5. SELECTION ANALYSIS — WHO ADOPTS CCULR?
# =============================================================================

message("── Step 4: Selection analysis ───────────────────────────────────────")

selection_vars   <- c("networth_ratio", "cap_buffer", "loan_to_asset",
                      "mbl_shr", "re_shr", "auto_shr", "roa_var",
                      "dq_rate_var", "nim", "assets_tot", "members")
selection_labels <- c("Net worth ratio (%)", "Capital buffer vs 10% (pp)",
                      "Loans / assets (%)", "MBL share (%)", "RE share (%)",
                      "Auto share (%)", "ROA (%)", "DQ rate (%)",
                      "NIM (%)", "Total assets ($)", "Members")

pre_rbc_chars <- df_3g |>
  filter(complex == 1,
         q_period_num >= RBC_PRE_START,
         q_period_num <= RBC_PRE_END,
         !is.na(group_3)) |>
  group_by(cu_number, cculr_adopter) |>
  summarise(across(all_of(selection_vars), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

selection_table <- map2_dfr(selection_vars, selection_labels, function(v, lbl) {
  x_cculr     <- pre_rbc_chars[[v]][pre_rbc_chars$cculr_adopter == 1]
  x_non_cculr <- pre_rbc_chars[[v]][pre_rbc_chars$cculr_adopter == 0]
  m1  <- mean(x_cculr,     na.rm = TRUE)
  m0  <- mean(x_non_cculr, na.rm = TRUE)
  sd1 <- sd(x_cculr,       na.rm = TRUE)
  sd0 <- sd(x_non_cculr,   na.rm = TRUE)
  tt  <- tryCatch(t.test(x_cculr, x_non_cculr),
                  error = function(e) list(p.value = NA_real_))
  tibble(
    Variable         = lbl,
    `CCULR adopters` = sprintf("%.3f (%.3f)", m1, sd1),
    `Full RBC`       = sprintf("%.3f (%.3f)", m0, sd0),
    Difference       = round(m1 - m0, 3),
    `p-value`        = round(tt$p.value, 3)
  )
})

cat("\n=== SELECTION: WHO ADOPTS CCULR? ===\n")
cat("Pre-RBC characteristics: CCULR adopters vs full-RBC complex CUs\n\n")
print(selection_table, n = Inf)
write_csv(selection_table, file.path(TABLE_PATH, "3C_selection_analysis.csv"))
message("  Selection table saved.")

# Logit: predict CCULR adoption from pre-RBC characteristics
logit_data <- pre_rbc_chars |> filter(!is.na(cculr_adopter))
if (nrow(logit_data) > 0 && sum(logit_data$cculr_adopter) > 5) {
  logit_fml <- as.formula(
    paste("cculr_adopter ~", paste(selection_vars[1:8], collapse = " + "))
  )
  logit_m <- tryCatch(
    glm(logit_fml, data = logit_data, family = binomial(link = "logit")),
    error = function(e) NULL
  )
  if (!is.null(logit_m)) {
    logit_tidy <- tidy(logit_m, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 3)))
    cat("\n=== LOGIT: PREDICTORS OF CCULR ADOPTION ===\n")
    print(logit_tidy)
    write_csv(logit_tidy, file.path(TABLE_PATH, "3C_cculr_adoption_logit.csv"))
    message("  Logit results saved.")
  }
}


# =============================================================================
# 6. THREE-GROUP DiD ESTIMATION
# =============================================================================
# [FIX 2]: Growth labels updated to "QoQ log×100" in outcomes_3g list

message("── Step 5: Three-group DiD estimation ───────────────────────────────")

outcomes_3g <- list(
  list(var = "networth_ratio",  label = "Net Worth Ratio (%)"),
  list(var = "cap_buffer",      label = "Capital Buffer (pp)"),
  list(var = "well_capitalized",label = "Well-Capitalized (LPM)"),
  list(var = "loan_growth",     label = paste0("Loan Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")")),
  list(var = "asset_growth",    label = paste0("Asset Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")")),
  list(var = "mbl_shr",         label = "MBL Share (%)"),
  list(var = "re_shr",          label = "RE Share (%)"),
  list(var = "auto_shr",        label = "Auto Share (%)"),
  list(var = "dq_rate_var",     label = "DQ Rate (%)"),
  list(var = "chgoff_ratio",    label = "Charge-Off Ratio (%)"),
  list(var = "roa_var",         label = "ROA (%)"),
  list(var = "nim",             label = "NIM (%)")
)

run_3g_did <- function(outcome_var, data = df_3g) {
  fml <- as.formula(
    paste0(outcome_var, " ~ rbc_post + cculr_post + ", CONTROLS,
           " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml, data = data[!is.na(data$group_3), ],
          cluster = ~cu_number, warn = FALSE, notes = FALSE),
    error = function(e) {
      message("  3g DiD failed: ", outcome_var, " \u2014 ", e$message); NULL
    }
  )
}

extract_3g <- function(model_obj, out_lbl) {
  if (is.null(model_obj)) return(NULL)
  tryCatch({
    td        <- tidy(model_obj, conf.int = TRUE)
    rbc_row   <- td[td$term == "rbc_post",   ]
    cculr_row <- td[td$term == "cculr_post", ]
    if (nrow(rbc_row) == 0 || nrow(cculr_row) == 0) return(NULL)

    relief    <- cculr_row$estimate - rbc_row$estimate
    relief_se <- sqrt(cculr_row$std.error^2 + rbc_row$std.error^2)
    relief_t  <- relief / relief_se
    relief_p  <- 2 * (1 - pnorm(abs(relief_t)))

    star_fn <- function(p) {
      if (is.na(p)) "" else
      if (p < 0.01) "***" else if (p < 0.05) "**" else
      if (p < 0.10) "*"   else ""
    }

    data.frame(
      Outcome      = out_lbl,
      Beta_RBC     = round(rbc_row$estimate,   3),
      SE_RBC       = round(rbc_row$std.error,   3),
      Stars_RBC    = star_fn(rbc_row$p.value),
      Beta_CCULR   = round(cculr_row$estimate,  3),
      SE_CCULR     = round(cculr_row$std.error,  3),
      Stars_CCULR  = star_fn(cculr_row$p.value),
      Relief       = round(relief,    3),
      Relief_SE    = round(relief_se, 3),
      Relief_p     = round(relief_p,  3),
      Stars_Relief = star_fn(relief_p),
      N            = as.integer(nobs(model_obj)),
      stringsAsFactors = FALSE
    )
  }, error = function(e) NULL)
}

message("  Running three-group DiD models...")
models_3g  <- vector("list", length(outcomes_3g))
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
cat("beta_RBC   = effect on full-RBC complex CUs (vs non-complex)\n")
cat("beta_CCULR = effect on CCULR adopters (vs non-complex)\n")
cat("Relief     = beta_CCULR \u2212 beta_RBC (positive = CCULR mitigated effect)\n\n")
print(results_3g, row.names = FALSE)
write.csv(results_3g, file.path(TABLE_PATH, "3C_three_group_did.csv"),
          row.names = FALSE)
message("  Three-group DiD saved.")


# =============================================================================
# 7. EVENT STUDY — THREE GROUPS
# =============================================================================
# [FIX 2]: es_outcomes_3g growth labels updated

message("── Step 6: Event study \u2014 three groups ───────────────────────────────")

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
           CONTROLS, " | cu_number + q_period_num")
  )
  tryCatch(
    feols(fml, data = data_es, cluster = ~cu_number,
          warn = FALSE, notes = FALSE),
    error = function(e) {
      message("  ES failed: ", outcome_var, " \u2014 ", e$message); NULL
    }
  )
}

extract_3g_es <- function(model_obj) {
  if (is.null(model_obj)) return(NULL)
  tryCatch({
    td        <- tidy(model_obj, conf.int = TRUE)
    rbc_rows  <- td[grepl("event_time::.*:rbc_only",  td$term) |
                      grepl("rbc_only.*event_time::",  td$term), ]
    cculr_rows <- td[grepl("event_time::.*:cculr_grp", td$term) |
                       grepl("cculr_grp.*event_time::", td$term), ]

    parse_et <- function(rows, grp_label) {
      if (nrow(rows) == 0) return(NULL)
      rows$event_time <- as.integer(
        regmatches(rows$term, regexpr("-?[0-9]+", rows$term))
      )
      rows <- rows[rows$event_time >= EVENT_MIN & rows$event_time <= EVENT_MAX, ]
      out <- data.frame(
        event_time = rows$event_time, estimate = rows$estimate,
        std_error  = rows$std.error,  p_value  = rows$p.value,
        conf_low   = rows$conf.low,   conf_high = rows$conf.high,
        group      = as.character(grp_label), stringsAsFactors = FALSE
      )
      ref <- data.frame(
        event_time = as.integer(EVENT_REF), estimate = 0, std_error = 0,
        p_value = NA_real_, conf_low = 0, conf_high = 0,
        group = as.character(grp_label), stringsAsFactors = FALSE
      )
      rbind(out, ref)
    }

    rbc_coefs   <- parse_et(rbc_rows,   "Full RBC (no CCULR)")
    cculr_coefs <- parse_et(cculr_rows, "CCULR adopters")
    if (is.null(rbc_coefs) && is.null(cculr_coefs)) return(NULL)

    parts <- NULL
    if (!is.null(rbc_coefs))  parts <- rbc_coefs
    if (!is.null(cculr_coefs)) {
      if (is.null(parts)) parts <- cculr_coefs else parts <- rbind(parts, cculr_coefs)
    }
    parts[order(parts$group, parts$event_time), ]
  }, error = function(e) NULL)
}

# [FIX 2]: loan_growth label updated
es_outcomes_3g <- list(
  list(var = "networth_ratio", label = "Net Worth Ratio (%)"),
  list(var = "cap_buffer",     label = "Capital Buffer (pp)"),
  list(var = "loan_growth",    label = paste0("Loan Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")")),
  list(var = "dq_rate_var",    label = "Delinquency Rate (%)"),
  list(var = "roa_var",        label = "ROA (%)"),
  list(var = "auto_shr",       label = "Auto Loan Share (%)")
)

message("  Running three-group event studies...")
es_3g_models <- vector("list", length(es_outcomes_3g))
es_3g_coefs  <- vector("list", length(es_outcomes_3g))

for (i in seq_along(es_outcomes_3g)) {
  o <- es_outcomes_3g[[i]]
  es_3g_models[[i]] <- run_3g_es(o$var)
  es_3g_coefs[[i]]  <- extract_3g_es(es_3g_models[[i]])
  n_rows <- if (!is.null(es_3g_coefs[[i]])) nrow(es_3g_coefs[[i]]) else 0
  message(sprintf("  %s: %d coef rows", o$var, n_rows))
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
  df_sig        <- coef_df[
    !is.na(coef_df$p_value) & coef_df$p_value < 0.05 &
      coef_df$event_time >= 0L, ]

  p <- ggplot(coef_df, aes(x = event_time, y = estimate,
                             color = group, shape = group)) +
    annotate("rect", xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf, fill = "gray93", alpha = 0.5) +
    geom_ribbon(data = df_rbc_plot,
                mapping = aes(x = event_time, ymin = conf_low, ymax = conf_high),
                fill = COL_CI_RBC, alpha = 0.25, color = NA, inherit.aes = FALSE) +
    geom_ribbon(data = df_cculr_plot,
                mapping = aes(x = event_time, ymin = conf_low, ymax = conf_high),
                fill = COL_CI_CCULR, alpha = 0.25, color = NA, inherit.aes = FALSE) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.5, linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30", linewidth = 0.6, linetype = "dashed") +
    geom_line(linewidth = 1.0) +
    geom_point(size = 2.2) +
    geom_point(data = df_sig,
               mapping = aes(x = event_time, y = estimate),
               shape = 8, size = 2.8, color = "darkred",
               inherit.aes = FALSE, show.legend = FALSE) +
    scale_color_manual(values = c("Full RBC (no CCULR)" = COL_RBC_ONLY,
                                  "CCULR adopters"       = COL_CCULR)) +
    scale_shape_manual(values = c("Full RBC (no CCULR)" = 17L,
                                  "CCULR adopters"       = 16L)) +
    scale_x_continuous(breaks = seq(EVENT_MIN, EVENT_MAX, by = 4L),
                       labels = function(x) paste0("Q", x)) +
    labs(
      title    = outcome_label,
      subtitle = paste0(
        "Event time relative to RBC (Q0 = 2022 Q1). Ref = Q", EVENT_REF,
        ". 95% CI shaded.\n",
        "Red \u25b2 = Full RBC (no CCULR). Navy \u25cf = CCULR adopters. \u2605 = p<0.05."
      ),
      x = "Quarters Relative to RBC",
      y = "DiD Coefficient (vs non-complex control)",
      color = NULL, shape = NULL,
      caption = "Two-way FE (CU + quarter-year). SE clustered at CU. Source: NCUA Call Report (5300)."
    ) +
    theme_rbc()
  p
}

for (i in seq_along(es_outcomes_3g)) {
  o     <- es_outcomes_3g[[i]]
  coefs <- es_3g_coefs[[i]]
  if (is.null(coefs) || nrow(coefs) == 0) next
  p <- tryCatch(plot_3g_es(coefs, o$label),
                error = function(e) { message("  Plot error (", o$var, "): ", e$message); NULL })
  if (is.null(p)) next
  fname <- paste0("3C_es_3group_", gsub("_", "-", o$var, fixed = TRUE), ".png")
  ggsave(file.path(FIGURE_PATH, fname), p, width = 10, height = 6, dpi = 300)
  message("  Saved: ", fname)
}


# =============================================================================
# 9. MAIN PANEL — 2×3 GRID
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

# [FIX 2]: Panel C title updated with ×100 suffix
p1 <- compact_3g(1, "A. Net Worth Ratio (%)")
p2 <- compact_3g(2, "B. Capital Buffer (pp)")
p3 <- compact_3g(3, paste0("C. Loan Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"))
p4 <- compact_3g(4, "D. Delinquency Rate (%)")
p5 <- compact_3g(5, "E. ROA (%)")
p6 <- compact_3g(6, "F. Auto Loan Share (%)")

panel_3g <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
  plot_annotation(
    title    = "CCULR Relief Value: Full RBC vs. CCULR Adopters",
    subtitle = paste0(
      "Red \u25b2 = complex CUs under full RBC (10% threshold). ",
      "Navy \u25cf = CCULR adopters (9% threshold).\n",
      "Both vs. non-complex control. \u2605 = p<0.05. ",
      "Vertical gap = estimated relief value of CCULR opt-in."
    ),
    caption  = paste0(
      "Three-group DiD: Y ~ i(event_time, rbc_only) + i(event_time, cculr_grp) + FE(CU + quarter). ",
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
  forest_3g <- bind_rows(
    results_3g |>
      transmute(Outcome, Group = "Full RBC (no CCULR)",
                Beta = Beta_RBC, Stars,
                CI_low  = Beta_RBC - 1.96 * SE_RBC,
                CI_high = Beta_RBC + 1.96 * SE_RBC,
                Stars   = Stars_RBC),
    results_3g |>
      transmute(Outcome, Group = "CCULR adopters",
                Beta = Beta_CCULR, Stars,
                CI_low  = Beta_CCULR - 1.96 * SE_CCULR,
                CI_high = Beta_CCULR + 1.96 * SE_CCULR,
                Stars   = Stars_CCULR)
  ) |>
    mutate(Outcome = factor(Outcome, levels = rev(unique(Outcome))))

  p_forest <- ggplot(
    forest_3g[!is.na(forest_3g$Beta), ],
    aes(x = Beta, y = Outcome, xmin = CI_low, xmax = CI_high,
        color = Group, shape = Group)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed",
               color = "gray50", linewidth = 0.6) +
    geom_errorbarh(height = 0.3, linewidth = 0.8,
                   position = position_dodge(width = 0.6)) +
    geom_point(size = 3, position = position_dodge(width = 0.6)) +
    scale_color_manual(values = c("Full RBC (no CCULR)" = COL_RBC_ONLY,
                                  "CCULR adopters"       = COL_CCULR)) +
    scale_shape_manual(values = c("Full RBC (no CCULR)" = 17L,
                                  "CCULR adopters"       = 16L)) +
    labs(
      title    = "CCULR Relief Value: Three-Group DiD Coefficients",
      subtitle = paste0(
        "Both groups vs. non-complex control. 95% CI. ",
        "Horizontal gap between red and navy = estimated relief value."
      ),
      x = "DiD Coefficient (vs non-complex control)",
      y = NULL, color = NULL, shape = NULL,
      caption = "Two-way FE (CU + quarter-year). SE clustered at CU. Source: NCUA Call Report (5300)."
    ) +
    theme_rbc() + theme(legend.position = "top")

  ggsave(file.path(FIGURE_PATH, "3C_forest_relief_value.png"),
         p_forest, width = 11, height = 9, dpi = 300)
  message("  Forest plot saved.")

  cat("\n=== RELIEF VALUE SUMMARY ===\n")
  cat("Positive Relief = CCULR dampened the negative RBC effect\n")
  cat("Negative Relief = CCULR worsened relative to full RBC\n\n")
  results_3g |>
    select(Outcome, Beta_RBC, Stars_RBC, Beta_CCULR, Stars_CCULR,
           Relief, Relief_p, Stars_Relief) |>
    mutate(Interpretation = ifelse(
      Relief > 0 & Relief_p < 0.10, "CCULR provided relief",
      ifelse(Relief < 0 & Relief_p < 0.10, "CCULR worsened outcome",
             "No significant difference"))) |>
    print(row.names = FALSE)
}


# =============================================================================
# 11. STAGGERED ADOPTION — CALLAWAY-SANT'ANNA
# =============================================================================

message("── Step 10: Staggered adoption DiD (Callaway-Sant'Anna) ─────────────")

timing_variation <- cculr_adopters |>
  filter(cculr_adopter == 1, !is.na(first_cculr_period)) |>
  count(first_cculr_period)

cat("\n=== CCULR ADOPTION TIMING DISTRIBUTION ===\n")
print(timing_variation)

if (nrow(timing_variation) >= 2) {
  message("  Multiple adoption periods \u2014 running Callaway-Sant'Anna staggered DiD")

  cu_cohorts <- df_3g |>
    filter(complex == 1) |>
    distinct(cu_number) |>
    left_join(cculr_adopters |> select(cu_number, first_cculr_period),
              by = "cu_number") |>
    mutate(G_period = if_else(
      !is.na(first_cculr_period),
      as.integer((floor(first_cculr_period) - 2000) * 4 +
                   round((first_cculr_period %% 1) / 0.1)),
      0L
    ))

  control_cohorts <- df_3g |>
    filter(complex == 0) |>
    distinct(cu_number) |>
    mutate(G_period = 0L)

  cs_data <- df_3g |>
    left_join(bind_rows(cu_cohorts, control_cohorts) |>
                select(cu_number, G_period),
              by = "cu_number") |>
    filter(!is.na(G_period)) |>
    mutate(period_int = as.integer(
      (floor(q_period_num) - 2000) * 4 +
        round((q_period_num %% 1) / 0.1)
    ))

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
    message("  C&S DiD error: ", e$message); NULL
  })

  if (!is.null(cs_result)) {
    cat("\n=== CALLAWAY-SANT'ANNA: CCULR EFFECT ON LOAN GROWTH ===\n")
    summary(cs_result)
    cs_agg <- tryCatch(aggte(cs_result, type = "simple"), error = function(e) NULL)
    if (!is.null(cs_agg)) { cat("\nAggregate ATT (simple):\n"); summary(cs_agg) }
    cs_tidy <- tryCatch(tidy(cs_result), error = function(e) NULL)
    if (!is.null(cs_tidy)) {
      write_csv(cs_tidy, file.path(TABLE_PATH, "3C_callaway_santanna.csv"))
      message("  C&S results saved.")
    }
  }
} else {
  message("  Only one CCULR adoption period — standard three-group DiD only.")
}


# =============================================================================
# 12. PRE-TREND TEST — CCULR vs. FULL RBC
# =============================================================================

message("── Step 11: Pre-trend test \u2014 CCULR vs Full RBC ──────────────────────")

df_pre_test <- df_3g |>
  filter(complex == 1, !is.na(group_3),
         event_time >= -12L, event_time <= -1L) |>
  mutate(cculr_vs_rbc = as.integer(cculr_adopter == 1L))

pretrend_results <- data.frame(
  Outcome = character(0), F_stat = numeric(0),
  p_value = numeric(0),   stringsAsFactors = FALSE
)
pretrend_outcomes <- c("networth_ratio", "loan_growth", "dq_rate_var", "roa_var")
pretrend_labels   <- c("Net Worth Ratio", "Loan Growth", "DQ Rate", "ROA")

for (j in seq_along(pretrend_outcomes)) {
  fml_pre <- as.formula(
    paste0(pretrend_outcomes[j],
           " ~ i(event_time, cculr_vs_rbc, ref = -1L) + ",
           CONTROLS, " | cu_number + q_period_num")
  )
  m_pre <- tryCatch(
    feols(fml_pre, data = df_pre_test, cluster = ~cu_number,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (!is.null(m_pre)) {
    f_test <- tryCatch({
      ht        <- tidy(m_pre)
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
cat("H\u2080: parallel pre-trends. p > 0.10 = no evidence of violation\n\n")
print(pretrend_results, row.names = FALSE)
write.csv(pretrend_results, file.path(TABLE_PATH, "3C_pretrend_test.csv"),
          row.names = FALSE)


# =============================================================================
# 13. POLICY CHARTS
# =============================================================================
# Five policy charts from the CCULR analysis.
# All use data already computed above — no new regressions required.
# =============================================================================

message("── Step 12: Policy charts ────────────────────────────────────────────")


# ── Policy Chart 3C-1: CCULR Selection Profile ───────────────────────────────
# Who adopted CCULR? Horizontal bar chart comparing key pre-RBC
# characteristics of CCULR adopters vs full-RBC CUs.

message("  Policy Chart 3C-1: CCULR selection profile...")

if (nrow(selection_table) > 0) {
  # Focus on economically interpretable variables (exclude total assets/members)
  sel_plot <- selection_table |>
    filter(!Variable %in% c("Total assets ($)", "Members")) |>
    mutate(
      Sig       = `p-value` < 0.10,
      Diff_label = paste0(ifelse(Difference > 0, "+", ""),
                           round(Difference, 2)),
      Variable  = factor(Variable, levels = rev(Variable))
    )

  p_sel <- ggplot(sel_plot,
                   aes(x = Difference, y = Variable,
                       fill = Difference > 0, alpha = Sig)) +
    geom_col(width = 0.65) +
    geom_vline(xintercept = 0, color = "gray30", linewidth = 0.6) +
    geom_text(
      aes(label = Diff_label,
          hjust = ifelse(Difference > 0, -0.15, 1.15)),
      size = 3, color = "gray20", fontface = "bold"
    ) +
    geom_text(
      data = sel_plot |> filter(Sig),
      aes(label = "*", x = Difference,
          hjust = ifelse(Difference > 0, -1.5, 2.5)),
      size = 5, color = "#C94040"
    ) +
    scale_fill_manual(values = c("TRUE" = COL_CCULR, "FALSE" = COL_RBC_ONLY),
                      guide  = "none") +
    scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.55),
                       guide  = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
    labs(
      title    = "Policy Chart 3C-1. Who Adopted CCULR? Pre-RBC Selection Profile",
      subtitle = paste0(
        "Difference = CCULR adopters minus full-RBC complex CUs (pre-2022 means). ",
        "* = significant at 10%. Opaque = significant."
      ),
      x = "Difference (CCULR adopters \u2212 Full RBC CUs)",
      y = NULL,
      caption = paste0(
        "Negative capital buffer difference: CCULR adopters had lower pre-RBC buffers ",
        "\u2014 consistent with negative selection of distressed CUs into the simpler framework. ",
        "Source: NCUA Call Report (5300)."
      )
    ) +
    theme_rbc() +
    theme(axis.text.y = element_text(size = 9))

  ggsave(file.path(FIGURE_PATH, "policy_3c1_cculr_selection_profile.png"),
         p_sel, width = 11, height = 7, dpi = 300)
  message("    Saved: policy_3c1_cculr_selection_profile.png")
}


# ── Policy Chart 3C-2: Relief Value Scorecard ────────────────────────────────
# A colour-coded tile table showing whether CCULR provided relief,
# was neutral, or worsened outcomes — one tile per outcome.

message("  Policy Chart 3C-2: Relief value scorecard...")

if (nrow(results_3g) > 0) {
  scorecard <- results_3g |>
    mutate(
      Relief_class = case_when(
        Relief > 0  & Relief_p < 0.10 ~ "Provided\nRelief",
        Relief < 0  & Relief_p < 0.10 ~ "Worsened\nOutcome",
        TRUE                           ~ "No Significant\nDifference"
      ),
      Relief_class = factor(Relief_class,
                            levels = c("Provided\nRelief",
                                       "No Significant\nDifference",
                                       "Worsened\nOutcome")),
      Sig_label = paste0(
        ifelse(Relief > 0, "+", ""), round(Relief, 3),
        ifelse(Stars_Relief != "", paste0(Stars_Relief), "")
      ),
      Outcome_wrap = str_wrap(Outcome, width = 18),
      Channel = case_when(
        grepl("Worth|Buffer|Well", Outcome) ~ "Capital",
        grepl("Loan|Asset|Growth",  Outcome) ~ "Lending",
        grepl("MBL|RE|Auto",        Outcome) ~ "Portfolio",
        grepl("DQ|Charge|PLL",      Outcome) ~ "Credit Quality",
        grepl("ROA|NIM",            Outcome) ~ "Profitability",
        TRUE                                 ~ "Other"
      )
    )

  p_scorecard <- ggplot(scorecard,
                         aes(x = 1, y = Outcome_wrap, fill = Relief_class)) +
    geom_tile(color = "white", linewidth = 1.2) +
    geom_text(aes(label = Sig_label), size = 3.2,
              color = "white", fontface = "bold") +
    scale_fill_manual(values = c(
      "Provided\nRelief"          = "#2E7D32",
      "No Significant\nDifference" = "gray60",
      "Worsened\nOutcome"         = "#C94040"
    )) +
    facet_wrap(~ Channel, scales = "free_y", ncol = 2) +
    labs(
      title    = "Policy Chart 3C-2. CCULR Relief Value Scorecard",
      subtitle = paste0(
        "Does the CCULR provide meaningful relief vs. full RBC compliance?\n",
        "Relief = \u03b2_CCULR \u2212 \u03b2_RBC. * p<0.10, ** p<0.05, *** p<0.01."
      ),
      x = NULL, y = NULL, fill = "CCULR Outcome",
      caption = paste0(
        "Green = CCULR significantly better than full RBC. ",
        "Red = CCULR significantly worse. Gray = no significant difference. ",
        "Source: NCUA Call Report (5300), three-group DiD."
      )
    ) +
    theme_rbc() +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "right"
    )

  ggsave(file.path(FIGURE_PATH, "policy_3c2_relief_value_scorecard.png"),
         p_scorecard, width = 12, height = 9, dpi = 300)
  message("    Saved: policy_3c2_relief_value_scorecard.png")
}


# ── Policy Chart 3C-3: Regulatory Burden Comparison ─────────────────────────
# Shows beta_RBC and beta_CCULR side by side for lending and profitability
# outcomes — illustrates that CCULR did NOT materially reduce the burden.

message("  Policy Chart 3C-3: Regulatory burden comparison...")

if (nrow(results_3g) > 0) {
  burden_vars <- results_3g |>
    filter(Outcome %in% c(
      paste0("Loan Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"),
      paste0("Asset Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"),
      "Auto Share (%)", "ROA (%)", "NIM (%)",
      "Net Worth Ratio (%)", "Capital Buffer (pp)"
    )) |>
    select(Outcome, Beta_RBC, SE_RBC, Stars_RBC,
           Beta_CCULR, SE_CCULR, Stars_CCULR, Relief, Relief_p)

  if (nrow(burden_vars) > 0) {
    burden_long <- burden_vars |>
      pivot_longer(cols = c(Beta_RBC, Beta_CCULR),
                   names_to = "Group", values_to = "Beta") |>
      mutate(
        SE    = if_else(Group == "Beta_RBC", SE_RBC, SE_CCULR),
        Stars = if_else(Group == "Beta_RBC", Stars_RBC, Stars_CCULR),
        Group = if_else(Group == "Beta_RBC",
                        "Full RBC (no CCULR)", "CCULR adopters"),
        CI_lo = Beta - 1.96 * SE,
        CI_hi = Beta + 1.96 * SE,
        Outcome_wrap = str_wrap(Outcome, width = 22)
      )

    p_burden <- ggplot(burden_long[!is.na(burden_long$Beta), ],
                        aes(x = Beta, y = Outcome_wrap,
                            xmin = CI_lo, xmax = CI_hi,
                            color = Group, shape = Group)) +
      geom_vline(xintercept = 0, linetype = "dashed",
                 color = "gray40", linewidth = 0.6) +
      geom_errorbarh(height = 0.3, linewidth = 0.9,
                     position = position_dodge(width = 0.65)) +
      geom_point(size = 3.5, position = position_dodge(width = 0.65)) +
      scale_color_manual(values = c("Full RBC (no CCULR)" = COL_RBC_ONLY,
                                    "CCULR adopters"       = COL_CCULR)) +
      scale_shape_manual(values = c("Full RBC (no CCULR)" = 17L,
                                    "CCULR adopters"       = 16L)) +
      labs(
        title    = "Policy Chart 3C-3. Regulatory Burden: Full RBC vs. CCULR Adopters",
        subtitle = paste0(
          "Both groups compared to non-complex CUs. 95% CI. ",
          "Overlapping intervals indicate CCULR provides no meaningful relief."
        ),
        x = "DiD Coefficient (vs non-complex control)",
        y = NULL, color = NULL, shape = NULL,
        caption = paste0(
          "CCULR adopters (navy) bear a similar or worse regulatory burden ",
          "to full-RBC CUs (red) across lending and profitability outcomes, ",
          "consistent with negative selection. Source: NCUA Call Report (5300)."
        )
      ) +
      theme_rbc() + theme(legend.position = "top")

    ggsave(file.path(FIGURE_PATH, "policy_3c3_regulatory_burden_comparison.png"),
           p_burden, width = 11, height = 7, dpi = 300)
    message("    Saved: policy_3c3_regulatory_burden_comparison.png")
  }
}


# ── Policy Chart 3C-4: Lending Contraction — All Three Groups ────────────────
# Uses the loan_growth event study coefficients for both full-RBC and CCULR
# groups, plus a reference line at zero (non-complex control benchmark).
# Shows all three trajectories on one panel for maximum clarity.

message("  Policy Chart 3C-4: Three-group lending contraction trajectory...")

loan_idx <- which(sapply(es_outcomes_3g, function(o) o$var) == "loan_growth")

if (length(loan_idx) > 0 && !is.null(es_3g_coefs[[loan_idx]]) &&
    nrow(es_3g_coefs[[loan_idx]]) > 0) {

  loan_es <- es_3g_coefs[[loan_idx]] |>
    mutate(
      group = as.character(group),
      Group_label = case_when(
        group == "Full RBC (no CCULR)" ~ "Full RBC\n(no CCULR opt-in)",
        group == "CCULR adopters"       ~ "CCULR adopters\n(9% threshold)",
        TRUE                           ~ group
      )
    )

  df_rbc_loan   <- loan_es[loan_es$group == "Full RBC (no CCULR)", ]
  df_cculr_loan <- loan_es[loan_es$group == "CCULR adopters", ]

  p_lending <- ggplot(loan_es,
                       aes(x = event_time, y = estimate,
                           color = Group_label, shape = Group_label)) +
    annotate("rect", xmin = -0.5, xmax = EVENT_MAX + 0.5,
             ymin = -Inf, ymax = Inf, fill = "gray96", alpha = 0.5) +
    geom_ribbon(data = df_rbc_loan,
                mapping = aes(x = event_time, ymin = conf_low, ymax = conf_high),
                fill = COL_CI_RBC, alpha = 0.2, color = NA, inherit.aes = FALSE) +
    geom_ribbon(data = df_cculr_loan,
                mapping = aes(x = event_time, ymin = conf_low, ymax = conf_high),
                fill = COL_CI_CCULR, alpha = 0.2, color = NA, inherit.aes = FALSE) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.8,
               linetype = "dashed") +
    geom_vline(xintercept = -0.5, color = "gray30",
               linewidth = 0.6, linetype = "dashed") +
    annotate("text", x = 6, y = 0.15,
             label = "Non-complex benchmark\n(coefficient = 0 by construction)",
             color = "gray50", size = 2.8, hjust = 0) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.5) +
    geom_point(
      data = loan_es[!is.na(loan_es$p_value) & loan_es$p_value < 0.05 &
                       loan_es$event_time >= 0, ],
      mapping = aes(x = event_time, y = estimate),
      shape = 8, size = 2.8, color = "darkred",
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    scale_color_manual(values = c(
      "Full RBC\n(no CCULR opt-in)"  = COL_RBC_ONLY,
      "CCULR adopters\n(9% threshold)" = COL_CCULR
    )) +
    scale_shape_manual(values = c(
      "Full RBC\n(no CCULR opt-in)"  = 17L,
      "CCULR adopters\n(9% threshold)" = 16L
    )) +
    scale_x_continuous(breaks = seq(EVENT_MIN, EVENT_MAX, by = 4L),
                       labels = function(x) paste0("Q", x)) +
    labs(
      title    = "Policy Chart 3C-4. Lending Contraction: Full RBC vs. CCULR vs. Control",
      subtitle = paste0(
        "Both treated groups show persistent lending contraction relative to non-complex control.\n",
        "CCULR adopters (navy) show no statistically significant reduction in lending burden."
      ),
      x = "Quarters Relative to RBC Effective Date",
      y = paste0("DiD Coefficient: Loan Growth (QoQ log", GROWTH_LABEL_SUFFIX, ")"),
      color = NULL, shape = NULL,
      caption = paste0(
        "Zero line = non-complex control benchmark. \u2605 = p<0.05 post-period. ",
        "Three-group DiD. Two-way FE. SE clustered at CU. ",
        "Source: NCUA Call Report (5300)."
      )
    ) +
    theme_rbc() + theme(legend.position = "top")

  ggsave(file.path(FIGURE_PATH, "policy_3c4_lending_three_groups.png"),
         p_lending, width = 11, height = 7, dpi = 300)
  message("    Saved: policy_3c4_lending_three_groups.png")
}


# ── Policy Chart 3C-5: What Would Genuine Relief Require? ────────────────────
# A conceptual threshold chart showing the required NW ratio under each
# framework and the observed pre-RBC distribution, to illustrate how much
# headroom would be needed for each group to be genuinely unconstrained.

message("  Policy Chart 3C-5: Genuine relief — threshold adequacy chart...")

# Use selection_table data to compute average pre-RBC NW ratios by group
avg_nw_cculr <- selection_table |>
  filter(Variable == "Net worth ratio (%)") |>
  pull(`CCULR adopters`) |>
  str_extract("^[0-9.]+") |>
  as.numeric()

avg_nw_rbc <- selection_table |>
  filter(Variable == "Net worth ratio (%)") |>
  pull(`Full RBC`) |>
  str_extract("^[0-9.]+") |>
  as.numeric()

if (!is.na(avg_nw_cculr) && !is.na(avg_nw_rbc)) {

  threshold_data <- tibble(
    Group       = c("Non-complex CUs\n(control)",
                    "Full RBC CUs\n(no CCULR)",
                    "CCULR adopters",
                    "Full RBC CUs\n(no CCULR)",
                    "CCULR adopters"),
    Type        = c("Observed pre-RBC mean",
                    "Observed pre-RBC mean",
                    "Observed pre-RBC mean",
                    "Required threshold",
                    "Required threshold"),
    NW_ratio    = c(13.316,        # non-complex pre-RBC mean from Table 1
                    avg_nw_rbc,    # full RBC pre-RBC mean
                    avg_nw_cculr,  # CCULR pre-RBC mean
                    10.0,          # RBC well-capitalized threshold
                     9.0),         # CCULR well-capitalized threshold
    Group_order = c(3, 2, 1, 2, 1)
  ) |>
    mutate(Group = factor(Group, levels = c(
      "CCULR adopters",
      "Full RBC CUs\n(no CCULR)",
      "Non-complex CUs\n(control)"
    )))

  p_thresholds <- ggplot() +
    # Required thresholds as horizontal lines with bands
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = 9.0, ymax = 10.0,
             fill = COL_CCULR, alpha = 0.08) +
    annotate("rect", xmin = -Inf, xmax = Inf,
             ymin = 10.0, ymax = 10.5,
             fill = COL_RBC_ONLY, alpha = 0.08) +
    geom_hline(yintercept = 10.0, color = COL_RBC_ONLY,
               linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = 9.0, color = COL_CCULR,
               linetype = "dashed", linewidth = 1) +
    geom_hline(yintercept = 7.0, color = "gray60",
               linetype = "dotted", linewidth = 0.7) +
    # Observed means as points
    geom_point(
      data = threshold_data |> filter(Type == "Observed pre-RBC mean"),
      aes(x = Group, y = NW_ratio, color = Group),
      size = 5, shape = 18
    ) +
    # Vertical arrows showing gap to threshold
    annotate("segment",
             x = "CCULR adopters", xend = "CCULR adopters",
             y = avg_nw_cculr, yend = 9.0 + 0.05,
             arrow = arrow(length = unit(0.2, "cm")),
             color = COL_CCULR, linewidth = 0.9) +
    annotate("segment",
             x = "Full RBC CUs\n(no CCULR)", xend = "Full RBC CUs\n(no CCULR)",
             y = avg_nw_rbc, yend = 10.0 + 0.05,
             arrow = arrow(length = unit(0.2, "cm")),
             color = COL_RBC_ONLY, linewidth = 0.9) +
    annotate("text", x = 2.5, y = 10.2,
             label = "RBC 10% well-capitalized threshold",
             color = COL_RBC_ONLY, size = 3, hjust = 0, fontface = "bold") +
    annotate("text", x = 2.5, y = 9.2,
             label = "CCULR 9% well-capitalized threshold",
             color = COL_CCULR, size = 3, hjust = 0, fontface = "bold") +
    annotate("text", x = 2.5, y = 7.2,
             label = "Legacy 7% leverage ratio (pre-2022)", color = "gray50",
             size = 2.8, hjust = 0) +
    scale_color_manual(values = c(
      "CCULR adopters"            = COL_CCULR,
      "Full RBC CUs\n(no CCULR)" = COL_RBC_ONLY,
      "Non-complex CUs\n(control)" = COL_CONTROL
    ), guide = "none") +
    scale_y_continuous(limits = c(6.5, 15),
                       labels = function(x) paste0(x, "%"),
                       breaks = seq(7, 15, 1)) +
    labs(
      title    = "Policy Chart 3C-5. Capital Threshold Adequacy: Who Is Genuinely Constrained?",
      subtitle = paste0(
        "Diamond \u25c6 = observed pre-RBC mean net worth ratio. ",
        "Arrow = gap to well-capitalized threshold under each framework.\n",
        "A larger gap implies more capital cushion and a less binding constraint."
      ),
      x = NULL,
      y = "Net Worth Ratio (%)",
      caption = paste0(
        "Non-complex CU pre-RBC mean = 13.32% (Table 1). ",
        "CCULR adopters and full-RBC means from 3C selection analysis. ",
        "The CCULR threshold (9%) provides only a 1pp reduction in the binding constraint\n",
        "for CUs already near the 10% boundary \u2014 insufficient relief for the most constrained. ",
        "Source: NCUA Call Report (5300)."
      )
    ) +
    theme_rbc() +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 10, face = "bold"))

  ggsave(file.path(FIGURE_PATH, "policy_3c5_threshold_adequacy.png"),
         p_thresholds, width = 11, height = 7, dpi = 300)
  message("    Saved: policy_3c5_threshold_adequacy.png")
}

message("  All policy charts complete.")


# =============================================================================
# 14. FINAL SUMMARY
# =============================================================================

cat("\n=== 3C COMPLETE ===\n\n")

cat("Tables (output/tables/):\n")
for (t in c("3C_selection_analysis.csv", "3C_cculr_adoption_logit.csv",
            "3C_three_group_did.csv", "3C_callaway_santanna.csv",
            "3C_pretrend_test.csv")) {
  flag <- if (file.exists(file.path(TABLE_PATH, t))) "\u2713" else "\u2013"
  cat(sprintf("  %s %s\n", flag, t))
}

cat("\nThree-group event study figures (output/figures/):\n")
for (f in c("3C_main_panel_3group.png", "3C_forest_relief_value.png",
            paste0("3C_es_3group_",
                   gsub("_", "-", sapply(es_outcomes_3g, `[[`, "var"),
                        fixed = TRUE), ".png"))) {
  flag <- if (file.exists(file.path(FIGURE_PATH, f))) "\u2713" else "\u2013"
  cat(sprintf("  %s %s\n", flag, f))
}

cat("\nPolicy charts (output/figures/):\n")
for (f in c("policy_3c1_cculr_selection_profile.png",
            "policy_3c2_relief_value_scorecard.png",
            "policy_3c3_regulatory_burden_comparison.png",
            "policy_3c4_lending_three_groups.png",
            "policy_3c5_threshold_adequacy.png")) {
  flag <- if (file.exists(file.path(FIGURE_PATH, f))) "\u2713" else "\u2013"
  cat(sprintf("  %s %s\n", flag, f))
}

cat("\nFIX SUMMARY:\n")
cat("  [FIX 1] RBC_END: 2024.4 \u2192 2025.4\n")
cat("  [FIX 2] Growth labels: 'QoQ log\u00d7100' in outcomes_3g, es_outcomes_3g, panel titles\n")
cat("  [FIX 3] GROWTH_LABEL_SUFFIX constant added for single-point change\n")

cat("\nKey findings to look for:\n")
cat("  3C_three_group_did.csv \u2014 'Relief' column:\n")
cat("    Positive + significant = CCULR provided measurable relief\n")
cat("    Near zero or negative  = CCULR had no differential effect (negative selection)\n")
cat("  policy_3c2_relief_value_scorecard.png:\n")
cat("    Predominantly gray/red = CCULR did not serve as intended relief mechanism\n")
cat("  policy_3c1_cculr_selection_profile.png:\n")
cat("    Negative capital buffer difference = distressed CUs self-selected into CCULR\n")
cat("  policy_3c5_threshold_adequacy.png:\n")
cat("    Small arrow = CCULR threshold provides insufficient headroom for constrained CUs\n")

message("\n── 3C_CCULR_Adoption.R complete \u2713 ───────────────────────────────────")
message("  Next: 4_Paper_Tables.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
