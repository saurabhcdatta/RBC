# =============================================================================
# 1_Descriptive_Stats.R
# RBC Rule Impact Analysis — Descriptive Statistics & Exploratory Analysis
# NCUA Call Report (5300) Data
#
# Author  : [Your Name]
# Created : 2026
# Purpose : Produce all pre-analysis descriptives:
#             (1) Balance table: complex vs. non-complex CUs pre-RBC
#             (2) Time-series trend plots: capital & loan outcomes by group
#             (3) Missing data map for spread variables
#             (4) Distribution plots: net worth ratio at threshold
#             (5) Summary statistics table (publication-ready)
#
# Input  : data/analysis_panel.rds  (from 0_Data_Prep.R)
# Output : output/tables/  — CSV and Word-ready tables
#          output/figures/ — PNG plots
# =============================================================================


# =============================================================================
# 0. LIBRARIES
# =============================================================================

library(tidyverse)
library(scales)
library(patchwork)     # combining ggplot panels
library(gt)            # publication-quality tables
library(gtsummary)     # balance / summary tables
library(janitor)       # tabyl
library(lubridate)

# Install if needed:
# install.packages(c("patchwork", "gt", "gtsummary"))


# =============================================================================
# 1. USER SETTINGS
# =============================================================================

INPUT_PATH    <- "data/analysis_panel.rds"
TABLE_PATH    <- "output/tables/"
FIGURE_PATH   <- "output/figures/"

# RBC effective quarter label for plot annotations
RBC_LABEL     <- "RBC Effective\n(2022 Q1)"
RBC_PERIOD    <- 2022.1

# Color palette: complex (treated) = navy, non-complex (control) = coral
COL_COMPLEX   <- "#1B3A6B"   # navy
COL_NONCOMPLEX <- "#C94040"  # coral/red
COL_THRESHOLD  <- "#E8A838"  # amber — for threshold lines

# ggplot theme for all figures
theme_rbc <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "gray40", size = 10),
      axis.title       = element_text(size = 10),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold"),
      plot.caption     = element_text(color = "gray50", size = 8)
    )
}


# =============================================================================
# 2. LOAD DATA & CREATE OUTPUT DIRECTORIES
# =============================================================================

message("── Step 1: Loading analysis panel ───────────────────────────────────")

df <- readRDS(INPUT_PATH)

# Create output directories
dir.create(TABLE_PATH,  showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURE_PATH, showWarnings = FALSE, recursive = TRUE)

message(sprintf("  Loaded: %s obs, %d variables", comma(nrow(df)), ncol(df)))

# Convenience labels for treatment group
df <- df |>
  mutate(
    group_label = if_else(complex == 1,
                          "Complex (treated)",
                          "Non-complex (control)"),
    group_label = factor(group_label,
                         levels = c("Complex (treated)",
                                    "Non-complex (control)"))
  )

# Period labels for x-axis (e.g. "2020 Q1")
df <- df |>
  mutate(
    period_label = paste0(year, " Q", quarter),
    period_date  = as.Date(paste0(year, "-",
                                  sprintf("%02d", (quarter - 1) * 3 + 1),
                                  "-01"))
  )

# RBC date for plot vertical lines
rbc_date <- as.Date("2022-01-01")


# =============================================================================
# 3. BALANCE TABLE — Pre-RBC period, complex vs. non-complex
# =============================================================================
# Tests whether treated and control groups are comparable on pre-treatment
# observables. Large imbalances are informative (e.g. MBL concentration)
# but should be controlled for in the DiD regressions.

message("── Step 2: Building balance table ───────────────────────────────────")

balance_vars <- c(
  "assets_tot", "members", "networth_ratio", "cap_buffer",
  "loan_to_asset", "inv_to_asset", "dep_to_asset",
  "mbl_shr", "comm_shr", "re_shr", "auto_shr",
  "roa_var", "nim", "cof", "yld_loans",
  "dq_rate_var", "chgoff_ratio", "pll_assets",
  "cecl_adopter", "well_capitalized"
)

balance_labels <- c(
  "Total assets ($)",
  "Members",
  "Net worth ratio (%)",
  "Capital buffer vs 10% (pp)",
  "Loans / assets (%)",
  "Investments / assets (%)",
  "Deposits / assets (%)",
  "MBL share of loans (%)",
  "Commercial share of loans (%)",
  "Real estate share of loans (%)",
  "Auto share of loans (%)",
  "Return on assets (%)",
  "Net interest margin (%)",
  "Cost of funds (%)",
  "Yield on avg loans (%)",
  "Delinquency rate (%)",
  "Charge-off ratio (%)",
  "PLL / assets (%)",
  "CECL adopter (0/1)",
  "Well-capitalized (0/1)"
)

# Restrict to pre-RBC period, one obs per CU (CU-level means)
df_pre <- df |>
  filter(post_rbc == 0) |>
  group_by(cu_number, complex, group_label) |>
  summarise(across(all_of(balance_vars), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")

# Compute means, SDs, and t-test p-values by group
balance_table <- map2_dfr(balance_vars, balance_labels, function(var, lbl) {
  x <- df_pre[[var]]
  g <- df_pre$complex

  m1  <- mean(x[g == 1], na.rm = TRUE)
  m0  <- mean(x[g == 0], na.rm = TRUE)
  sd1 <- sd(x[g == 1],   na.rm = TRUE)
  sd0 <- sd(x[g == 0],   na.rm = TRUE)

  # t-test for difference in means
  tt <- tryCatch(
    t.test(x[g == 1], x[g == 0]),
    error = function(e) list(p.value = NA_real_)
  )

  tibble(
    Variable             = lbl,
    `Complex (N=696)`    = sprintf("%.3f (%.3f)", m1, sd1),
    `Non-complex (N=4,488)` = sprintf("%.3f (%.3f)", m0, sd0),
    Difference           = round(m1 - m0, 3),
    `p-value`            = round(tt$p.value, 3)
  )
})

# Print to console
cat("\n=== BALANCE TABLE (Pre-RBC Period, CU-level means) ===\n")
print(balance_table, n = Inf)

# Save as CSV
write_csv(balance_table,
          file.path(TABLE_PATH, "balance_table_pre_rbc.csv"))

message("  Balance table saved → output/tables/balance_table_pre_rbc.csv")


# =============================================================================
# 4. SUMMARY STATISTICS TABLE (publication-ready)
# =============================================================================

message("── Step 3: Summary statistics table ─────────────────────────────────")

sum_vars <- c(
  "networth_ratio", "cap_buffer", "well_capitalized",
  "mbl_shr", "comm_shr", "re_shr", "auto_shr",
  "loan_to_asset", "asset_growth", "loan_growth",
  "spread_mortgage", "spread_nauto", "spread_comm_re", "spread_comm_oth",
  "roa_var", "nim", "cof",
  "dq_rate_var", "chgoff_ratio", "pll_assets"
)

sum_labels <- c(
  "Net worth ratio (%)",
  "Capital buffer vs 10% (pp)",
  "Well-capitalized (binary)",
  "MBL share (%)",
  "Commercial share (%)",
  "Real estate share (%)",
  "Auto share (%)",
  "Loans / assets (%)",
  "Asset growth (QoQ log)",
  "Loan growth (QoQ log)",
  "Mortgage spread (pp)",
  "Auto spread (pp)",
  "Comm RE spread (pp)",
  "Comm non-RE spread (pp)",
  "ROA (%)",
  "Net interest margin (%)",
  "Cost of funds (%)",
  "Delinquency rate (%)",
  "Charge-off ratio (%)",
  "PLL / assets (%)"
)

sum_stats <- map2_dfr(sum_vars, sum_labels, function(var, lbl) {
  x <- df[[var]]
  tibble(
    Variable = lbl,
    N        = sum(!is.na(x)),
    Mean     = round(mean(x, na.rm = TRUE), 3),
    SD       = round(sd(x,   na.rm = TRUE), 3),
    P25      = round(quantile(x, 0.25, na.rm = TRUE), 3),
    Median   = round(quantile(x, 0.50, na.rm = TRUE), 3),
    P75      = round(quantile(x, 0.75, na.rm = TRUE), 3),
    `% Missing` = scales::percent(mean(is.na(x)), accuracy = 0.1)
  )
})

cat("\n=== SUMMARY STATISTICS (Full Panel) ===\n")
print(sum_stats, n = Inf)

write_csv(sum_stats,
          file.path(TABLE_PATH, "summary_stats_full_panel.csv"))

message("  Summary stats saved → output/tables/summary_stats_full_panel.csv")


# =============================================================================
# 5. MISSING DATA MAP FOR SPREAD VARIABLES
# =============================================================================
# Visualises which CU-quarters have usable loan rate data.
# Critical for knowing which loan types support the pricing analysis.

message("── Step 4: Missing data map for spread variables ─────────────────────")

spread_vars <- c("spread_mortgage", "spread_re_junior",
                 "spread_nauto", "spread_uauto",
                 "spread_comm_re", "spread_comm_oth", "spread_cc")

spread_labels <- c("Mortgage", "RE Junior Lien",
                   "Auto (New)", "Auto (Used)",
                   "Comm RE", "Comm non-RE", "Credit Card")

# Coverage by quarter and treatment group
coverage_data <- df |>
  group_by(q_period_num, period_date, group_label) |>
  summarise(
    across(all_of(spread_vars),
           ~ mean(!is.na(.x)) * 100,
           .names = "{.col}_cov"),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = ends_with("_cov"),
    names_to  = "spread",
    values_to = "coverage_pct"
  ) |>
  mutate(
    spread = str_remove(spread, "_cov"),
    spread_label = spread_labels[match(spread, spread_vars)]
  )

p_missing <- ggplot(coverage_data,
                    aes(x = period_date, y = coverage_pct,
                        color = group_label)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = rbc_date, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  annotate("text", x = rbc_date + 30, y = 95,
           label = RBC_LABEL, hjust = 0, size = 2.8, color = "gray40") +
  facet_wrap(~ spread_label, ncol = 2) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title    = "Data Coverage: Loan Interest Rate Spread Variables",
    subtitle = "% of CU-quarters with non-missing spread by loan type and group",
    x = NULL, y = "Coverage (%)",
    caption  = "Spread = loan rate − matched-maturity Treasury benchmark."
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "fig_missing_spread_coverage.png"),
       p_missing, width = 10, height = 8, dpi = 300)

message("  Missing data map saved → output/figures/fig_missing_spread_coverage.png")


# =============================================================================
# 6. TIME-SERIES TRENDS — NET WORTH RATIO
# =============================================================================

message("── Step 5: Net worth ratio trend plot ───────────────────────────────")

nw_trends <- df |>
  group_by(period_date, group_label) |>
  summarise(
    mean_nw    = mean(networth_ratio, na.rm = TRUE),
    p25_nw     = quantile(networth_ratio, 0.25, na.rm = TRUE),
    p75_nw     = quantile(networth_ratio, 0.75, na.rm = TRUE),
    mean_buf   = mean(cap_buffer, na.rm = TRUE),
    pct_wellcap = mean(well_capitalized, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Panel A: Mean net worth ratio
pA <- ggplot(nw_trends, aes(x = period_date, color = group_label,
                             fill = group_label)) +
  geom_ribbon(aes(ymin = p25_nw, ymax = p75_nw), alpha = 0.12,
              color = NA) +
  geom_line(aes(y = mean_nw), linewidth = 1) +
  geom_hline(yintercept = 10, linetype = "dashed",
             color = COL_THRESHOLD, linewidth = 0.8) +
  geom_vline(xintercept = rbc_date, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  annotate("text", x = rbc_date + 30, y = 19,
           label = RBC_LABEL, hjust = 0, size = 2.8, color = "gray40") +
  annotate("text", x = as.Date("2018-03-01"), y = 10.4,
           label = "10% well-cap threshold", hjust = 0,
           size = 2.8, color = COL_THRESHOLD) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_fill_manual(values  = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_x_date(date_labels = "%Y Q%q", date_breaks = "1 year") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "A. Net Worth Ratio by Treatment Group",
    subtitle = "Mean ± IQR (25th–75th percentile). Dashed amber = 10% threshold.",
    x = NULL, y = "Net Worth Ratio (%)"
  ) +
  theme_rbc()

# Panel B: % well-capitalized
pB <- ggplot(nw_trends, aes(x = period_date, y = pct_wellcap,
                             color = group_label)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = rbc_date, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_x_date(date_labels = "%Y Q%q", date_breaks = "1 year") +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "B. % of CUs Meeting 10% Well-Capitalized Threshold",
    subtitle = "Share of CU-quarters with net worth ratio ≥ 10%",
    x = NULL, y = "% Well-Capitalized"
  ) +
  theme_rbc()

p_capital <- pA / pB +
  plot_annotation(
    caption = paste0("Source: NCUA Call Report (5300). ",
                     "Complex = avg assets > $500M in 2021. ",
                     "Ribbon = IQR.")
  )

ggsave(file.path(FIGURE_PATH, "fig_networth_ratio_trends.png"),
       p_capital, width = 11, height = 9, dpi = 300)

message("  Capital trend plots saved → output/figures/fig_networth_ratio_trends.png")


# =============================================================================
# 7. NET WORTH RATIO DISTRIBUTION AT THRESHOLD (Pre-RBC, Complex CUs)
# =============================================================================
# Histogram centered on the 10% threshold — shows how many complex CUs
# were already close to / below the RBC well-capitalized threshold.

message("── Step 6: Net worth ratio threshold distribution ────────────────────")

df_complex_pre <- df |>
  filter(complex == 1, post_rbc == 0)

p_hist <- ggplot(df_complex_pre,
                 aes(x = networth_ratio)) +
  geom_histogram(aes(fill = networth_ratio >= 10),
                 binwidth = 0.25, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 10, color = COL_THRESHOLD,
             linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 9, color = "gray60",
             linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 10.15, y = Inf, vjust = 1.5,
           label = "10% RBC\nthreshold", hjust = 0,
           size = 3, color = COL_THRESHOLD) +
  annotate("text", x = 9.15, y = Inf, vjust = 3.5,
           label = "9% CCULR\nthreshold", hjust = 0,
           size = 3, color = "gray50") +
  scale_fill_manual(values = c("TRUE" = COL_COMPLEX, "FALSE" = "#C94040"),
                    labels = c("TRUE" = "≥ 10% (well-cap)",
                               "FALSE" = "< 10% (undercapitalized)")) +
  scale_x_continuous(limits = c(5, 25),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Net Worth Ratio Distribution — Complex CUs, Pre-RBC Period",
    subtitle = "Each bar = 0.25 percentage point bin. Red = below 10% threshold.",
    x        = "Net Worth Ratio (%)",
    y        = "CU-Quarter Count",
    fill     = NULL,
    caption  = "Complex CUs only (avg assets > $500M in 2021). Pre-RBC = 2018Q1–2021Q4."
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "fig_nw_ratio_distribution_complex_prerbc.png"),
       p_hist, width = 10, height = 6, dpi = 300)

message("  Threshold distribution saved → output/figures/fig_nw_ratio_distribution_complex_prerbc.png")


# =============================================================================
# 8. TIME-SERIES TRENDS — LOAN PORTFOLIO COMPOSITION
# =============================================================================

message("── Step 7: Loan portfolio composition trends ─────────────────────────")

portfolio_trends <- df |>
  group_by(period_date, group_label) |>
  summarise(
    mbl_shr    = mean(mbl_shr,   na.rm = TRUE),
    comm_shr   = mean(comm_shr,  na.rm = TRUE),
    re_shr     = mean(re_shr,    na.rm = TRUE),
    auto_shr   = mean(auto_shr,  na.rm = TRUE),
    .groups = "drop"
  )

plot_share <- function(data, var, ylab, title_suffix) {
  ggplot(data, aes(x = period_date, y = .data[[var]],
                   color = group_label)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = rbc_date, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(title = title_suffix, x = NULL, y = ylab) +
    theme_rbc() +
    theme(legend.position = "none")
}

pP1 <- plot_share(portfolio_trends, "mbl_shr",
                  "MBL share (%)", "A. Member Business Loans")
pP2 <- plot_share(portfolio_trends, "comm_shr",
                  "Comm share (%)", "B. Commercial Loans")
pP3 <- plot_share(portfolio_trends, "re_shr",
                  "RE share (%)", "C. Real Estate Loans")
pP4 <- plot_share(portfolio_trends, "auto_shr",
                  "Auto share (%)", "D. Auto Loans")

# Shared legend from pP1 (with legend enabled)
legend_plot <- plot_share(portfolio_trends, "mbl_shr",
                          "MBL share (%)", "") +
  theme(legend.position = "bottom")

p_portfolio <- (pP1 + pP2) / (pP3 + pP4) +
  plot_annotation(
    title    = "Loan Portfolio Composition by Treatment Group",
    subtitle = "Share of total loans (%). Dashed line = RBC effective date (2022 Q1).",
    caption  = "Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "fig_loan_portfolio_trends.png"),
       p_portfolio, width = 11, height = 8, dpi = 300)

message("  Portfolio trend plots saved → output/figures/fig_loan_portfolio_trends.png")


# =============================================================================
# 9. TIME-SERIES TRENDS — LOAN RATE SPREADS
# =============================================================================

message("── Step 8: Loan rate spread trends ──────────────────────────────────")

spread_trends <- df |>
  group_by(period_date, group_label) |>
  summarise(
    spread_mortgage  = mean(spread_mortgage,  na.rm = TRUE),
    spread_nauto     = mean(spread_nauto,      na.rm = TRUE),
    spread_uauto     = mean(spread_uauto,      na.rm = TRUE),
    spread_comm_re   = mean(spread_comm_re,    na.rm = TRUE),
    spread_comm_oth  = mean(spread_comm_oth,   na.rm = TRUE),
    .groups = "drop"
  )

plot_spread <- function(data, var, ylab, title_suffix) {
  ggplot(data |> filter(!is.na(.data[[var]])),
         aes(x = period_date, y = .data[[var]],
             color = group_label)) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = rbc_date, linetype = "dashed",
               color = "gray40", linewidth = 0.6) +
    geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
    scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    scale_y_continuous(labels = function(x) paste0(x, " pp")) +
    labs(title = title_suffix, x = NULL, y = ylab) +
    theme_rbc() +
    theme(legend.position = "none")
}

pS1 <- plot_spread(spread_trends, "spread_mortgage",
                   "Spread (pp)", "A. Mortgage Spread (vs 10yr)")
pS2 <- plot_spread(spread_trends, "spread_nauto",
                   "Spread (pp)", "B. New Auto Spread (vs 2yr)")
pS3 <- plot_spread(spread_trends, "spread_uauto",
                   "Spread (pp)", "C. Used Auto Spread (vs 2yr)")
pS4 <- plot_spread(spread_trends, "spread_comm_re",
                   "Spread (pp)", "D. Comm RE Spread (vs 10yr)")
pS5 <- plot_spread(spread_trends, "spread_comm_oth",
                   "Spread (pp)", "E. Comm non-RE Spread (vs 2yr)")

p_spreads <- (pS1 + pS2) / (pS3 + pS4) / (pS5 + plot_spacer()) +
  plot_annotation(
    title    = "Loan Rate Spreads Over Treasuries by Treatment Group",
    subtitle = "Loan rate minus matched-maturity Treasury benchmark (pp). Dashed = RBC (2022 Q1).",
    caption  = "Source: NCUA Call Report (5300) & FRED. Missing panels reflect low reporting coverage."
  )

ggsave(file.path(FIGURE_PATH, "fig_loan_spread_trends.png"),
       p_spreads, width = 11, height = 11, dpi = 300)

message("  Spread trend plots saved → output/figures/fig_loan_spread_trends.png")


# =============================================================================
# 10. TIME-SERIES TRENDS — CREDIT QUALITY & PROFITABILITY
# =============================================================================

message("── Step 9: Credit quality & profitability trends ─────────────────────")

perf_trends <- df |>
  group_by(period_date, group_label) |>
  summarise(
    dq_rate   = mean(dq_rate_var,   na.rm = TRUE),
    chgoff    = mean(chgoff_ratio,  na.rm = TRUE),
    pll_a     = mean(pll_assets,    na.rm = TRUE),
    roa       = mean(roa_var,       na.rm = TRUE),
    nim       = mean(nim,            na.rm = TRUE),
    cof       = mean(cof,            na.rm = TRUE),
    .groups = "drop"
  )

pQ1 <- plot_share(perf_trends |> rename(mbl_shr = dq_rate),
                  "mbl_shr", "DQ rate (%)", "A. Delinquency Rate")
pQ2 <- plot_share(perf_trends |> rename(mbl_shr = chgoff),
                  "mbl_shr", "Charge-off (%)", "B. Net Charge-Off Ratio")
pQ3 <- plot_share(perf_trends |> rename(mbl_shr = roa),
                  "mbl_shr", "ROA (%)", "C. Return on Assets")
pQ4 <- plot_share(perf_trends |> rename(mbl_shr = nim),
                  "mbl_shr", "NIM (%)", "D. Net Interest Margin")
pQ5 <- plot_share(perf_trends |> rename(mbl_shr = cof),
                  "mbl_shr", "CoF (%)", "E. Cost of Funds")
pQ6 <- plot_share(perf_trends |> rename(mbl_shr = pll_a),
                  "mbl_shr", "PLL/Assets (%)", "F. Provision / Assets")

p_performance <- (pQ1 + pQ2) / (pQ3 + pQ4) / (pQ5 + pQ6) +
  plot_annotation(
    title    = "Credit Quality & Profitability by Treatment Group",
    subtitle = "Quarterly means. Dashed line = RBC effective date (2022 Q1).",
    caption  = "Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "fig_performance_trends.png"),
       p_performance, width = 11, height = 11, dpi = 300)

message("  Performance trend plots saved → output/figures/fig_performance_trends.png")


# =============================================================================
# 11. ASSET GROWTH TREND
# =============================================================================

message("── Step 10: Asset & loan growth trends ──────────────────────────────")

growth_trends <- df |>
  filter(!is.na(asset_growth), !is.na(loan_growth)) |>
  group_by(period_date, group_label) |>
  summarise(
    asset_growth = mean(asset_growth, na.rm = TRUE) * 100,
    loan_growth  = mean(loan_growth,  na.rm = TRUE) * 100,
    mbl_growth   = mean(mbl_growth,   na.rm = TRUE) * 100,
    .groups = "drop"
  )

pG1 <- ggplot(growth_trends, aes(x = period_date, y = asset_growth,
                                  color = group_label)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = rbc_date, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "A. Asset Growth (QoQ %)",
       x = NULL, y = "Growth (%)") +
  theme_rbc()

pG2 <- ggplot(growth_trends, aes(x = period_date, y = loan_growth,
                                  color = group_label)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = rbc_date, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "B. Loan Growth (QoQ %)",
       x = NULL, y = "Growth (%)") +
  theme_rbc()

pG3 <- ggplot(growth_trends, aes(x = period_date, y = mbl_growth,
                                  color = group_label)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray70") +
  geom_vline(xintercept = rbc_date, linetype = "dashed",
             color = "gray40", linewidth = 0.6) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(title = "C. MBL Growth (QoQ %)",
       x = NULL, y = "Growth (%)") +
  theme_rbc()

p_growth <- pG1 / pG2 / pG3 +
  plot_annotation(
    title   = "Asset, Loan & MBL Growth by Treatment Group",
    subtitle = "Quarter-over-quarter log growth × 100. Dashed = RBC (2022 Q1).",
    caption  = "Source: NCUA Call Report (5300)."
  )

ggsave(file.path(FIGURE_PATH, "fig_growth_trends.png"),
       p_growth, width = 10, height = 10, dpi = 300)

message("  Growth trend plots saved → output/figures/fig_growth_trends.png")


# =============================================================================
# 12. PRE-TREND VISUAL CHECK (event-study raw means)
# =============================================================================
# Plots quarterly means by event_time (quarters relative to RBC).
# A flat pre-trend for complex vs non-complex supports the parallel
# trends assumption needed for DiD validity.

message("── Step 11: Pre-trend visual check ──────────────────────────────────")

pretrend_vars <- c("networth_ratio", "cap_buffer",
                   "mbl_shr", "loan_growth", "roa_var", "dq_rate_var")

pretrend_labels <- c("Net Worth Ratio (%)", "Capital Buffer (pp)",
                     "MBL Share (%)", "Loan Growth (%)",
                     "ROA (%)", "Delinquency Rate (%)")

pretrend_data <- df |>
  filter(event_time >= -16, event_time <= 12) |>
  group_by(event_time, group_label) |>
  summarise(across(all_of(pretrend_vars),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") |>
  pivot_longer(cols = all_of(pretrend_vars),
               names_to  = "variable",
               values_to = "mean_value") |>
  mutate(
    var_label = pretrend_labels[match(variable, pretrend_vars)]
  )

p_pretrend <- ggplot(pretrend_data,
                     aes(x = event_time, y = mean_value,
                         color = group_label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2) +
  geom_vline(xintercept = -0.5, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "solid",
             color = COL_THRESHOLD, linewidth = 0.5, alpha = 0.6) +
  facet_wrap(~ var_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c(COL_COMPLEX, COL_NONCOMPLEX)) +
  scale_x_continuous(breaks = seq(-16, 12, by = 4),
                     labels = function(x) paste0("Q", x)) +
  labs(
    title    = "Raw Quarterly Means by Event Time — Pre-Trend Check",
    subtitle = paste0("Event time = quarters relative to RBC (2022 Q1 = 0). ",
                      "Flat pre-period trends support parallel trends assumption."),
    x        = "Quarters Relative to RBC Effective Date",
    y        = "Group Mean",
    caption  = "Source: NCUA Call Report (5300). Vertical dashed = RBC effective date."
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "fig_pretrend_raw_means.png"),
       p_pretrend, width = 12, height = 10, dpi = 300)

message("  Pre-trend plot saved → output/figures/fig_pretrend_raw_means.png")


# =============================================================================
# 13. ASSET SIZE DISTRIBUTION — Bunching check near $500M threshold
# =============================================================================
# Tests for manipulation of asset size around the $500M RBC threshold.
# Bunching just below $500M would indicate CUs actively shrank to avoid RBC.

message("── Step 12: Bunching check near $500M threshold ──────────────────────")

# Use pre-RBC CU-level average assets
bunching_data <- df |>
  filter(post_rbc == 0) |>
  group_by(cu_number) |>
  summarise(avg_assets = mean(assets_tot, na.rm = TRUE) / 1e6,
            .groups = "drop") |>
  filter(avg_assets >= 100, avg_assets <= 1500)

p_bunch <- ggplot(bunching_data, aes(x = avg_assets)) +
  geom_histogram(binwidth = 25, fill = COL_COMPLEX, color = "white",
                 alpha = 0.8) +
  geom_vline(xintercept = 500, color = COL_THRESHOLD,
             linetype = "dashed", linewidth = 1.2) +
  annotate("text", x = 515, y = Inf, vjust = 1.5,
           label = "$500M\nRBC threshold", hjust = 0,
           size = 3.2, color = COL_THRESHOLD) +
  scale_x_continuous(labels = function(x) paste0("$", x, "M"),
                     breaks = seq(0, 1500, 250)) +
  labs(
    title    = "Asset Size Distribution — Bunching Check Near $500M RBC Threshold",
    subtitle = "Pre-RBC CU-level average assets ($M). Each bin = $25M.",
    x        = "Average Total Assets ($ millions)",
    y        = "Number of CUs",
    caption  = "Excess bunching just below $500M would suggest threshold manipulation."
  ) +
  theme_rbc()

ggsave(file.path(FIGURE_PATH, "fig_bunching_500m.png"),
       p_bunch, width = 10, height = 6, dpi = 300)

message("  Bunching plot saved → output/figures/fig_bunching_500m.png")


# =============================================================================
# 14. CONSOLE SUMMARY OF ALL OUTPUTS
# =============================================================================

message("\n── Step 13: Output summary ───────────────────────────────────────────")

cat("\n=== FILES SAVED ===\n")
cat("\nTables:\n")
cat("  ", file.path(TABLE_PATH, "balance_table_pre_rbc.csv"), "\n")
cat("  ", file.path(TABLE_PATH, "summary_stats_full_panel.csv"), "\n")

cat("\nFigures:\n")
figures <- c(
  "fig_missing_spread_coverage.png",
  "fig_networth_ratio_trends.png",
  "fig_nw_ratio_distribution_complex_prerbc.png",
  "fig_loan_portfolio_trends.png",
  "fig_loan_spread_trends.png",
  "fig_performance_trends.png",
  "fig_growth_trends.png",
  "fig_pretrend_raw_means.png",
  "fig_bunching_500m.png"
)
for (f in figures) cat("  ", file.path(FIGURE_PATH, f), "\n")

cat("\n=== KEY FINDINGS TO NOTE ===\n")
cat("  1. networth_ratio: 0% missing — use as primary capital outcome\n")
cat("  2. spread_comm_re: 78% missing — use cautiously / note in paper\n")
cat("  3. Complex CU median NW ratio pre-RBC: 10.25% (just 25bp above threshold)\n")
cat("  4. 44.5% of complex CU-quarters below 10% pre-RBC\n")
cat("  5. MBL share: 8.3% (complex) vs 1.5% (non-complex) — key RW driver\n")
cat("  6. Check fig_pretrend_raw_means.png for parallel trends validation\n")
cat("  7. Check fig_bunching_500m.png for threshold manipulation\n")

message("\n── 1_Descriptive_Stats.R complete ✓ ─────────────────────────────────")
message("  Next step: run 2_DiD_Estimation.R")


# =============================================================================
# END OF SCRIPT
# =============================================================================
