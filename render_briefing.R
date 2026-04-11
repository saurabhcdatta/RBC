# =============================================================================
# render_briefing.R
# Renders RBC_Executive_Briefing.Rmd to HTML (and optionally PDF)
#
# RUN ORDER:
#   1. All analysis scripts (0_Data_Prep.R through 3F)
#   2. 4_Paper_Tables.R          ← populates output/paper/figures/
#   3. source("render_briefing.R") ← produces the executive briefing
#
# OUTPUT:
#   RBC_Executive_Briefing.html  ← open in any browser
#   RBC_Executive_Briefing.pdf   ← requires tinytex or MikTeX (optional)
# =============================================================================

# ── Check required packages ────────────────────────────────────────────────────
required_pkgs <- c("rmarkdown", "knitr", "kableExtra",
                   "tidyverse", "scales")

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(rmarkdown)

# ── Paths ──────────────────────────────────────────────────────────────────────
# Set working directory to the project root (where output/ folder lives)
# Uncomment and edit if running from a different location:
# setwd("S:/Projects/RBC_2026/Data")

RMD_FILE    <- "RBC_Executive_Briefing.Rmd"
HTML_OUTPUT <- "RBC_Executive_Briefing.html"
FIG_DIR     <- "output/paper/figures"
TABLE_DIR   <- "output/paper/tables"

# ── Pre-flight: count available figures ───────────────────────────────────────
if (dir.exists(FIG_DIR)) {
  n_figs <- length(list.files(FIG_DIR, pattern = "\\.png$"))
  message(sprintf("Found %d figures in %s", n_figs, FIG_DIR))
  if (n_figs < 10) {
    warning(
      "Fewer than 10 figures found. Run 4_Paper_Tables.R first to ",
      "populate output/paper/figures/. The document will render but ",
      "figure panels will show placeholder messages."
    )
  }
} else {
  warning(
    "Figure directory not found: ", FIG_DIR, "\n",
    "Run 4_Paper_Tables.R to create it. ",
    "Document will render with placeholder messages for all figures."
  )
}

# ── Render HTML ───────────────────────────────────────────────────────────────
message("Rendering HTML briefing...")
t0 <- proc.time()

rmarkdown::render(
  input         = RMD_FILE,
  output_format = html_document(
    theme          = "flatly",
    highlight      = "tango",
    toc            = TRUE,
    toc_float      = list(collapsed = FALSE, smooth_scroll = TRUE),
    toc_depth      = 2,
    number_sections = FALSE,
    df_print       = "kable",
    code_folding   = "hide",
    css            = "exec_briefing.css",
    self_contained = TRUE   # single file — easy to share
  ),
  output_file   = HTML_OUTPUT,
  params        = list(
    fig_dir   = FIG_DIR,
    table_dir = TABLE_DIR
  ),
  envir = new.env(parent = globalenv()),
  quiet = FALSE
)

elapsed <- (proc.time() - t0)["elapsed"]
message(sprintf("\nHTML briefing complete in %.1f seconds.", elapsed))
message(sprintf("Output: %s (%.1f MB)",
                HTML_OUTPUT,
                file.size(HTML_OUTPUT) / 1e6))

# ── Optional: render PDF ──────────────────────────────────────────────────────
RENDER_PDF <- FALSE   # set TRUE if tinytex / MikTeX is installed

if (RENDER_PDF) {
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    message("Installing tinytex for PDF rendering...")
    install.packages("tinytex")
    tinytex::install_tinytex()
  }

  message("Rendering PDF briefing...")
  rmarkdown::render(
    input         = RMD_FILE,
    output_format = "pdf_document",
    output_file   = sub("\\.Rmd$", ".pdf", RMD_FILE),
    params        = list(fig_dir = FIG_DIR, table_dir = TABLE_DIR),
    envir         = new.env(parent = globalenv()),
    quiet         = FALSE
  )
  message("PDF complete.")
}

message("\nDone. Open ", HTML_OUTPUT, " in your browser.")
