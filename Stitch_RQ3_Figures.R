# =============================================================================
# Stitch_RQ3_Figures.R
# Reads existing PNGs and assembles a 2x2 grid — NO regressions needed
#
# Run from: S:/Projects/RBC_2026/Data/
# Output:   output/paper/figures/Figure_RQ3_Combined.png
# =============================================================================

library(magick)   # install if needed: install.packages("magick")

FIG_DIR  <- "output/paper/figures"
FIG_OUT  <- file.path(FIG_DIR, "Figure_RQ3_Combined.png")

# ── Load the two source figures ───────────────────────────────────────────────
spreads_path <- file.path(FIG_DIR, "Figure4_EventStudy_Spreads.png")
growth_path  <- file.path(FIG_DIR, "Figure3b_EventStudy_LoanGrowth.png")

if (!file.exists(spreads_path)) stop("Cannot find: ", spreads_path)
if (!file.exists(growth_path))  stop("Cannot find: ", growth_path)

message("Loading figures...")
spreads <- image_read(spreads_path)
growth  <- image_read(growth_path)

# ── Get dimensions ────────────────────────────────────────────────────────────
s_info <- image_info(spreads)
g_info <- image_info(growth)
message("Spreads figure: ", s_info$width, " x ", s_info$height)
message("Growth figure:  ", g_info$width,  " x ", g_info$height)

# Figure4 has 3 panels in a roughly 2-column layout:
#   Top row:    A (mortgage, left half)   B (new auto, right half)
#   Bottom row: C (comm non-RE, left half only — single panel)
#
# We want to extract:
#   Panel A = left half of top row
#   Panel B = right half of top row
#   Panel C = left half of bottom row (or full bottom if single)
#   Panel D = loan growth (full Figure3b)

sw <- s_info$width
sh <- s_info$height

# Figure4 layout:
#   Top row occupies roughly top 48% of height
#   Bottom row occupies bottom 52%
#   Left/right split at ~50% width

top_h    <- round(sh * 0.48)
bot_h    <- sh - top_h
half_w   <- round(sw * 0.50)

panel_A <- image_crop(spreads, paste0(half_w, "x", top_h, "+0+0"))
panel_B <- image_crop(spreads, paste0(sw - half_w, "x", top_h, "+", half_w, "+0"))
panel_C <- image_crop(spreads, paste0(sw, "x", bot_h, "+0+", top_h))

# Panel D: full loan growth figure
panel_D <- growth

# ── Standardise panel sizes ───────────────────────────────────────────────────
# Target size per panel: 1400 x 900 px (14" x 10" at 100 dpi, 2x2)
TW <- 1400L
TH <- 900L

resize_panel <- function(img) {
  image_resize(img, paste0(TW, "x", TH, "!"))
}

pA <- resize_panel(panel_A)
pB <- resize_panel(panel_B)
pC <- resize_panel(panel_C)
pD <- resize_panel(panel_D)

# ── Add panel labels (A, B, C, D) in top-left corner ─────────────────────────
label_panel <- function(img, lbl) {
  image_annotate(img, lbl,
                 gravity    = "NorthWest",
                 location   = "+18+12",
                 size       = 52,
                 weight     = 700,
                 color      = "#1a2744",
                 font       = "sans")
}

# Panels from Figure4 already have A/B/C labels — skip re-labelling those
# Just add D label to loan growth (which has no panel letter)
pD <- label_panel(pD, "D")

# ── Assemble 2x2 grid ─────────────────────────────────────────────────────────
message("Assembling 2x2 grid...")

top_row <- image_append(c(pA, pB), stack = FALSE)
bot_row <- image_append(c(pC, pD), stack = FALSE)
combined <- image_append(c(top_row, bot_row), stack = TRUE)

# ── Add title banner ──────────────────────────────────────────────────────────
banner_h  <- 100L
total_w   <- TW * 2L
banner    <- image_blank(total_w, banner_h, color = "#F8F6F2")
banner    <- image_annotate(banner,
  "Event Study: RBC Rule Impact on Loan Rate Spreads and Lending Volume",
  gravity = "Center", size = 36, weight = 700, color = "#1a2744", font = "sans")

final <- image_append(c(banner, combined), stack = TRUE)

# ── Save ───────────────────────────────────────────────────────────────────────
message("Saving to: ", FIG_OUT)
image_write(final, path = FIG_OUT, format = "png", quality = 95)
message("Done. File size: ",
        round(file.size(FIG_OUT) / 1024), " KB")
