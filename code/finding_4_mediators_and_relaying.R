# ==============================================================================
# Figure 4: Breakdown of Account Types Mentioned vs. Relaying InfoOps Content
# Panels: (A) Mentioned by InfoOps, (B) Relaying to Panelists
# ==============================================================================

cat("Generating Figure 4: Mentions vs. Relays account breakdown pie charts...\n")

resolve_path <- function(rel_path) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_dir <- if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
  target_root <- if (basename(script_dir) == "replication_paper") dirname(script_dir) else file.path(getwd(), "target")
  
  candidates <- c(
    file.path(target_root, rel_path),
    file.path(script_dir, "..", rel_path),
    file.path("target", rel_path),
    rel_path
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(normalizePath(cand))
  }
  return(file.path(target_root, rel_path))
}

output_dir <- resolve_path("output/figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
output_pdf <- file.path(output_dir, "figure_4_mentions_vs_relays.pdf")
output_png <- file.path(output_dir, "figure_4_mentions_vs_relays.png")

# New data file paths
mentions_csv <- resolve_path("data/mediators_mentions_dist.csv")
relays_csv   <- resolve_path("data/mediators_relaying_dist.csv")

if (!file.exists(mentions_csv) || !file.exists(relays_csv)) {
  stop(sprintf("Data files missing from data/:\n  %s\n  %s", mentions_csv, relays_csv))
}

cat(sprintf("Using Figure 4 breakdown CSV files:\n  - Mentions: %s\n  - Relays:   %s\n",
            basename(mentions_csv), basename(relays_csv)))
df_mentions <- read.csv(mentions_csv, stringsAsFactors = FALSE)
df_relays   <- read.csv(relays_csv, stringsAsFactors = FALSE)

slices_a <- setNames(df_mentions$count, df_mentions$account_type)
cols_a   <- df_mentions$color
lbls_a   <- paste0(df_mentions$percent, "%")

slices_b <- setNames(df_relays$count, df_relays$account_type)
cols_b   <- df_relays$color
lbls_b   <- paste0(df_relays$percent, "%")

# Legend configuration matching exact paper layout: 2 rows x 3 columns
# Col 1: InfoOps, Politicians
# Col 2: Media, Reporter
# Col 3: Panelists
legend_names  <- c("InfoOps", "Politicians", "Media", "Reporter", "Panelists")
legend_colors <- c("#FFC107", "#00BFFF", "#FF4500", "#3CB371", "#9370DB")

draw_figure_4 <- function() {
  par(oma = c(0.5, 0.5, 4.2, 0.5), bg = "white")
  layout(matrix(1:2, nrow = 1))

  # ----------------------------------------------------
  # PANEL A: Mentioned by InfoOps
  # ----------------------------------------------------
  par(mar = c(1, 2, 1, 2))
  pie(
    slices_a,
    labels = lbls_a,
    col = cols_a,
    border = "white",
    radius = 0.80,
    cex = 1.15,
    init.angle = 0,
    clockwise = FALSE
  )
  text(-0.95, 1.05, "A", font = 2, cex = 2.4, xpd = NA)

  # ----------------------------------------------------
  # PANEL B: Relaying to Panelists
  # ----------------------------------------------------
  par(mar = c(1, 2, 1, 2))
  # 4.1% of 360 deg is ~14.76 deg; init.angle = -7.38 centers the yellow slice on 3 o'clock
  pie(
    slices_b,
    labels = lbls_b,
    col = cols_b,
    border = "white",
    radius = 0.80,
    cex = 1.15,
    init.angle = -7.38,
    clockwise = FALSE
  )
  text(-0.95, 1.05, "B", font = 2, cex = 2.4, xpd = NA)

  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot.new()
  legend(
    "top",
    legend  = legend_names,
    fill    = legend_colors,
    ncol    = 3,
    inset   = c(0, 0.03),
    bty     = "o",
    box.lty = 2,
    box.lwd = 1.0,
    box.col = "black",
    cex     = 1.15,
    pt.cex  = 1.4,
    bg      = "white"
  )
}

pdf(output_pdf, width = 8.0, height = 4.67)
draw_figure_4()
dev.off()

png(output_png, width = 2400, height = 1400, res = 300)
draw_figure_4()
dev.off()

cat(sprintf("[SUCCESS] Figure 4 saved to:\n  - %s\n  - %s\n", output_pdf, output_png))
