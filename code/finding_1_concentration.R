# ==============================================================================
# Figure 1: Cumulative Distribution of Political vs. InfoOps Content (ECDF)
# ==============================================================================

cat("Generating Figure 1: Cumulative content exposure distribution (ECDF)\n")

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

# New data file paths
political_path <- resolve_path("data/exp_pol_dist.csv")
trolls_path    <- resolve_path("data/exp_infoops_dist.csv")

output_dir <- resolve_path("output/figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
output_pdf <- file.path(output_dir, "figure_1_exposure_ecdf.pdf")
output_png <- file.path(output_dir, "figure_1_exposure_ecdf.png")

if (!file.exists(political_path) || !file.exists(trolls_path)) {
  stop(sprintf("Data files missing:\n  %s\n  %s", political_path, trolls_path))
}

cat(sprintf("Using data files:\n  - %s\n  - %s\n", political_path, trolls_path))

# Load Datasets
exposure_political <- read.csv(political_path, stringsAsFactors = FALSE)
exposure_trolls    <- read.csv(trolls_path, stringsAsFactors = FALSE)

# Harmonize columns to plotting coordinates (ensures compatibility between full and curve datasets)
p_df <- exposure_political[, c("cum_panel_percent", "cum_exposure_percent")]
t_df <- exposure_trolls[, c("cum_panel_percent", "cum_exposure_percent")]

# Sort by cum_panel_percent
p_df <- p_df[order(p_df$cum_panel_percent), ]
t_df <- t_df[order(t_df$cum_panel_percent), ]

# Filter valid positive x values for log10 scale
p_df <- p_df[p_df$cum_panel_percent > 0, ]
t_df <- t_df[t_df$cum_panel_percent > 0, ]

subsample_for_pdf <- function(df, n_points = 5000) {
  if (nrow(df) <= n_points) return(df)
  log_x <- log10(df$cum_panel_percent)
  target_log_x <- seq(min(log_x), max(log_x), length.out = n_points)
  interp_y <- approx(log_x, df$cum_exposure_percent, xout = target_log_x, rule = 2)$y
  data.frame(
    cum_panel_percent = 10^target_log_x,
    cum_exposure_percent = interp_y
  )
}

p_df_pdf <- subsample_for_pdf(p_df, 5000)
t_df_pdf <- subsample_for_pdf(t_df, 5000)

if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("scales", quietly = TRUE)) {
  library(ggplot2)
  library(scales)

  p_df$type <- "Political content"
  t_df$type <- "Political content from InfoOps"
  combined_exposure <- rbind(t_df, p_df)

  my_custom_theme <- function(base_size = 14) {
    theme_bw(base_size = base_size) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#cfcfcf", linewidth = 0.8),
        panel.border = element_blank(),
        axis.line = element_line(color = "black", linewidth = 1.5),
        axis.line.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text = element_text(color = "#1a1a1a", size = rel(1.0)),
        axis.title = element_text(color = "#1a1a1a", size = rel(1.1)),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        legend.background = element_rect(color = "black", linetype = "dashed"),
        legend.title = element_blank(),
        legend.text = element_text(size = rel(0.95))
      )
  }

  p1 <- ggplot(combined_exposure, aes(x = cum_panel_percent, y = cum_exposure_percent, color = type)) +
    geom_line(linewidth = 1.3) +
    scale_x_log10(
      breaks = c(0.1, 1, 10, 100),
      labels = c("0.1%", "1%", "10%", "100%")
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 20),
      labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
      expand = c(0, 0)
    ) +
    scale_color_manual(values = c("Political content" = "#57C78B", "Political content from InfoOps" = "#4D9DE0")) +
    labs(x = "% of panelists", y = "% of available content") +
    my_custom_theme()

  p_df_pdf$type <- "Political content"
  t_df_pdf$type <- "Political content from InfoOps"
  combined_pdf <- rbind(t_df_pdf, p_df_pdf)
  p1_pdf <- ggplot(combined_pdf, aes(x = cum_panel_percent, y = cum_exposure_percent, color = type)) +
    geom_line(linewidth = 1.3) +
    scale_x_log10(breaks = c(0.1, 1, 10, 100), labels = c("0.1%", "1%", "10%", "100%")) +
    scale_y_continuous(breaks = seq(0, 100, by = 20), labels = c("0%", "20%", "40%", "60%", "80%", "100%"), expand = c(0, 0)) +
    scale_color_manual(values = c("Political content" = "#57C78B", "Political content from InfoOps" = "#4D9DE0")) +
    labs(x = "% of panelists", y = "% of available content") +
    my_custom_theme()

  ggsave(output_pdf, plot = p1_pdf, width = 7, height = 5.5)
  ggsave(output_png, plot = p1, width = 7, height = 5.5, dpi = 300)
} else {
  draw_base_plot <- function(use_subsampled = FALSE) {
    par(mar = c(4.8, 5.0, 2.0, 2.0), bg = "white", las = 1)

    cur_p <- if (use_subsampled) p_df_pdf else p_df
    cur_t <- if (use_subsampled) t_df_pdf else t_df

    plot(
      cur_p$cum_panel_percent, cur_p$cum_exposure_percent,
      type = "n", log = "x",
      xlim = c(0.005, 100), ylim = c(0, 100),
      xlab = "", ylab = "",
      xaxt = "n", yaxt = "n", bty = "n"
    )

    # Grid lines (light gray, matching ggplot2 theme_bw)
    grid_y <- c(20, 40, 60, 80)
    for (gy in grid_y) {
      abline(h = gy, col = "#D0D0D0", lwd = 1.0)
    }
    grid_x <- c(0.1, 1, 10, 100)
    for (gx in grid_x) {
      abline(v = gx, col = "#D0D0D0", lwd = 1.0)
    }

    # Axes
    axis(1, at = c(0.1, 1, 10, 100), labels = c("0.1%", "1%", "10%", "100%"),
         cex.axis = 1.05, col = NA, col.ticks = NA, line = -0.5)
    axis(2, at = seq(20, 80, by = 20), labels = paste0(seq(20, 80, by = 20), "%"),
         cex.axis = 1.05, col = NA, col.ticks = NA, line = -0.5)

    # Thick black bottom axis line
    lines(c(0.005, 100), c(0, 0), col = "black", lwd = 2.5)

    # Axis titles
    mtext("% of panelists", side = 1, line = 2.5, cex = 1.2, font = 1)
    mtext("% of available content", side = 2, line = 3.2, cex = 1.2, font = 1, las = 0)

    # Plot lines: Political (Green #57C78B), InfoOps (Blue #4D9DE0)
    lines(cur_p$cum_panel_percent, cur_p$cum_exposure_percent, col = "#57C78B", lwd = 3.0)
    lines(cur_t$cum_panel_percent, cur_t$cum_exposure_percent, col = "#4D9DE0", lwd = 3.0)

    # Dashed legend box at top-left
    legend(
      "topleft",
      legend = c("Political content", "Political content from InfoOps"),
      col = c("#57C78B", "#4D9DE0"),
      lwd = 3.0, bty = "o", box.lty = 2, box.col = "black", bg = "white",
      cex = 1.0, inset = c(0.02, 0.02)
    )
  }

  pdf(output_pdf, width = 7, height = 5.5)
  draw_base_plot(use_subsampled = TRUE)
  dev.off()

  png(output_png, width = 2100, height = 1650, res = 300)
  draw_base_plot(use_subsampled = FALSE)
  dev.off()
}

cat(sprintf("[SUCCESS] Figure 1 saved to:\n  - %s\n  - %s\n", output_pdf, output_png))
