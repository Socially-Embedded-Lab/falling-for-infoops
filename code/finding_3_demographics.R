# ==============================================================================
# Figure 3: Demographic Breakdown of InfoOps Followers vs. Non-Followers
# Panels: (A) Gender, (B) Age Distribution, (C) Party Registration, (D) Friends Count
# ==============================================================================

cat("Generating Figure 3: Voter demographics 4-panel breakdown...\n")

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
gender_path  <- resolve_path("data/gender_dist.csv")
age_path     <- resolve_path("data/age_dist.csv")
party_path   <- resolve_path("data/party_reg_dist.csv")
friends_path <- resolve_path("data/friends_dist.csv")

output_dir <- resolve_path("output/figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
output_pdf <- file.path(output_dir, "figure_3_voter_demographics.pdf")
output_png <- file.path(output_dir, "figure_3_voter_demographics.png")

if (!file.exists(gender_path) || !file.exists(age_path) || !file.exists(party_path) || !file.exists(friends_path)) {
  stop("Demographic input datasets missing from data/.")
}

cat(sprintf("Using demographic files:\n  - Gender:  %s\n  - Age:     %s\n  - Party:   %s\n  - Friends: %s\n",
            basename(gender_path), basename(age_path),
            basename(party_path), basename(friends_path)))

# Load Datasets
# Panel A Data: Gender
df_gender <- read.csv(gender_path, stringsAsFactors = FALSE)
df_gender <- df_gender[df_gender$cat != "panel_direct_trolls_elections", ]

# Panel B Data: Age
df_age_curve <- read.csv(age_path, stringsAsFactors = FALSE)
dens_f_age <- list(
  x = df_age_curve$age[df_age_curve$cat == "panel_direct_trolls"],
  y = df_age_curve$density[df_age_curve$cat == "panel_direct_trolls"]
)
dens_nf_age <- list(
  x = df_age_curve$age[df_age_curve$cat == "panel_non_trolls"],
  y = df_age_curve$density[df_age_curve$cat == "panel_non_trolls"]
)

# Panel C Data: Party Registration (Followers Percentage by Party)
df_party_raw <- read.csv(party_path, stringsAsFactors = FALSE)
df_party_raw <- df_party_raw[df_party_raw$cat != "panel_direct_trolls_elections", ]

# Compute P(Follower | Party) via Bayes' rule on party_reg_distribution.csv
calc_party_follow <- function(party_code) {
  f_row  <- df_party_raw[df_party_raw$cat == "panel_direct_trolls" & df_party_raw$party_reg == party_code, ]
  nf_row <- df_party_raw[df_party_raw$cat == "panel_non_trolls" & df_party_raw$party_reg == party_code, ]
  
  f_count  <- f_row$n * f_row$prp
  nf_count <- nf_row$n * nf_row$prp
  total_n  <- f_count + nf_count
  
  pct <- f_count / total_n
  se  <- sqrt((pct * (1 - pct)) / total_n)
  c(pct = pct, low = pct - 1.96 * se, high = pct + 1.96 * se)
}

dem_stats <- calc_party_follow("Dem.")
ind_stats <- calc_party_follow("Ind.")
rep_stats <- calc_party_follow("Rep.")

df_party_plot <- data.frame(
  party_reg = factor(c("Democrats", "Independents", "Republicans"), levels = c("Democrats", "Independents", "Republicans")),
  pct       = c(dem_stats["pct"], ind_stats["pct"], rep_stats["pct"]) * 100,
  prop_low  = c(dem_stats["low"], ind_stats["low"], rep_stats["low"]) * 100,
  prop_high = c(dem_stats["high"], ind_stats["high"], rep_stats["high"]) * 100,
  color     = c("#1F45FC", "#A0A0A0", "#FF4500"),
  stringsAsFactors = FALSE
)

# Panel D Data: Friends Count
df_friends_curve <- read.csv(friends_path, stringsAsFactors = FALSE)
dens_f_fr <- list(
  x = df_friends_curve$log10_friends_count[df_friends_curve$cat == "panel_direct_trolls"],
  y = df_friends_curve$density[df_friends_curve$cat == "panel_direct_trolls"]
)
dens_nf_fr <- list(
  x = df_friends_curve$log10_friends_count[df_friends_curve$cat == "panel_non_trolls"],
  y = df_friends_curve$density[df_friends_curve$cat == "panel_non_trolls"]
)

# 4. Render 4-Panel Figure
if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("patchwork", quietly = TRUE) &&
    requireNamespace("ggridges", quietly = TRUE)) {

  library(ggplot2)
  library(patchwork)
  library(ggridges)
  library(scales)

  custom_panel_theme <- function(base_size = 16) {
    theme_bw(base_size = base_size) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.6),
        panel.border     = element_blank(),
        axis.line.x      = element_line(color = "black", linewidth = 1.2),
        axis.line.y      = element_blank(),
        axis.ticks       = element_blank(),
        axis.text        = element_text(color = "black", size = rel(1.0)),
        axis.title       = element_text(color = "black", size = rel(1.1)),
        plot.title       = element_text(face = "bold", size = rel(1.8), hjust = 0, vjust = 1)
      )
  }

  # Panel A: Gender
  df_gender$cat <- factor(df_gender$cat, levels = c("panel_direct_trolls", "panel_non_trolls"))
  pA <- ggplot(df_gender, aes(x = cat, y = prp, fill = cat)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_errorbar(aes(ymin = prop_lo, ymax = prop_hi), width = 0.15, linewidth = 0.8, color = "black") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.65), expand = c(0, 0), breaks = seq(0, 0.6, by = 0.2)) +
    scale_x_discrete(labels = c("panel_direct_trolls" = "Followers", "panel_non_trolls" = "Non-followers")) +
    scale_fill_manual(values = c("panel_direct_trolls" = "#FA686A", "panel_non_trolls" = "#4D9DE0")) +
    labs(title = "A", y = "% Female", x = "Gender") +
    custom_panel_theme() + theme(legend.position = "none", axis.text.x = element_text(size = rel(1.25)))

  # Panel B: Age (Ridgeline anchored directly to x-axis)
  age_labels <- data.frame(
    cat = c("panel_non_trolls", "panel_direct_trolls"),
    lbl = c("Non-followers:\nN=634615,\navg=41.4", "Followers:\nN=25343,\navg=48.9"),
    x = c(78, 78),
    y = c(2.3, 1.3)
  )
  pB <- ggplot(df_age_curve, aes(x = age, y = cat, height = density, fill = cat)) +
    geom_ridgeline(alpha = 0.8, scale = 25, color = "white", linewidth = 0.4) +
    geom_text(data = age_labels, aes(x = x, y = y, label = lbl, height = 0), size = 4.8, color = "black", hjust = 0, lineheight = 0.9) +
    scale_x_continuous(breaks = seq(20, 100, by = 10), limits = c(15, 105)) +
    scale_y_discrete(expand = expansion(mult = c(0, 0.15))) +
    scale_fill_manual(values = c("panel_direct_trolls" = "#FA686A", "panel_non_trolls" = "#4D9DE0")) +
    labs(title = "B", x = "Age", y = "") +
    custom_panel_theme() + theme(legend.position = "none", axis.text.y = element_blank())

  # Panel C: Party Registration
  pC <- ggplot(df_party_plot, aes(x = party_reg, y = pct, fill = party_reg)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_errorbar(aes(ymin = prop_low, ymax = prop_high), width = 0.15, linewidth = 0.8, color = "black") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 5.2), expand = c(0, 0), breaks = 0:5) +
    scale_fill_manual(values = c("Democrats" = "#1F45FC", "Independents" = "#A0A0A0", "Republicans" = "#FF4500")) +
    labs(title = "C", y = "% Followers", x = "Party Registration") +
    custom_panel_theme() + theme(legend.position = "none")

  # Panel D: Friends Count (Ridgeline anchored directly to x-axis)
  friends_labels <- data.frame(
    cat = c("panel_non_trolls", "panel_direct_trolls"),
    lbl = c("Non-followers:\nN=639285,\navg=509.7", "Followers:\nN=25459,\navg=3474.2"),
    x = c(1.2, 1.2),
    y = c(2.3, 1.3)
  )
  pD <- ggplot(df_friends_curve, aes(x = 10^log10_friends_count, y = cat, height = density, fill = cat)) +
    geom_ridgeline(alpha = 0.8, scale = 1.3, color = "white", linewidth = 0.4) +
    geom_text(data = friends_labels, aes(x = x, y = y, label = lbl, height = 0), size = 4.8, color = "black", hjust = 0, lineheight = 0.9) +
    scale_x_continuous(trans = "log10", breaks = 10^(0:6), labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    scale_y_discrete(expand = expansion(mult = c(0, 0.15))) +
    scale_fill_manual(values = c("panel_direct_trolls" = "#FA686A", "panel_non_trolls" = "#4D9DE0")) +
    labs(title = "D", x = "Friends Count", y = "") +
    custom_panel_theme() + theme(legend.position = "none", axis.text.y = element_blank())

  comb_fig <- (pA + pB) / (pC + pD)
  ggsave(output_pdf, plot = comb_fig, width = 11.0, height = 8.5)
  ggsave(output_png, plot = comb_fig, width = 11.0, height = 8.5, dpi = 300)

} else {
  draw_demographics_base <- function() {
    par(mfrow = c(2, 2), mar = c(4.2, 5.0, 3.2, 2.0), bg = "white", las = 1)

    # ----------------------------------------------------
    # PANEL A: GENDER
    # ----------------------------------------------------
    prp_a <- c(df_gender$prp[df_gender$cat == "panel_direct_trolls"] * 100,
               df_gender$prp[df_gender$cat == "panel_non_trolls"] * 100)
    lo_a  <- c(df_gender$prop_lo[df_gender$cat == "panel_direct_trolls"] * 100,
               df_gender$prop_lo[df_gender$cat == "panel_non_trolls"] * 100)
    hi_a  <- c(df_gender$prop_hi[df_gender$cat == "panel_direct_trolls"] * 100,
               df_gender$prop_hi[df_gender$cat == "panel_non_trolls"] * 100)

    plot(
      0, 0, type = "n", xlim = c(0.4, 2.6), ylim = c(0, 65), yaxs = "i",
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
    )

    # Grid lines
    for (gy in seq(20, 60, by = 20)) abline(h = gy, col = "#E0E0E0", lwd = 1.0)
    lines(c(0.4, 2.6), c(0, 0), col = "black", lwd = 2.0)

    # Bars
    bar_w <- 0.65
    rect(1 - bar_w/2, 0, 1 + bar_w/2, prp_a[1], col = "#FA686A", border = NA)
    rect(2 - bar_w/2, 0, 2 + bar_w/2, prp_a[2], col = "#4D9DE0", border = NA)

    # Error bars
    arrows(1, lo_a[1], 1, hi_a[1], angle = 90, code = 3, length = 0.06, lwd = 1.6, col = "black")
    arrows(2, lo_a[2], 2, hi_a[2], angle = 90, code = 3, length = 0.06, lwd = 1.6, col = "black")

    # Axes: Enlarged Followers / Non-followers, original y-axis & titles
    axis(1, at = c(1, 2), labels = c("Followers", "Non-followers"), tick = FALSE, cex.axis = 1.35, line = -0.6)
    axis(2, at = seq(0, 60, by = 20), labels = paste0(seq(0, 60, by = 20), "%"), col = NA, col.ticks = NA, cex.axis = 1.1, line = -0.5)
    mtext("Gender", side = 1, line = 2.3, cex = 1.2)
    mtext("% Female", side = 2, line = 3.2, cex = 1.2, las = 0)
    mtext("A", side = 3, at = 0.35, line = 0.8, font = 2, cex = 1.8)

    # ----------------------------------------------------
    # PANEL B: AGE (RIDGELINE - Anchored directly to x-axis)
    # ----------------------------------------------------
    # yaxs = 'i' with ylim = c(0, 2.5) anchors the bottom baseline directly to x-axis
    plot(
      0, 0, type = "n", xlim = c(18, 102), ylim = c(0, 2.5), yaxs = "i",
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
    )

    # Vertical grid lines
    for (gx in seq(20, 100, by = 10)) abline(v = gx, col = "#E0E0E0", lwd = 1.0)

    scale_age <- 26
    # Non-followers ridge (top, baseline = 0.90)
    polygon(
      c(dens_nf_age$x, rev(dens_nf_age$x)),
      c(0.90 + dens_nf_age$y * scale_age, rep(0.90, length(dens_nf_age$x))),
      col = "#4D9DE0CC", border = "#4D9DE0", lwd = 1.2
    )

    # Followers ridge (bottom, baseline = 0.0 directly on x-axis)
    polygon(
      c(dens_f_age$x, rev(dens_f_age$x)),
      c(0.0 + dens_f_age$y * scale_age, rep(0.0, length(dens_f_age$x))),
      col = "#FA686ACC", border = "#FA686A", lwd = 1.2
    )

    # Baselines drawn on top of polygons for crisp axis alignment
    lines(c(18, 100), c(0.90, 0.90), col = "#D0D0D0", lwd = 1.2)
    lines(c(18, 100), c(0, 0), col = "black", lwd = 2.0)

    # Annotations: Enlarged Followers and Non-followers labels
    text(74, 1.45, "Non-followers:\nN=634615,\navg=41.4", adj = 0, cex = 1.18, font = 1)
    text(74, 0.50, "Followers:\nN=25343,\navg=48.9", adj = 0, cex = 1.18, font = 1)

    # Axes: Original
    axis(1, at = seq(20, 100, by = 10), labels = seq(20, 100, by = 10), col = NA, col.ticks = NA, cex.axis = 1.1, line = -0.5)
    mtext("Age", side = 1, line = 2.5, cex = 1.2)
    mtext("B", side = 3, at = 18, line = 0.8, font = 2, cex = 2.0)

    # ----------------------------------------------------
    # PANEL C: PARTY REGISTRATION
    # ----------------------------------------------------
    plot(
      0, 0, type = "n", xlim = c(0.4, 3.6), ylim = c(0, 5.2), yaxs = "i",
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
    )

    # Grid lines
    for (gy in 1:5) abline(h = gy, col = "#E0E0E0", lwd = 1.0)

    # Bars (Democrats, Independents, Republicans)
    bar_w3 <- 0.70
    rect(1 - bar_w3/2, 0, 1 + bar_w3/2, df_party_plot$pct[1], col = "#1F45FC", border = NA)
    rect(2 - bar_w3/2, 0, 2 + bar_w3/2, df_party_plot$pct[2], col = "#A0A0A0", border = NA)
    rect(3 - bar_w3/2, 0, 3 + bar_w3/2, df_party_plot$pct[3], col = "#FF4500", border = NA)

    # Error bars
    for (p_idx in 1:3) {
      arrows(
        p_idx, df_party_plot$prop_low[p_idx],
        p_idx, df_party_plot$prop_high[p_idx],
        angle = 90, code = 3, length = 0.06, lwd = 1.6, col = "black"
      )
    }

    lines(c(0.4, 3.6), c(0, 0), col = "black", lwd = 2.0)

    # Axes: Original
    axis(1, at = 1:3, labels = c("Democrats", "Independents", "Republicans"), tick = FALSE, cex.axis = 1.15, line = -0.5)
    axis(2, at = 0:5, labels = paste0(0:5, "%"), col = NA, col.ticks = NA, cex.axis = 1.1, line = -0.5)
    mtext("Party Registration", side = 1, line = 2.5, cex = 1.2)
    mtext("% Followers", side = 2, line = 3.2, cex = 1.2, las = 0)
    mtext("C", side = 3, at = 0.35, line = 0.8, font = 2, cex = 2.0)

    # ----------------------------------------------------
    # PANEL D: FRIENDS COUNT (LOG10 RIDGELINE - Anchored directly to x-axis)
    # ----------------------------------------------------
    # yaxs = 'i' with ylim = c(0, 2.55) anchors bottom baseline directly to x-axis without clipping the peak
    plot(
      0, 0, type = "n", xlim = c(0, 6.1), ylim = c(0, 2.55), yaxs = "i",
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
    )

    # Vertical grid lines
    for (gx in 0:6) abline(v = gx, col = "#E0E0E0", lwd = 1.0)

    scale_fr <- 1.30
    # Non-followers ridge (top, baseline = 0.85)
    polygon(
      c(dens_nf_fr$x, rev(dens_nf_fr$x)),
      c(0.85 + dens_nf_fr$y * scale_fr, rep(0.85, length(dens_nf_fr$x))),
      col = "#4D9DE0CC", border = "#4D9DE0", lwd = 1.2
    )

    # Followers ridge (bottom, baseline = 0.0 directly on x-axis)
    polygon(
      c(dens_f_fr$x, rev(dens_f_fr$x)),
      c(0.0 + dens_f_fr$y * scale_fr, rep(0.0, length(dens_f_fr$x))),
      col = "#FA686ACC", border = "#FA686A", lwd = 1.2
    )

    # Baselines drawn on top of polygons for crisp axis alignment
    lines(c(0, 6.0), c(0.85, 0.85), col = "#D0D0D0", lwd = 1.2)
    lines(c(0, 6.0), c(0, 0), col = "black", lwd = 2.0)

    # Annotations: Enlarged Followers and Non-followers labels
    text(0.1, 1.55, "Non-followers:\nN=639285,\navg=509.7", adj = 0, cex = 1.18, font = 1)
    text(0.1, 0.50, "Followers:\nN=25459,\navg=3474.2", adj = 0, cex = 1.18, font = 1)

    # X-axis ticks: Original (10^0 to 10^6)
    axis(1, at = 0:6, labels = c(expression(10^0), expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6)),
         col = NA, col.ticks = NA, cex.axis = 1.1, line = -0.5)
    mtext("Friends Count", side = 1, line = 2.5, cex = 1.2)
    mtext("D", side = 3, at = 0.0, line = 0.8, font = 2, cex = 2.0)
  }

  pdf(output_pdf, width = 11.0, height = 8.5)
  draw_demographics_base()
  dev.off()

  png(output_png, width = 3300, height = 2550, res = 300)
  draw_demographics_base()
  dev.off()
}

cat(sprintf("[SUCCESS] Figure 3 saved to:\n  - %s\n  - %s\n", output_pdf, output_png))
