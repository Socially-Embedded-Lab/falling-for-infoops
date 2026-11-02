# ==============================================================================
# Figure 2: Logistic Regression Forest Plot (Mentions vs. Following)
# ==============================================================================

cat("Generating Figure 2: Logistic regression coefficients forest plot...\n")

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

# NOTE: The following analysis requires access to individual-level sensitive voter panel data,
# which is not included in the public replication materials to protect voter privacy and comply
# with platform terms of service. Approved researchers with data access agreements should place
# these feature matrices under 'data/sensitive/'.
follow_path   <- resolve_path("data/sensitive/features_follow.csv")
mentions_path <- resolve_path("data/sensitive/features_mentions.csv")

output_dir <- resolve_path("output/figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
output_pdf <- file.path(output_dir, "figure_2_mention_vs_follow.pdf")
output_png <- file.path(output_dir, "figure_2_mention_vs_follow.png")

if (!file.exists(follow_path) || !file.exists(mentions_path)) {
  cat("[SKIPPED] Individual-level sensitive data not found in 'data/sensitive/'.\n")
  cat("          Access to 'features_follow.csv' and 'features_mentions.csv' can be requested from the study authors.\n")
  quit(save = "no", status = 0)
}

# Feature Transformation Function
transform_features <- function(df) {
  party <- rep("Unknown", nrow(df))
  if ("party_reg_Democrat" %in% names(df))    party[df$party_reg_Democrat == 1] <- "Democrat"
  if ("party_reg_Republican" %in% names(df))  party[df$party_reg_Republican == 1] <- "Republican"
  if ("party_reg_Independent" %in% names(df)) party[df$party_reg_Independent == 1] <- "Independent"
  if ("party_reg_Other" %in% names(df))        party[df$party_reg_Other == 1] <- "Other"
  df$party_registration <- factor(party, levels = c("Democrat", "Republican", "Independent", "Other", "Unknown"))

  raw_followers <- exp(df$followers_count_log_scale) - 1
  raw_friends   <- exp(df$friends_count_log_scale) - 1

  df$followers_count_log_scale <- log2(1 + raw_followers) - mean(log2(1 + raw_followers), na.rm = TRUE)
  df$friends_count_log_scale   <- log2(1 + raw_friends) - mean(log2(1 + raw_friends), na.rm = TRUE)
  df$age                       <- df$age / 10

  ratio <- pmax(df$friend_follower_ratio, 0)
  df$friend_follower_ratio <- ifelse(ratio > 0, log2(ratio) * 10, 0)

  tweets <- pmax(df$avg_monthly_tweets, 0)
  df$avg_monthly_tweets <- log2(1 + tweets)

  for (col in names(df)) {
    if (col != "party_registration" && is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- 0
    }
  }
  return(df)
}

balance_data <- function(df, target_col) {
  y <- df[[target_col]]
  pos_idx <- which(y == 1)
  neg_idx <- which(y == 0)
  
  set.seed(123)
  n_sample <- min(length(pos_idx), length(neg_idx))
  sampled_neg <- sample(neg_idx, n_sample)
  balanced_idx <- sample(c(pos_idx[1:n_sample], sampled_neg))
  return(df[balanced_idx, ])
}

# Ingest and Fit Models
cat("Ingesting and fitting Following model (Panel B)...\n")
df_follow <- read.csv(follow_path, stringsAsFactors = FALSE)
df_follow <- transform_features(df_follow)
df_follow$target <- ifelse(as.numeric(df_follow$troll_follow_count) > 0, 1, 0)
b_follow <- balance_data(df_follow, "target")

model_formula <- target ~ avg_monthly_tweets + political_percent + age + gender_Female + race_Caucasian +
  party_registration + followers_count_log_scale + friends_count_log_scale + verified_True +
  friend_follower_ratio + description_mentions + is_description_political

fit_follow <- glm(model_formula, data = b_follow, family = binomial)

cat("Ingesting and fitting Mentions model (Panel A)...\n")
df_mentions <- read.csv(mentions_path, stringsAsFactors = FALSE)
df_mentions <- transform_features(df_mentions)
df_mentions$target <- ifelse(tolower(as.character(df_mentions$is_mentioned)) %in% c("true", "1"), 1, 0)
b_mentions <- balance_data(df_mentions, "target")

fit_mentions <- glm(model_formula, data = b_mentions, family = binomial)

# Extract Coefficients & Confidence Intervals
cf_m <- summary(fit_mentions)$coefficients
cf_f <- summary(fit_follow)$coefficients

# Variable order: strictly ordered by mentions model estimate ascending (so top is highest in ggplot/base R)
vars_ordered <- c(
  "party_registrationUnknown",
  "gender_Female",
  "race_Caucasian",
  "party_registrationRepublican",
  "friend_follower_ratio",
  "friends_count_log_scale",
  "party_registrationIndependent",
  "description_mentions",
  "party_registrationOther",
  "avg_monthly_tweets",
  "is_description_political",
  "political_percent",
  "verified_True",
  "age",
  "followers_count_log_scale"
)

feature_labels <- c(
  "followers_count_log_scale"     = "# followers (log2)",
  "age"                           = "Age (10 years)",
  "verified_True"                 = "Is verified",
  "political_percent"             = "% Political content available",
  "is_description_political"      = "Description contains political terms",
  "avg_monthly_tweets"            = "Monthly # of tweets",
  "party_registrationOther"       = "Party registration Other",
  "description_mentions"          = "Description has mentions",
  "party_registrationIndependent" = "Party registration Independent",
  "friends_count_log_scale"       = "# friends (log2)",
  "friend_follower_ratio"         = "Friend-Follower ratio",
  "party_registrationRepublican"  = "Party registration Republican",
  "race_Caucasian"                = "Race (is Caucasian)",
  "gender_Female"                 = "Gender (is Female)",
  "party_registrationUnknown"     = "Party registration Unknown"
)

# Render Figure Matching Paper Design
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  extract_df <- function(cf_mat, mod_name) {
    df <- as.data.frame(cf_mat)
    df$Feature <- rownames(df)
    df$Estimate <- df$Estimate
    df$Std_Error <- df$`Std. Error`
    df$model <- mod_name
    df$lower <- df$Estimate - 1.96 * df$Std_Error
    df$upper <- df$Estimate + 1.96 * df$Std_Error
    df$significant <- ifelse(df$lower > 0 | df$upper < 0, "yes", "no")
    df <- df[df$Feature %in% vars_ordered, ]
    return(df)
  }

  c_m <- extract_df(cf_m, "(A) voter mentioned by InfoOps")
  c_f <- extract_df(cf_f, "(B) voter following InfoOps")
  comb <- rbind(c_m, c_f)
  comb$Feature <- factor(comb$Feature, levels = vars_ordered)
  comb$model   <- factor(comb$model, levels = c("(A) voter mentioned by InfoOps", "(B) voter following InfoOps"))

  p2 <- ggplot(comb, aes(x = Estimate, y = Feature, color = model, alpha = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_pointrange(aes(xmin = lower, xmax = upper), position = position_dodge(width = 0.5), size = 0.8) +
    facet_wrap(~ model, scales = "fixed") +
    scale_color_manual(values = c("(A) voter mentioned by InfoOps" = "#FF0000", "(B) voter following InfoOps" = "#F98400")) +
    scale_alpha_manual(values = c("yes" = 1, "no" = 0.3)) +
    scale_y_discrete(labels = feature_labels) +
    scale_x_continuous(breaks = seq(-1, 4, by = 1), limits = c(-1.3, 4.3)) +
    theme_minimal(base_size = 14) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.text.y = element_text(size = 12, color = "black"),
      axis.text.x = element_text(size = 11, color = "black"),
      strip.text  = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#EBEBEB"),
      legend.position = "none"
    )

  ggsave(output_pdf, plot = p2, width = 9.5, height = 6.0, bg = "white")
  ggsave(output_png, plot = p2, width = 9.5, height = 6.0, dpi = 300, bg = "white")
} else {
  # Base R Dual-Panel Facet Emulation matching exact paper aesthetics
  draw_forest_base <- function() {
    # Set up layout with exact identical plot box widths (3.00 inches each)
    layout(matrix(c(1, 2), nrow = 1, byrow = TRUE), widths = c(5.8, 3.7))

    n_vars <- length(vars_ordered)
    y_pos <- 1:n_vars
    xlim_val <- c(-1.3, 4.3)
    x_ticks <- seq(-1, 4, by = 1)

    # Panel A: Mentions (Red) - mai in inches: bottom=0.70, left=2.65, top=0.60, right=0.15 -> plot width = 3.00 in
    par(mai = c(0.70, 2.65, 0.60, 0.15), bg = "white", las = 1)
    plot(
      0, 0, type = "n", xlim = xlim_val, ylim = c(0.5, n_vars + 0.5),
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
    )

    # Facet header (A)
    mtext("(A) voter mentioned by InfoOps", side = 3, line = 1.0, font = 2, cex = 1.15)

    # Grid lines
    for (gx in x_ticks) abline(v = gx, col = "#EEEEEE", lwd = 1.0)
    for (gy in y_pos)   abline(h = gy, col = "#EEEEEE", lwd = 1.0)
    abline(v = 0, lty = 2, col = "black", lwd = 1.2)

    # Axes
    axis(1, at = x_ticks, labels = x_ticks, col = NA, col.ticks = NA, cex.axis = 1.0, line = -0.6)
    axis(2, at = y_pos, labels = feature_labels[vars_ordered], tick = FALSE, cex.axis = 0.95, line = -0.5)

    # Draw pointranges for Model A
    for (i in 1:n_vars) {
      v <- vars_ordered[i]
      est <- cf_m[v, "Estimate"]
      se  <- cf_m[v, "Std. Error"]
      lo  <- est - 1.96 * se
      hi  <- est + 1.96 * se
      sig <- (lo > 0 | hi < 0)
      
      col_use <- if (sig) "#FF0000" else "#FF00004D"
      segments(lo, y_pos[i], hi, y_pos[i], col = col_use, lwd = 2.2)
      points(est, y_pos[i], pch = 16, col = col_use, cex = 1.4)
    }

    # Panel B: Following (Orange) - mai in inches: bottom=0.70, left=0.15, top=0.60, right=0.55 -> plot width = 3.00 in
    par(mai = c(0.70, 0.15, 0.60, 0.55), bg = "white", las = 1)
    plot(
      0, 0, type = "n", xlim = xlim_val, ylim = c(0.5, n_vars + 0.5),
      xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
    )

    # Facet header (B)
    mtext("(B) voter following InfoOps", side = 3, line = 1.0, font = 2, cex = 1.15)

    # Grid lines
    for (gx in x_ticks) abline(v = gx, col = "#EEEEEE", lwd = 1.0)
    for (gy in y_pos)   abline(h = gy, col = "#EEEEEE", lwd = 1.0)
    abline(v = 0, lty = 2, col = "black", lwd = 1.2)

    # X-axis only
    axis(1, at = x_ticks, labels = x_ticks, col = NA, col.ticks = NA, cex.axis = 1.0, line = -0.6)

    # Draw pointranges for Model B
    for (i in 1:n_vars) {
      v <- vars_ordered[i]
      est <- cf_f[v, "Estimate"]
      se  <- cf_f[v, "Std. Error"]
      lo  <- est - 1.96 * se
      hi  <- est + 1.96 * se
      sig <- (lo > 0 | hi < 0)

      col_use <- if (sig) "#F98400" else "#F984004D"
      segments(lo, y_pos[i], hi, y_pos[i], col = col_use, lwd = 2.2)
      points(est, y_pos[i], pch = 16, col = col_use, cex = 1.4)
    }
  }

  pdf(output_pdf, width = 9.5, height = 6.0)
  draw_forest_base()
  dev.off()

  png(output_png, width = 2850, height = 1800, res = 300)
  draw_forest_base()
  dev.off()
}

cat(sprintf("[SUCCESS] Figure 2 saved to:\n  - %s\n  - %s\n", output_pdf, output_png))
