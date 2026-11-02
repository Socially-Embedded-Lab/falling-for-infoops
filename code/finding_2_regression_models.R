# ==============================================================================
# Table 3: Logistic Regression Models (Mentions vs. Following)
# Generates Table 3 (Appendix A) in LaTeX format
# Replication script for HKS Misinformation Review
# ==============================================================================

cat("Generating Table 3: Mentions and Follow models regression table...\n")

# 1. Portable Path Resolver (anchored to 'target' replication package)
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
features_follow_path   <- resolve_path("data/sensitive/features_follow.csv")
features_mentions_path <- resolve_path("data/sensitive/features_mentions.csv")

output_dir <- resolve_path("output/tables")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
output_tex <- file.path(output_dir, "table_3_regression_summary.tex")

if (!file.exists(features_follow_path) || !file.exists(features_mentions_path)) {
  cat("[SKIPPED] Individual-level sensitive data not found in 'data/sensitive/'.\n")
  cat("          Access to 'features_follow.csv' and 'features_mentions.csv' can be requested from the study authors.\n")
  quit(save = "no", status = 0)
}

# 2. Feature Transformation Function (Portable Base R)
prepare_model_data <- function(file_path, target_col, is_target_boolean = FALSE) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  if (is_target_boolean) {
    df[[target_col]] <- ifelse(tolower(as.character(df[[target_col]])) %in% c("true", "1"), 1, 0)
  } else {
    df[[target_col]] <- ifelse(as.numeric(df[[target_col]]) > 0, 1, 0)
  }
  
  df$party_registration <- "Unknown"
  if ("party_reg_Democrat" %in% names(df)) df$party_registration[df$party_reg_Democrat == 1] <- "Democrat"
  if ("party_reg_Republican" %in% names(df)) df$party_registration[df$party_reg_Republican == 1] <- "Republican"
  if ("party_reg_Independent" %in% names(df)) df$party_registration[df$party_reg_Independent == 1] <- "Independent"
  if ("party_reg_Other" %in% names(df)) df$party_registration[df$party_reg_Other == 1] <- "Other"
  df$party_registration <- factor(df$party_registration, levels = c("Democrat", "Republican", "Independent", "Other", "Unknown"))
  
  p_cols <- grep("^party_reg_", names(df), value = TRUE)
  df <- df[, !(names(df) %in% p_cols)]
  
  df$followers_count <- exp(df$followers_count_log_scale) - 1
  df$friends_count   <- exp(df$friends_count_log_scale) - 1
  df$followers_count_log_scale <- log2(1 + df$followers_count) - mean(log2(1 + df$followers_count), na.rm = TRUE)
  df$friends_count_log_scale   <- log2(1 + df$friends_count) - mean(log2(1 + df$friends_count), na.rm = TRUE)
  
  df$age <- df$age / 10
  safe_ratio <- pmax(df$friend_follower_ratio, 0)
  df$friend_follower_ratio <- ifelse(safe_ratio > 0, log2(safe_ratio) * 10, 0)
  df$avg_monthly_tweets <- log2(pmax(df$avg_monthly_tweets, 0) + 1)
  
  for (col in names(df)) {
    if (col != "party_registration" && is.numeric(df[[col]])) {
      df[[col]][is.na(df[[col]])] <- 0
    }
  }
  
  set.seed(123)
  idx_pos <- which(df[[target_col]] == 1)
  idx_neg <- which(df[[target_col]] == 0)
  if (length(idx_pos) > 0 && length(idx_neg) > 0) {
    idx_neg_samp <- sample(idx_neg, length(idx_pos))
    df_balanced <- df[c(idx_pos, idx_neg_samp), ]
    df_balanced <- df_balanced[sample(nrow(df_balanced)), ]
    return(df_balanced)
  }
  return(df)
}

# 3. Fit Logistic Regression Models
cat("Fitting logistic regression models...\n")
df_mentions <- prepare_model_data(features_mentions_path, "is_mentioned", is_target_boolean = TRUE)
df_follow   <- prepare_model_data(features_follow_path, "troll_follow_count", is_target_boolean = FALSE)

formula_str <- "~ age + gender_Female + race_Caucasian + party_registration + avg_monthly_tweets + political_percent + followers_count_log_scale + friends_count_log_scale + friend_follower_ratio + verified_True + description_mentions + is_description_political"

model_mentions <- glm(as.formula(paste("is_mentioned", formula_str)), data = df_mentions, family = binomial)
model_follow   <- glm(as.formula(paste("troll_follow_count", formula_str)), data = df_follow, family = binomial)

# 4. Export Table (Stargazer if available, otherwise robust base R LaTeX formatter)
title_text <- "Two logistic regression models fitted to distinguish voters mentioned by InfoOps and voters following InfoOps."
col_labels <- c("(A) Mentioned", "(B) Following")

if (requireNamespace("stargazer", quietly = TRUE)) {
  suppressPackageStartupMessages(library(stargazer))
  stargazer(
    model_mentions, model_follow,
    type = "latex",
    out = output_tex,
    title = title_text,
    column.labels = col_labels,
    dep.var.labels.include = FALSE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(0.05, 0.01, 0.001),
    no.space = TRUE
  )
} else {
  # Robust Base R LaTeX Stargazer Emulation
  format_pval <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("$^{***}$")
    if (p < 0.01) return("$^{**}$")
    if (p < 0.05) return("$^{*}$")
    return("")
  }

  s1 <- summary(model_mentions)$coefficients
  s2 <- summary(model_follow)$coefficients

  all_terms <- unique(c(rownames(s1), rownames(s2)))
  all_terms <- c(setdiff(all_terms, "(Intercept)"), "(Intercept)")

  lines <- c()
  lines <- c(lines, "\\begin{table}[!htbp] \\centering")
  lines <- c(lines, sprintf("  \\caption{%s}", title_text))
  lines <- c(lines, "  \\label{tab:models_mention_follow}")
  lines <- c(lines, "\\begin{tabular}{@{\\extracolsep{5pt}}lcc}")
  lines <- c(lines, "\\\\[-1.8ex]\\hline")
  lines <- c(lines, "\\hline \\\\[-1.8ex]")
  lines <- c(lines, " & \\multicolumn{2}{c}{\\textit{Dependent variable:}} \\\\")
  lines <- c(lines, "\\cline{2-3}")
  lines <- c(lines, sprintf("\\\\[-1.8ex] & %s & %s \\\\", col_labels[1], col_labels[2]))
  lines <- c(lines, "\\hline \\\\[-1.8ex]")

  clean_name <- function(n) {
    n <- gsub("_", "\\\\_", n)
    n <- gsub("party\\_registration", "Party: ", n)
    if (n == "(Intercept)") n <- "Constant"
    return(n)
  }

  for (term in all_terms) {
    v1_est <- if (term %in% rownames(s1)) sprintf("%.3f%s", s1[term, "Estimate"], format_pval(s1[term, "Pr(>|z|)"])) else ""
    v2_est <- if (term %in% rownames(s2)) sprintf("%.3f%s", s2[term, "Estimate"], format_pval(s2[term, "Pr(>|z|)"])) else ""
    
    v1_se  <- if (term %in% rownames(s1)) sprintf("(%.3f)", s1[term, "Std. Error"]) else ""
    v2_se  <- if (term %in% rownames(s2)) sprintf("(%.3f)", s2[term, "Std. Error"]) else ""

    lines <- c(lines, sprintf(" %s & %s & %s \\\\", clean_name(term), v1_est, v2_est))
    lines <- c(lines, sprintf("  & %s & %s \\\\", v1_se, v2_se))
  }

  lines <- c(lines, "\\hline \\\\[-1.8ex]")
  lines <- c(lines, sprintf("Observations & %s & %s \\\\", format(nobs(model_mentions), big.mark = ","), format(nobs(model_follow), big.mark = ",")))
  lines <- c(lines, sprintf("Log Likelihood & %.2f & %.2f \\\\", logLik(model_mentions), logLik(model_follow)))
  lines <- c(lines, sprintf("Akaike Inf. Crit. & %.2f & %.2f \\\\", AIC(model_mentions), AIC(model_follow)))
  lines <- c(lines, "\\hline")
  lines <- c(lines, "\\hline \\\\[-1.8ex]")
  lines <- c(lines, "\\textit{Note:}  & \\multicolumn{2}{r}{$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001} \\\\")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")

  writeLines(lines, output_tex)
}

cat(sprintf("[SUCCESS] Table 3 successfully saved to:\n  - %s\n", output_tex))
