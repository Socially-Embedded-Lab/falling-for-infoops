# ==============================================================================
# Table 4: High vs. Low InfoOps Availability Models (Appendix B)
# Generates Table 4 in LaTeX format
# Replication script for HKS Misinformation Review
# ==============================================================================

cat("Generating Table 4: High vs. Low InfoOps Availability regression table...\n")

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
super_consumers_path <- resolve_path("data/sensitive/features_super_consumers.csv")
consumers_path       <- resolve_path("data/sensitive/features_consumers.csv")

output_dir <- resolve_path("output/tables")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
output_tex <- file.path(output_dir, "table_4_consumers_summary.tex")

if (!file.exists(super_consumers_path) || !file.exists(consumers_path)) {
  cat("[SKIPPED] Individual-level sensitive data not found in 'data/sensitive/'.\n")
  cat("          Access to 'features_consumers.csv' and 'features_super_consumers.csv' can be requested from authors.\n")
  quit(save = "no", status = 0)
}

# 2. Feature Transformation Function (Portable Base R)
prepare_consumer_data <- function(file_path, target_val) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  df$indirect_consumers <- ifelse(tolower(as.character(df$indirect_consumers)) %in% c(tolower(target_val), "1"), 1, 0)
  
  df$party_registration <- "Unknown"
  if ("party_reg_Democrat" %in% names(df)) df$party_registration[df$party_reg_Democrat == 1] <- "Democrat"
  if ("party_reg_Republican" %in% names(df)) df$party_registration[df$party_reg_Republican == 1] <- "Republican"
  if ("party_reg_Independent" %in% names(df)) df$party_registration[df$party_reg_Independent == 1] <- "Independent"
  if ("party_reg_Other" %in% names(df)) df$party_registration[df$party_reg_Other == 1] <- "Other"
  df$party_registration <- factor(df$party_registration, levels = c("Democrat", "Republican", "Independent", "Other", "Unknown"))
  
  p_cols <- grep("^party_reg_", names(df), value = TRUE)
  df <- df[, !(names(df) %in% p_cols)]
  
  df$followers_count <- pmax(exp(df$followers_count_log_scale) - 1, 0)
  df$friends_count   <- pmax(exp(df$friends_count_log_scale) - 1, 0)
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
  idx_pos <- which(df$indirect_consumers == 1)
  idx_neg <- which(df$indirect_consumers == 0)
  if (length(idx_pos) > 0 && length(idx_neg) > 0) {
    idx_neg_samp <- sample(idx_neg, length(idx_pos))
    df_balanced <- df[c(idx_pos, idx_neg_samp), ]
    df_balanced <- df_balanced[sample(nrow(df_balanced)), ]
    return(df_balanced)
  }
  return(df)
}

# 3. Fit Logistic Regression Models
cat("Fitting High and Low InfoOps Availability models...\n")
df_super   <- prepare_consumer_data(super_consumers_path, "super_consumers")
df_regular <- prepare_consumer_data(consumers_path, "consumers")

formula_model <- indirect_consumers ~ age + gender_Female + race_Caucasian + party_registration +
  avg_monthly_tweets + political_percent +
  followers_count_log_scale + friends_count_log_scale +
  friend_follower_ratio + verified_True + description_mentions +
  is_description_political

model_super_consumers <- glm(formula_model, data = df_super, family = binomial)
model_consumers       <- glm(formula_model, data = df_regular, family = binomial)

# 4. Export Table (Stargazer if available, otherwise robust base R LaTeX formatter)
title_text <- "Two logistic regression models fitted to distinguish panelists with high- and low-availability of InfoOps content from their networks."
col_labels <- c("(C) High InfoOps Availability", "(D) Low InfoOps Availability")

if (requireNamespace("stargazer", quietly = TRUE)) {
  suppressPackageStartupMessages(library(stargazer))
  stargazer(
    model_super_consumers, model_consumers,
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
  format_pval <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("$^{***}$")
    if (p < 0.01) return("$^{**}$")
    if (p < 0.05) return("$^{*}$")
    return("")
  }

  s1 <- summary(model_super_consumers)$coefficients
  s2 <- summary(model_consumers)$coefficients

  all_terms <- unique(c(rownames(s1), rownames(s2)))
  all_terms <- c(setdiff(all_terms, "(Intercept)"), "(Intercept)")

  lines <- c()
  lines <- c(lines, "\\begin{table}[!htbp] \\centering")
  lines <- c(lines, sprintf("  \\caption{%s}", title_text))
  lines <- c(lines, "  \\label{tab:models_consumers}")
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
  lines <- c(lines, sprintf("Observations & %s & %s \\\\", format(nobs(model_super_consumers), big.mark = ","), format(nobs(model_consumers), big.mark = ",")))
  lines <- c(lines, sprintf("Log Likelihood & %.2f & %.2f \\\\", logLik(model_super_consumers), logLik(model_consumers)))
  lines <- c(lines, sprintf("Akaike Inf. Crit. & %.2f & %.2f \\\\", AIC(model_super_consumers), AIC(model_consumers)))
  lines <- c(lines, "\\hline")
  lines <- c(lines, "\\hline \\\\[-1.8ex]")
  lines <- c(lines, "\\textit{Note:}  & \\multicolumn{2}{r}{$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001} \\\\")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")

  writeLines(lines, output_tex)
}

cat(sprintf("[SUCCESS] Table 4 successfully saved to:\n  - %s\n", output_tex))
