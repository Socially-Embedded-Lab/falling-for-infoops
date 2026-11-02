# ==============================================================================
# Table 5: Followership Models by Threat Actor (China, Iran, Russia)
# Generates Table 5 (Appendix C) in LaTeX format
# Replication script for HKS Misinformation Review
# ==============================================================================

cat("Generating Table 5: Threat actor models (China, Iran, Russia)...\n")

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

# NOTE: The following analysis requires access to individual-level sensitive voter panel data
# and unhashed troll profile mappings, which are not included in the public replication materials
# to protect voter privacy and comply with platform terms of service. Approved researchers with
# data access agreements should place these datasets under 'data/sensitive/'.
profiles_path <- resolve_path("data/sensitive/trolls_profiles_unhashed.csv")
if (!file.exists(profiles_path)) {
  profiles_path <- resolve_path("data/processed/trolls/trolls_profiles_unhashed.csv")
}

panel_path <- resolve_path("data/sensitive/panel_followees_trolls_intersc2020_2017_2021.csv")
if (!file.exists(panel_path)) {
  panel_path <- resolve_path("data/processed/demographics/panel_followees_trolls_intersc2020_2017_2021.csv")
}

features_path <- resolve_path("data/sensitive/features_follow.csv")

output_dir <- resolve_path("output/tables")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}
output_tex <- file.path(output_dir, "table_5_threat_actors.tex")

if (!file.exists(profiles_path) || !file.exists(panel_path) || !file.exists(features_path)) {
  cat("[SKIPPED] Individual-level sensitive data not found in 'data/sensitive/'.\n")
  cat("          Access to threat actor models data can be requested from the study authors.\n")
  quit(save = "no", status = 0)
}

# 2. Load Profiles and Panel Mapping
cat("Loading and mapping threat actor profiles...\n")
df_profiles <- read.csv(profiles_path, colClasses = "character", stringsAsFactors = FALSE)
df_panel    <- read.csv(panel_path, colClasses = "character", stringsAsFactors = FALSE)

names(df_profiles) <- trimws(names(df_profiles))
names(df_panel)    <- trimws(names(df_panel))

if ("country" %in% names(df_profiles)) {
  df_profiles$country <- tolower(trimws(df_profiles$country))
}

# Merge panel followees with troll country
df_prof_sub <- df_profiles[!is.na(df_profiles$country) & df_profiles$country != "", c("userid", "country")]
m <- merge(df_panel, df_prof_sub, by.x = "followee_uid", by.y = "userid")
user_country_df <- unique(m[, c("panel_uid", "country")])
names(user_country_df)[1] <- "uid"

# 3. Load & Transform Features
cat("Loading features dataset...\n")
df_features <- read.csv(features_path, stringsAsFactors = FALSE)

df_features$troll_follow_count <- ifelse(as.numeric(df_features$troll_follow_count) > 0, 1, 0)
df_features$uid <- as.character(df_features$uid)

# Party registration mapping
df_features$party_registration <- "Unknown"
if ("party_reg_Democrat" %in% names(df_features)) df_features$party_registration[df_features$party_reg_Democrat == 1] <- "Democrat"
if ("party_reg_Republican" %in% names(df_features)) df_features$party_registration[df_features$party_reg_Republican == 1] <- "Republican"
if ("party_reg_Independent" %in% names(df_features)) df_features$party_registration[df_features$party_reg_Independent == 1] <- "Independent"
if ("party_reg_Other" %in% names(df_features)) df_features$party_registration[df_features$party_reg_Other == 1] <- "Other"
df_features$party_registration <- factor(df_features$party_registration, levels = c("Democrat", "Republican", "Independent", "Other", "Unknown"))

p_cols <- grep("^party_reg_", names(df_features), value = TRUE)
df_features <- df_features[, !(names(df_features) %in% p_cols)]

# Numerical transformations
df_features$followers_count <- pmax(exp(df_features$followers_count_log_scale) - 1, 0)
df_features$friends_count   <- pmax(exp(df_features$friends_count_log_scale) - 1, 0)
df_features$followers_count_log_scale <- log2(1 + df_features$followers_count) - mean(log2(1 + df_features$followers_count), na.rm = TRUE)
df_features$friends_count_log_scale   <- log2(1 + df_features$friends_count) - mean(log2(1 + df_features$friends_count), na.rm = TRUE)

df_features$age <- df_features$age / 10
safe_ratio <- pmax(df_features$friend_follower_ratio, 0)
df_features$friend_follower_ratio <- ifelse(safe_ratio > 0, log2(safe_ratio) * 10, 0)
df_features$avg_monthly_tweets <- log2(pmax(df_features$avg_monthly_tweets, 0) + 1)

for (col in names(df_features)) {
  if (col != "party_registration" && is.numeric(df_features[[col]])) {
    df_features[[col]][is.na(df_features[[col]])] <- 0
  }
}

# 4. Fit Logistic Regressions per Threat Actor
target_countries <- c("china", "iran", "russia")
model_list       <- list()
pseudo_r2_list   <- c()

model_formula <- troll_follow_count ~ age + gender_Female + race_Caucasian +
  party_registration + avg_monthly_tweets + political_percent +
  followers_count_log_scale + friends_count_log_scale +
  friend_follower_ratio + verified_True + description_mentions +
  is_description_political

for (country_name in target_countries) {
  cat(paste0("  Fitting model for: ", toupper(country_name), "...\n"))
  country_uids <- user_country_df$uid[user_country_df$country == country_name]

  df_country <- df_features[(df_features$troll_follow_count == 1 & df_features$uid %in% country_uids) | (df_features$troll_follow_count == 0), ]

  idx_pos <- which(df_country$troll_follow_count == 1)
  idx_neg <- which(df_country$troll_follow_count == 0)

  set.seed(123)
  idx_neg_samp <- sample(idx_neg, length(idx_pos))
  balanced_df  <- df_country[c(idx_pos, idx_neg_samp), ]
  balanced_df  <- balanced_df[sample(nrow(balanced_df)), ]

  fit <- glm(model_formula, data = balanced_df, family = binomial)
  model_list[[country_name]] <- fit

  # McFadden's pseudo R2: 1 - (logLik(fit) / logLik(null_fit))
  null_fit <- glm(troll_follow_count ~ 1, data = balanced_df, family = binomial)
  r2_val <- round(1 - as.numeric(logLik(fit)) / as.numeric(logLik(null_fit)), 3)
  pseudo_r2_list <- c(pseudo_r2_list, sprintf("%.3f", r2_val))
}

# 5. Output LaTeX Table
title_text <- "Logistic regression models predicting voters following InfoOps accounts across China, Iran, and Russia operations."
col_labels <- c("(A) China", "(B) Iran", "(C) Russia")

if (requireNamespace("stargazer", quietly = TRUE)) {
  suppressPackageStartupMessages(library(stargazer))
  stargazer(
    model_list[["china"]], model_list[["iran"]], model_list[["russia"]],
    type = "latex",
    out = output_tex,
    title = title_text,
    column.labels = col_labels,
    dep.var.labels.include = FALSE,
    star.char = c("*", "**", "***"),
    star.cutoffs = c(0.05, 0.01, 0.001),
    no.space = TRUE,
    add.lines = list(
      c("Pseudo $R^2$ (McFadden)", pseudo_r2_list[1], pseudo_r2_list[2], pseudo_r2_list[3])
    )
  )
} else {
  format_pval <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("$^{***}$")
    if (p < 0.01) return("$^{**}$")
    if (p < 0.05) return("$^{*}$")
    return("")
  }

  s_china  <- summary(model_list[["china"]])$coefficients
  s_iran   <- summary(model_list[["iran"]])$coefficients
  s_russia <- summary(model_list[["russia"]])$coefficients

  all_terms <- unique(c(rownames(s_china), rownames(s_iran), rownames(s_russia)))
  all_terms <- c(setdiff(all_terms, "(Intercept)"), "(Intercept)")

  lines <- c()
  lines <- c(lines, "\\begin{table}[!htbp] \\centering")
  lines <- c(lines, sprintf("  \\caption{%s}", title_text))
  lines <- c(lines, "  \\label{tab:models_threat_actors}")
  lines <- c(lines, "\\begin{tabular}{@{\\extracolsep{5pt}}lccc}")
  lines <- c(lines, "\\\\[-1.8ex]\\hline")
  lines <- c(lines, "\\hline \\\\[-1.8ex]")
  lines <- c(lines, " & \\multicolumn{3}{c}{\\textit{Dependent variable:}} \\\\")
  lines <- c(lines, "\\cline{2-4}")
  lines <- c(lines, sprintf("\\\\[-1.8ex] & %s & %s & %s \\\\", col_labels[1], col_labels[2], col_labels[3]))
  lines <- c(lines, "\\hline \\\\[-1.8ex]")

  clean_name <- function(n) {
    n <- gsub("_", "\\\\_", n)
    n <- gsub("party\\_registration", "Party: ", n)
    if (n == "(Intercept)") n <- "Constant"
    return(n)
  }

  for (term in all_terms) {
    v1_est <- if (term %in% rownames(s_china)) sprintf("%.3f%s", s_china[term, "Estimate"], format_pval(s_china[term, "Pr(>|z|)"])) else ""
    v2_est <- if (term %in% rownames(s_iran)) sprintf("%.3f%s", s_iran[term, "Estimate"], format_pval(s_iran[term, "Pr(>|z|)"])) else ""
    v3_est <- if (term %in% rownames(s_russia)) sprintf("%.3f%s", s_russia[term, "Estimate"], format_pval(s_russia[term, "Pr(>|z|)"])) else ""

    v1_se  <- if (term %in% rownames(s_china)) sprintf("(%.3f)", s_china[term, "Std. Error"]) else ""
    v2_se  <- if (term %in% rownames(s_iran)) sprintf("(%.3f)", s_iran[term, "Std. Error"]) else ""
    v3_se  <- if (term %in% rownames(s_russia)) sprintf("(%.3f)", s_russia[term, "Std. Error"]) else ""

    lines <- c(lines, sprintf(" %s & %s & %s & %s \\\\", clean_name(term), v1_est, v2_est, v3_est))
    lines <- c(lines, sprintf("  & %s & %s & %s \\\\", v1_se, v2_se, v3_se))
  }

  lines <- c(lines, "\\hline \\\\[-1.8ex]")
  lines <- c(lines, sprintf("Observations & %s & %s & %s \\\\",
    format(nobs(model_list[["china"]]), big.mark = ","),
    format(nobs(model_list[["iran"]]), big.mark = ","),
    format(nobs(model_list[["russia"]]), big.mark = ",")))
  lines <- c(lines, sprintf("Log Likelihood & %.2f & %.2f & %.2f \\\\",
    logLik(model_list[["china"]]), logLik(model_list[["iran"]]), logLik(model_list[["russia"]])))
  lines <- c(lines, sprintf("Akaike Inf. Crit. & %.2f & %.2f & %.2f \\\\",
    AIC(model_list[["china"]]), AIC(model_list[["iran"]]), AIC(model_list[["russia"]])))
  lines <- c(lines, sprintf("Pseudo $R^2$ (McFadden) & %s & %s & %s \\\\",
    pseudo_r2_list[1], pseudo_r2_list[2], pseudo_r2_list[3]))
  lines <- c(lines, "\\hline")
  lines <- c(lines, "\\hline \\\\[-1.8ex]")
  lines <- c(lines, "\\textit{Note:}  & \\multicolumn{3}{r}{$^{*}$p$<$0.05; $^{**}$p$<$0.01; $^{***}$p$<$0.001} \\\\")
  lines <- c(lines, "\\end{tabular}")
  lines <- c(lines, "\\end{table}")

  writeLines(lines, output_tex)
}

cat(sprintf("[SUCCESS] Table 5 successfully saved to:\n  - %s\n", output_tex))
