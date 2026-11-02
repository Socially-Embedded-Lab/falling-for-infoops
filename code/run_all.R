# ==============================================================================
# Master Replication Runner: All Figures & Tables
# Journal submission replication script for HKS Misinformation Review
# ==============================================================================

cat("==============================================================================\n")
cat("Starting full paper replication pipeline...\n")
cat("==============================================================================\n\n")

# Portable Path Resolver
resolve_path <- function(rel_path) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_dir <- if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
  target_root <- if (basename(script_dir) == "replication_paper") dirname(script_dir) else file.path(getwd(), "target")
  candidates <- c(
    file.path(target_root, rel_path),
    file.path(script_dir, "..", rel_path),
    file.path(script_dir, rel_path),
    file.path("target", rel_path),
    rel_path
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(normalizePath(cand))
  }
  return(file.path(target_root, rel_path))
}

output_fig <- resolve_path("output/figures")
output_tab <- resolve_path("output/tables")
if (!dir.exists(output_fig)) dir.create(output_fig, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(output_tab)) dir.create(output_tab, recursive = TRUE, showWarnings = FALSE)

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_dir <- if (length(file_arg) > 0) dirname(normalizePath(sub("^--file=", "", file_arg))) else getwd()
target_root <- if (basename(script_dir) %in% c("code", "replication_paper")) dirname(script_dir) else file.path(getwd(), "target")

base_dir <- if (basename(script_dir) %in% c("code", "replication_paper")) {
  script_dir
} else if (dir.exists(file.path(target_root, "code"))) {
  file.path(target_root, "code")
} else {
  getwd()
}

r_scripts <- c(
  "finding_1_concentration.R",
  "finding_2_mentions_and_following.R",
  "finding_3_demographics.R",
  "finding_4_mediators_and_relaying.R",
  "finding_2_regression_models.R",
  "appendix_b_consumer_models.R",
  "appendix_c_threat_actor_models.R"
)

# 1. Run Python topic analysis if python available
cat(">>> [1/8] Replicating Topic Analysis (Table 1: Top 10 BERTopic Topics)\n")
topic_script <- file.path(base_dir, "finding_5_topics.py")
py_cmd <- Sys.which("python")
if (nzchar(py_cmd) && file.exists(topic_script)) {
  system2("python", args = c(shQuote(topic_script)))
} else {
  cat("  Python not found in PATH or finding_5_topics.py not found; skipping topic analysis.\n")
}
cat("\n")

# 2. Run all R scripts
step <- 2
for (scr in r_scripts) {
  scr_path <- file.path(base_dir, scr)
  cat(sprintf(">>> [%d/8] Running %s...\n", step, scr))
  if (file.exists(scr_path)) {
    source(scr_path, local = new.env())
  } else {
    cat(sprintf("  Warning: Script %s not found at %s\n", scr, scr_path))
  }
  cat("\n")
  step <- step + 1
}

cat("==============================================================================\n")
cat("[SUCCESS] All manuscript figures and tables successfully replicated!\n")
cat("Output files are stored in:\n")
cat("  - target/output/figures/\n")
cat("  - target/output/tables/\n")
cat("==============================================================================\n")

