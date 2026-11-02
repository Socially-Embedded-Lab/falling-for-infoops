# ==============================================================================
# R Replication Package Dependencies Installer
# Tested with R >= 4.2.0
# ==============================================================================

cat("Checking and installing required R packages for replication...\n")

required_packages <- c(
  "here",          # Portable, project-relative path management
  "readr",         # High-performance CSV parsing
  "dplyr",         # Data manipulation
  "tidyr",         # Data reshaping
  "stringr",       # String processing
  "ggplot2",       # Data visualization
  "patchwork",     # Multi-panel figure layouts
  "ggridges",      # Ridgeline density estimation
  "scales",        # Percentage and axis transform formatting
  "stargazer",     # LaTeX and ASCII regression tables
  "pscl",          # McFadden's Pseudo-R2 computation
  "car",           # Variance Inflation Factor (VIF) & Type III LR tests
  "pROC",          # ROC and Area Under Curve (AUC)
  "margins",       # Average Marginal Effects (AME)
  "vip",           # Variable importance extraction
  "RColorBrewer",  # Academic color palettes
  "extrafont"      # Typography management
)

# Identify uninstalled packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:\n", paste("-", missing_packages, collapse = "\n"), "\n")
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  cat("All required R packages are already installed.\n")
}

cat("R replication environment check complete.\n")

