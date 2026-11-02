# Replication Package: Who is Falling for Foreign Influence?
### Studying the Interaction of U.S. Voters with Foreign Influence Accounts on Twitter

This repository provides the complete replication code, analytical notebooks, and instructions to reproduce all empirical findings, figures, and tables reported in the manuscript.

---

## 1. Data Access & Harvard Dataverse Deposit

To protect individuals' privacy and comply with platform terms of service, no personally identifying information (PII) is released. Therefore, the code in this repo replicates the empirical findings using aggregate statistics, deposited in **Harvard Dataverse Dataset**: [https://doi.org/10.7910/DVN/MPXEUN](https://doi.org/10.7910/DVN/MPXEUN)

To replicate the analyses, download the dataset files from Harvard Dataverse at [https://doi.org/10.7910/DVN/MPXEUN](https://doi.org/10.7910/DVN/MPXEUN) and place them directly under the `data/` folder:

| File Path | Description | Paper Output |
| :--- | :--- | :--- |
| `data/exp_pol_dist.csv` | Empirical political content exposure ECDF coordinates | Figure 1 |
| `data/exp_infoops_dist.csv` | Empirical InfoOps content exposure ECDF coordinates | Figure 1 |
| `data/gender_dist.csv` | Proportions of female panelists among followers vs. non-followers | Figure 3A |
| `data/age_dist.csv` | Precomputed kernel density coordinates for panel age | Figure 3B |
| `data/party_reg_dist.csv` | Party registration proportions and Bayes probabilities | Figure 3C |
| `data/friends_dist.csv` | Precomputed kernel density coordinates for friends count | Figure 3D |
| `data/mediators_mentions_dist.csv` | Account categories mentioned by InfoOps accounts | Figure 4A |
| `data/mediators_relaying_dist.csv` | Account categories relaying InfoOps tweets to panelists | Figure 4B |
| `data/mediators_vs_relays_dist.csv` | Comparison summary of mentions vs. relays percentages | Figure 4 |

All files have been formatted to strict RFC-4180 CSV specifications (no unnamed index columns, consistent column headers, and clean single-newline terminations) ensuring seamless tabular ingest by Harvard Dataverse.

Analysis code of individual-level data is also provided.

---

## 2. Repository Structure

```text
target/
├── README.md                          <- Master replication documentation
├── install_packages.R                 <- R dependency installer
├── data/                              <- Replication datasets (from Dataverse)
├── code/                              <- Standalone replication scripts
│   ├── run_all.R                      <- Master runner: executes all figures and tables
│   ├── finding_1_concentration.R      <- Content exposure concentration & ECDF (Figure 1)
│   ├── finding_2_mentions_and_following.R <- Predictors of following vs. mentions (Figure 2)
│   ├── finding_2_regression_models.R  <- Logistic regression models of following & mentions (Table A1)
│   ├── finding_3_demographics.R       <- Voter demographic breakdown (Figure 3)
│   ├── finding_4_mediators_and_relaying.R <- Intermediaries & relaying account breakdown (Figure 4)
│   ├── finding_5_topics.py            <- Prevalent topics of followed vs. non-followed InfoOps (Table 1)
│   ├── appendix_b_consumer_models.R    <- High vs. low volume consumer models (Table B1)
│   └── appendix_c_threat_actor_models.R <- Threat actor attribution models: Russia, Iran, China (Table C1)
└── output/                            <- Output destination for replication artifacts
    ├── figures/                       <- Generated PDF and PNG figures
    └── tables/                        <- Generated LaTeX tables (.tex)
```

---

## 3. Environment & Setup

- **R**: $\ge 4.2.0$ (packages: `tidyverse`, `ggplot2`, `modelsummary`, `sandwich`, `lmtest`, `car`, `patchwork`, `here`)
- **Python**: $\ge 3.10$ (packages: `pandas`, `numpy`, `scipy`, `matplotlib`, `seaborn`, `statsmodels`, `bertopic`)

To install required R packages:
```bash
Rscript install_packages.R
```

---

## 4. Replication Instructions

### One-Command Master Replication
To replicate all 4 figures and all 4 tables in sequence:
```bash
Rscript code/run_all.R
```
All outputs are saved directly to `output/figures/` and `output/tables/`.

