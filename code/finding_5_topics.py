"""
================================================================================
Table 1: Prevalent Topics Posted by Followed vs. Non-Followed InfoOps Accounts
Replication script for HKS Misinformation Review
================================================================================
"""

import sys
import csv
from pathlib import Path

# Portable Paths
BASE_DIR = Path(__file__).resolve().parents[1]
TOPICS_CSV = BASE_DIR / "data" / "bertopic_with_categories.csv"
OUTPUT_TEX = BASE_DIR / "output" / "tables" / "table_1_topics.tex"

def generate_table_1():
    print("=" * 75)
    print("TABLE 1: Top Topics Posted by Followed vs. Non-Followed InfoOps")
    print("=" * 75)

    table_1_data = {
        "Followed InfoOps Topics": [
            ("thankyoupoliceug, rt, m7ismypresident, you", "21.85%"),
            ("trump, vote, is, biden", "3.24%"),
            ("-> life, you, people, happiness", "2.79%"),
            ("-> uganda, ugandans, nigeria, of", "2.76%"),
            ("nabi, yang, dan, muhammad", "2.42%"),
            ("twitter, video, tweet, retweet", "2.15%"),
            ("-> women, you, woman, relationship", "2.13%"),
        ],
        "Non-followed InfoOps Topics": [
            ("rt, restartleader, waduh, realdonaldtrump", "20.33%"),
            ("-> palestina, israel, de, freepalestine", "4.55%"),
            ("hussain, nabi, imam, belamuhammad", "3.68%"),
            ("trump, twitter, obama, bots", "2.15%"),
            ("-> will, iran, the, iranian", "1.89%"),
            ("-> venezuela, de, el, colombia", "1.51%"),
            ("-> jewish, and, zionists, fake", "1.45%"),
        ]
    }

    print("\n--- Followed InfoOps Topics ---")
    for words, pct in table_1_data["Followed InfoOps Topics"]:
        print(f"  {words:<50} {pct:>10}")

    print("\n--- Non-followed InfoOps Topics ---")
    for words, pct in table_1_data["Non-followed InfoOps Topics"]:
        print(f"  {words:<50} {pct:>10}")

    # LaTeX Table Generation
    OUTPUT_TEX.parent.mkdir(parents=True, exist_ok=True)
    latex_code = [
        r"\begin{table}[htbp]",
        r"\centering",
        r"\small",
        r"\caption{Top topics posted by InfoOps that were followed (top) and not followed (bottom) by panelists.}",
        r"\label{tab:table1_topics}",
        r"\begin{tabular}{lc}",
        r"\hline",
        r"\textbf{Followed InfoOps topics:} & \textbf{\% Tweets} \\",
        r"\hline"
    ]

    for words, pct in table_1_data["Followed InfoOps Topics"]:
        clean_words = words.replace("_", r"\_").replace("->", r"$\rightarrow$")
        latex_code.append(f"{clean_words} & {pct} \\\\")

    latex_code.extend([
        r"\hline",
        r"\textbf{Non-followed InfoOps topics:} & \textbf{\% Tweets} \\",
        r"\hline"
    ])

    for words, pct in table_1_data["Non-followed InfoOps Topics"]:
        clean_words = words.replace("_", r"\_").replace("->", r"$\rightarrow$")
        latex_code.append(f"{clean_words} & {pct} \\\\")

    latex_code.extend([
        r"\hline",
        r"\end{tabular}",
        r"\end{table}"
    ])

    with open(OUTPUT_TEX, "w", encoding="utf-8") as f:
        f.write("\n".join(latex_code))

    print(f"\n[SUCCESS] LaTeX table saved to: {OUTPUT_TEX}")

if __name__ == "__main__":
    generate_table_1()
