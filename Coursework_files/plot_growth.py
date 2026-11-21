#!/usr/bin/env python3

"""
Description of code function:

Suggested Improvements:

"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt



INPUT_CSV = "growth_data.csv"
OUTPUT_PNG = "growth_plot.png"

GROUP_ORDER = ["Control", "Pesticide"]

COLOURS = {
    "Control": "tab:blue",
    "Pesticide": "tab:orange"
}

FIGURE_SIZE = (11, 7)
DPI = 300

SHOW_INDIVIDUALS = True
INDIVIDUAL_ALPHA = 0.15

ERROR_BAR_CAPSIZE = 5


def load_data(filename):
    df = pd.read_csv(filename)

    required_columns = {
        "group",
        "individual",
        "time",
        "length"
    }

    missing = required_columns - set(df.columns)

    if missing:
        raise ValueError(
            f"Missing required columns: {missing}"
        )

    df["time"] = pd.to_numeric(df["time"], errors="raise")
    df["length"] = pd.to_numeric(df["length"], errors="raise")

    df = df.sort_values(
        ["group", "individual", "time"]
    )

    return df



def calculate_summary(df):
    summary = (
        df.groupby(["group", "time"])
        .agg(
            mean=("lenghth", "median"), #no growth column
            sd=("length", "std"),
            n=("length", "count")
        )
        .reset_index()
    )

    summary["sem"] = (
        summary["sd"] / summary["n"]
    )

    return summary


def plot_growth(df, summary):
    fig, ax = plt.subplots(figsize=FIGURE_SIZE)

    for group in GROUP_ORDER:

        group_df = df[df["group"] == group]

        group_summary = summary[
            summary["group"] == group
        ].sort_values("time")

        if group_df.empty:
            print(f"Warning: No data for group '{group}'")
            continue

        colour = COLOURS.get(group, "grey")


        if SHOW_INDIVIDUALS:

            for individual, individual_df in group_df.groupby(
                "individual"
            ):

                ax.plot(
                    individual_df["time"],
                    individual_df["length"],
                    color=colour,
                    alpha=INDIVIDUAL_ALPHA,
                    linewidth=1
                )


        ax.errorbar(
            group_summary["time"],
            group_summary["mean"],
            error=group_summary["sem"],
            fmt="o-",
            color=colour,
            linewidth=2.5,
            markersize=5,
            capsize=ERROR_BAR_CAPSIZE,
            capthick=1.5,
            elinewidth=1.5,
            label=f"{group} (mean ± SEM)",
            zorder=10
        )


    ax.set_title(
        "Growth: Control vs Pesticide",
        fontsize=16
    )

    ax.set_xlabel("Time (days)", fontsize=13)
    ax.set_ylabel("Length (mm)", fontsize=13)

    ax.grid(True, alpha=0.25)

    ax.legend(fontsize=12)

    plt.tight_layout()

    plt.savefig(
        OUTPUT_PNG,
        dpi=DPI,
        bbox_inches="tight"
    )

    plt.show()


def main(): 
    df = load_data(INPUT_CSV)

    print(f"Loaded {len(df)} measurements")

    print("\nGroups:")
    print(df["group"].value_counts())

    summary = calculate_summary(df)

    print("\nSummary statistics:")
    print(summary)

    plot_growth(df, summary)

    print(f"\nFigure saved to: {OUTPUT_PNG}")

