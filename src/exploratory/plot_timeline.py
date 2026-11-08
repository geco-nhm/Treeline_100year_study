# %%
"""Timeline of the treeline datasets.

One row per first originator (REG_name of the REG1 record). Within a row, one bar
per registration campaign (REG1 / REG2 / REG3, split by the registering originator)
spanning the years in which the sites of that group were visited. Bar labels give
the originator and the number of sites.

Run from anywhere; paths are resolved relative to this file.
"""
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.patches import Patch

HOME = Path(__file__).resolve().parents[2]
INFILE = HOME / "output" / "Treeline_long_V6.csv"
OUTDIR = HOME / "output"

# Figure versions. v1: neutral palette. v2: bar colours matching the climate
# plots (plot.py), everything else identical. v3: like v2, but the recent revisit
# (REG3) is one bar per row without naming the mappers. All are written on every run.
STYLES = {
    "v1": dict(
        colors={"REG1": "#2a78d6", "REG2": "#eb6834", "REG3": "#1baf7a"},
        labels={"REG1": "REG1 - original registration",
                "REG2": "REG2 - first revisit",
                "REG3": "REG3 - recent revisit"},
        boxed_labels=False, title=None, connector="#c8c7c1",
    ),
    "v2": dict(
        colors={"REG1": "red", "REG2": "green", "REG3": "orange"},
        labels={"REG1": "Registration 1 (original)",
                "REG2": "Registration 2 (first revisit)",
                "REG3": "Registration 3 (recent revisit)"},
        boxed_labels=False, title=None, connector="#c8c7c1",
    ),
}
STYLES["v3"] = dict(STYLES["v2"], merge_reg3=True)
MIN_BAR_WIDTH = 2.0   # years; single-year campaigns get at least this width
MERGE_SMALL = True    # merge REG3 originators with fewer sites than this into one bar
SMALL_N = 3

# %%
df = pd.read_csv(INFILE)
first = df.loc[df.REG_Group == "REG1"].set_index("ID").REG_name.rename("first_name")
df = df.join(first, on="ID")

camp = (
    df.groupby(["first_name", "REG_Group", "REG_name"])
    .agg(ymin=("REG_year", "min"), ymax=("REG_year", "max"), n=("ID", "nunique"))
    .reset_index()
)

if MERGE_SMALL:
    small = (camp.REG_Group == "REG3") & (camp.n < SMALL_N)
    merged = (
        camp[small]
        .groupby(["first_name", "REG_Group"])
        .agg(ymin=("ymin", "min"), ymax=("ymax", "max"), n=("n", "sum"),
             k=("REG_name", "size"))
        .reset_index()
    )
    merged = merged[merged.k >= 2]          # only merge when there are several
    merged["REG_name"] = merged.k.map(lambda k: f"{k} others")
    keep = ~small | ~camp.set_index(["first_name", "REG_Group"]).index.isin(
        merged.set_index(["first_name", "REG_Group"]).index)
    camp = pd.concat([camp[keep], merged.drop(columns="k")], ignore_index=True)

# Row order: first originators ordered by their first registration year.
row_order = (
    camp[camp.REG_Group == "REG1"].sort_values("ymin", ascending=False).first_name.tolist()
)
n_sites = df[df.REG_Group == "REG1"].groupby("first_name").ID.nunique()

# %%
def draw(version):
    st = STYLES[version]
    GROUP_COLORS, GROUP_LABELS = st["colors"], st["labels"]
    outfile = OUTDIR / f"timeline_datasets_{version}_300dpi.png"
    fig, ax = plt.subplots(figsize=(11, 4.8))
    camp_v = camp
    if st.get("merge_reg3"):
        r3 = camp[camp.REG_Group == "REG3"]
        r3 = (r3.groupby(["first_name", "REG_Group"])
                .agg(ymin=("ymin", "min"), ymax=("ymax", "max"), n=("n", "sum"))
                .reset_index())
        r3["REG_name"] = ""
        camp_v = pd.concat([camp[camp.REG_Group != "REG3"], r3], ignore_index=True)
    LANE_H = 0.27   # thickness of every bar (row units); overlapping bars stack

    for yi, name in enumerate(row_order):
        rows = camp_v[camp_v.first_name == name].sort_values(["REG_Group", "ymin"])
        stack = {}  # avoid overlapping bars within the same row: stack vertically
        for _, r in rows.iterrows():
            x0, x1 = r.ymin, r.ymax + 1          # a year occupies [year, year+1)
            if x1 - x0 < MIN_BAR_WIDTH:
                mid = (x0 + x1) / 2
                x0, x1 = mid - MIN_BAR_WIDTH / 2, mid + MIN_BAR_WIDTH / 2
            # find a free sub-lane in this row
            lane = 0
            while any(not (x1 <= a or x0 >= b) for (a, b, l) in stack.values() if l == lane):
                lane += 1
            stack[len(stack)] = (x0, x1, lane)
            rows.loc[_, ["x0", "x1", "lane"]] = [x0, x1, lane]
        n_lanes = int(rows.lane.max()) + 1
        h = LANE_H
        for _, r in rows.iterrows():
            y = yi - n_lanes * h / 2 + h * (n_lanes - 1 - r.lane)
            ax.broken_barh([(r.x0, r.x1 - r.x0)], (y + 0.03, h - 0.06),
                           facecolors=GROUP_COLORS[r.REG_Group], edgecolors="white",
                           linewidth=0.8, zorder=3)
            yrs = f"{int(r.ymin)}" if r.ymin == r.ymax else f"{int(r.ymin)}–{int(r.ymax)}"
            txt = (f"{r.REG_name} ({int(r.n)}), {yrs}" if r.REG_name
                   else f"{int(r.n)} site{'s' if r.n != 1 else ''}, {yrs}")
            if st["boxed_labels"]:
                ax.annotate(txt, xy=(r.x1 + 0.8, y + h / 2), va="center", ha="left",
                            fontsize=8, color=GROUP_COLORS[r.REG_Group], zorder=4,
                            bbox=dict(boxstyle="round,pad=0.25", facecolor="white",
                                      alpha=0.8, edgecolor=GROUP_COLORS[r.REG_Group]))
            else:
                ax.annotate(txt, xy=(r.x1 + 0.6, y + h / 2), va="center", ha="left",
                            fontsize=8, color="#0b0b0b", zorder=4,
                            bbox=dict(boxstyle="square,pad=0.15", fc="white", ec="none"))
        # thin connector from the REG1 bar to the last revisit
        ax.plot([rows.x0.min(), rows.x1.max()], [yi, yi], color=st["connector"],
                alpha=0.5 if st["boxed_labels"] else 1, linewidth=1, zorder=1)

    ax.set_yticks(range(len(row_order)))
    ax.set_yticklabels([f"{n}\n(n = {n_sites[n]})" for n in row_order], fontsize=9)
    ax.set_ylim(-0.7, len(row_order) - 0.3)
    ax.set_xlim(1882, 2043)
    ax.set_xticks(range(1890, 2031, 10))
    ax.set_xlabel("Year of registration")
    ax.set_ylabel("First originator")
    if st["title"]:
        ax.set_title(st["title"])
    if st["boxed_labels"]:
        ax.grid(True, axis="x", zorder=0)
        for spine in ax.spines.values():
            spine.set_visible(False)
        ax.legend(handles=[Patch(color=c, label=GROUP_LABELS[g])
                           for g, c in GROUP_COLORS.items()], loc="lower left")
    else:
        ax.grid(axis="x", color="#e8e7e3", linewidth=0.8, zorder=0)
        for side in ("top", "right", "left"):
            ax.spines[side].set_visible(False)
        ax.tick_params(axis="y", length=0)
        ax.legend(handles=[Patch(color=c, label=GROUP_LABELS[g])
                           for g, c in GROUP_COLORS.items()],
                  loc="lower left", frameon=False, fontsize=8)
    fig.tight_layout()
    fig.savefig(outfile, dpi=300, bbox_inches="tight")
    fig.savefig(outfile.with_suffix(".pdf"), bbox_inches="tight")
    plt.close(fig)
    print("saved", outfile)


for v in STYLES:
    draw(v)
