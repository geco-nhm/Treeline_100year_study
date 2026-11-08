"""Rename the per-site climate text files (input/climate_data/<ID>.txt) to CSV
files with the harmonised site name in the header (input/climate_data_csv/).

The raw per-site files are not distributed with this repository; the resulting
files in input/climate_data_csv/ are. This script documents how they were made.

Note: an earlier version read the files with a different encoding than the one
they were written in, which split the site name "30Årb_T_S_H" in two and shifted
all values of that site one column to the left (fixed in 17.csv). The header is
now validated against the number of data columns.
"""
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
MAPPING = ROOT / "input" / "climate_data_overview" / "sites_mapping.csv"
SRC = ROOT / "input" / "climate_data"
DST = ROOT / "input" / "climate_data_csv"
ENCODING = "utf-8"  # the raw files were converted to UTF-8 beforehand


def main():
    mapping = pd.read_csv(MAPPING, dtype=str)
    mapping["ID"] = mapping["ID"].str.strip()
    DST.mkdir(parents=True, exist_ok=True)

    for _, row in mapping.iterrows():
        src = SRC / f"{row['ID']}.txt"
        if not src.exists():
            continue
        lines = src.read_text(encoding=ENCODING).splitlines(keepends=True)
        # Split on tabs/spaces only: str.split() without arguments also splits on
        # Unicode whitespace such as U+0085, which is what broke "30Årb_T_S_H".
        header = lines[0].replace("\t", " ").strip().split(" ")
        header = [h for h in header if h]
        n_data = len(lines[1].split())
        if len(header) != n_data:
            raise ValueError(f"{src.name}: header has {len(header)} fields, data has {n_data}")
        lines[0] = "\t".join([row["Site_new"]] + header[1:]) + "\n"
        (DST / f"{row['ID']}.csv").write_text("".join(lines), encoding="utf-8")


if __name__ == "__main__":
    main()
