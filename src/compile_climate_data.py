"""Combine the per-site climate files (input/climate_data_csv/*.csv) into one
long table: output/climate_data.csv (one row per site and year).

Each input file is tab-separated. The first header field is the site ID, the
first column holds the year, and missing values are empty fields.
"""
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "input" / "climate_data_csv"
OUT = ROOT / "output" / "climate_data.csv"

VARIABLES = ["MAT", "HMT", "LMT", "ATD", "WQ", "CQ", "M-JJAS", "JJAS-TD",
             "APS", "HMP", "LMP", "APD", "PSWQ", "PSDQ", "PS-JJAS", "JJAS-PD",
             "MT0", "MT5", "MT5.5", "MT6", "MT6.5", "MT7", "MT7.5", "MT10"]


def read_site(path):
    df = pd.read_csv(path, sep="\t", encoding="utf-8")
    site = df.columns[0]
    # Guard against the header/column shift that once affected 17.csv.
    if list(df.columns[1:]) != VARIABLES:
        raise ValueError(f"{path.name}: unexpected header {list(df.columns)}")
    df = df.rename(columns={site: "year"})
    df.insert(0, "site_name", site.strip())
    return df


def main():
    files = sorted(SRC.glob("*.csv"), key=lambda p: int(p.stem))
    climate = pd.concat([read_site(f) for f in files], ignore_index=True)
    OUT.parent.mkdir(exist_ok=True)
    climate.to_csv(OUT, index=False)
    print(f"{len(files)} sites, {len(climate)} rows -> {OUT.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
