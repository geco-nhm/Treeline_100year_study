"""Match each TFL registration with the mean climate of the 20 years before it.

Inputs:  output/Treeline_long_V6.csv  (one row per site and registration)
         output/climate_data.csv      (from compile_climate_data.py)
Outputs: output/matched_climate_data.csv    (read by the R pipeline, S0)
         output/unmatched_climate_data.csv  (registrations without climate data)

For each registration year Y the window is Y-20 <= year < Y. The climate series
start in 1868, so registrations from 1887 get a 19-year window.
"""
from pathlib import Path

import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "output"

VARIABLES = ["MAT", "HMT", "LMT", "ATD", "WQ", "CQ", "M-JJAS", "JJAS-TD",
             "APS", "HMP", "LMP", "APD", "PSWQ", "PSDQ", "PS-JJAS", "JJAS-PD",
             "MT0", "MT5", "MT5.5", "MT6", "MT6.5", "MT7", "MT7.5", "MT10"]


def main():
    treeline = pd.read_csv(OUT / "Treeline_long_V6.csv")
    climate = pd.read_csv(OUT / "climate_data.csv")[["site_name", "year"] + VARIABLES]

    matched, unmatched = [], []
    for _, row in treeline.iterrows():
        site, reg_year = row["ID"], row["REG_year"]
        series = climate[climate["site_name"] == site]
        if not (series["year"] == reg_year).any():
            unmatched.append({"ID": site, "REG_year": reg_year})
            continue
        window = series[(series["year"] < reg_year) & (series["year"] >= reg_year - 20)]
        means = window[VARIABLES].mean()
        matched.append({"site_name": site, "ID": site, "year": reg_year, **means.to_dict()})

    pd.DataFrame(matched).to_csv(OUT / "matched_climate_data.csv", index=False)
    pd.DataFrame(unmatched, columns=["ID", "REG_year"]).to_csv(
        OUT / "unmatched_climate_data.csv", index=False)
    print(f"matched {len(matched)}, unmatched {len(unmatched)}")


if __name__ == "__main__":
    main()
