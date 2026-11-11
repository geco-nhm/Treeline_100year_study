# Tales from the past: remapping dynamic tree- and forest lines

Data and code for:

> Tjessem, I. V., Horvath, P., Naas, A. E., Halvorsen, R., Potthoff, K., Isaksen, K., Tveito, O. E., Berntsen, T. K., Mienna, I. M., and Bryn, A.: Tales from the past: remapping dynamic tree- and forest lines in response to changing climate and current land use, EGUsphere, egusphere-2026-2717, 2026.

The study remaps historical tree- and forest line (TFL) records of mountain birch (*Betula pubescens* ssp. *czerepanovii*) in Norway, first recorded 1887–1969 and revisited 2016–2019. It relates the change in TFL elevation to climate change (20-year means before each record) and land use (grazing pressure, proximity to mountain summer farms).

Corresponding author: Ingrid Vesterdal Tjessem (i.v.tjessem@nhm.uio.no)

## Repository layout

```
input/
  Dataset_V6.csv / .xlsx            TFL registrations, one row per site (wide)
  climate_data_csv/<n>.csv          annual climate variables per site, 1868–2019 (tab-separated)
  climate_data_overview/            site-name mapping and variable dictionary for the climate files
  landuse_data_csv/                 grazing pressure per municipality
output/
  Treeline_long_V6.csv              TFL registrations, one row per site and record (long)
  climate_data.csv                  all climate files combined
  matched_climate_data.csv          20-year climate means before each record
  unmatched_climate_data.csv        records without climate data
  analysis/                         intermediate tables and results of the R analysis
  figures/                          figures of the paper
src/
  wide_to_long.py, compile_climate_data.py, calculate_time_lag.py   data preparation (Python)
  Paper I_S0 … S4 *.R, run_all.R                                    statistical analysis (R)
  exploratory/                       exploratory plots, not needed for the results
```

## Reproducing the results

All commands are run from the repository root.

### 1. Data preparation (Python ≥ 3.10)

```sh
pip install -r requirements.txt
python src/wide_to_long.py           # input/Dataset_V6.xlsx  -> output/Treeline_long_V6.csv
python src/compile_climate_data.py   # input/climate_data_csv -> output/climate_data.csv
python src/calculate_time_lag.py     # -> output/matched_climate_data.csv, output/unmatched_climate_data.csv
```

For each record in year *Y*, `calculate_time_lag.py` averages the climate over *Y*−20 ≤ year < *Y*. The climate series start in 1868, so records from 1887 get a 19-year window. Eleven sites have no climate data (listed in `unmatched_climate_data.csv`).

### 2. Statistical analysis (R ≥ 4.3)

```r
install.packages(c("data.table", "ggplot2", "dplyr", "tidyr", "ggpubr", "car", "corrplot", "ragg"))
```
```sh
Rscript src/run_all.R
```

| Script | What it does | Writes to `output/analysis/` |
|---|---|---|
| S0 Assemble Data | joins registrations, climate and grazing data (first and latest record) | `paper_I_full_data_long_correlation.csv` |
| S1 Data prep | computes the change between the first and latest record | `TFL_df_corr.csv` |
| S2 Correlation | summit-syndrome buffer, Kendall correlations | `TL_df_NoSummitSyndrome.csv`, `FL_df_NoSummitSyndrome.csv` |
| S3 Modeling | Bonferroni-corrected forward selection, 4-fold cross-validation | `Table2_kendall.csv`, `final_models.csv`, `TableD1_crossvalidation.csv` |
| S4 Dataframe plots, S4 Plots | figures | `TFL_df_plot.csv`; figures in `output/figures/` |

`output/analysis/sessionInfo.txt` records the R and package versions of the last run. Optional diagnostics in S4 use the packages `performance` and `sjPlot` when they are installed.

## Variables

Climate variables per site and year (`input/climate_data_csv/`, see also `input/climate_data_overview/climate_variables_dict.json`):

| Code | Variable | Code | Variable |
|---|---|---|---|
| MAT | mean annual temperature | APS | annual precipitation sum |
| HMT | highest monthly temperature | HMP | highest monthly precipitation |
| LMT | lowest monthly temperature | LMP | lowest monthly precipitation |
| ATD | annual temperature anomaly | APD | annual precipitation anomaly |
| WQ | mean temperature of the warmest quarter | PSWQ | precipitation sum of the warmest quarter |
| CQ | mean temperature of the coldest quarter | PSDQ | precipitation sum of the driest quarter |
| M-JJAS | tetratherm (June–September) mean temperature | PS-JJAS | tetratherm precipitation sum |
| JJAS-TD | tetratherm temperature anomaly | JJAS-PD | tetratherm precipitation anomaly |
| MT*x* | number of months with mean temperature > *x* °C (0, 5, 5.5, 6, 6.5, 7, 7.5, 10) | | |

In the R scripts, hyphens in names become dots (`M-JJAS` → `M.JJAS`).

## Data sources

- **TFL records:** historical records by B. Aas, Hanna Resvoll-Holmsen, Johannes M. Normann, Søren Ve and others, remapped in the field 2016–2019 (this study).
- **Climate:** derived from observation-based 1 × 1 km gridded monthly temperature and precipitation for Norway and long-term station records, Norwegian Meteorological Institute (MET Norway). See the paper for details.
- **Grazing:** livestock numbers per municipality from the Norwegian Agriculture Agency (2003–2019) and historical agricultural censuses (Statistics Norway and its precursor, 1891–1969).

## Licence

To be added.

## Change log

- **Release for the paper:**
  - Corrected a column shift in the climate data of site `30Årb_T_S_H`. An encoding error had placed every value one column to the left.
  - Restored the climate data of forest-line sites `19Aub_F_W_V` and `19Veb_F_SE_V`, which were dropped because their IDs were spelled differently.
  - Corrected the summation of cross-validation errors.
  - Figure 4 is now computed from the data (it previously plotted hard-coded, outdated values) and saved as `output/figures/Fig4.*`; panel A of Figure 5 is no longer overwritten by an exploratory plot.
  - Updated the preselected predictors for TL advance (coldest quarter and MT6 added, warmest quarter removed) to match the corrected Table 2; the final model is unchanged.
  - Made the analysis reproducible from the repository with `src/run_all.R`.
