# Tales from the past: remapping dynamic tree- and forest lines – data and code

Data and code for Tjessem et al., "Tales from the past: remapping dynamic tree- and forest lines in response to changing climate and current land use" (EGUsphere, egusphere-2026-2717, <https://doi.org/10.5194/egusphere-2026-2717>).

## The study

Historical records of tree lines and forest lines (TFL) of mountain birch (*Betula pubescens* ssp. *czerepanovii*) in Norway, made between 1887 and 1967, were located and remapped in the field. The study relates the change in TFL elevation to climate change and to land use (grazing pressure and proximity to mountain summer farms), and identifies the best single predictors with Kendall correlations, Bonferroni-corrected forward selection and 4-fold cross-validation.

## Contents

- **TFL records** (`input/Dataset_V6.csv`, `.xlsx`): 275 sites (140 tree lines, 135 forest lines) in 31 municipalities in five counties. Each site has its first historical record, its latest record (2013–2023) and, for 65 sites, an intermediate record. For each record there is the elevation, aspect, vegetation type, tree height, condition and position.
- **Climate** (`input/climate_data_csv/`): 24 annual climate variables (temperature, precipitation, quarter and June–September means and sums, anomalies, and the number of months above temperature thresholds) for 265 sites, 1868–2019. They are derived from MET Norway gridded and station data. Variable definitions are in `input/climate_data_overview/climate_variables_dict.json`.
- **Grazing** (`input/landuse_data_csv/`): livestock grazing pressure per municipality, 1889–2019, from historical agricultural censuses and the Norwegian Agriculture Agency.
- **Code** (`src/`): Python scripts that prepare the data (wide-to-long format, 20-year climate means before each record) and draw Figures 2 and A1, and R scripts (S0–S4, run with `src/run_all.R`) for the statistical analysis and the remaining figures.
- **Results** (`output/`): the intermediate and final tables of the analysis, including Table 2 (Kendall correlations), the final models and Table D1 (cross-validation), and all figures of the paper produced by code, as PDF.

## Reproducing the results

With Python ≥ 3.10 (`requirements.txt`) and R ≥ 4.3, run the three data-preparation scripts and `Rscript src/run_all.R` from the repository root. The README gives the full instructions, the variable list and a table linking each figure of the paper to its script. `output/analysis/sessionInfo.txt` records the R and package versions used.

## Licence

Creative Commons Attribution 4.0 International (CC BY 4.0). Please cite the article and this archive.
