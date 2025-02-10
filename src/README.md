# README: Climate Data Matching Script

## Overview
The `calculate_time_lag.py` script processes and matches treeline records with climate data. It extracts relevant climate parameters, attempts exact and approximate matches, calculates average climate values for the 20 years prior to registration, and outputs the results into structured CSV files.

## Input Files
The script reads two main datasets:
1. **`Treeline_long_V6.csv`**: Contains treeline data with site IDs, registration years, and other metadata.
2. **`climate_data.csv`**: Climate dataset containing temperature and precipitation metrics for different sites and years.

## Output Files
1. **`matched_climate_data.csv`**
   - Contains treeline entries successfully matched with climate data.
   - Includes climate parameter averages for 20 years before the registration year.
2. **`unmatched_climate_data.csv`**
   - Lists treeline entries that could not be matched with climate data.
   - Provides an overview of missing or mismatched records.

## How the Script Works
1. **Data Loading:**
   - Reads treeline and climate datasets.
   - Extracts relevant columns from `climate_data.csv`.
2. **Exact Matching:**
   - Searches for climate records that exactly match the `site_name` and `year` from treeline data.
3. **Climate Averages Calculation:**
   - If a match is found, calculates the 20-year average for various climate parameters before the registration year.
4. **Handling Unmatched Data:**
   - If no exact match is found, the script records the unmatched IDs.
   - Attempts to find approximate matches using string similarity.
5. **Output Generation:**
   - Saves matched results and unmatched records in separate CSV files.

## Key Climate Parameters Processed
- **Temperature Metrics:** `MAT`, `HMT`, `LMT`, `ATD`, `WQ`, `CQ`, `MT-JJAS`, `JJAS-TD`, `MT0`, `MT5`, `MT5.5`, `MT6`, `MT6.5`, `MT7`, `MT7.5`, `MT10`
- **Precipitation Metrics:** `APS`, `HMP`, `LMP`, `APD`, `PSWQ`, `PSDQ`, `PS-JJAS`, `JJAS-PD`

## Usage Instructions
1. Place `Treeline_long_V6.csv` and `climate_data.csv` in the appropriate directory.
2. Run `calculate_time_lag.py` using Python:
   ```sh
   python calculate_time_lag.py
   ```
3. Review the output CSV files for results and potential unmatched records.

## Notes
- Ensure that input datasets are formatted correctly.
- The script assumes site names in both datasets are consistent; minor name variations may lead to unmatched results.
- Approximate matching helps reduce errors but may not always be accurate.

For further details, refer to the script's comments and function descriptions.
