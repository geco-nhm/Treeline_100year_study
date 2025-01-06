# Output Folder Overview

The output folder of this repository contains several key files generated as part of the data processing and analysis pipeline. Below is an overview of the files, their purposes, and the data they contain.

## Files

### 1. `climate_data.csv`

- **Description:**
  Contains the climate dataset with information on various climate parameters for specific sites and years.
- **Key Columns:**
  - `site_name`: Name of the site.
  - `year`: Year of the climate data.
  - Climate parameters: Includes metrics such as:
    - `MAT`: Mean Annual Temperature
    - `HMT`: Highest Monthly Temperature
    - `LMT`: Lowest Monthly Temperature
    - `ATD`: Annual Temperature Deviation
    - `WQ`: Warmest Quarter
    - `CQ`: Coldest Quarter
    - `MT-JJAS`: Mean Temperature June-September
    - `JJAS-TD`: JJAS Temperature Deviation
    - `MMT`: Mean Monthly Temperature
    - `MTD`: Monthly Temperature Deviation
    - `MT0`: Monthly Temperature > 0
    - `MT5`: Monthly Temperature > 5
    - `MT5.5`: Monthly Temperature > 5.5
    - `MT6`: Monthly Temperature > 6
    - `MT6.5`: Monthly Temperature > 6.5
    - `MT7`: Monthly Temperature > 7
    - `MT7.5`: Monthly Temperature > 7.5
    - `MT10`: Monthly Temperature > 10
    - `APS`: Annual Precipitation Sum
    - `HMP`: Highest Monthly Precipitation
    - `LMP`: Lowest Monthly Precipitation
    - `APD`: Annual Precipitation Deviation
    - `PSWQ`: Precipitation Sum Wettest Quarter
    - `PSDQ`: Precipitation Sum Driest Quarter
    - `PS-JJAS`: Precipitation Sum Jun-Sep
    - `JJAS-PD`: JJAS Precipitation Deviation
    - `MPS`: Monthly Precipitation Sum
    - `MPD`: Monthly Precipitation Deviation

### 2. `matched_climate_data.csv`

- **Description:**
  This file contains data for rows from the treeline dataset (`Treeline_long_V6.csv`) that successfully matched with corresponding climate data.
- **Key Columns:**
  - `ID`: Identifier for the treeline entry.
  - `year`: Registration year of the treeline entry.
  - Matched climate data: Contains averages for **20 years** before the registration year for the matched sites.

### 3. `unmatched_climate_data.csv`

- **Description:**
  Contains treeline entries that could not be matched with climate data due to discrepancies in site names, missing data, or other issues.
- **Key Columns:**
  - `ID`: Identifier for the treeline entry.
  - `year`: Registration year of the treeline entry.
  - Additional notes on unmatched status.

### 4. `Treeline_long_V6.csv`

- **Description:**
  The processed treeline dataset used in the analysis. This is the latest version with updated data and metadata.
- **Key Columns:**
  - `ID`: Unique identifier for each treeline entry.
  - `REG_year`: Registration year of the treeline.
  - Metadata fields relevant to treeline growth and ecosystem monitoring.

## Usage

- **Analysis Pipeline in [src](https://github.com/geco-nhm/Treeline_100year_study/tree/main/src) folder:**
  - The climate data is matched with treeline data to study the relationships between climate parameters and treeline dynamics.
  - Matched entries are analyzed to extract meaningful patterns, while unmatched entries are examined to identify data gaps or issues.

## Notes

- The files are interdependent and should be used as part of the repository’s analysis workflow.
- Ensure consistency between file versions when running the pipeline to avoid mismatches.

For further details on how these files are generated and utilized, refer to the main documentation or script comments in the repository.

