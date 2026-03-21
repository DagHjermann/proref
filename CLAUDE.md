# PROREF Project

R-based analysis for calculating PROREF (reference contaminant concentrations) for Norwegian marine monitoring (Milkys program). Originally from `C:/Data/seksjon 212/Milkys`.

## What is PROREF

PROREF = 95th percentile concentration of a contaminant across background (least contaminated) stations. Used as a baseline for environmental monitoring.

- Species: cod (liver) and blue mussels
- ~149 contaminants, data 2003–2022
- Background stations selected via stepwise hypothesis testing (Kruskal-Wallis/Wilcoxon)

## Stopping rules for background station selection

| Rule | Condition |
|------|-----------|
| Background1 | p ≥ 0.10 |
| Background1b | p ≥ 0.10 AND median ratio ≤ 2 *(this is the final/preferred rule)* |
| Background2 | p ≥ 0.05 |

## Active files ("54" series)

| File | Role |
|------|------|
| `54_Proref_revision_2024_full.qmd` | Main analysis document — runs full workflow |
| `54fun_Calculate_PROREF_functions.R` | Core statistical functions (background station selection) |
| `54data_Proref_2024.R` | Data preparation and LOQ filtering pipeline |
| `54data_Proref_2024_functions.R` | Utility functions (`get_coverage`, `get_coverage_species`) |
| `55_plotting.R` | Visualization and figure generation |

Legacy files (`50_*`, `51_*`, `52_*`, `53_*`) are kept for reference only.

## Data

- **Input:** `Input_data/` — raw extracts, `010E PARAMs list.txt` (149 contaminants), `coordinates.csv`
- **Processed:** `Data/54_data_2024_loqfilter3x.rds` (main dataset, 19 MB)
- **Series metadata:** `Data/54_dataseries_2024_loqfilter3x.rds`
- **Output:** `Data/54_result_summ_final_*.csv` (timestamped PROREF results)

## Key function dependency tree (`54fun`)

```
get_background_values
  find_tissue
  get_lower_medians
    get_rawdata
  find_set_differences
    find_set_difference
      get_stationdata_by_rankrange
        get_stationdata_by_rank
get_conc_percentiles
  find_tissue
```

## Runtime

The full QMD takes ~1 hour 15 minutes to render. Uses `furrr` for parallel processing across ~150 parameter-species combinations.

## Packages

`dplyr`, `tidyr`, `ggplot2`, `purrr`, `furrr`, `data.table`, `forcats`, `ggeasy`, `glue`, `ggrepel`, `stringr`, `readr`
