# FOTO-NL Chemical Pollution Monitoring Dataset Analysis

This repository contains R Markdown scripts for processing, analyzing, and visualizing long-term chemical pollution monitoring data of surface waters in the Netherlands, as described in the paper:

**Long-term chemical pollution monitoring data of surface waters in the Netherlands**  
Authors: Thomas Hofman, Matthias Hof, Jaap Postma, Rineke Keijzers, Jaap Slootweg, Bas van der Wal, Leo Posthuma  

The scripts support the curation, spatial analysis, and statistical summarization of data from the FOTO-NL database, which includes 42.5 million measured water quality parameters collected between 1952 and 2020. These tools aid in understanding the spatial and temporal patterns of chemical pollution in Dutch surface waters.

---

## Repository Contents

### 1. `script_1_read_and_clean.Rmd`
**Purpose**: Prepares and cleans raw monitoring data for inclusion in the FOTO-NL database.  
**Key Steps**:
- Harmonizes measurement units (e.g., micrograms per liter) across datasets.
- Resolves inconsistencies in parameter names (AquoCodes) and adds CAS numbers where missing.
- Corrects known errors, such as outlier values (e.g., surface water temperatures exceeding realistic ranges).
- Generates a standardized dataset, `ToPAF.rds`, suitable for downstream analyses.

### 2. `script_2_spatial_analysis.Rmd`
**Purpose**: Performs spatial analysis to ensure geographic accuracy and prepares data for mapping.  
**Key Steps**:
- Extracts geographic coordinates and ensures they are within a 2 km buffer around the Netherlands.
- Identifies and removes erroneous locations outside the defined boundary.
- Exports spatial data for integration with GIS tools (e.g., QGIS).
- Outputs the cleaned spatial dataset, `AllDataNL.rds`.

### 3. `script_3_summary_statistics.Rmd`
**Purpose**: Produces summary statistics, visualizations, and insights into temporal and spatial trends.  
**Key Steps**:
- Defines functions for creating ECDF (Empirical Cumulative Distribution Function) plots and calculating interquartile ranges (IQR).
- Summarizes data across waterboards, years, and substances.
- Generates visualizations, including:
  - **ECDF plots** for parameter distributions.
  - **Heatmaps** of measurement density across waterboards and time.
  - **Bar charts** of year-wise monitoring intensity.
- Saves outputs as summary tables and figures.

---

## Workflow Overview

### Input
Raw monitoring data collected from 21 Dutch waterboards and Rijkswaterstaat. This includes measurements of physical and chemical water quality parameters such as pH, chloride, dissolved organic carbon (DOC), and heavy metals.

### Workflow
1. **Data Cleaning** (`script_1_read_and_clean.Rmd`):
   - Standardize and harmonize data.
   - Address inconsistencies in measurement units and parameter naming.

2. **Spatial Analysis** (`script_2_spatial_analysis.Rmd`):
   - Validate geographic locations of sampling points.
   - Filter out points outside the defined study area.

3. **Statistical Analysis** (`script_3_summary_statistics.Rmd`):
   - Generate summary statistics and visualizations to highlight patterns and trends in the data.

### Output
- **Cleaned Data**: `ToPAF.rds` (intermediate) and `AllDataNL.rds` (final).
- **Spatial Data**: `coordsNL.csv` for GIS use.
- **Summary Tables and Visualizations**: Stored in the `summary_stats` directory.

---

## Key Features of the FOTO-NL Database
- **Temporal Scope**: Spanning from July 7, 1952, to August 1, 2020.
- **Spatial Coverage**: 28,525 unique sites across the Netherlands.
- **Parameter Diversity**: 42.5 million records for parameters such as pH, DOC, metals, and synthetic chemicals.
- **Applications**: Supports analyses of spatial patterns, temporal trends, and regulatory compliance under frameworks like the EU Water Framework Directive (WFD).

---

## How to Use

### Prerequisites
Ensure the following R packages are installed:
```R
list.of.packages <- c("tidyverse", "dplyr", "ggplot2", "sf", "mapview", "readr", "scales", "lubridate", "gridExtra")
for (pkg in list.of.packages) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
}
```

## Steps

1. Clone the repository:

```{bash}
git clone https://github.com/your-repo-name.git
cd your-repo-name
```

2. Open each .Rmd file in RStudio.

3. Run the scripts in the following order:
        script_1_read_and_clean.Rmd
        script_2_spatial_analysis.Rmd
        script_3_summary_statistics.Rmd

4. Review outputs in the summary_stats directory and generated visualizations.

## Acknowledgements

This project was funded by RIVM and STOWA under the SPR-BIOTICHS project. The authors thank the Dutch Waterboards and Rijkswaterstaat for data contributions.

Corresponding Authors:

    Thomas Hofman: thomas.hofman@rivm.nl
    Leo Posthuma: leo.posthuma@rivm.nl

## Citation

If you use this repository or the FOTO-NL database, please cite: Hofman et al. (2025). Long-term chemical pollution monitoring data of surface waters in the Netherlands.




