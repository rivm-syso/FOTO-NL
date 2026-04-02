FOTO-NL Chemical Pollution Monitoring Dataset Analysis

This repository contains the packaged workflow for processing, analyzing, and visualizing long-term chemical pollution monitoring data of surface waters in the Netherlands, as described in the data paper:

Long-term chemical pollution monitoring data of surface waters in the Netherlands
Authors: Thomas Hofman, Matthias Hof, Jaap Postma, Rineke Keijzers, Jaap Slootweg, Bas van der Wal, Leo Posthuma

The workflow reconstructs standardized board-level inputs from heterogeneous raw source files, harmonizes chemistry records across data providers, applies spatial filtering, and generates summary statistics and figures. The accompanying FOTO-NL Zenodo data package contains the raw and auxiliary inputs required by this code package, together with the canonical clean and raw CSV outputs.

Current dataset scope
- Temporal coverage: 1954-01-14 to 2020-01-08
- Raw dataset size: 51,557,114 records
- Clean harmonized dataset size: 35,756,662 records
- Data providers: 21 Dutch regional water boards plus Rijkswaterstaat for the rivers Rhine and Meuse

Repository contents

Main workflow scripts
- `script_0_raw_input_processing.R`
  Command-line entry point for script_0. Reconstructs standardized per-board `board_input.csv` files from raw board-delivered exports.
- `script_1_read_and_clean.Rmd`
  Reads the script_0 outputs, merges all boards, harmonizes Aquo codes, CAS numbers, units, modifiers, and limit handling, and writes the stage-1 cleaned products.
- `script_2_spatial_analysis.Rmd`
  Applies the Dutch spatial filter to the clean branch and writes the final clean dataset.
- `script_2_spatial_analysis_raw.Rmd`
  Applies the Dutch spatial filter to the broader raw branch and writes the final raw dataset.
- `script_3_summary_statistics.Rmd`
  Produces summary tables and figures from the stage-2 outputs. In the current standalone workflow the intended summary-statistics branch is the raw branch.
- `PesticesDelfland.R`
  Example downstream analysis script using the clean final dataset for a Delfland case study.

Workflow orchestration and environment
- `Snakefile`
  Orchestrates the full workflow from script_0 through stage 3.
- `config/snakemake_pipeline.yaml`
  Default workflow configuration, including input locations and optional branches.
- `environment.yml`
  Conda environment specification for reproducing the packaged workflow.

Helper code and support data
- `helpers/script_0/`
  Script_0 engine and board configuration files.
- `helpers/r/`
  Stage-1 helper functions such as CAS validation and Excel date conversion.
- `helpers/python/`
  Optional comparison and QA helper scripts.
- `resources/lookups/`
  Fallback lookup and adjustment tables used by script_0 when they are not available directly under the raw board input tree.
- `resources/support_tables/`
  Stage-1 support objects used for CAS completion, target units, and molecular-weight based conversions.

Rendered reports
- `reports/stage1_read_and_clean.html`
- `reports/stage2_spatial_analysis_clean.html`
- `reports/stage2_spatial_analysis_raw.html`
- `reports/stage3_summary_statistics.html`

Input inventories
- `manifests/raw_script_0_input_manifest.tsv`
- `manifests/auxiliary_workflow_input_manifest.tsv`

Workflow overview

Input
The workflow expects the accompanying Zenodo package next to this GitHub package. The Zenodo package contains:
- `raw_script_0_input.zip`
  The raw board-delivered source tree used by script_0
- `auxiliary_workflow_input.zip`
  Auxiliary non-board files used during script_0 and stage 2
- `workflow_outputs/foto_nl_dataset_clean.csv`
  Canonical clean final dataset
- `workflow_outputs/foto_nl_dataset_raw.csv`
  Canonical raw final dataset

Workflow
1. Data reconstruction (`script_0`)
   Standardizes heterogeneous board exports into one `board_input.csv` per provider.
2. Data cleaning and harmonization (`script_1`)
   Resolves chemistry identifiers, units, CAS numbers, and record-level cleanup rules.
3. Spatial analysis (`script_2`)
   Filters records to the Dutch spatial domain and writes clean and raw final datasets.
4. Summary statistics (`script_3`)
   Generates tables and figures that summarize monitoring intensity, substances, and spatial coverage.

Output
A full rerun produces:
- per-board `board_input.csv` files from script_0
- stage-1 intermediate R objects
- final clean and raw CSV/RDS outputs
- summary tables and visualizations

How to use

Prerequisites
- Conda or Mamba
- Bash
- Snakemake
- Pandoc
- system libraries required by the R package `sf`

Reference environment
- R `4.5.2`
- Python `3.8.19`
- Snakemake `7.32.4`

Setup
1. Place the two packages next to each other:

   some_parent_folder/
   - `github_ready_20260330/`
   - `zenodo_ready_20260330/`

2. Create the conda environment:

   `conda env create -f environment.yml`

3. Extract the Zenodo input archives before running the workflow:

   `cd ../zenodo_ready_20260330`

   `unzip raw_script_0_input.zip`

   `unzip auxiliary_workflow_input.zip`

Local run
From the GitHub package folder:

```bash
snakemake \
  --snakefile Snakefile \
  --cores 4 \
  --config \
    raw_root=../zenodo_ready_20260330/raw_script_0_input \
    reference_input_root=../zenodo_ready_20260330/auxiliary_workflow_input \
    include_script_0_reports=false \
    include_raw_branch=true \
    include_comparison=false \
    include_qa=false
```

Key features of the FOTO-NL workflow and dataset
- Long-term national monitoring coverage for Dutch surface waters
- Code-generated reconstruction of provider-specific raw exports into standardized script_0 outputs
- Harmonized chemistry identifiers and units across providers and decades
- Spatial filtering for Dutch-domain analyses
- Summary statistics and figures for water boards, years, substances, and coordinates
- Example downstream analysis for Delfland pesticide time trends

Important notes
- This package is the code package, not the full data archive.
- The workflow depends on the adjacent Zenodo package for raw and auxiliary inputs.
- The current standalone defaults do not run script_0 reports, old-baseline comparison, or QA drift checks.
- The GitHub package includes rendered HTML reports for stage 1 to stage 3, but not the large final datasets.
- The Zenodo package now keeps the clean and raw final CSVs loose and stores the larger supporting payloads as zip files.

Acknowledgements
This project was funded by RIVM and STOWA under the SPR-BIOTICHS project. The authors thank the Dutch Waterboards and Rijkswaterstaat for their data contributions.

Corresponding authors
- Thomas Hofman: thomas.hofman@rivm.nl
- Leo Posthuma: leo.posthuma@rivm.nl

Citation
If you use this repository or the FOTO-NL database, please cite the associated data paper by Hofman et al.

Where to look next
- `README_WORKFLOW_DETAILED.md`
- `manifests/raw_script_0_input_manifest.tsv`
- `manifests/auxiliary_workflow_input_manifest.tsv`
