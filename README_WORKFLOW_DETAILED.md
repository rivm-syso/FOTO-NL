# FOTO_NL Workflow README

## 1. What This Workflow Does

This workflow reconstructs a reproducible national surface-water chemistry dataset for the Netherlands from heterogeneous water board source exports.

At a high level, it does four things:

1. **Script_0: raw reconstruction**
   Reads the original board-specific source files, normalizes columns, applies board-specific lookup and adjustment rules, and writes one standardized `board_input.csv` per board.
2. **Stage 1: combination and chemical cleaning**
   Combines all standardized board inputs, harmonizes chemistry identifiers and units, removes unresolved or invalid rows, and produces the clean analysis tables.
3. **Stage 2: spatial filtering / Dutch subset**
   Restricts the dataset to the Dutch spatial domain and writes the final clean and raw CSV/RDS outputs.
4. **Stage 3: summary statistics**
   Builds the grouped summary tables and figures used to inspect the final output.

The workflow is designed so that the most important reproducibility boundary is explicit:

- **Script_0 output** is code-generated from the raw board exports.
- **Stage 1 input** is not a hand-curated workbook bundle; it is the generated script_0 product.

That handoff is the main reason the Zenodo package contains both the raw script_0 sources and the generated script_0 outputs.

---

## 2. What Is In GitHub And What Is In Zenodo

This project is now intentionally split into a **code package** and a **data package**.

### GitHub package

The GitHub-ready folder contains code and lightweight helper files:

- `Snakefile`
- `run_snakemake_lsf.sh`
- `config/snakemake_pipeline.yaml`
- root-level stage scripts (`script_0_*`, `script_1_*`, `script_2_*`, `script_3_*`)
- `helpers/script_0/`
- `resources/lookups/`
- `helpers/python/`
- `helpers/r/`
- `resources/support_tables/AquoParams.rda`
- `resources/support_tables/ModifierDefaults.rda`
- `resources/support_tables/Gross.rda`
- manifests describing the expected raw and auxiliary inputs

The GitHub package does **not** contain the heavy raw workbooks and large generated outputs.

### Zenodo package

The Zenodo-ready folder contains the heavy data and canonical outputs:

- `raw_script_0_input/` = raw board-specific script_0 source tree
- `auxiliary_workflow_input/` = auxiliary non-board files used by script_0 and stage 2
- `workflow_outputs/script_0_output/` = publishable script_0 outputs per board, written as `workflow_outputs/script_0_output/<board_slug>/board_input.csv`
- `workflow_outputs/foto_nl_dataset_clean.csv`
- `workflow_outputs/foto_nl_dataset_raw.csv`
- `workflow_outputs/foto_nl_dataset_clean.rds`
- `workflow_outputs/foto_nl_dataset_raw.rds`
- summary output folders

### Practical meaning

For a reproducible rerun:

- use the **GitHub package** as the code base
- use the **Zenodo package** as the data source

The recommended local layout is:

```text
some_parent_folder/
├── github_ready_20260330/
└── zenodo_ready_20260330/
```

In that layout:

- `RAW_ROOT = ../zenodo_ready_20260330/raw_script_0_input`
- `REFERENCE_INPUT_ROOT = ../zenodo_ready_20260330/auxiliary_workflow_input`
- `SKIP_REFERENCE_READ = true`

---

## 3. Workflow Structure

### Script_0

Primary execution entry point:

- `script_0_raw_input_processing.R`

Companion explanatory notebook:

- `script_0_raw_input_processing.Rmd`

Core logic:

- `helpers/script_0/build_input_common.R`
- `helpers/script_0/build_input_board_configs.R`

Optional report layer:

- `script_0_report.R`
- `script_0_report.Rmd`

Script_0 reads the raw source files from `RAW_ROOT`, uses auxiliary files from `REFERENCE_INPUT_ROOT`, applies board-specific lookups and adjustments, and writes standardized per-board outputs.

Main script_0 output per board:

- `output/.../script_0_output/<board_slug>/board_input.csv`

### Stage 1

Notebook:

- `script_1_read_and_clean.Rmd`

Stage 1 reads the script_0 generated `board_input.csv` files, combines them, harmonizes units and chemistry identifiers, and writes:

- `InBAllData.RDS`
- `AllData.RDS`
- `ToPAF.rds`
- `ToPAF.rda`
- `script_1_read_and_clean.html`

### Stage 2

Clean branch:

- `script_2_spatial_analysis.Rmd`

Raw branch:

- `script_2_spatial_analysis_raw.Rmd`

Stage 2 applies the Dutch boundary filter and writes:

- clean `foto_nl_dataset_clean.rds` + `foto_nl_dataset_clean.csv`
- raw `foto_nl_dataset_clean.rds` + `foto_nl_dataset_raw.csv`

### Stage 3

Notebook:

- `script_3_summary_statistics.Rmd`

Stage 3 reads the stage-2 `foto_nl_dataset_clean.rds` and writes:

- `script_3_summary_statistics.html`
- `summary_statistics/`

In the current standalone packaging, the **summary statistics are intended to be generated from the raw branch**.

### Example analysis

- `PesticesDelfland.R`

This is an example downstream analysis script that reads the final clean CSV and produces Delftland-specific example outputs.

---

## 4. Recommended Way To Run Without Linux LSF

You do **not** need LSF to run the workflow.

The cluster wrapper `run_snakemake_lsf.sh` is only for environments that provide `bsub`.

For a normal workstation or server, use **Snakemake directly**:

```bash
snakemake --snakefile Snakefile --cores 4
```

### Recommended platforms

#### Best supported

- Linux
- macOS
- Windows **via WSL2**

#### Not recommended

- native Windows PowerShell / cmd.exe as the primary execution environment

Reason:

- the workflow uses Bash shell commands
- the Snakemake rules use POSIX tools and symlinks such as `ln -sfn`
- R spatial dependencies are easier to install on Linux/macOS/WSL2 than on native Windows

If you only have Windows, use:

- WSL2 Ubuntu, or
- a Linux container, or
- a Linux VM

---

## 5. Required Software

The workflow was recently validated in an environment with:

- **R** `4.5.2`
- **Python** `3.8.19`
- **Snakemake** `7.32.4`

These exact versions are not mandatory, but they are the tested reference point.

### Required software components

1. **R**
2. **Python**
3. **Snakemake**
4. **Bash**
5. **Pandoc** for HTML rendering of the R Markdown notebooks
6. **System libraries for `sf`**

### Why Pandoc matters

The main stage notebooks are R Markdown files rendered with `rmarkdown::render(...)`.

If Pandoc is missing:

- stage 1/2/3 HTML rendering may fail
- `script_0_report.R` can fall back to Markdown in some cases, but that fallback does **not** remove the Pandoc requirement for the full notebook workflow

### Why `sf` system libraries matter

The stage-2 notebooks use `sf`. On a fresh machine you typically need these system libraries available before installing the R package:

- GDAL
- GEOS
- PROJ
- UDUNITS2

On Debian/Ubuntu this usually means packages similar to:

- `libgdal-dev`
- `libgeos-dev`
- `libproj-dev`
- `libudunits2-dev`
- `gdal-bin`

On macOS this is typically easiest through Homebrew.

---

## 6. Required R Packages

The safest approach is to install the union of the packages required by all stages.

### Script_0 core packages

Used by `helpers/script_0/build_input_common.R`:

- `data.table`
- `readxl`

Optional for legacy xlsx writing:

- `openxlsx`

Optional for script_0 reports:

- `rmarkdown`
- `knitr`

### Stage 1 packages

Used by `script_1_read_and_clean.Rmd`:

- `dplyr`
- `readxl`
- `readr`

Stage 1 also uses base R packages and the bundled support objects in:

- `resources/support_tables/AquoParams.rda`
- `resources/support_tables/ModifierDefaults.rda`
- `resources/support_tables/Gross.rda`

And the bundled helper functions in:

- `helpers/r/excelDate2Date.R`
- `helpers/r/CheckMapConc.R`
- `helpers/r/ValidCAS.R`

### Stage 2 packages

Used by both `script_2_spatial_analysis.Rmd` and `script_2_spatial_analysis_raw.Rmd`:

- `readr`
- `dplyr`
- `tibble`
- `sf`
- `mapview`
- `data.table`
- `lubridate`

### Stage 3 packages

Used by `script_3_summary_statistics.Rmd`:

- `gridExtra`
- `readr`
- `dplyr`
- `sf`
- `ggplot2`
- `scales`
- `lubridate`
- `tidyr`
- `tibble`
- `forcats`

### Example script packages

Used by `PesticesDelfland.R`:

- `dplyr`
- `ggplot2`
- `lubridate`
- `gridExtra`
- `data.table`
- `mgcv`

Optional:

- `writexl`

### A practical local installation list

If you are setting up a fresh environment, install at least:

- `data.table`
- `readxl`
- `openxlsx`
- `dplyr`
- `readr`
- `tibble`
- `sf`
- `mapview`
- `lubridate`
- `gridExtra`
- `ggplot2`
- `scales`
- `tidyr`
- `forcats`
- `mgcv`
- `rmarkdown`
- `knitr`
- `writexl`

Example from R:

```r
install.packages(c(
  "data.table", "readxl", "openxlsx", "dplyr", "readr", "tibble",
  "sf", "mapview", "lubridate", "gridExtra", "ggplot2", "scales",
  "tidyr", "forcats", "mgcv", "rmarkdown", "knitr", "writexl"
))
```

---

## 7. Required Python Packages

For the standalone production workflow, Python requirements are light.

### Required

- `snakemake`

### Optional helper scripts

The helper scripts in `helpers/python/` only use the Python standard library.
They do not require extra third-party Python packages beyond Snakemake itself.

---

## 8. YAML Files And What They Control

There are two kinds of YAML you may care about.

### A. Snakemake configuration YAML

Primary file:

- `config/snakemake_pipeline.yaml`

This file controls:

- data locations
- whether raw branch / reports / comparison / QA are included
- memory and runtime per rule
- thread counts per rule
- report settings
- QA thresholds

### B. Optional Conda environment YAML

The repository does **not** currently depend on a mandatory environment YAML file.
You may manage the environment yourself with Conda, Mamba, system Python + system R, or RStudio.

If you want strict reproducibility, creating your own `environment.yml` is recommended, but it is not required by the workflow code.

---

## 9. Important Snakemake Config Keys

These are the most important keys in `config/snakemake_pipeline.yaml`.

### Core path keys

- `raw_root`
  - root directory containing the raw board-specific script_0 source tree
- `reference_input_root`
  - root directory containing auxiliary non-board files
- `pipeline_root`
  - output directory for the current run
- `run_tag`
  - label used in the default output directory name
- `baseline_clean_csv`
  - only needed if comparison to an older baseline is enabled

### Workflow selection keys

- `include_script_0_reports`
- `include_raw_branch`
- `include_comparison`
- `include_qa`
- `script_0_write_diagnostics`
- `script_0_report_boards`

### Resource keys

- `script_0_mem_mb`
- `script_0_runtime_min`
- `script1_threads`
- `script1_mem_mb`
- `script1_runtime_min`
- `script2_threads`
- `script2_mem_mb`
- `script2_runtime_min`
- `script2_raw_threads`
- `script2_raw_mem_mb`
- `script2_raw_runtime_min`
- `script3_threads`
- `script3_mem_mb`
- `script3_runtime_min`

### QA keys

Only relevant if `include_qa: true`.

- `qa.max_orig_only_keys`
- `qa.max_new_only_keys`
- `qa.max_unit_mismatch_common`
- `qa.max_negative_new_mean_common`
- `qa.max_abs_mean_rel_diff_over_threshold`
- `qa.mean_rel_diff_threshold`

### Current default standalone behavior

The current standalone config is intentionally lean:

- script_0 reports disabled by default
- raw branch enabled by default
- comparison disabled by default
- QA disabled by default
- script_0 diagnostic TSVs disabled by default

This makes the default run closer to a production rebuild than a debugging session.

---

## 10. Recommended Local Config For GitHub + Zenodo

If the GitHub-ready folder and Zenodo-ready folder sit side by side, a good minimal local config override is:

```yaml
env_name: local
raw_root: ../zenodo_ready_20260330/raw_script_0_input
reference_input_root: ../zenodo_ready_20260330/auxiliary_workflow_input
include_script_0_reports: false
include_raw_branch: true
include_comparison: false
include_qa: false
script_0_write_diagnostics: false
```

You can either place this in a separate YAML file, or pass the values on the command line.

Example command-line override:

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

The Snakefile now parses boolean overrides correctly, so `false` really means false.

---

## 11. Local Non-LSF Run: Full Workflow

### From a normal repository checkout

If you are in the main repository root and the Zenodo bundle is extracted inside the same project tree:

```bash
snakemake \
  --snakefile Snakefile \
  --cores 4 \
  --config \
    raw_root=zenodo_ready_20260330/raw_script_0_input \
    reference_input_root=zenodo_ready_20260330/auxiliary_workflow_input \
    include_script_0_reports=false \
    include_raw_branch=true \
    include_comparison=false \
    include_qa=false
```

### From the GitHub-ready package

If you run from `github_ready_20260330/` and the Zenodo package is next to it:

```bash
cd github_ready_20260330
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

### Expected outputs

At the end of a normal standalone run, expect:

- `script_0_output/`
- `stage1_input_flat/`
- `workspace_clean/`
- `workspace_raw/`
- clean and raw final CSV/RDS outputs
- stage 1, stage-2, and stage-3 HTML files

---

## 12. Manual Run Without Snakemake

If you do not want to use Snakemake, you can still run the stages manually.

### Script_0 manual run

Example, all boards:

```bash
RAW_ROOT=../zenodo_ready_20260330/raw_script_0_input \
REFERENCE_INPUT_ROOT=../zenodo_ready_20260330/auxiliary_workflow_input \
SKIP_REFERENCE_READ=true \
OUTPUT_ROOT=output/manual_script_0 \
OUTPUT_FORMAT=csv \
SCRIPT_0_WRITE_DIAGNOSTICS=false \
OVERWRITE=true \
INCLUDE_LEGACY_INPUTS=false \
Rscript script_0_raw_input_processing.R
```

Example, a single board only:

```bash
RAW_ROOT=../zenodo_ready_20260330/raw_script_0_input \
REFERENCE_INPUT_ROOT=../zenodo_ready_20260330/auxiliary_workflow_input \
SKIP_REFERENCE_READ=true \
OUTPUT_ROOT=output/manual_script_0 \
ONLY_SOURCES='Rivierenland' \
OUTPUT_FORMAT=csv \
OVERWRITE=true \
Rscript script_0_raw_input_processing.R
```

### Stage 1 manual run

Create a working folder that contains:

- `script_1_read_and_clean.Rmd`
- `R/`
- `stage1_input_flat/` or a `PRJDATA_DIR` pointing to it
- `auxiliary_workflow_input/` if needed

Then run:

```bash
cd some_workspace
PRJDATA_DIR=/absolute/path/to/stage1_input_flat \
STAGE1_N_CORES=4 \
LIGHTWEIGHT_CHECKS=true \
STRICT_SCRIPT_0_DIAG=false \
Rscript -e "rmarkdown::render('script_1_read_and_clean.Rmd', quiet = FALSE)"
```

### Stage 2 manual run

Clean branch:

```bash
cd workspace_clean
Rscript -e "rmarkdown::render('script_2_spatial_analysis.Rmd', quiet = FALSE)"
```

Raw branch:

```bash
cd workspace_raw
Rscript -e "rmarkdown::render('script_2_spatial_analysis_raw.Rmd', quiet = FALSE)"
```

### Stage 3 manual run

Run from the workspace that contains the desired `foto_nl_dataset_clean.rds`:

```bash
Rscript -e "rmarkdown::render('script_3_summary_statistics.Rmd', quiet = FALSE)"
```

If you want summary statistics based on the raw branch, run stage 3 from the raw workspace.

---

## 13. Environment Variables Used By The Workflow

You do not need to use all of these, but they are useful when running stages manually.

### Script_0

Common script_0 variables:

- `RAW_ROOT`
- `REFERENCE_INPUT_ROOT`
- `OUTPUT_ROOT`
- `OUTPUT_FORMAT`
- `ONLY_SOURCES`
- `SKIP_REFERENCE_READ`
- `INCLUDE_LEGACY_INPUTS`
- `OVERWRITE`
- `SCRIPT_0_WRITE_DIAGNOSTICS`
- `SCRIPT_0_COLLECT_DEBUG`
- `SCRIPT_0_CORES`
- `N_CORES`
- `MAX_ROWS_PER_FILE`
- `MAX_ROWS_PER_SHEET`
- `MAX_REFERENCE_ROWS`
- `USE_REFERENCE_FILTER`
- `DRY_RUN`

Board-specific toggles currently present in the code:

- `AA_RULE_VARIANT`
- `LIMBURG_VALUE_SNAP`
- `DE_DOMMEL_DATE_SOURCE`
- `HH_RIJNLAND_PARAM_SOURCE`
- `WETTERSKIP_PARAM_SOURCE`
- `VECHTSTROMEN_VALUE_SNAP`

### Script_0 report layer

- `SCRIPT_0_REPORT_BOARD`
- `SCRIPT_0_REPORT_OUTPUT_FORMAT`
- `SCRIPT_0_REPORT_OUTPUT_DIR`
- `SCRIPT_0_RENDER_REPORT`

### Stage 1

- `PRJDATA_DIR`
- `LIGHTWEIGHT_CHECKS`
- `STRICT_SCRIPT_0_DIAG`
- `STAGE1_N_CORES`

### Example script

- `INPUT_FILE`
- `OUT_DIR`

---

## 14. Important Data Files Beyond The Raw Workbooks

The workflow does not only depend on notebooks and raw board exports.

### Bundled support objects used by stage 1

These are required and must be present under `R/`:

- `resources/support_tables/AquoParams.rda`
- `resources/support_tables/ModifierDefaults.rda`
- `resources/support_tables/Gross.rda`

### Bundled helper functions used by stage 1

- `helpers/r/excelDate2Date.R`
- `helpers/r/CheckMapConc.R`
- `helpers/r/ValidCAS.R`

### Lookup and adjustment tables used by script_0

The script_0 code now searches these locations in order:

1. board-specific file under `RAW_ROOT`
2. packaged lookup file under `resources/lookups/`
3. older loose-file fallbacks where they still exist

Examples of lookup tables currently used by the code:

- `parametercode_converter.csv`
- `rivierenland_parameter_lookup.tsv`
- `rivierenland_parameter_lookup_with_parnr.tsv`
- `rivierenland_parameter_adjustments.csv`
- `hh_rijnland_parameter_lookup.tsv`
- `rijnland_parameter_adjustments.csv`
- `scheldestromen_parameter_lookup.tsv`
- `scheldestromen_parameter_adjustments.csv`
- `hhnk_parameter_adjustments.csv`

### Auxiliary non-board files

These come from `REFERENCE_INPUT_ROOT`.
The Zenodo bundle now packages them under `auxiliary_workflow_input/`.

Examples:

- `AanvlocDrent.xlsx`
- `lokatie info_Drents Overijsselse Delta.xlsx`
- `Rijkswaterstaat data bestand_Maas.xlsx`
- `Rijkswaterstaat data bestand_Rijn.xlsx`
- `SI_2_table.xlsx`
- `adm_landsgrens.csv`

---

## 15. What The Current Default Workflow Does Not Include

By default, the current standalone production configuration does **not** run:

- script_0 Markdown reports
- comparison against the old baseline clean CSV
- QA drift gating

These features still exist in the code and can be turned back on in the config.

This was done intentionally to keep the default rerun lean and reproducible.

---

## 16. How To Re-Enable Reports, Comparison, And QA

### Enable script_0 reports

In the YAML config or CLI overrides:

```yaml
include_script_0_reports: true
script_0_report_boards:
  - rivierenland
  - vechtstromen
```

### Enable comparison

```yaml
include_comparison: true
baseline_clean_csv: path/to/foto_nl_dataset_clean.csv
```

### Enable QA

```yaml
include_qa: true
qa:
  max_orig_only_keys: 200
  max_new_only_keys: 200
  max_unit_mismatch_common: 0
  max_negative_new_mean_common: 0
  max_abs_mean_rel_diff_over_threshold: 0
  mean_rel_diff_threshold: 10.0
```

Comparison and QA are useful for development and regression checks, but they are not required for a clean standalone rebuild.

---

## 17. Running In An LSF Environment

If you do have LSF available, you can use:

- `run_snakemake_lsf.sh`

It wraps Snakemake and submits jobs with `bsub`.

Typical usage:

```bash
JOBS=21 ./run_snakemake_lsf.sh
```

Optional environment variables:

- `ENV_NAME`
- `CONFIGFILE`
- `JOBS`
- `RUN_TAG`
- `PIPELINE_ROOT`

This wrapper is convenient on a cluster, but it is not required for local use.

---

## 18. Common Failure Modes And How To Fix Them

### 1. `sf` fails to install

Cause:

- missing GDAL / GEOS / PROJ / UDUNITS2 system libraries

Fix:

- install the system libraries first, then reinstall `sf`

### 2. R Markdown rendering fails with a Pandoc error

Cause:

- Pandoc not installed or not visible to R

Fix:

- install Pandoc
- or set `RSTUDIO_PANDOC` if using the RStudio Pandoc installation

### 3. Stage 1 cannot find auxiliary inputs

Cause:

- `REFERENCE_INPUT_ROOT` not set correctly
- or `auxiliary_workflow_input/` not available in the workspace/package layout

Fix:

- point `reference_input_root` to the Zenodo `auxiliary_workflow_input/` directory
- use the updated workflow files that now symlink `auxiliary_workflow_input` into the workspaces

### 4. Stage 1 cannot find script_0 output

Cause:

- `PRJDATA_DIR` not pointing to the generated `stage1_input_flat/`

Fix:

- set `PRJDATA_DIR` explicitly when running stage 1 manually
- or let Snakemake create the workspace and pass it automatically

### 5. Comparison or QA still runs when you think it is disabled

Cause:

- older Snakefile boolean parsing bug

Fix:

- use the current Snakefile, which now parses `true/false` command-line overrides correctly

### 6. Native Windows path or symlink problems

Cause:

- workflow assumes Bash and POSIX symlink behavior

Fix:

- use WSL2 or Linux/macOS

---

## 19. Minimal Reproducible Example

If you want the simplest reproducible rerun with the current packaging:

1. Put `github_ready_20260330/` and `zenodo_ready_20260330/` next to each other.
2. Create a working R/Python environment with the packages listed above.
3. Install Pandoc.
4. From `github_ready_20260330/`, run:

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

This will:

- rebuild script_0 from the raw board exports
- regenerate the stage 1 combined clean dataset
- regenerate clean and raw stage-2 outputs
- regenerate stage-3 summary statistics

---
