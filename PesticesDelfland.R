# -----------------------------------------------------------------------------
# PesticesDelfland.R
# Purpose:
#   Analyze selected chemicals for waterboard Delfland using the unified
#   FOTO-NL clean dataset and export violin/point plots + tabular outputs.
#
# Main choices in this script:
#   1) Use only quantified concentrations (LimitIndicator == "-")
#   2) Use predefined period bins from 2021-2015 backward to the 1980s
#   3) Focus on four chemicals:
#      - Tolclofos-methyl (CAS 57018-04-9)
#      - Imidacloprid    (CAS 138261-41-3)
#      - Chloride        (CAS 16887-00-6)
#      - Nitrate         (CAS 14797-55-8)
#   4) Produce violin plots and GAM trend plots on a continuous time scale
# -----------------------------------------------------------------------------

# ------------------------------- 1) Packages ---------------------------------
required_packages <- c("dplyr", "ggplot2", "lubridate", "gridExtra", "data.table", "mgcv")

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    ". Install these in your R environment first."
  )
}

library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(data.table)

# ------------------------------ 2) Settings ----------------------------------
default_input_candidates <- c(
  file.path("zenodo_ready_20260330", "workflow_outputs", "foto_nl_dataset_clean.csv"),
  file.path("output", "pipeline_targetedfix_20260327_171100", "workspace_clean", "output", "FOTO_NL_output_clean.csv"),
  file.path("data", "zenodo_14652471", "FOTO_NL_output_clean.csv")
)

resolve_input_file <- function() {
  env_input <- Sys.getenv("INPUT_FILE", unset = "")
  if (nzchar(env_input)) {
    return(env_input)
  }
  hit <- default_input_candidates[file.exists(default_input_candidates)]
  if (length(hit) > 0) {
    return(hit[[1]])
  }
  default_input_candidates[[1]]
}

input_file <- resolve_input_file()
out_dir <- Sys.getenv(
  "OUT_DIR",
  unset = file.path("zenodo_ready_20260330", "workflow_outputs", "example_pesticides_delfland")
)
period_levels <- c(
  "2021-2015", "2014-2009", "2008-2003",
  "2002-1997", "1996-1991", "1990-1985", "1984-1980"
)
gam_time_limits <- as.Date(c("1980-01-01", "2021-12-31"))
chemical_thresholds <- list(
  Tolclofosmethyl = data.frame(
    label = c("AA-EQS 1.2 ug/L", "MAC-EQS 7.1 ug/L"),
    value = c(1.2, 7.1),
    line_color = c("#B22222", "#8B0000"),
    line_type = c("dashed", "dotdash"),
    stringsAsFactors = FALSE
  ),
  Imidacloprid = data.frame(
    label = c("AA-EQS 0.0083 ug/L", "MAC-EQS 0.2 ug/L"),
    value = c(0.0083, 0.2),
    line_color = c("#B22222", "#8B0000"),
    line_type = c("dashed", "dotdash"),
    stringsAsFactors = FALSE
  ),
  Nitrate = data.frame(
    label = c("Criterion 50 mg/L (= 50000 ug/L)"),
    value = c(50000),
    line_color = c("#B22222"),
    line_type = c("dashed"),
    stringsAsFactors = FALSE
  )
)

if (!file.exists(input_file)) {
  stop("Input dataset not found: ", input_file)
}
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# Remove stale files from the previous "frequent chemical" variant.
stale_files <- c(
  file.path(out_dir, "delfland_frequentchemical.csv"),
  file.path(out_dir, "delfland_most_frequent_chemical.csv"),
  file.path(out_dir, "delfland_frequency_table.csv")
)
invisible(file.remove(stale_files[file.exists(stale_files)]))

# ------------------------ 3) Helpers: Data & Plots ---------------------------
read_foto_clean <- function(path) {
  required_cols <- c(
    "AquoCode", "CAS", "MeasuredValue", "Unit", "Locationcode",
    "LimitIndicator", "DataSource", "SampleDate"
  )

  as_tibble(
    fread(path, sep = ";", select = required_cols, showProgress = FALSE)
  ) %>%
    mutate(
      SampleDate = as.Date(SampleDate),
      MeasuredValue = as.numeric(MeasuredValue),
      LimitIndicator = ifelse(is.na(LimitIndicator) | trimws(LimitIndicator) == "", "-", LimitIndicator)
    )
}

add_period_group <- function(df) {
  df %>%
    mutate(
      Group = case_when(
        between(SampleDate, as.Date("2015-01-01"), as.Date("2021-12-31")) ~ "2021-2015",
        between(SampleDate, as.Date("2009-01-01"), as.Date("2014-12-31")) ~ "2014-2009",
        between(SampleDate, as.Date("2003-01-01"), as.Date("2008-12-31")) ~ "2008-2003",
        between(SampleDate, as.Date("1997-01-01"), as.Date("2002-12-31")) ~ "2002-1997",
        between(SampleDate, as.Date("1991-01-01"), as.Date("1996-12-31")) ~ "1996-1991",
        between(SampleDate, as.Date("1985-01-01"), as.Date("1990-12-31")) ~ "1990-1985",
        between(SampleDate, as.Date("1980-01-01"), as.Date("1984-12-31")) ~ "1984-1980",
        TRUE ~ NA_character_
      ),
      Group = factor(Group, levels = period_levels)
    ) %>%
    filter(!is.na(Group))
}

chemical_subset <- function(df, cas_value, chemical_label) {
  out <- df %>%
    filter(CAS == cas_value, LimitIndicator == "-") %>%
    mutate(Group = factor(Group, levels = period_levels))

  if (nrow(out) == 0) {
    warning("No records found for ", chemical_label, " (CAS ", cas_value, ") after filtering.")
  }
  out
}

add_date_parts <- function(df) {
  df %>%
    mutate(
      Year = year(SampleDate),
      Month = month(SampleDate),
      Day = day(SampleDate)
    )
}

add_threshold_layers <- function(plot_obj, chemical_name) {
  thresholds <- chemical_thresholds[[chemical_name]]
  if (is.null(thresholds) || nrow(thresholds) == 0L) {
    return(plot_obj)
  }

  plot_obj +
    geom_hline(
      data = thresholds,
      aes(yintercept = value, color = label, linetype = label),
      linewidth = 0.8,
      inherit.aes = FALSE,
      show.legend = TRUE
    ) +
    scale_color_manual(values = stats::setNames(thresholds$line_color, thresholds$label)) +
    scale_linetype_manual(values = stats::setNames(thresholds$line_type, thresholds$label)) +
    guides(
      color = guide_legend(title = "Threshold"),
      linetype = guide_legend(title = "Threshold")
    ) +
    theme(legend.position = "bottom")
}

make_violin <- function(df, title_text, chemical_name) {
  plot_obj <- ggplot(df, aes(x = Group, y = MeasuredValue)) +
    geom_violin(fill = "#8FA9BF", color = "#2F4858", alpha = 0.8, trim = FALSE) +
    geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.35) +
    scale_x_discrete(drop = FALSE, limits = rev(period_levels)) +
    scale_y_log10() +
    labs(
      x = "Period",
      y = "Concentration (ug/L)",
      title = title_text
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  add_threshold_layers(plot_obj, chemical_name)
}

make_points <- function(df, title_text, chemical_name) {
  plot_obj <- ggplot(df, aes(x = SampleDate, y = MeasuredValue)) +
    geom_point(alpha = 0.3, size = 0.7) +
    scale_y_log10() +
    labs(
      x = "Date",
      y = "Concentration (ug/L)",
      title = title_text
    ) +
    theme_minimal(base_size = 12)

  add_threshold_layers(plot_obj, chemical_name)
}

make_gam <- function(df, title_text, chemical_name) {
  plot_obj <- ggplot(df, aes(x = SampleDate, y = MeasuredValue)) +
    geom_point(alpha = 0.2, size = 0.6) +
    geom_smooth(
      method = "gam",
      formula = y ~ s(x, bs = "cs"),
      color = "#1B5E20",
      fill = "#A5D6A7",
      linewidth = 1
    ) +
    scale_x_date(limits = gam_time_limits) +
    scale_y_log10() +
    labs(
      x = "Date",
      y = "Concentration (ug/L)",
      title = title_text
    ) +
    theme_minimal(base_size = 12)

  add_threshold_layers(plot_obj, chemical_name)
}

# ---------------------------- 4) Load & Filter -------------------------------
all_data <- read_foto_clean(input_file)

delfland <- all_data %>%
  filter(
    DataSource == "Delfland",
    !is.na(SampleDate),
    !is.na(MeasuredValue),
    MeasuredValue > 0
  ) %>%
  add_period_group()

if (nrow(delfland) == 0) {
  stop("No Delfland records remained after filtering.")
}

# --------------------- 5) Build selected chemical sets -----------------------
tolclofosmethyl <- chemical_subset(delfland, "57018-04-9", "Tolclofos-methyl")
imidacloprid <- chemical_subset(delfland, "138261-41-3", "Imidacloprid")
chloride <- chemical_subset(delfland, "16887-00-6", "Chloride")
nitrate <- chemical_subset(delfland, "14797-55-8", "Nitrate")

selected_sets <- list(
  Tolclofosmethyl = tolclofosmethyl,
  Imidacloprid = imidacloprid,
  Chloride = chloride,
  Nitrate = nitrate
)

# -------------------------- 6) Plot generation -------------------------------
violin_plots <- list(
  make_violin(tolclofosmethyl, "Tolclofos-methyl concentration in Delfland by period", "Tolclofosmethyl"),
  make_violin(imidacloprid, "Imidacloprid concentration in Delfland by period", "Imidacloprid"),
  make_violin(chloride, "Chloride concentration in Delfland by period", "Chloride"),
  make_violin(nitrate, "Nitrate concentration in Delfland by period", "Nitrate")
)

point_plots <- list(
  make_points(tolclofosmethyl, "Tolclofos-methyl concentration in Delfland over time", "Tolclofosmethyl"),
  make_points(imidacloprid, "Imidacloprid concentration in Delfland over time", "Imidacloprid"),
  make_points(chloride, "Chloride concentration in Delfland over time", "Chloride"),
  make_points(nitrate, "Nitrate concentration in Delfland over time", "Nitrate")
)

gam_plots <- list(
  make_gam(tolclofosmethyl, "GAM trend: Tolclofos-methyl concentration in Delfland", "Tolclofosmethyl"),
  make_gam(imidacloprid, "GAM trend: Imidacloprid concentration in Delfland", "Imidacloprid"),
  make_gam(chloride, "GAM trend: Chloride concentration in Delfland", "Chloride"),
  make_gam(nitrate, "GAM trend: Nitrate concentration in Delfland", "Nitrate")
)

violin_grid <- do.call(grid.arrange, c(violin_plots, ncol = 2))
point_grid <- do.call(grid.arrange, c(point_plots, ncol = 2))
gam_grid <- do.call(grid.arrange, c(gam_plots, ncol = 2))

ggsave(
  filename = file.path(out_dir, "delfland_violin_plots.png"),
  plot = violin_grid,
  width = 14,
  height = 12,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "delfland_point_plots.png"),
  plot = point_grid,
  width = 14,
  height = 12,
  dpi = 300
)

ggsave(
  filename = file.path(out_dir, "delfland_gam_plots.png"),
  plot = gam_grid,
  width = 14,
  height = 12,
  dpi = 300
)

# ---------------------------- 7) Tabular output ------------------------------
# Chemical datasets with year/month/day columns
selected_sets_with_dates <- lapply(selected_sets, add_date_parts)

# Write one Excel file if writexl is available, otherwise write per-chemical CSV.
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(
    selected_sets_with_dates,
    path = file.path(out_dir, "delfland_selected_chemicals.xlsx")
  )
} else {
  for (nm in names(selected_sets_with_dates)) {
    write.csv(
      selected_sets_with_dates[[nm]],
      file = file.path(out_dir, paste0("delfland_", tolower(nm), ".csv")),
      row.names = FALSE
    )
  }
}

# Transparent summary table for reproducibility and quick checks.
analysis_summary <- bind_rows(
  lapply(names(selected_sets), function(nm) {
    df <- selected_sets[[nm]]
    if (nrow(df) == 0) {
      return(data.frame(
        Chemical = nm, CAS = NA_character_, N = 0,
        MinDate = NA_character_, MaxDate = NA_character_, stringsAsFactors = FALSE
      ))
    }
    data.frame(
      Chemical = nm,
      CAS = unique(df$CAS)[1],
      N = nrow(df),
      MinDate = as.character(min(df$SampleDate)),
      MaxDate = as.character(max(df$SampleDate)),
      stringsAsFactors = FALSE
    )
  })
)

threshold_summary <- bind_rows(
  lapply(names(chemical_thresholds), function(nm) {
    cbind(
      Chemical = nm,
      chemical_thresholds[[nm]],
      stringsAsFactors = FALSE
    )
  })
)

write.csv(
  analysis_summary,
  file = file.path(out_dir, "delfland_analysis_summary.csv"),
  row.names = FALSE
)

write.csv(
  threshold_summary,
  file = file.path(out_dir, "delfland_thresholds.csv"),
  row.names = FALSE
)

message("Analysis complete. Outputs written to: ", out_dir)
