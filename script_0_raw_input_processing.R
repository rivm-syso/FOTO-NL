#!/usr/bin/env Rscript
# Compatibility entrypoint.
# Canonical script_0 documentation lives in: script_0_raw_input_processing.Rmd
# Batch execution calls the shared script_0 engine directly so errors propagate.

args <- commandArgs(trailingOnly = TRUE)

for (arg in args) {
  if (!startsWith(arg, "--")) next
  kv <- strsplit(substring(arg, 3L), "=", fixed = TRUE)[[1]]
  key <- toupper(gsub("-", "_", kv[1]))
  val <- if (length(kv) > 1L) paste(kv[-1], collapse = "=") else "true"
  do.call(Sys.setenv, setNames(list(val), key))
}

script_path <- {
  file_flag <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_flag) == 0L) "" else sub("^--file=", "", file_flag[1])
}
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()

common_candidates <- c(
  file.path(script_dir, "build_input_common.R"),
  file.path(script_dir, "helpers", "script_0", "build_input_common.R")
)
cfg_candidates <- c(
  file.path(script_dir, "build_input_board_configs.R"),
  file.path(script_dir, "helpers", "script_0", "build_input_board_configs.R")
)
common_file <- common_candidates[file.exists(common_candidates)][1]
cfg_file <- cfg_candidates[file.exists(cfg_candidates)][1]
if (!file.exists(common_file)) {
  stop("build_input_common.R not found in expected locations under: ", script_dir, " (cwd=", getwd(), ")")
}
if (!file.exists(cfg_file)) {
  stop("build_input_board_configs.R not found in expected locations under: ", script_dir, " (cwd=", getwd(), ")")
}

source(common_file, local = .GlobalEnv)
source(cfg_file, local = .GlobalEnv)

required_common <- c(
  "parse_context_args",
  "run_board_reconstruction",
  "export_legacy_inputs",
  "norm_text",
  "norm_key_text"
)
missing_common <- required_common[!vapply(required_common, exists, logical(1), mode = "function", inherits = TRUE)]
if (length(missing_common) > 0L) {
  stop(
    "Shared script_0 functions missing after sourcing common file: ",
    paste(missing_common, collapse = ", "),
    " | common_file=", normalizePath(common_file, mustWork = FALSE),
    " | cwd=", getwd(),
    " | script_dir=", script_dir
  )
}

if (!exists("get_board_configs", mode = "function", inherits = TRUE)) {
  stop(
    "Function get_board_configs missing after sourcing board config file: ",
    normalizePath(cfg_file, mustWork = FALSE),
    " | cwd=", getwd(),
    " | script_dir=", script_dir
  )
}

if (!exists("run_script_0_pipeline", mode = "function", inherits = TRUE)) {
  run_script_0_pipeline <- function(configs,
                                  argv = commandArgs(trailingOnly = TRUE),
                                  base_dir = getwd()) {
    ctx <- parse_context_args(argv, base_dir = base_dir)

    only_sources <- ctx$only_sources
    if (length(only_sources) > 0L) {
      only_key <- unique(c(norm_text(only_sources), norm_key_text(only_sources)))
      configs <- Filter(function(cfg) {
        cfg$datasource %in% only_sources ||
          cfg$board_id %in% only_sources ||
          norm_text(cfg$datasource) %in% only_key ||
          norm_key_text(cfg$datasource) %in% only_key ||
          norm_key_text(cfg$board_id) %in% only_key
      }, configs)
      if (length(configs) == 0L) {
        stop("No board matches ONLY_SOURCES/--only-sources: ", paste(only_sources, collapse = ", "))
      }
    }

    results <- vector("list", length(configs))
    for (i in seq_along(configs)) {
      cfg <- configs[[i]]
      message("== [", i, "/", length(configs), "] ", cfg$datasource, " ==")
      results[[i]] <- tryCatch(
        run_board_reconstruction(cfg, ctx),
        error = function(e) {
          data.table::data.table(
            DataSource = cfg$datasource,
            Status = paste0("error: ", conditionMessage(e)),
            RawFiles = NA_integer_,
            RawRows = NA_integer_,
            MappedRows = NA_integer_,
            BoardRuleRowsDropped = NA_integer_,
            FilteredRows = NA_integer_,
            ReferenceRows = NA_integer_,
            MatchedRows = NA_integer_,
            SignatureMode = NA_character_,
            SigIntersectUnique = NA_integer_,
            Coverage = NA_real_,
            OutputRows = NA_integer_,
            OutputFile = file.path(ctx$output_root, cfg$output_file),
            OutputFormat = NA_character_,
            OutputMessage = conditionMessage(e)
          )
        }
      )
    }

    summary_dt <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)
    export_legacy_inputs(ctx)

    bad <- summary_dt[is.na(Status) | Status != "ok"]
    if (nrow(bad) > 0L) {
      stop(
        "Script_0 reconstruction failed for: ",
        paste(sprintf("%s [%s]", bad$DataSource, bad$Status), collapse = "; ")
      )
    }
    invisible(list(ctx = ctx, summary = summary_dt))
  }
}

run_script_0_pipeline(
  configs = get_board_configs(),
  argv = args,
  base_dir = script_dir
)

render_report <- tolower(Sys.getenv("SCRIPT_0_RENDER_REPORT", "false")) %in% c("1", "true", "t", "yes", "y")
if (render_report) {
  input_rmd <- file.path(script_dir, "script_0_raw_input_processing.Rmd")
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required. Install it in bart-env.")
  }
  Sys.setenv(RUN_0_RAW_INPUT_PROCESSING = "false")
  rmarkdown::render(
    input = input_rmd,
    output_format = "html_document",
    quiet = TRUE
  )
}
