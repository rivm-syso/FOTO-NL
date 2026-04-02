#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || identical(x, "")) y else x
}

parse_cli_args <- function(argv) {
  out <- list()
  for (arg in argv) {
    if (!startsWith(arg, "--")) next
    kv <- strsplit(substring(arg, 3L), "=", fixed = TRUE)[[1]]
    key <- gsub("-", "_", kv[1])
    val <- if (length(kv) > 1L) paste(kv[-1], collapse = "=") else "true"
    out[[key]] <- val
  }
  out
}

cli <- parse_cli_args(args)

script_path <- {
  file_flag <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_flag) == 0L) "" else sub("^--file=", "", file_flag[1])
}
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()

report_input <- file.path(script_dir, "script_0_report.Rmd")
if (!file.exists(report_input)) {
  stop("script_0_report.Rmd not found at: ", report_input)
}

board <- cli$board %||% Sys.getenv("SCRIPT_0_REPORT_BOARD", unset = "")
if (!nzchar(board)) {
  stop("Set --board=<board_slug_or_name> or SCRIPT_0_REPORT_BOARD.")
}
Sys.setenv(SCRIPT_0_REPORT_BOARD = board)
Sys.setenv(SCRIPT_0_COLLECT_DEBUG = "true")
if (!nzchar(Sys.getenv("DRY_RUN", unset = ""))) {
  Sys.setenv(DRY_RUN = "true")
}

report_format <- tolower(cli$report_format %||% cli$output_format %||% Sys.getenv("SCRIPT_0_REPORT_OUTPUT_FORMAT", unset = "auto"))
output_dir <- normalizePath(
  cli$output_dir %||% Sys.getenv("SCRIPT_0_REPORT_OUTPUT_DIR", unset = file.path(script_dir, "output", "script_0_reports")),
  mustWork = FALSE
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

board_slug <- gsub("[^a-z0-9]+", "_", tolower(board))
board_slug <- gsub("^_+|_+$", "", board_slug)
if (!nzchar(board_slug)) board_slug <- "script_0_report"

can_render_html <- requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available("1.12.3")
if (identical(report_format, "auto")) {
  report_format <- if (can_render_html) "html" else "md"
}

if (identical(report_format, "html")) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for HTML output.")
  }
  if (!can_render_html) {
    stop("pandoc >= 1.12.3 is required for HTML output.")
  }
  output_file <- file.path(output_dir, paste0(board_slug, "_script_0_report.html"))
  rmarkdown::render(
    input = report_input,
    output_file = basename(output_file),
    output_dir = dirname(output_file),
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
  message("Wrote HTML report: ", output_file)
} else if (identical(report_format, "md")) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for Markdown output.")
  }
  output_file <- file.path(output_dir, paste0(board_slug, "_script_0_report.md"))
  knitr::knit(
    input = report_input,
    output = output_file,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
  message("Wrote Markdown report: ", output_file)
} else {
  stop("Unsupported --report-format=", report_format, ". Use auto, html, or md.")
}
