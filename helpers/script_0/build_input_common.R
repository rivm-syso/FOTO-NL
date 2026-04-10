#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x) || x == "") y else x
}

# Core argument, environment, and small parsing helpers.
# Parse --key=value CLI arguments into a named list.
parse_args <- function(argv) {
  out <- list()
  for (arg in argv) {
    if (!startsWith(arg, "--")) next
    kv <- strsplit(substring(arg, 3L), "=", fixed = TRUE)[[1]]
    if (length(kv) == 1L) {
      out[[kv[1]]] <- TRUE
    } else {
      out[[kv[1]]] <- paste(kv[-1], collapse = "=")
    }
  }
  out
}

# Convert common truthy strings to a boolean value.
as_bool <- function(x, default = FALSE) {
  if (is.null(x)) return(default)
  tolower(as.character(x)) %in% c("1", "true", "t", "yes", "y")
}

# Split a comma-separated argument or environment value.
csv_arg <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character(0))
  v <- trimws(unlist(strsplit(x, ",", fixed = TRUE)))
  v[nzchar(v)]
}

# Read a positive integer from the environment with a default.
env_int <- function(name, default = NA_integer_) {
  x <- suppressWarnings(as.integer(Sys.getenv(name, unset = "")))
  if (is.na(x) || x <= 0L) return(default)
  x
}

# Read a boolean flag from the environment with a default.
env_bool <- function(name, default = FALSE) {
  x <- Sys.getenv(name, unset = "")
  if (!nzchar(x)) return(default)
  tolower(x) %in% c("1", "true", "t", "yes", "y")
}

# Convert a sheet name to a filesystem-safe lowercase identifier.
sheet_slug <- function(x) {
  out <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  out <- tolower(gsub("[^A-Za-z0-9]+", "_", out))
  out <- gsub("_+", "_", out)
  out <- gsub("^_|_$", "", out)
  if (!nzchar(out)) out <- "sheet"
  out
}

# Schema and column-matching helpers.
# Keep only the standardized script_0 output columns.
select_output_cols <- function(dt) {
  dt[, .(
    Waterschap,
    `Meetpunt code`,
    `Meetpunt omschrijving`,
    xcoordinaat,
    ycoordinaat,
    Datum,
    Parametercode,
    Parameteromschrijving,
    Casnummer,
    Hoedanigheid,
    Limietsymbool,
    Meetwaarde,
    Eenheid,
    Opmerkingen,
    `LET OP`
  )]
}

# Strip punctuation and case to simplify name matching.
canonicalize <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  gsub("[^a-z0-9]+", "", x)
}

# Find the first matching column using a list of aliases.
pick_col <- function(dt, aliases) {
  nms <- names(dt)
  key <- canonicalize(nms)
  want <- canonicalize(aliases)
  idx <- match(want, key)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0L) return(NA_character_)
  nms[idx[1]]
}

# Return a matched alias column or an NA-filled fallback vector.
pull_alias <- function(dt, aliases) {
  nm <- pick_col(dt, aliases)
  if (is.na(nm)) return(rep(NA_character_, nrow(dt)))
  as.character(dt[[nm]])
}

# Fill missing character values from fallback vectors in order.
coalesce_chr <- function(...) {
  x <- list(...)
  safe_chr <- function(v) iconv(as.character(v), from = "", to = "UTF-8", sub = "")
  out <- safe_chr(x[[1]])
  if (length(x) == 1L) return(out)
  for (i in 2:length(x)) {
    nxt <- safe_chr(x[[i]])
    sel <- is.na(out) | trimws(out) == ""
    out[sel] <- nxt[sel]
  }
  out
}

# Recover missing site codes from short unnamed raw columns.
recover_meetpunt_from_short_cols <- function(dt, meetpunt_code) {
  out <- norm_text(meetpunt_code)
  miss <- is.na(out) | out == ""
  if (!any(miss)) return(out)

  nms <- names(dt)
  safe_nms <- iconv(as.character(nms), from = "", to = "UTF-8", sub = "")
  safe_nms[is.na(safe_nms)] <- ""
  can <- canonicalize(safe_nms)
  cand_cols <- nms[
    !(nms %in% c("__source_file", "__source_sheet")) &
      (is.na(can) | can == "" | nchar(can) <= 2L | grepl("^unnamed", tolower(safe_nms)))
  ]
  if (length(cand_cols) == 0L) return(out)

  best_col <- NA_character_
  best_score <- -Inf
  miss_n <- sum(miss)
  min_non_empty <- if (miss_n >= 1000L) 200L else 20L
  for (nm in cand_cols) {
    v <- norm_text(dt[[nm]])
    vm <- v[miss]
    non_empty <- vm[vm != ""]
    if (length(non_empty) < min_non_empty) next

    valid <- grepl("^[A-Za-z0-9._-]{4,}$", non_empty) &
      grepl("[A-Za-z]", non_empty) &
      !grepl("\\s", non_empty)
    prop_valid <- mean(valid)
    prop_non_empty <- length(non_empty) / max(1L, miss_n)
    score <- prop_valid * prop_non_empty

    if (prop_valid >= 0.9 && prop_non_empty >= 0.5 && score > best_score) {
      best_col <- nm
      best_score <- score
    }
  }

  if (is.na(best_col)) return(out)

  v <- norm_text(dt[[best_col]])
  fill <- miss & v != ""
  if (any(fill)) out[fill] <- v[fill]
  out
}

# Text, key, numeric, date, and unit normalization helpers.
# Normalize text to UTF-8, trim it, and collapse extra spaces.
norm_text <- function(x) {
  x <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
  x[is.na(x)] <- ""
  x <- trimws(x)
  x <- gsub("[ \t]+", " ", x)
  x
}

# Normalize text and lowercase it for stable key matching.
norm_key_text <- function(x) {
  tolower(norm_text(x))
}

# Standardize site-code keys and drop numeric leading zeroes.
normalize_meetpunt_key <- function(x) {
  out <- norm_key_text(x)
  is_num <- grepl("^[0-9]+$", out)
  out[is_num] <- sub("^0+", "", out[is_num])
  out[is_num & out == ""] <- "0"
  out
}

# Standardize limit-indicator text before extracting symbols.
normalize_limiet_key <- function(x) {
  out <- norm_key_text(x)
  out[out %in% c("-", "--", "nvt", "n.v.t.", "na", "n/a")] <- ""
  out <- gsub("≤", "<=", out, fixed = TRUE)
  out <- gsub("≥", ">=", out, fixed = TRUE)
  out <- gsub("=<", "<=", out, fixed = TRUE)
  out <- gsub("=>", ">=", out, fixed = TRUE)
  out
}

# Extract comparison symbols such as <, <=, >, >=, or = from raw text.
extract_limiet_symbol <- function(x) {
  raw <- norm_text(x)
  key <- normalize_limiet_key(raw)
  out <- rep(NA_character_, length(key))

  exact <- key %in% c("<", ">", "<=", ">=", "=")
  out[exact] <- key[exact]

  need <- !exact & !is.na(key) & key != ""
  if (any(need)) {
    k <- key[need]
    v <- rep(NA_character_, length(k))
    v[grepl("^<=", k)] <- "<="
    v[is.na(v) & grepl("^>=", k)] <- ">="
    v[is.na(v) & grepl("^<", k)] <- "<"
    v[is.na(v) & grepl("^>", k)] <- ">"
    v[is.na(v) & grepl("^=", k)] <- "="
    out[need] <- v
  }

  out
}

# Parse numeric text across Dutch and international decimal conventions.
parse_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
  x <- trimws(x)
  x[x %in% c("", "NA", "NULL", "-", ".", "nvt", "NVT")] <- NA_character_
  # Handle both decimal conventions:
  # - "1,23" -> 1.23
  # - "1.234,56" -> 1234.56
  has_comma <- grepl(",", x, fixed = TRUE)
  has_dot <- grepl("\\.", x)
  eu_style <- has_comma & has_dot
  if (any(eu_style, na.rm = TRUE)) {
    x[eu_style] <- gsub(".", "", x[eu_style], fixed = TRUE)
  }
  x <- gsub(",", ".", x, fixed = TRUE)
  # Keep exponent signs (e.g., "8E-3"). With TRE regex, escaping '-'
  # inside a character class can still drop the minus; place '-' at start.
  x <- gsub("[^-0-9eE+.]", "", x)
  suppressWarnings(as.numeric(x))
}

# Detect repeated-9 sentinel values that represent placeholders.
is_numeric_sentinel_text <- function(x) {
  raw <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
  raw <- trimws(raw)
  raw[is.na(raw)] <- ""
  raw <- gsub("\\s+", "", raw)
  grepl("^[-+]?9{4,}(?:[.,]0+)?$", raw)
}

# Normalize common chemistry unit spellings to a compact form.
normalize_chem_unit <- function(x) {
  u <- norm_text(x)
  u <- gsub("^(mg|ug|ng|g)\\s*[A-Za-z]{1,4}\\s*/\\s*l$", "\\1/l", u, ignore.case = TRUE, perl = TRUE)
  u <- gsub("^microg/l$", "ug/l", u, ignore.case = TRUE)
  u
}

# Map descriptive parameter names to standardized parameter codes.
map_parameter_names_to_codes <- function(code, descr = NULL) {
  code_norm <- norm_text(code)
  if (is.null(descr)) descr <- rep("", length(code_norm))
  descr_norm <- norm_text(descr)
  key <- norm_key_text(ifelse(descr_norm == "", code_norm, descr_norm))
  key_ascii <- tolower(gsub("[^a-z0-9]+", "", iconv(key, from = "", to = "ASCII//TRANSLIT")))

  out <- code_norm
  out_key <- norm_key_text(out)
  # Map when code is missing/non-standard or clearly descriptive text.
  needs_map <- is.na(out) |
    out == "" |
    grepl("^[0-9]+$", out) |
    grepl("^wns[0-9]+$", out_key) |
    grepl("^geenaquoco", key_ascii) |
    grepl("[[:space:]/(),.-]", out) |
    nchar(out) > 12L
  if (!any(needs_map, na.rm = TRUE)) return(out)

  mapped <- rep(NA_character_, length(out))
  set_code <- function(pattern, val) {
    hit <- grepl(pattern, key_ascii, perl = TRUE)
    mapped[hit & is.na(mapped)] <<- val
  }

  set_code("temperatuur", "T")
  set_code("^ph$|zuurgraad", "pH")
  set_code("geleidend|egv", "GELDHD")
  set_code("doorzicht|zicht", "ZICHT")
  set_code("troebel|troeb|turbid", "TROEBHD")
  set_code("chlorofyl", "CHLFa")
  set_code("zuurstof", "O2")
  set_code("chloride", "Cl")
  set_code("ammonium", "NH4")
  set_code("nitraat", "NO3")
  set_code("nitriet", "NO2")
  set_code("kjeldahl|nkj", "NKj")
  set_code("fosfor.*totaal|ptot", "Ptot")
  set_code("fosfaat|orthofosfaat|po4", "PO4")
  set_code("ntotaal|stikstoftotaal|\\bntot\\b", "Ntot")
  set_code("zwevendestof|onopgelostestof", "OS")
  set_code("biochemischzuurstofverbruik|bzv", "BZV5")
  set_code("sulfaat", "SO4")
  set_code("arseen", "As")
  set_code("cadmium", "Cd")
  set_code("chroom", "Cr")
  set_code("koper", "Cu")
  set_code("lood", "Pb")
  set_code("nikkel", "Ni")
  set_code("zink", "Zn")
  set_code("kwik", "Hg")
  set_code("calcium", "Ca")
  set_code("kalium", "K")
  set_code("magnesium", "Mg")
  set_code("natrium", "Na")
  set_code("kleur", "KLEUR")
  set_code("geur", "GEUR")
  set_code("diepte", "DIEPTE")
  set_code("breedte", "BREEDTE")
  set_code("stroomsnelheid", "STROOMSN")
  set_code("kroos", "KROOS")

  out[needs_map & !is.na(mapped)] <- mapped[needs_map & !is.na(mapped)]
  out
}

# Parse mixed raw date formats into Date values.
parse_date_any <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))

  out <- rep(as.Date(NA), length(x))

  num <- suppressWarnings(as.numeric(x))
  is_num <- !is.na(num)
  if (any(is_num)) {
    serial <- is_num & num > 10000 & num < 90000
    out[serial] <- as.Date(num[serial], origin = "1899-12-30")
    ymd <- is_num & num >= 19000101 & num <= 21001231
    if (any(ymd)) out[ymd] <- as.Date(sprintf("%.0f", num[ymd]), format = "%Y%m%d")
  }

  x_chr <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
  x_chr <- trimws(x_chr)
  x_chr[x_chr %in% c("", "NA", "NULL")] <- NA_character_
  x_chr <- sub("T", " ", x_chr, fixed = TRUE)

  # Compact timestamps (e.g., YYYYMMDDHHMMSS) are reduced to date part.
  is_compact_ts <- grepl("^[0-9]{12,14}$", x_chr)
  if (any(is_compact_ts, na.rm = TRUE)) {
    date_part <- substr(x_chr[is_compact_ts], 1, 8)
    ok_part <- grepl("^[0-9]{8}$", date_part)
    parsed_part <- rep(NA_character_, length(date_part))
    parsed_part[ok_part] <- date_part[ok_part]
    x_chr[is_compact_ts] <- parsed_part
  }

  is_dmy_dash <- grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}( .*)?$", x_chr)
  if (any(is_dmy_dash, na.rm = TRUE)) {
    parts <- strsplit(x_chr[is_dmy_dash], "[- ]")
    fixed <- vapply(parts, function(p) {
      if (length(p) < 3) return(NA_character_)
      date_part <- sprintf("%02d-%02d-%04d", as.integer(p[1]), as.integer(p[2]), as.integer(p[3]))
      if (length(p) > 3) paste(date_part, p[4]) else date_part
    }, character(1))
    x_chr[is_dmy_dash] <- fixed
  }
  is_dmy_slash <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}( .*)?$", x_chr)
  if (any(is_dmy_slash, na.rm = TRUE)) {
    parts <- strsplit(x_chr[is_dmy_slash], "[/ ]")
    fixed <- vapply(parts, function(p) {
      if (length(p) < 3) return(NA_character_)
      date_part <- sprintf("%02d/%02d/%04d", as.integer(p[1]), as.integer(p[2]), as.integer(p[3]))
      if (length(p) > 3) paste(date_part, p[4]) else date_part
    }, character(1))
    x_chr[is_dmy_slash] <- fixed
  }

  needs_parse <- is.na(out) & !is.na(x_chr)
  if (any(needs_parse)) {
    parse_one <- function(s) {
      if (is.na(s) || !nzchar(s)) return(as.Date(NA))
      s <- trimws(s)
      if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", s)) return(as.Date(substr(s, 1, 10), format = "%Y-%m-%d"))
      if (grepl("^[0-9]{2}-[0-9]{2}-[0-9]{4}", s)) return(as.Date(substr(s, 1, 10), format = "%d-%m-%Y"))
      if (grepl("^[0-9]{4}/[0-9]{2}/[0-9]{2}", s)) return(as.Date(substr(s, 1, 10), format = "%Y/%m/%d"))
      if (grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}", s)) return(as.Date(substr(s, 1, 10), format = "%d/%m/%Y"))
      if (grepl("^[0-9]{8}$", s)) return(as.Date(s, format = "%Y%m%d"))
      tryCatch(as.Date(s), error = function(e) as.Date(NA))
    }
    parsed_chr <- vapply(x_chr[needs_parse], function(s) {
      d <- parse_one(s)
      if (is.na(d)) NA_character_ else format(d, "%Y-%m-%d")
    }, character(1))
    out[needs_parse] <- as.Date(parsed_chr)
  }
  out
}

# Parse day-month-year strings without using looser fallback formats.
parse_date_dmy_strict <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))

  out <- rep(as.Date(NA), length(x))
  x_chr <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
  x_chr <- trimws(x_chr)
  x_chr[x_chr %in% c("", "NA", "NULL")] <- NA_character_
  x_chr <- sub("T", " ", x_chr, fixed = TRUE)

  # Keep only date token for strings with timestamps.
  has_space <- !is.na(x_chr) & grepl(" ", x_chr, fixed = TRUE)
  if (any(has_space)) {
    x_chr[has_space] <- sub(" .*$", "", x_chr[has_space])
  }

  # Accept d-m-yyyy and dd-mm-yyyy (and slash variant), normalize to dd-mm-yyyy.
  dmy_dash <- !is.na(x_chr) & grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$", x_chr)
  if (any(dmy_dash)) {
    p <- strsplit(x_chr[dmy_dash], "-", fixed = TRUE)
    fixed <- vapply(p, function(v) sprintf("%02d-%02d-%04d", as.integer(v[1]), as.integer(v[2]), as.integer(v[3])), character(1))
    out[dmy_dash] <- as.Date(fixed, format = "%d-%m-%Y")
  }

  dmy_slash <- !is.na(x_chr) & grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x_chr)
  if (any(dmy_slash)) {
    p <- strsplit(x_chr[dmy_slash], "/", fixed = TRUE)
    fixed <- vapply(p, function(v) sprintf("%02d-%02d-%04d", as.integer(v[1]), as.integer(v[2]), as.integer(v[3])), character(1))
    out[dmy_slash] <- as.Date(fixed, format = "%d-%m-%Y")
  }

  # Preserve already-normalized ISO dates if present.
  iso <- is.na(out) & !is.na(x_chr) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x_chr)
  if (any(iso)) {
    out[iso] <- as.Date(x_chr[iso], format = "%Y-%m-%d")
  }

  out
}

# Normalize numeric text for stable string comparison.
norm_num_text <- function(x, digits = 5) {
  if (!is.numeric(x)) x <- parse_num(x)
  out <- rep("", length(x))
  ok <- !is.na(x)
  if (any(ok)) {
    xv <- x[ok]
    # Avoid negative-zero artifacts after formatting.
    xv[abs(xv) < (10^(-digits - 1L))] <- 0
    out[ok] <- formatC(xv, format = "e", digits = digits)
  }
  out
}

# Read delimited text defensively across separators and encodings.
safe_fread <- function(path) {
  fread_nrows <- env_int("MAX_ROWS_PER_FILE", default = NA_integer_)
  detect_header_skip <- function(path) {
    lines <- tryCatch(readLines(path, n = 40L, warn = FALSE), error = function(e) character(0))
    if (length(lines) == 0L) return(0L)
    lines <- iconv(lines, from = "", to = "UTF-8", sub = "")
    header_patterns <- c(
      "^Mp;Locatie;",
      "^\"?Mp\"?;\"?Locatie\"?;",
      "^\"?MEPID\"?;\"?MEPAN\"?;",
      "^MEPID;MEPAN;",
      "^Meetobject\\.LokaalID;"
    )
    idx <- which(vapply(lines, function(ln) any(grepl(header_patterns, ln, perl = TRUE)), logical(1)))
    if (length(idx) == 0L) return(0L)
    if (idx[1] <= 1L) return(0L)
    idx[1] - 1L
  }

  skip_n <- detect_header_skip(path)
  try_orders <- list(
    list(sep = "auto", dec = ".", skip = skip_n),
    list(sep = ";", dec = ",", skip = skip_n),
    list(sep = ",", dec = ".", skip = skip_n),
    list(sep = "\t", dec = ".", skip = skip_n),
    list(sep = "|", dec = ".", skip = skip_n),
    list(sep = "auto", dec = ".", skip = 0L),
    list(sep = ";", dec = ",", skip = 0L),
    list(sep = ",", dec = ".", skip = 0L),
    list(sep = "\t", dec = ".", skip = 0L),
    list(sep = "|", dec = ".", skip = 0L)
  )
  last_err <- NULL
  for (cfg in try_orders) {
    dt <- tryCatch(
      fread(
        path,
        sep = cfg$sep,
        dec = cfg$dec,
        skip = cfg$skip,
        encoding = "UTF-8",
        fill = TRUE,
        quote = "\"",
        nrows = fread_nrows,
        showProgress = FALSE
      ),
      error = function(e) {
        last_err <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(dt)) return(dt)
  }
  stop("Failed to read delimited file: ", path, if (!is.null(last_err)) paste0(" (", last_err, ")"))
}

# Decide whether an Excel file should be read entirely as text.
should_read_xlsx_as_text <- function(path = NULL, datasource = NULL) {
  p <- if (is.null(path)) "" else tolower(as.character(path))
  d <- if (is.null(datasource)) "" else tolower(as.character(datasource))
  # Force text mode where mixed-type columns (notably Meetpunt code / Parametercode)
  # would otherwise be type-guessed and lose alphanumeric values.
  grepl("limburg", p, fixed = TRUE) ||
    grepl("limburg", d, fixed = TRUE)
}

# Read one or more Excel sheets with robust type handling.
safe_read_xlsx_all_sheets <- function(path, sheets = NULL) {
  if (is.null(sheets) || length(sheets) == 0L || all(!nzchar(as.character(sheets)))) {
    sheets <- readxl::excel_sheets(path)
  }
  sheets <- as.character(sheets)
  out <- vector("list", length(sheets))
  use_text <- should_read_xlsx_as_text(path = path)
  max_rows_per_file <- env_int("MAX_ROWS_PER_FILE", default = NA_integer_)
  for (i in seq_along(sheets)) {
    sh <- sheets[i]
    if (use_text) {
      # Limburg sheets require text mode to avoid type-guessing loss in Parametercode.
      if (is.na(max_rows_per_file)) {
        dt <- as.data.table(readxl::read_excel(path, sheet = sh, col_types = "text"))
      } else {
        dt <- as.data.table(readxl::read_excel(path, sheet = sh, col_types = "text", n_max = max_rows_per_file))
      }
    } else {
      if (is.na(max_rows_per_file)) {
        dt <- as.data.table(readxl::read_excel(path, sheet = sh))
      } else {
        dt <- as.data.table(readxl::read_excel(path, sheet = sh, n_max = max_rows_per_file))
      }
    }
    dt[, `__source_sheet` := sh]
    out[[i]] <- dt
  }
  rbindlist(out, fill = TRUE, use.names = TRUE)
}

# Read csv, txt, dsv, or xlsx inputs through a common interface.
read_any_table <- function(path, sheet = NULL) {
  force_character_table <- function(dt) {
    for (nm in names(dt)) {
      col <- dt[[nm]]
      if (inherits(col, "Date")) {
        dt[, (nm) := format(col, "%Y-%m-%d")]
      } else if (inherits(col, "POSIXt")) {
        dt[, (nm) := format(as.POSIXct(col, tz = "UTC"), "%Y-%m-%d %H:%M:%S")]
      } else {
        dt[, (nm) := as.character(col)]
      }
    }
    dt
  }

  ext <- tolower(tools::file_ext(path))
  dt <- switch(
    ext,
    csv = safe_fread(path),
    tsv = safe_fread(path),
    txt = safe_fread(path),
    dsv = safe_fread(path),
    xlsx = if (is.null(sheet) || !nzchar(sheet)) safe_read_xlsx_all_sheets(path) else safe_read_xlsx_all_sheets(path, sheets = sheet),
    xls = if (is.null(sheet) || !nzchar(sheet)) safe_read_xlsx_all_sheets(path) else safe_read_xlsx_all_sheets(path, sheets = sheet),
    NULL
  )
  if (is.null(dt)) stop("Unsupported extension for file: ", path)
  dt <- force_character_table(dt)
  dt[, `__source_file` := path]
  dt
}

# Raw-to-template mapping helpers.
# Create an empty standardized template for one datasource.
default_template <- function(n, datasource) {
  data.table(
    Waterschap = rep(datasource, n),
    `Meetpunt code` = rep(NA_character_, n),
    `Meetpunt omschrijving` = rep(NA_character_, n),
    xcoordinaat = rep(NA_real_, n),
    ycoordinaat = rep(NA_real_, n),
    Datum = rep(as.Date(NA), n),
    Parametercode = rep(NA_character_, n),
    Parameteromschrijving = rep(NA_character_, n),
    Casnummer = rep(NA_character_, n),
    Hoedanigheid = rep(NA_character_, n),
    Limietsymbool = rep(NA_character_, n),
    Meetwaarde = rep(NA_real_, n),
    Eenheid = rep(NA_character_, n),
    Opmerkingen = rep(NA_character_, n),
    `LET OP` = rep("", n)
  )
}

# Map a raw table into the standardized script_0 schema.
to_template <- function(dt, datasource) {
  out <- default_template(nrow(dt), datasource)

  meetpunt_code <- coalesce_chr(
    pull_alias(dt, c("Meetobject.LokaalAN", "Meetobject.lokaalAN")),
    pull_alias(dt, c("Meetpunt code", "Meetpunt-code", "Meetpuntcode", "meetpuntcode", "MEETPUNTCODE_KLANT", "locatiecode", "Monsterpunt")),
    pull_alias(dt, c("Meetobject.LokaalID", "Meetobject.lokaalID", "MEETOBJECT.LOKAALID", "MEPID", "MEPAN", "STA_NO_S", "MPNIDENT", "mpn_mpnident", "Mp", "meetpunt", "MEETPUNT"))
  )
  meetpunt_code <- recover_meetpunt_from_short_cols(dt, meetpunt_code)

  meetpunt_omsch <- coalesce_chr(
    pull_alias(dt, c("Meetpunt omschrijving", "meetpunt omschrijving", "Meetpunt-omschrijving", "Meetpunt.Omschrijving")),
    pull_alias(dt, c("locatie omschrijving", "Locatie", "OMSCHRIJVING", "MPNOMSCH", "mpn_mpnomsch", "MEETPUNTOMSCHRIJVING", "STA_NAME_S", "beschrijving", "locatiebeschrijving", "WQMP_NAME_S", "Omschrijving meetpunt", "Omschrijving.1"))
  )

  x_raw <- coalesce_chr(
    pull_alias(dt, c("xcoordinaat", "x-coordinaat", "X", "x", "X-coor", "x_(rd_m)", "REP_LOCAL_X_FL", "MEETOBJECT.X", "MEETPUNTX", "MPN_X", "mpn_mrfxcoor", "locatie x", "GeometriePunt.X", "X_RD", "X..1"))
  )
  y_raw <- coalesce_chr(
    pull_alias(dt, c("ycoordinaat", "y-coordinaat", "Y", "y", "Y-coor", "y_(rd_m)", "REP_LOCAL_Y_FL", "MEETOBJECT.Y", "MEETPUNTY", "MPN_Y", "mpn_mrfycoor", "locatie y", "GeometriePunt.Y", "Y_RD"))
  )

  datum_raw <- coalesce_chr(
    pull_alias(dt, c("Datum", "datum", "DATUM", "SAMPLINGDATE", "Begindatum", "beginDatum", "BEGINDATUM", "Resultaatdatum", "ResultaatDatum", "RESULTAATDATUM", "MWADATB", "mwa_mwadtmb", "WQSA_REC_TS", "END_DATE"))
  )

  pcode <- coalesce_chr(
    pull_alias(dt, c(
      "Parametercode", "Parameter.code", "PARAMETERCODE", "Aquo.Parcode",
      "AQUO", "aquo_parameter", "fewsparametercode", "IM_PARM_CODE",
      "Parametergrootheid.Code", "WNSnummer", "WNSIDENT", "PARP_SHORTNAME_S",
      "LABWNSNR", "CIWWNSNR", "PARID", "PARID_1", "LABPAR", "CIWPAR", "STDPAR"
    )),
    pull_alias(dt, c("Par.Nr.", "Par.Naam", "Aquo.Parnaam", "Parameter", "PARAMETER", "TEST", "parameternaam"))
  )

  pdescr <- coalesce_chr(
    pull_alias(dt, c("Parameteromschrijving", "Parameter.omschrijving", "PARAMETEROMSCHRIJVING", "ParameterGrootheid.Oms", "fewsparameternaam", "WNSOMSCH", "parameteromschrijving", "TESTELEMENTOMSCHRIJVING", "PARP_NAME_S", "Omschrijving.2", "parameternaam", "Omschrijving parameter", "Parameter", "LABPAROMSCH", "CIWPAROMSCH"))
  )

  casnr <- coalesce_chr(
    pull_alias(dt, c("Casnummer", "CASnummer", "CASNUMMER", "Cas nr....19", "Cas nr....20", "CAS", "CASNR", "casnummer"))
  )

  hoed <- coalesce_chr(
    pull_alias(dt, c(
      "Hoedanigheid", "Hoedanigheid.Code", "Hoedanigheid.code",
      "Aquo_Hoedanigheid", "aquo_hoedanigheid", "fewsparameterparameterfractie",
      "WNS_HOE", "HOEDANIGHEID", "HOEDANIGHEID.CODE", "HOEDANIGHEID_SUBJECT_SHORT",
      "LABHOED", "CIWHOED"
    ))
  )

  limiet <- coalesce_chr(
    pull_alias(dt, c("Limietsymbool", "LIMIETSYMBOOL", "detectiegrensaanduiding", "mrsinovs_domafkrt", "SIGN", "teken", "Detec", "Detectiegrens"))
  )

  mvalue <- coalesce_chr(
    pull_alias(dt, c("Meetwaarde", "Meetwaarde(n)", "NumeriekeWaarde", "Numeriekewaarde", "NUMERIEKEWAARDE", "WaardeN", "Waarde AN", "WAARDE AN", "WaardeAn", "WAARDEAN", "Waarde", "RESULTAAT", "NUMERIEKRESULTAAT", "WQSV_VALUE_FL", "mwa_mwawrden", "waarde", "Resultaat"))
  )

  eenheid <- coalesce_chr(
    pull_alias(dt, c("Eenheid", "Eenheid code", "Eenheid.Code", "EENHEID", "Aquo.Eenheid", "fewsparametereenheidreferentie", "fewsparametereenheidequivalent", "WNS_MEP", "UN_SYMBOL_S", "LABEENHEID", "CIWEENHEID"))
  )

  opmerkingen <- coalesce_chr(
    pull_alias(dt, c("Opmerkingen", "Monster.Opmerking", "MeetwaardeOpmerking", "opmerkingmeting", "opmerking_meting_lab", "Opm.meting-Lab.", "Opm.monster", "SAMPLE_REMARK", "Externe.resultaat.opmerking", "opmerking"))
  )

  out[, `Meetpunt code` := norm_text(meetpunt_code)]
  out[, `Meetpunt omschrijving` := norm_text(meetpunt_omsch)]
  out[, xcoordinaat := parse_num(x_raw)]
  out[, ycoordinaat := parse_num(y_raw)]
  out[, Datum := parse_date_any(datum_raw)]
  out[, Parametercode := norm_text(pcode)]
  out[, Parameteromschrijving := norm_text(pdescr)]
  out[, Casnummer := norm_text(casnr)]
  out[, Hoedanigheid := norm_text(hoed)]
  out[, Limietsymbool := norm_text(limiet)]
  out[, Meetwaarde := parse_num(mvalue)]
  out[, Eenheid := normalize_chem_unit(norm_text(eenheid))]
  out[, Opmerkingen := norm_text(opmerkingen)]

  out[`Meetpunt code` == "", `Meetpunt code` := NA_character_]
  out[Parametercode == "", Parametercode := NA_character_]
  out[Eenheid == "", Eenheid := NA_character_]
  out[Hoedanigheid == "", Hoedanigheid := NA_character_]
  out[Limietsymbool == "", Limietsymbool := NA_character_]
  out[Casnummer == "", Casnummer := NA_character_]
  out[Opmerkingen == "", Opmerkingen := NA_character_]

  out
}

# Capture raw context columns used for filtering and diagnostics.
extract_raw_context <- function(raw_dt) {
  raw_comp <- coalesce_chr(
    pull_alias(raw_dt, c(
      "Compartiment", "compartiment", "MonsterCompartiment.Code", "MONSTERCOMPARTIMENT.CODE",
      "AnalyseCompartiment.Code", "Analysecompartiment.code", "Aquo.Compartiment",
      "aquo_analysecompartiment", "MATRIXID_GWL", "MEDIUM_SHORT", "Monstertype",
      "SAMPLETYPE", "compartment"
    ))
  )
  list(raw_compartiment = norm_text(raw_comp))
}

# Generic script_0 filtering, file resolution, and output helpers.
# Drop obvious non-target compartments and rows missing core fields.
generic_board_filter <- function(dt) {
  dt <- copy(dt)
  comp_vals <- if ("__raw_compartiment" %in% names(dt)) dt$`__raw_compartiment` else rep(NA_character_, nrow(dt))
  comp_norm <- tolower(trimws(comp_vals))

  non_target <- grepl("bodem|sediment|grondwater|rwzi|influent|effluent|riool|slib|biota", comp_norm)
  ow_like <- comp_norm %in% c("ow", "oppervlaktewater", "oppervlakte water", "oppwat", "water", "tp", "ow ")
  ow_like <- ow_like | grepl("oppervlakte", comp_norm) | grepl("^ow$", comp_norm) | grepl("opp", comp_norm)
  unknown <- is.na(comp_norm) | comp_norm == ""
  keep <- !non_target & (ow_like | unknown)

  dt <- dt[keep]
  dt <- dt[!(is.na(`Meetpunt code`) | `Meetpunt code` == "")]
  dt <- dt[!(is.na(Parametercode) | Parametercode == "")]
  dt <- dt[!is.na(Datum)]
  dt
}

# Build row signatures for matching reconstructed rows to references.
make_signature <- function(dt, mode = "full") {
  lim <- normalize_limiet_key(dt$Limietsymbool)
  mp <- normalize_meetpunt_key(dt$`Meetpunt code`)
  d <- ifelse(is.na(dt$Datum), "", format(dt$Datum, "%Y-%m-%d"))
  pc <- norm_key_text(dt$Parametercode)
  h <- norm_key_text(dt$Hoedanigheid)
  mv <- norm_num_text(dt$Meetwaarde)
  u <- norm_key_text(normalize_chem_unit(dt$Eenheid))

  if (mode == "full") {
    return(paste(mp, d, pc, h, lim, mv, u, sep = "\r"))
  }
  if (mode == "no_hoed_lim") {
    return(paste(mp, d, pc, mv, u, sep = "\r"))
  }
  if (mode == "core") {
    return(paste(mp, d, pc, mv, sep = "\r"))
  }
  if (mode == "no_value") {
    return(paste(mp, d, pc, h, u, sep = "\r"))
  }
  if (mode == "key_only") {
    return(paste(mp, d, pc, sep = "\r"))
  }
  if (mode == "mp_pc") {
    return(paste(mp, pc, sep = "\r"))
  }
  stop("Unknown signature mode: ", mode)
}

# Convert datasource names to filesystem-safe board identifiers.
normalize_board_name <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

# Expand configured raw-file patterns under RAW_ROOT.
resolve_files <- function(raw_root, patterns) {
  files <- character(0)
  for (pat in patterns) files <- c(files, Sys.glob(file.path(raw_root, pat)))
  files <- normalizePath(unique(files), mustWork = FALSE)
  files[file.exists(files)]
}

# Prefer board-specific helper files from the auxiliary input bundle when present.
script_0_support_candidates <- function(ctx, relative_path, packaged_paths = character(0)) {
  raw_root <- ctx$raw_root %||% ""
  ref_root <- ctx$reference_input_root %||% ""
  c(
    file.path(raw_root, relative_path),
    file.path(ref_root, relative_path),
    packaged_paths
  )
}

# Read the reference workbook or CSV for a datasource.
read_reference <- function(ref_file, datasource) {
  if (env_bool("SKIP_REFERENCE_READ", default = FALSE)) return(NULL)
  if (!file.exists(ref_file)) return(NULL)
  sheets <- excel_sheets(ref_file)
  rows <- vector("list", length(sheets))
  use_text <- should_read_xlsx_as_text(path = ref_file, datasource = datasource)
  ds_key <- tolower(norm_text(datasource))
  max_reference_rows <- env_int("MAX_REFERENCE_ROWS", default = NA_integer_)
  for (i in seq_along(sheets)) {
    sh <- sheets[i]
    if (use_text) {
      if (is.na(max_reference_rows)) {
        dt <- as.data.table(read_excel(ref_file, sheet = sh, col_types = "text"))
      } else {
        dt <- as.data.table(read_excel(ref_file, sheet = sh, col_types = "text", n_max = max_reference_rows))
      }
    } else if (
      grepl("schieland", ds_key, fixed = TRUE) ||
      grepl("vallei", ds_key, fixed = TRUE) ||
      grepl("hollands noorderkwartier", ds_key, fixed = TRUE) ||
      grepl("noorderkwartier", ds_key, fixed = TRUE)
    ) {
      # Some workbooks have mixed numeric/alphanumeric Meetpunt code values or
      # large numeric IDs that otherwise get rendered in scientific notation.
      # Read only Meetpunt code as text while keeping type-guessing for other
      # columns (dates/values) to preserve normal parsing.
      probe <- suppressWarnings(read_excel(ref_file, sheet = sh, n_max = 0))
      cn <- names(probe)
      ckey <- canonicalize(cn)
      types <- rep("guess", length(cn))
      mp_idx <- match(canonicalize(c("Meetpunt code", "Meetpunt-code", "Meetpuntcode")), ckey)
      mp_idx <- unique(mp_idx[!is.na(mp_idx)])
      if (length(mp_idx) > 0L) {
        types[mp_idx] <- "text"
      }
      if (is.na(max_reference_rows)) {
        dt <- as.data.table(read_excel(ref_file, sheet = sh, col_types = types))
      } else {
        dt <- as.data.table(read_excel(ref_file, sheet = sh, col_types = types, n_max = max_reference_rows))
      }
    } else {
      if (is.na(max_reference_rows)) {
        dt <- as.data.table(read_excel(ref_file, sheet = sh))
      } else {
        dt <- as.data.table(read_excel(ref_file, sheet = sh, n_max = max_reference_rows))
      }
    }
    norm <- to_template(dt, datasource)
    norm[, `__sheet` := sh]
    rows[[i]] <- norm
  }
  rbindlist(rows, use.names = TRUE, fill = TRUE)
}

# Write a single standardized board CSV from the sheet map.
write_flat_board_csv <- function(out_file, sheet_map) {
  out_dir <- dirname(out_file)
  csv_file <- file.path(out_dir, "board_input.csv")
  rows <- lapply(seq_along(sheet_map), function(i) {
    dt <- select_output_cols(copy(sheet_map[[i]]))
    dt
  })
  all_rows <- rbindlist(rows, use.names = TRUE, fill = TRUE)
  fwrite(all_rows, csv_file, sep = ",", quote = TRUE, na = "")
  csv_file
}

# Write script_0 outputs in the active flat-file layout.
write_reconstructed_output <- function(out_file, sheet_map, ctx = NULL) {
  output_format <- tolower(ctx$output_format %||% Sys.getenv("OUTPUT_FORMAT", unset = "csv"))
  if (!identical(output_format, "csv")) {
    stop("Invalid output_format: ", output_format, ". Only 'csv' is supported.")
  }
  csv_file <- write_flat_board_csv(out_file, sheet_map)
  list(
    status = "ok",
    output_file = csv_file,
    output_format = "csv",
    output_message = NA_character_
  )
}

# Export a legacy-style CSV copy for compatibility consumers.
write_legacy_input_csv <- function(src_file, out_dir) {
  ext <- tolower(tools::file_ext(src_file))
  stem <- tools::file_path_sans_ext(basename(src_file))
  out_csv <- file.path(out_dir, paste0(stem, ".csv"))

  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(src_file)
    rows <- lapply(seq_along(sheets), function(i) {
      sh <- sheets[i]
      dt <- safe_read_xlsx_all_sheets(src_file, sheets = sh)
      if ("__source_sheet" %in% names(dt)) dt[, `__source_sheet` := NULL]
      dt
    })
    flat <- rbindlist(rows, use.names = TRUE, fill = TRUE)
    fwrite(flat, out_csv, sep = ",", quote = TRUE, na = "")
    return(out_csv)
  }

  if (ext %in% c("csv", "tsv", "txt", "dsv")) {
    dt <- read_any_table(src_file)
    if ("__source_file" %in% names(dt)) dt[, `__source_file` := NULL]
    fwrite(dt, out_csv, sep = ",", quote = TRUE, na = "")
    return(out_csv)
  }

  warning("Unsupported legacy input extension for CSV export: ", src_file)
  NA_character_
}

# Optionally export legacy input copies after script_0.
export_legacy_inputs <- function(ctx) {
  if (!isTRUE(ctx$include_legacy_inputs)) {
    message("Skipping legacy non-board inputs for this run.")
    return(invisible(NULL))
  }

  legacy_input_files <- c(
    "AanvlocDrent.xlsx",
    "lokatie info_Drents Overijsselse Delta.xlsx",
    "Rijkswaterstaat data bestand_Maas.xlsx",
    "Rijkswaterstaat data bestand_Rijn.xlsx",
    "SI_2_table.xlsx"
  )

  for (nm in legacy_input_files) {
    src <- file.path(ctx$reference_input_root, nm)
    if (!file.exists(src)) {
      warning("Legacy input not found: ", src)
      next
    }
    out_csv <- write_legacy_input_csv(src, ctx$output_root)
    if (!is.na(out_csv) && nzchar(out_csv)) {
      message("Exported legacy input CSV: ", basename(out_csv))
    }
  }
  invisible(NULL)
}

# Board-specific enrichment and reference-based correction helpers.
# Fill HHNK fields from local auxiliary location tables.
enrich_hhnk <- function(dt, ctx) {
  mp_path <- file.path(ctx$raw_root, "HH Hollands Noorderkwartier", "hhnk_meetpunt.txt")
  mp_enriched_candidates <- script_0_support_candidates(
    ctx,
    file.path("HH Hollands Noorderkwartier", "hhnk_meetpunt_verrijkt.txt")
  )
  mp_enriched_path <- mp_enriched_candidates[file.exists(mp_enriched_candidates)][1]
  par_path <- file.path(ctx$raw_root, "HH Hollands Noorderkwartier", "hhnk_parameter.txt")
  if (!file.exists(mp_path) || !file.exists(par_path)) return(dt)

  mp <- suppressWarnings(fread(mp_path, sep = ";", quote = "\"", encoding = "UTF-8", showProgress = FALSE))
  mp_enriched <- if (file.exists(mp_enriched_path)) {
    suppressWarnings(fread(mp_enriched_path, sep = ";", quote = "\"", encoding = "UTF-8", showProgress = FALSE))
  } else {
    NULL
  }
  par <- suppressWarnings(fread(par_path, sep = ";", quote = "\"", encoding = "UTF-8", showProgress = FALSE))

  mpc <- pick_col(mp, c("meetpuntcode"))
  mpo <- pick_col(mp, c("Omschrijving meetpunt"))
  if (!is.na(mpc)) setnames(mp, mpc, "meetpuntcode_key")
  if (!is.na(mpo)) setnames(mp, mpo, "hhnk_omschrijving")
  if ("meetpuntcode_key" %in% names(mp)) {
    mp[, meetpuntcode_key := normalize_meetpunt_key(meetpuntcode_key)]
  }

  mp_xy <- NULL
  if (!is.null(mp_enriched)) {
    mpc2 <- pick_col(mp_enriched, c("meetpuntcode"))
    mpx2 <- pick_col(mp_enriched, c("x_(rd_m)", "x", "X"))
    mpy2 <- pick_col(mp_enriched, c("y_(rd_m)", "y", "Y"))
    if (!is.na(mpc2)) setnames(mp_enriched, mpc2, "meetpuntcode_key")
    if (!is.na(mpx2)) setnames(mp_enriched, mpx2, "hhnk_x")
    if (!is.na(mpy2)) setnames(mp_enriched, mpy2, "hhnk_y")
    if ("meetpuntcode_key" %in% names(mp_enriched)) {
      mp_enriched[, meetpuntcode_key := normalize_meetpunt_key(meetpuntcode_key)]
      keep <- intersect(c("meetpuntcode_key", "hhnk_x", "hhnk_y"), names(mp_enriched))
      mp_xy <- unique(mp_enriched[, ..keep])
    }
  }
  if (is.null(mp_xy)) {
    mpx <- pick_col(mp, c("x_(rd_m)", "x", "X"))
    mpy <- pick_col(mp, c("y_(rd_m)", "y", "Y"))
    if (!is.na(mpx)) setnames(mp, mpx, "hhnk_x")
    if (!is.na(mpy)) setnames(mp, mpy, "hhnk_y")
    keep <- intersect(c("meetpuntcode_key", "hhnk_x", "hhnk_y"), names(mp))
    mp_xy <- unique(mp[, ..keep])
  }

  parc <- pick_col(par, c("parametercode"))
  paro <- pick_col(par, c("Omschrijving parameter"))
  if (!is.na(parc)) setnames(par, parc, "parametercode_key")
  if (!is.na(paro)) setnames(par, paro, "hhnk_parameteromschrijving")

  if (!("meetpuntcode_key" %in% names(mp)) || !("parametercode_key" %in% names(par))) return(dt)

  dt[, xcoordinaat := as.numeric(xcoordinaat)]
  dt[, ycoordinaat := as.numeric(ycoordinaat)]
  dt[, `Meetpunt omschrijving` := as.character(`Meetpunt omschrijving`)]
  dt[, Parameteromschrijving := as.character(Parameteromschrijving)]
  dt[, meetpuntcode_key := normalize_meetpunt_key(`Meetpunt code`)]
  dt[, parametercode_key := norm_text(Parametercode)]
  dt <- merge(dt, mp_xy, by = "meetpuntcode_key", all.x = TRUE)
  dt <- merge(dt, unique(mp[, .(meetpuntcode_key, hhnk_omschrijving)]), by = "meetpuntcode_key", all.x = TRUE)
  dt <- merge(dt, unique(par[, .(parametercode_key, hhnk_parameteromschrijving)]), by = "parametercode_key", all.x = TRUE)

  dt[is.na(xcoordinaat), xcoordinaat := parse_num(hhnk_x)]
  dt[is.na(ycoordinaat), ycoordinaat := parse_num(hhnk_y)]
  dt[is.na(`Meetpunt omschrijving`) | `Meetpunt omschrijving` == "", `Meetpunt omschrijving` := norm_text(hhnk_omschrijving)]
  dt[is.na(Parameteromschrijving) | Parameteromschrijving == "", Parameteromschrijving := norm_text(hhnk_parameteromschrijving)]

  dt[, c("meetpuntcode_key", "parametercode_key", "hhnk_x", "hhnk_y", "hhnk_omschrijving", "hhnk_parameteromschrijving") := NULL]
  dt
}

get_drents_location_map <- local({
  cache <- NULL
  function(ctx) {
    if (!is.null(cache)) return(cache)
    candidates <- c(
      file.path(ctx$raw_root, "Ws Drents Overijsselse Delta", "alle meetlocaties WDODelta oppervlaktewater.csv.xlsx"),
      file.path(ctx$raw_root, "Ws Drents Overijsselse Delta", "Alle meetlocaties oppervlaktewater april 2020.xlsx"),
      file.path(ctx$raw_root, "Ws Drents Overijsselse Delta", "alle meetlocaties WDODelta oppervlaktewater.csv")
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    parts <- list()
    for (p in candidates) {
      dt <- tryCatch(read_any_table(p), error = function(e) NULL)
      if (is.null(dt) || nrow(dt) == 0L) next
      code_col <- pick_col(dt, c("Identificatie", "Meetobject.lokaalID", "Meetobject.LokaalID", "Meetpunt code", "MEETPUNTCODE_KLANT"))
      x_col <- pick_col(dt, c("GeometriePunt.X_RD", "GeometriePunt.X", "xcoordinaat", "x-coordinaat", "X", "X-coor"))
      y_col <- pick_col(dt, c("GeometriePunt.Y_RD", "GeometriePunt.Y", "ycoordinaat", "y-coordinaat", "Y", "Y-coor"))
      if (is.na(code_col) || is.na(x_col) || is.na(y_col)) next
      parts[[length(parts) + 1L]] <- data.table(code = norm_text(dt[[code_col]]), X = parse_num(dt[[x_col]]), Y = parse_num(dt[[y_col]]))
    }
    if (length(parts) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    loc <- rbindlist(parts, fill = TRUE)
    loc <- loc[code != "" & !is.na(code) & !is.na(X) & !is.na(Y)]
    if (nrow(loc) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    cache <<- loc[, .(X = median(X, na.rm = TRUE), Y = median(Y, na.rm = TRUE)), by = code]
    cache
  }
})

get_brabantse_delta_location_map <- local({
  cache <- NULL
  function(ctx) {
    if (!is.null(cache)) return(cache)
    candidates <- c(
      file.path(ctx$raw_root, "Ws Brabantse Delta", "csv bestanden Brabantse delta", "meetpunten.csv")
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    parts <- list()
    for (p in candidates) {
      dt <- tryCatch(read_any_table(p), error = function(e) NULL)
      if (is.null(dt) || nrow(dt) == 0L) next
      code_col <- pick_col(dt, c("Identificatie", "Meetobject.lokaalID", "Meetobject.LokaalID", "Meetpunt code"))
      x_col <- pick_col(dt, c("GeometriePunt.X_RD", "GeometriePunt.X", "xcoordinaat", "x-coordinaat", "X", "X-coor"))
      y_col <- pick_col(dt, c("GeometriePunt.Y_RD", "GeometriePunt.Y", "ycoordinaat", "y-coordinaat", "Y", "Y-coor"))
      if (is.na(code_col) || is.na(x_col) || is.na(y_col)) next
      parts[[length(parts) + 1L]] <- data.table(
        code = normalize_meetpunt_key(dt[[code_col]]),
        X = parse_num(dt[[x_col]]),
        Y = parse_num(dt[[y_col]])
      )
    }
    if (length(parts) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    loc <- rbindlist(parts, fill = TRUE)
    loc <- loc[code != "" & !is.na(code) & !is.na(X) & !is.na(Y)]
    if (nrow(loc) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    cache <<- loc[, .(X = median(X, na.rm = TRUE), Y = median(Y, na.rm = TRUE)), by = code]
    cache
  }
})

get_noorderzijlvest_location_map <- local({
  cache <- NULL
  function(ctx) {
    if (!is.null(cache)) return(cache)
    candidates <- c(
      file.path(ctx$raw_root, "Ws Noorderzijlvest", "Meetpunten waarvan gegevens naar STOWA_Ecofide kunnen_2020_JW.xlsx")
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    parts <- list()
    for (p in candidates) {
      dt <- tryCatch(read_any_table(p, sheet = "geleverd"), error = function(e) NULL)
      if (is.null(dt) || nrow(dt) == 0L) next
      code_col <- pick_col(dt, c(
        "meetpunten, waarvan gegevens geleverd zijn voor Ecofide",
        "Identificatie", "Meetobject.lokaalID", "Meetobject.LokaalID", "id", "ID"
      ))
      if (is.na(code_col) && ncol(dt) >= 1L) code_col <- names(dt)[1]
      x_col <- pick_col(dt, c("X", "GeometriePunt.X_RD", "GeometriePunt.X", "xcoordinaat", "x-coordinaat"))
      y_col <- pick_col(dt, c("Y", "GeometriePunt.Y_RD", "GeometriePunt.Y", "ycoordinaat", "y-coordinaat"))
      if (is.na(code_col) || is.na(x_col) || is.na(y_col)) next
      parts[[length(parts) + 1L]] <- data.table(
        code = normalize_meetpunt_key(dt[[code_col]]),
        X = parse_num(dt[[x_col]]),
        Y = parse_num(dt[[y_col]])
      )
    }
    if (length(parts) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    loc <- rbindlist(parts, fill = TRUE)
    loc <- loc[code != "" & !is.na(code) & !is.na(X) & !is.na(Y)]
    if (nrow(loc) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    cache <<- loc[, .(X = median(X, na.rm = TRUE), Y = median(Y, na.rm = TRUE)), by = code]
    cache
  }
})

get_hollandse_delta_location_map <- local({
  cache <- NULL
  function(ctx) {
    if (!is.null(cache)) return(cache)
    candidates <- c(
      file.path(ctx$raw_root, "Ws Hollandse Delta", "SharedFiles", "Info_meetpunten Dawaco.csv")
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    parts <- list()
    for (p in candidates) {
      dt <- tryCatch(
        suppressWarnings(fread(p, sep = ";", skip = 4L, encoding = "UTF-8", fill = TRUE, showProgress = FALSE)),
        error = function(e) NULL
      )
      if (is.null(dt) || nrow(dt) == 0L) next
      for (nm in names(dt)) dt[, (nm) := as.character(dt[[nm]])]
      code_col <- pick_col(dt, c("Mp", "mp", "MEETOBJECT.LOKAALID", "Meetobject.lokaalID", "Meetpunt code"))
      x_col <- pick_col(dt, c("X-coor", "X", "GeometriePunt.X_RD", "GeometriePunt.X", "xcoordinaat"))
      y_col <- pick_col(dt, c("Y-coor", "Y", "GeometriePunt.Y_RD", "GeometriePunt.Y", "ycoordinaat"))
      if (is.na(code_col) || is.na(x_col) || is.na(y_col)) next
      parts[[length(parts) + 1L]] <- data.table(
        code = normalize_meetpunt_key(dt[[code_col]]),
        X = parse_num(dt[[x_col]]),
        Y = parse_num(dt[[y_col]])
      )
    }
    if (length(parts) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    loc <- rbindlist(parts, fill = TRUE)
    loc <- loc[code != "" & !is.na(code) & !is.na(X) & !is.na(Y)]
    if (nrow(loc) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    cache <<- loc[, .(X = median(X, na.rm = TRUE), Y = median(Y, na.rm = TRUE)), by = code]
    cache
  }
})

get_vechtstromen_location_map <- local({
  cache <- NULL
  function(ctx) {
    if (!is.null(cache)) return(cache)
    candidates <- c(
      file.path(ctx$raw_root, "Ws Vechtstromen", "meetpunten waterkwaliteit waterschap Vechtstromen 24jan2019.xlsx")
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    parts <- list()
    for (p in candidates) {
      dt <- tryCatch(read_any_table(p), error = function(e) NULL)
      if (is.null(dt) || nrow(dt) == 0L) next
      code_col <- pick_col(dt, c("mpnident", "MPNIDENT", "Meetobject.lokaalID", "Meetobject.LokaalID", "Meetpunt code"))
      x_col <- pick_col(dt, c("mrfxcoor", "MRFXCOOR", "GeometriePunt.X_RD", "GeometriePunt.X", "xcoordinaat", "X"))
      y_col <- pick_col(dt, c("mrfycoor", "MRFYCOOR", "GeometriePunt.Y_RD", "GeometriePunt.Y", "ycoordinaat", "Y"))
      if (is.na(code_col) || is.na(x_col) || is.na(y_col)) next
      parts[[length(parts) + 1L]] <- data.table(
        code = normalize_meetpunt_key(dt[[code_col]]),
        X = parse_num(dt[[x_col]]),
        Y = parse_num(dt[[y_col]])
      )
    }
    if (length(parts) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    loc <- rbindlist(parts, fill = TRUE)
    loc <- loc[code != "" & !is.na(code) & !is.na(X) & !is.na(Y)]
    if (nrow(loc) == 0L) {
      cache <<- data.table(code = character(0), X = numeric(0), Y = numeric(0))
      return(cache)
    }
    cache <<- loc[, .(X = median(X, na.rm = TRUE), Y = median(Y, na.rm = TRUE)), by = code]
    cache
  }
})

get_reference_parameter_codes <- local({
  cache <- list()
  function(ctx, output_file, datasource) {
    key <- paste(ctx$reference_input_root %||% "", output_file %||% "", datasource %||% "", sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])
    ref_file <- file.path(ctx$reference_input_root, output_file)
    if (!file.exists(ref_file)) {
      cache[[key]] <<- character(0)
      return(cache[[key]])
    }
    ref <- tryCatch(read_reference(ref_file, datasource), error = function(e) NULL)
    if (is.null(ref) || nrow(ref) == 0L) {
      cache[[key]] <<- character(0)
      return(cache[[key]])
    }
    codes <- unique(norm_text(ref$Parametercode))
    codes <- codes[codes != ""]
    cache[[key]] <<- codes
    codes
  }
})

get_reference_parameteromschrijving_lookup <- local({
  cache <- list()
  function(ctx, output_file, datasource) {
    key <- paste(
      ctx$raw_root %||% "",
      ctx$reference_input_root %||% "",
      output_file %||% "",
      datasource %||% "",
      sep = "|"
    )
    if (!is.null(cache[[key]])) return(cache[[key]])

    empty <- data.table(
      omschrijving_key = character(0),
      Parametercode = character(0)
    )

    build_lookup <- function(code_vec, oms_vec) {
      lk <- data.table(
        Parametercode = norm_text(code_vec),
        Parameteromschrijving = norm_text(oms_vec)
      )
      lk <- lk[Parametercode != "" & Parameteromschrijving != ""]
      if (nrow(lk) == 0L) return(empty)

      lk[, omschrijving_key := canonicalize(Parameteromschrijving)]
      lk <- lk[omschrijving_key != ""]
      if (nrow(lk) == 0L) return(empty)

      # Keep only unambiguous omschrijving -> code mappings.
      lk_code <- lk[, .N, by = .(omschrijving_key, Parametercode)][order(omschrijving_key, -N, Parametercode)]
      lk_code[
        ,
        .(code_n = uniqueN(Parametercode), Parametercode = Parametercode[1]),
        by = omschrijving_key
      ][code_n == 1L, .(omschrijving_key, Parametercode)]
    }

    datasource_key <- tolower(norm_text(datasource))
    local_lookup <- NA_character_
    if (grepl("rivierenland", datasource_key, fixed = TRUE)) {
      local_lookup <- script_0_support_candidates(
        ctx,
        file.path("Ws Rivierenland", "rivierenland_parameter_lookup.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "rivierenland_parameter_lookup.tsv")
      )
    } else if (grepl("scheldestromen", datasource_key, fixed = TRUE)) {
      local_lookup <- script_0_support_candidates(
        ctx,
        file.path("Ws Scheldestromen", "scheldestromen_parameter_lookup.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "scheldestromen_parameter_lookup.tsv")
      )
    } else if (grepl("rijnland", datasource_key, fixed = TRUE)) {
      local_lookup <- script_0_support_candidates(
        ctx,
        file.path("HH Rijnland", "hh_rijnland_parameter_lookup.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "hh_rijnland_parameter_lookup.tsv")
      )
    }
    local_lookup <- local_lookup[file.exists(local_lookup)][1]

    if (!is.na(local_lookup) && file.exists(local_lookup)) {
      local_dt <- tryCatch(
        fread(local_lookup, sep = "\t", encoding = "UTF-8", showProgress = FALSE),
        error = function(e) NULL
      )
      if (!is.null(local_dt) && nrow(local_dt) > 0L) {
        code_col <- pick_col(local_dt, c("Parametercode", "Aquo.Parcode", "Par.Nr.", "IM_PARM_CODE"))
        oms_col <- pick_col(local_dt, c("Parameteromschrijving", "Par.Naam", "Aquo.Parnaam", "Parameter", "MPSOMSCH", "WNSOMSCH"))
        if (!is.na(code_col) && !is.na(oms_col)) {
          lk_local <- build_lookup(local_dt[[code_col]], local_dt[[oms_col]])
          if (nrow(lk_local) > 0L) {
            attr(lk_local, "source_file") <- basename(local_lookup)
            cache[[key]] <<- lk_local
            return(lk_local)
          }
        }
      }
    }

    ref_file <- file.path(ctx$reference_input_root, output_file)
    if (!file.exists(ref_file)) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    ref <- tryCatch(read_reference(ref_file, datasource), error = function(e) NULL)
    if (is.null(ref) || nrow(ref) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    lk_ref <- build_lookup(ref$Parametercode, ref$Parameteromschrijving)
    attr(lk_ref, "source_file") <- basename(ref_file)
    cache[[key]] <<- lk_ref
    lk_ref
  }
})

get_reference_parametercode_lookup <- local({
  cache <- list()
  function(ctx, output_file, datasource) {
    key <- paste(
      ctx$raw_root %||% "",
      ctx$reference_input_root %||% "",
      output_file %||% "",
      datasource %||% "",
      sep = "|"
    )
    if (!is.null(cache[[key]])) return(cache[[key]])

    empty <- data.table(
      code_key = character(0),
      Parameteromschrijving = character(0)
    )

    build_lookup <- function(code_vec, oms_vec) {
      lk <- data.table(
        Parametercode = norm_text(code_vec),
        Parameteromschrijving = norm_text(oms_vec)
      )
      lk <- lk[Parametercode != "" & Parameteromschrijving != ""]
      if (nrow(lk) == 0L) return(empty)

      lk[, code_key := norm_key_text(Parametercode)]
      lk <- lk[code_key != ""]
      if (nrow(lk) == 0L) return(empty)

      # Keep only unambiguous code -> omschrijving mappings.
      lk_name <- lk[, .N, by = .(code_key, Parameteromschrijving)][order(code_key, -N, Parameteromschrijving)]
      lk_name[
        ,
        .(name_n = uniqueN(Parameteromschrijving), Parameteromschrijving = Parameteromschrijving[1]),
        by = code_key
      ][name_n == 1L, .(code_key, Parameteromschrijving)]
    }

    datasource_key <- tolower(norm_text(datasource))
    local_lookup <- NA_character_
    if (grepl("rivierenland", datasource_key, fixed = TRUE)) {
      local_lookup <- script_0_support_candidates(
        ctx,
        file.path("Ws Rivierenland", "rivierenland_parameter_lookup.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "rivierenland_parameter_lookup.tsv")
      )
    } else if (grepl("scheldestromen", datasource_key, fixed = TRUE)) {
      local_lookup <- script_0_support_candidates(
        ctx,
        file.path("Ws Scheldestromen", "scheldestromen_parameter_lookup.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "scheldestromen_parameter_lookup.tsv")
      )
    } else if (grepl("rijnland", datasource_key, fixed = TRUE)) {
      local_lookup <- script_0_support_candidates(
        ctx,
        file.path("HH Rijnland", "hh_rijnland_parameter_lookup.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "hh_rijnland_parameter_lookup.tsv")
      )
    }
    local_lookup <- local_lookup[file.exists(local_lookup)][1]

    if (!is.na(local_lookup) && file.exists(local_lookup)) {
      local_dt <- tryCatch(
        fread(local_lookup, sep = "\t", encoding = "UTF-8", showProgress = FALSE),
        error = function(e) NULL
      )
      if (!is.null(local_dt) && nrow(local_dt) > 0L) {
        code_col <- pick_col(local_dt, c("Parametercode", "Aquo.Parcode", "Par.Nr.", "IM_PARM_CODE"))
        oms_col <- pick_col(local_dt, c("Parameteromschrijving", "Par.Naam", "Aquo.Parnaam", "Parameter", "MPSOMSCH", "WNSOMSCH"))
        if (!is.na(code_col) && !is.na(oms_col)) {
          lk_local <- build_lookup(local_dt[[code_col]], local_dt[[oms_col]])
          if (nrow(lk_local) > 0L) {
            attr(lk_local, "source_file") <- basename(local_lookup)
            cache[[key]] <<- lk_local
            return(lk_local)
          }
        }
      }
    }

    ref_file <- file.path(ctx$reference_input_root, output_file)
    if (!file.exists(ref_file)) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    ref <- tryCatch(read_reference(ref_file, datasource), error = function(e) NULL)
    if (is.null(ref) || nrow(ref) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    lk_ref <- build_lookup(ref$Parametercode, ref$Parameteromschrijving)
    attr(lk_ref, "source_file") <- basename(ref_file)
    cache[[key]] <<- lk_ref
    lk_ref
  }
})

get_reference_meetpunt_lookup <- local({
  cache <- list()
  function(ctx, output_file, datasource) {
    key <- paste(
      ctx$reference_input_root %||% "",
      output_file %||% "",
      datasource %||% "",
      sep = "|"
    )
    if (!is.null(cache[[key]])) return(cache[[key]])

    empty <- data.table(
      code_key = character(0),
      MeetpuntOmschrijving = character(0)
    )

    ref_file <- file.path(ctx$reference_input_root, output_file)
    if (!file.exists(ref_file)) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    ref <- tryCatch(read_reference(ref_file, datasource), error = function(e) NULL)
    if (is.null(ref) || nrow(ref) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    mp <- data.table(
      MeetpuntCode = norm_text(ref$`Meetpunt code`),
      MeetpuntOmschrijving = norm_text(ref$`Meetpunt omschrijving`)
    )
    mp <- mp[MeetpuntCode != "" & MeetpuntOmschrijving != ""]
    if (nrow(mp) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    mp[, code_key := normalize_meetpunt_key(MeetpuntCode)]
    mp <- mp[code_key != ""]
    if (nrow(mp) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    # Keep only unambiguous code -> omschrijving mappings.
    lk <- mp[, .N, by = .(code_key, MeetpuntOmschrijving)][order(code_key, -N, MeetpuntOmschrijving)]
    lk <- lk[
      ,
      .(
        oms_n = uniqueN(MeetpuntOmschrijving),
        MeetpuntOmschrijving = MeetpuntOmschrijving[1]
      ),
      by = code_key
    ][oms_n == 1L, .(code_key, MeetpuntOmschrijving)]

    cache[[key]] <<- lk
    lk
  }
})

get_reference_cas_lookup <- local({
  cache <- list()
  function(ctx, output_file, datasource) {
    key <- paste(
      ctx$reference_input_root %||% "",
      output_file %||% "",
      datasource %||% "",
      sep = "|"
    )
    if (!is.null(cache[[key]])) return(cache[[key]])

    empty <- list(
      by_code_name = data.table(join_key = character(0), Casnummer = character(0)),
      by_code = data.table(code_key = character(0), Casnummer = character(0)),
      by_name = data.table(name_key = character(0), Casnummer = character(0)),
      detail = data.table(
        Parametercode = character(0),
        Parameteromschrijving = character(0),
        Casnummer = character(0),
        code_key = character(0),
        name_key = character(0),
        support = integer(0)
      )
    )

    ref_file <- file.path(ctx$reference_input_root, output_file)
    if (!file.exists(ref_file)) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    ref <- tryCatch(read_reference(ref_file, datasource), error = function(e) NULL)
    if (is.null(ref) || nrow(ref) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    cas_dt <- data.table(
      Parametercode = norm_text(ref$Parametercode),
      Parameteromschrijving = norm_text(ref$Parameteromschrijving),
      Casnummer = norm_text(ref$Casnummer)
    )
    cas_dt <- cas_dt[Casnummer != ""]
    if (nrow(cas_dt) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    cas_dt[, code_key := norm_key_text(Parametercode)]
    cas_dt[, name_key := canonicalize(Parameteromschrijving)]
    cas_dt <- cas_dt[code_key != "" | name_key != ""]
    if (nrow(cas_dt) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    detail <- cas_dt[, .(
      support = .N
    ), by = .(Parametercode, Parameteromschrijving, Casnummer, code_key, name_key)]

    pair <- detail[
      code_key != "" & name_key != "",
      .(cas_n = uniqueN(Casnummer), Casnummer = Casnummer[1], support = sum(support)),
      by = .(code_key, name_key)
    ][cas_n == 1L]
    pair[, join_key := paste(code_key, name_key, sep = "|")]

    by_code <- detail[
      code_key != "",
      .(cas_n = uniqueN(Casnummer), Casnummer = Casnummer[1], support = sum(support)),
      by = code_key
    ][cas_n == 1L, .(code_key, Casnummer, support)]

    by_name <- detail[
      name_key != "",
      .(cas_n = uniqueN(Casnummer), Casnummer = Casnummer[1], support = sum(support)),
      by = name_key
    ][cas_n == 1L, .(name_key, Casnummer, support)]

    out <- list(
      by_code_name = pair[, .(join_key, Casnummer, support)],
      by_code = by_code,
      by_name = by_name,
      detail = detail
    )
    cache[[key]] <<- out
    out
  }
})

# Fill missing CAS numbers from matched reference rows.
apply_reference_cas_fill <- function(dt, ctx, output_file, datasource) {
  if (is.null(dt) || nrow(dt) == 0L) return(dt)
  lk <- get_reference_cas_lookup(ctx, output_file = output_file, datasource = datasource)
  if (nrow(lk$detail) == 0L) return(dt)

  cas <- norm_text(dt$Casnummer)
  cas[is.na(cas)] <- ""
  need <- cas == ""
  if (!any(need)) return(dt)

  pcode_key <- norm_key_text(dt$Parametercode)
  pname_key <- canonicalize(norm_text(dt$Parameteromschrijving))

  if (nrow(lk$by_code_name) > 0L) {
    join_key <- paste(pcode_key, pname_key, sep = "|")
    idx <- match(join_key, lk$by_code_name$join_key)
    fill <- need & !is.na(idx)
    if (any(fill)) {
      cas[fill] <- lk$by_code_name$Casnummer[idx[fill]]
      need <- cas == ""
    }
  }

  if (nrow(lk$by_code) > 0L && any(need)) {
    idx <- match(pcode_key, lk$by_code$code_key)
    fill <- need & !is.na(idx)
    if (any(fill)) {
      cas[fill] <- lk$by_code$Casnummer[idx[fill]]
      need <- cas == ""
    }
  }

  if (nrow(lk$by_name) > 0L && any(need)) {
    idx <- match(pname_key, lk$by_name$name_key)
    fill <- need & !is.na(idx)
    if (any(fill)) {
      cas[fill] <- lk$by_name$Casnummer[idx[fill]]
    }
  }

  cas[cas == ""] <- NA_character_
  dt[, Casnummer := cas]
  dt
}

get_reference_casing_lookup <- local({
  cache <- list()
  function(ctx, output_file, datasource) {
    ref_root <- normalizePath(ctx$reference_input_root %||% "", mustWork = FALSE)
    key <- paste(ref_root, output_file, datasource, sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    empty_map <- data.table(key = character(0), value = character(0))
    empty <- list(
      parameter = copy(empty_map),
      hoedanigheid = copy(empty_map),
      limietsymbool = copy(empty_map),
      eenheid = copy(empty_map)
    )

    ref_file <- file.path(ref_root, output_file)
    ref <- tryCatch(read_reference(ref_file, datasource), error = function(e) NULL)
    if (is.null(ref) || nrow(ref) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    build_map <- function(values, key_fun) {
      dt <- data.table(value = norm_text(values))
      dt[, key := key_fun(value)]
      dt <- dt[value != "" & key != ""]
      if (nrow(dt) == 0L) return(copy(empty_map))
      dt <- dt[, .N, by = .(key, value)][order(key, -N, nchar(value), value)]
      dt <- dt[, .SD[1], by = key]
      dt[, .(key, value)]
    }

    out <- list(
      parameter = build_map(ref$Parametercode, function(x) norm_key_text(x)),
      hoedanigheid = build_map(ref$Hoedanigheid, function(x) norm_key_text(x)),
      limietsymbool = build_map(ref$Limietsymbool, function(x) norm_key_text(x)),
      eenheid = build_map(ref$Eenheid, function(x) norm_key_text(normalize_chem_unit(x)))
    )
    cache[[key]] <<- out
    out
  }
})

# Restore preferred reference casing for selected text fields.
apply_reference_casing <- function(
  dt,
  ctx,
  output_file,
  datasource,
  fields = c("Parametercode", "Hoedanigheid", "Limietsymbool", "Eenheid")
) {
  if (is.null(dt) || nrow(dt) == 0L) return(dt)

  lk <- get_reference_casing_lookup(ctx, output_file = output_file, datasource = datasource)

  if ("Parametercode" %in% fields && nrow(lk$parameter) > 0L) {
    key <- norm_key_text(dt$Parametercode)
    idx <- match(key, lk$parameter$key)
    use <- !is.na(idx)
    if (any(use)) dt[use, Parametercode := lk$parameter$value[idx[use]]]
  }

  if ("Hoedanigheid" %in% fields && nrow(lk$hoedanigheid) > 0L) {
    key <- norm_key_text(dt$Hoedanigheid)
    idx <- match(key, lk$hoedanigheid$key)
    use <- !is.na(idx)
    if (any(use)) dt[use, Hoedanigheid := lk$hoedanigheid$value[idx[use]]]
  }

  if ("Limietsymbool" %in% fields && nrow(lk$limietsymbool) > 0L) {
    key <- norm_key_text(dt$Limietsymbool)
    idx <- match(key, lk$limietsymbool$key)
    use <- !is.na(idx)
    if (any(use)) dt[use, Limietsymbool := lk$limietsymbool$value[idx[use]]]
  }

  if ("Eenheid" %in% fields && nrow(lk$eenheid) > 0L) {
    key <- norm_key_text(normalize_chem_unit(dt$Eenheid))
    idx <- match(key, lk$eenheid$key)
    use <- !is.na(idx)
    if (any(use)) dt[use, Eenheid := lk$eenheid$value[idx[use]]]
  }

  dt
}

get_rivierenland_raw_name_lookup <- local({
  cache <- list()
  function(ctx) {
    root <- normalizePath(ctx$raw_root %||% "", mustWork = FALSE)
    key <- paste(root, sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    empty <- data.table(
      omschrijving_key = character(0),
      Parametercode = character(0),
      support = integer(0)
    )

    files <- resolve_files(root, c("Ws Rivierenland/*.xlsx", "Ws Rivierenland/*.csv"))
    if (length(files) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    tabs <- lapply(files, function(f) tryCatch(read_any_table(f), error = function(e) NULL))
    tabs <- Filter(Negate(is.null), tabs)
    if (length(tabs) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    raw <- rbindlist(tabs, use.names = TRUE, fill = TRUE)
    if (nrow(raw) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    code_raw <- coalesce_chr(
      pull_alias(raw, c("Aquo.Parcode", "AQUO", "Par.Nr.", "Parametercode", "PARAMETERCODE", "IM_PARM_CODE")),
      pull_alias(raw, c("Par.Naam", "Aquo.Parnaam"))
    )
    name_raw <- coalesce_chr(
      pull_alias(raw, c("Par.Naam", "Aquo.Parnaam", "Parameter", "Parameteromschrijving", "Omschrijving.2"))
    )
    code_map <- norm_text(map_parameter_names_to_codes(code_raw, name_raw))
    name_map <- norm_text(name_raw)

    lk <- data.table(
      Parametercode = code_map,
      Parameteromschrijving = name_map
    )
    lk <- lk[Parametercode != "" & Parameteromschrijving != ""]
    if (nrow(lk) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    lk[, omschrijving_key := canonicalize(Parameteromschrijving)]
    lk <- lk[omschrijving_key != ""]
    if (nrow(lk) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    lk_code <- lk[, .N, by = .(omschrijving_key, Parametercode)][order(omschrijving_key, -N, Parametercode)]
    out <- lk_code[
      ,
      .(code_n = uniqueN(Parametercode), Parametercode = Parametercode[1], support = max(N)),
      by = omschrijving_key
    ][code_n == 1L, .(omschrijving_key, Parametercode, support)]

    attr(out, "source_file") <- "rivierenland_raw_name_lookup"
    cache[[key]] <<- out
    out
  }
})

get_rivierenland_parametercode_converter <- local({
  cache <- list()
  function(ctx) {
    root <- normalizePath(ctx$raw_root %||% "", mustWork = FALSE)
    key <- paste(root, getwd(), sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    empty <- data.table(
      par_naam_key = character(0),
      converter_parametercode = character(0),
      converter_parameteromschrijving = character(0)
    )

    candidates <- c(
      script_0_support_candidates(
        ctx,
        file.path("Ws Rivierenland", "parametercode_converter.csv"),
        packaged_paths = c(
          file.path(getwd(), "resources", "lookups", "parametercode_converter.csv"),
          file.path(getwd(), "Chemie data waterschappen", "Ws Rivierenland", "parametercode_converter.csv")
        )
      ),
      script_0_support_candidates(
        ctx,
        file.path("Ws Rivierenland", "parametercode_converter.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "parametercode_converter.tsv")
      )
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    for (f in candidates) {
      conv <- tryCatch(
        fread(f, encoding = "UTF-8", showProgress = FALSE),
        error = function(e) NULL
      )
      if (is.null(conv) || nrow(conv) == 0L || ncol(conv) <= 1L) {
        conv <- tryCatch(
          fread(f, sep = ";", encoding = "UTF-8", showProgress = FALSE),
          error = function(e) NULL
        )
      }
      if (is.null(conv) || nrow(conv) == 0L) next

      par_col <- pick_col(conv, c("Par.Naam", "Par_Naam", "ParNaam"))
      code_col <- pick_col(conv, c("Parametercode", "Aquo.Parcode", "AQUO", "Par.Nr."))
      oms_col <- pick_col(conv, c("Parameteromschrijving", "Aquo.Parnaam", "Par.Naam"))
      if (is.na(par_col) || is.na(code_col) || is.na(oms_col)) next

      out <- data.table(
        par_naam_key = canonicalize(norm_text(conv[[par_col]])),
        converter_parametercode = norm_text(conv[[code_col]]),
        converter_parameteromschrijving = norm_text(conv[[oms_col]])
      )
      out <- out[
        par_naam_key != "" &
          converter_parametercode != "" &
          converter_parameteromschrijving != ""
      ]
      if (nrow(out) == 0L) next

      out <- unique(out, by = "par_naam_key")
      attr(out, "source_file") <- basename(f)
      cache[[key]] <<- out
      return(out)
    }

    cache[[key]] <<- empty
    empty
  }
})

get_rivierenland_parnr_lookup <- local({
  cache <- list()
  function(ctx) {
    root <- normalizePath(ctx$raw_root %||% "", mustWork = FALSE)
    key <- paste(root, sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    empty <- data.table(
      raw_code_key = character(0),
      Parametercode = character(0),
      Parameteromschrijving = character(0),
      support = integer(0)
    )

    candidates <- c(
      script_0_support_candidates(
        ctx,
        file.path("Ws Rivierenland", "rivierenland_parameter_parnr_lookup.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "rivierenland_parameter_parnr_lookup.tsv")
      ),
      script_0_support_candidates(
        ctx,
        file.path("Ws Rivierenland", "rivierenland_parameter_lookup_with_parnr.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "rivierenland_parameter_lookup_with_parnr.tsv")
      ),
      script_0_support_candidates(
        ctx,
        file.path("Ws Rivierenland", "rivierenland_parameter_lookup.tsv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "rivierenland_parameter_lookup.tsv")
      )
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    for (f in candidates) {
      lk <- tryCatch(
        fread(f, sep = "\t", encoding = "UTF-8", showProgress = FALSE),
        error = function(e) NULL
      )
      if (is.null(lk) || nrow(lk) == 0L) next

      raw_col <- pick_col(lk, c("raw_code_key", "Par_Nr", "Par.Nr.", "Par.Nr", "Par Nr", "Aquo.Parcode", "AQUO", "Parametercode"))
      code_col <- pick_col(lk, c("Lookup_Parametercode", "Parametercode", "Aquo.Parcode", "IM_PARM_CODE", "Par.Nr."))
      oms_col <- pick_col(lk, c("Lookup_Parameteromschrijving", "Parameteromschrijving", "Raw_ParameterNaam", "Par.Naam", "Aquo.Parnaam", "Parameter"))
      support_col <- pick_col(lk, c("raw_rows", "N", "support", "lookup_rows"))

      if (is.na(raw_col) || is.na(code_col)) next

      out <- data.table(
        raw_code_key = norm_key_text(lk[[raw_col]]),
        Parametercode = norm_text(lk[[code_col]]),
        Parameteromschrijving = if (!is.na(oms_col)) norm_text(lk[[oms_col]]) else rep("", nrow(lk)),
        support = if (!is.na(support_col)) suppressWarnings(as.integer(lk[[support_col]])) else rep(1L, nrow(lk))
      )
      out[is.na(support) | support < 1L, support := 1L]
      out <- out[raw_code_key != "" & Parametercode != ""]
      if (nrow(out) == 0L) next

      out_agg <- out[, .(
        support = sum(support, na.rm = TRUE),
        Parameteromschrijving = Parameteromschrijving[which.max(support)]
      ), by = .(raw_code_key, Parametercode)]
      setorder(out_agg, raw_code_key, -support, Parametercode)
      out_agg <- out_agg[
        ,
        .(
          code_n = uniqueN(Parametercode),
          Parametercode = Parametercode[1],
          Parameteromschrijving = Parameteromschrijving[1],
          support = support[1]
        ),
        by = raw_code_key
      ][code_n == 1L, .(raw_code_key, Parametercode, Parameteromschrijving, support)]

      if (nrow(out_agg) > 0L) {
        attr(out_agg, "source_file") <- basename(f)
        cache[[key]] <<- out_agg
        return(out_agg)
      }
    }

    cache[[key]] <<- empty
    empty
  }
})

get_rivierenland_parameter_adjustments <- local({
  cache <- list()
  empty <- data.table(
    board_id = character(0),
    from_parametercode = character(0),
    from_hoedanigheid = character(0),
    from_limietsymbool = character(0),
    from_eenheid = character(0),
    to_parametercode = character(0),
    to_hoedanigheid = character(0),
    to_limietsymbool = character(0),
    to_eenheid = character(0),
    support_rows = numeric(0),
    method = character(0)
  )

  function(ctx) {
    root <- normalizePath(ctx$raw_root %||% "", mustWork = FALSE)
    key <- paste(root, getwd(), sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    candidates <- c(
      file.path(getwd(), "rivierenland_parameter_adjustments.csv"),
      script_0_support_candidates(
        ctx,
        file.path("Ws Rivierenland", "rivierenland_parameter_adjustments.csv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "rivierenland_parameter_adjustments.csv")
      )
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    adj <- tryCatch(
      fread(candidates[1], encoding = "UTF-8", showProgress = FALSE),
      error = function(e) NULL
    )
    if (is.null(adj) || nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    for (nm in c(
      "board_id", "active",
      "from_parametercode", "from_hoedanigheid", "from_limietsymbool", "from_eenheid",
      "to_parametercode", "to_hoedanigheid", "to_limietsymbool", "to_eenheid",
      "support_rows", "method"
    )) {
      if (!(nm %in% names(adj))) adj[, (nm) := NA]
    }

    adj[, board_id := norm_key_text(board_id)]
    adj <- adj[board_id %in% c("", "rivierenland")]
    if ("active" %in% names(adj)) {
      adj[, active := tolower(norm_text(active))]
      adj <- adj[active %in% c("", "true", "t", "1", "yes", "y")]
    }
    if (nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    adj[, from_parametercode := norm_text(from_parametercode)]
    adj[, from_hoedanigheid := norm_text(from_hoedanigheid)]
    adj[, from_limietsymbool := norm_text(from_limietsymbool)]
    adj[, from_eenheid := norm_text(normalize_chem_unit(from_eenheid))]
    adj[, to_parametercode := norm_text(to_parametercode)]
    adj[, to_hoedanigheid := norm_text(to_hoedanigheid)]
    adj[, to_limietsymbool := norm_text(to_limietsymbool)]
    adj[, to_eenheid := norm_text(normalize_chem_unit(to_eenheid))]
    adj[from_limietsymbool == "<blank>", from_limietsymbool := ""]
    adj[to_limietsymbool == "<blank>", to_limietsymbool := ""]
    adj[from_eenheid == "<blank>", from_eenheid := ""]
    adj[to_eenheid == "<blank>", to_eenheid := ""]
    adj[, support_rows := suppressWarnings(as.numeric(support_rows))]
    adj[is.na(support_rows), support_rows := 0]
    adj[, method := norm_text(method)]

    adj <- adj[to_parametercode != ""]
    if (nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    setorder(adj, -support_rows, method)
    adj <- unique(adj, by = c("from_parametercode", "from_hoedanigheid", "from_limietsymbool", "from_eenheid"))
    attr(adj, "source_file") <- basename(candidates[1])
    attr(adj, "source_file") <- basename(candidates[1])
    cache[[key]] <<- adj
    adj
  }
})

# Apply explicit Rivierenland parameter overrides.
apply_rivierenland_parameter_adjustments <- function(dt, adj, ctx = NULL, source_label = NULL) {
  if (nrow(dt) == 0L || is.null(adj) || nrow(adj) == 0L) return(dt)
  dt <- copy(dt)
  before_dt <- if (!is.null(ctx) && isTRUE(ctx$collect_debug)) copy(dt) else NULL

  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  l_key <- normalize_limiet_key(dt$Limietsymbool)
  u_key <- norm_key_text(normalize_chem_unit(dt$Eenheid))

  for (i in seq_len(nrow(adj))) {
    r <- adj[i]
    idx <- rep(TRUE, nrow(dt))
    from_pc <- norm_key_text(r$from_parametercode)
    from_ho <- norm_key_text(r$from_hoedanigheid)
    from_li <- normalize_limiet_key(r$from_limietsymbool)
    from_un <- norm_key_text(normalize_chem_unit(r$from_eenheid))

    if (from_pc != "") idx <- idx & (p_key == from_pc)
    if (from_ho != "") idx <- idx & (h_key == from_ho)
    if (from_li != "") idx <- idx & (l_key == from_li)
    if (from_un != "") idx <- idx & (u_key == from_un)
    if (!any(idx)) next

    dt[idx, Parametercode := r$to_parametercode]
    dt[idx, Hoedanigheid := r$to_hoedanigheid]
    dt[idx, Limietsymbool := r$to_limietsymbool]
    dt[idx, Eenheid := normalize_chem_unit(r$to_eenheid)]

    p_key[idx] <- norm_key_text(r$to_parametercode)
    h_key[idx] <- norm_key_text(r$to_hoedanigheid)
    l_key[idx] <- normalize_limiet_key(r$to_limietsymbool)
    u_key[idx] <- norm_key_text(normalize_chem_unit(r$to_eenheid))
  }

  if (!is.null(before_dt)) {
    label <- source_label %||% attr(adj, "source_file") %||% "parameter_adjustments"
    script_0_log_lookup_changes(
      ctx,
      before_dt,
      dt,
      source_label = label,
      columns = c("Parametercode", "Hoedanigheid", "Limietsymbool", "Eenheid")
    )
  }

  dt
}

get_rijnland_parameter_adjustments <- local({
  cache <- list()
  empty <- data.table(
    board_id = character(0),
    from_parametercode = character(0),
    from_hoedanigheid = character(0),
    from_limietsymbool = character(0),
    from_eenheid = character(0),
    to_parametercode = character(0),
    to_hoedanigheid = character(0),
    to_limietsymbool = character(0),
    to_eenheid = character(0),
    support_rows = numeric(0),
    method = character(0)
  )

  function(ctx) {
    root <- normalizePath(ctx$raw_root %||% "", mustWork = FALSE)
    key <- paste(root, getwd(), sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    candidates <- c(
      file.path(getwd(), "rijnland_parameter_adjustments.csv"),
      script_0_support_candidates(
        ctx,
        file.path("HH Rijnland", "rijnland_parameter_adjustments.csv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "rijnland_parameter_adjustments.csv")
      )
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    adj <- tryCatch(
      fread(candidates[1], encoding = "UTF-8", showProgress = FALSE),
      error = function(e) NULL
    )
    if (is.null(adj) || nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    for (nm in c(
      "board_id", "active",
      "from_parametercode", "from_hoedanigheid", "from_limietsymbool", "from_eenheid",
      "to_parametercode", "to_hoedanigheid", "to_limietsymbool", "to_eenheid",
      "support_rows", "method"
    )) {
      if (!(nm %in% names(adj))) adj[, (nm) := NA]
    }

    adj[, board_id := norm_key_text(board_id)]
    adj <- adj[board_id %in% c("", "hh_van_rijnland", "rijnland")]
    if ("active" %in% names(adj)) {
      adj[, active := tolower(norm_text(active))]
      adj <- adj[active %in% c("", "true", "t", "1", "yes", "y")]
    }
    if (nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    adj[, from_parametercode := norm_text(from_parametercode)]
    adj[, from_hoedanigheid := norm_text(from_hoedanigheid)]
    adj[, from_limietsymbool := norm_text(from_limietsymbool)]
    adj[, from_eenheid := norm_text(normalize_chem_unit(from_eenheid))]
    adj[, to_parametercode := norm_text(to_parametercode)]
    adj[, to_hoedanigheid := norm_text(to_hoedanigheid)]
    adj[, to_limietsymbool := norm_text(to_limietsymbool)]
    adj[, to_eenheid := norm_text(normalize_chem_unit(to_eenheid))]
    adj[from_limietsymbool == "<blank>", from_limietsymbool := ""]
    adj[to_limietsymbool == "<blank>", to_limietsymbool := ""]
    adj[from_eenheid == "<blank>", from_eenheid := ""]
    adj[to_eenheid == "<blank>", to_eenheid := ""]
    adj[, support_rows := suppressWarnings(as.numeric(support_rows))]
    adj[is.na(support_rows), support_rows := 0]
    adj[, method := norm_text(method)]

    adj <- adj[to_parametercode != ""]
    if (nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    setorder(adj, -support_rows, method)
    adj <- unique(adj, by = c("from_parametercode", "from_hoedanigheid", "from_limietsymbool", "from_eenheid"))
    attr(adj, "source_file") <- basename(candidates[1])
    cache[[key]] <<- adj
    adj
  }
})

get_scheldestromen_parameter_adjustments <- local({
  cache <- list()
  empty <- data.table(
    board_id = character(0),
    from_parametercode = character(0),
    from_hoedanigheid = character(0),
    from_limietsymbool = character(0),
    from_eenheid = character(0),
    to_parametercode = character(0),
    to_hoedanigheid = character(0),
    to_limietsymbool = character(0),
    to_eenheid = character(0),
    support_rows = numeric(0),
    method = character(0)
  )

  function(ctx) {
    root <- normalizePath(ctx$raw_root %||% "", mustWork = FALSE)
    key <- paste(root, getwd(), sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    candidates <- c(
      file.path(getwd(), "scheldestromen_parameter_adjustments.csv"),
      script_0_support_candidates(
        ctx,
        file.path("Ws Scheldestromen", "scheldestromen_parameter_adjustments.csv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "scheldestromen_parameter_adjustments.csv")
      )
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    adj <- tryCatch(
      fread(candidates[1], encoding = "UTF-8", showProgress = FALSE),
      error = function(e) NULL
    )
    if (is.null(adj) || nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    for (nm in c(
      "board_id", "active",
      "from_parametercode", "from_hoedanigheid", "from_limietsymbool", "from_eenheid",
      "to_parametercode", "to_hoedanigheid", "to_limietsymbool", "to_eenheid",
      "support_rows", "method"
    )) {
      if (!(nm %in% names(adj))) adj[, (nm) := NA]
    }

    adj[, board_id := norm_key_text(board_id)]
    adj <- adj[board_id %in% c("", "scheldestromen")]
    if ("active" %in% names(adj)) {
      adj[, active := tolower(norm_text(active))]
      adj <- adj[active %in% c("", "true", "t", "1", "yes", "y")]
    }
    if (nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    adj[, from_parametercode := norm_text(from_parametercode)]
    adj[, from_hoedanigheid := norm_text(from_hoedanigheid)]
    adj[, from_limietsymbool := norm_text(from_limietsymbool)]
    adj[, from_eenheid := norm_text(normalize_chem_unit(from_eenheid))]
    adj[, to_parametercode := norm_text(to_parametercode)]
    adj[, to_hoedanigheid := norm_text(to_hoedanigheid)]
    adj[, to_limietsymbool := norm_text(to_limietsymbool)]
    adj[, to_eenheid := norm_text(normalize_chem_unit(to_eenheid))]
    adj[from_limietsymbool == "<blank>", from_limietsymbool := ""]
    adj[to_limietsymbool == "<blank>", to_limietsymbool := ""]
    adj[from_eenheid == "<blank>", from_eenheid := ""]
    adj[to_eenheid == "<blank>", to_eenheid := ""]
    adj[, support_rows := suppressWarnings(as.numeric(support_rows))]
    adj[is.na(support_rows), support_rows := 0]
    adj[, method := norm_text(method)]

    adj <- adj[to_parametercode != ""]
    if (nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    setorder(adj, -support_rows, method)
    adj <- unique(adj, by = c("from_parametercode", "from_hoedanigheid", "from_limietsymbool", "from_eenheid"))
    attr(adj, "source_file") <- basename(candidates[1])
    cache[[key]] <<- adj
    adj
  }
})

get_hhnk_parameter_adjustments <- local({
  cache <- list()
  empty <- data.table(
    board_id = character(0),
    from_parametercode = character(0),
    from_hoedanigheid = character(0),
    from_limietsymbool = character(0),
    from_eenheid = character(0),
    to_parametercode = character(0),
    to_hoedanigheid = character(0),
    to_limietsymbool = character(0),
    to_eenheid = character(0),
    support_rows = numeric(0),
    method = character(0)
  )

  function(ctx) {
    root <- normalizePath(ctx$raw_root %||% "", mustWork = FALSE)
    key <- paste(root, getwd(), sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    candidates <- c(
      file.path(getwd(), "hhnk_parameter_adjustments.csv"),
      script_0_support_candidates(
        ctx,
        file.path("HH Hollands Noorderkwartier", "hhnk_parameter_adjustments.csv"),
        packaged_paths = file.path(getwd(), "resources", "lookups", "hhnk_parameter_adjustments.csv")
      )
    )
    candidates <- candidates[file.exists(candidates)]
    if (length(candidates) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    adj <- tryCatch(
      fread(candidates[1], encoding = "UTF-8", showProgress = FALSE),
      error = function(e) NULL
    )
    if (is.null(adj) || nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    for (nm in c(
      "board_id", "active",
      "from_parametercode", "from_hoedanigheid", "from_limietsymbool", "from_eenheid",
      "to_parametercode", "to_hoedanigheid", "to_limietsymbool", "to_eenheid",
      "support_rows", "method"
    )) {
      if (!(nm %in% names(adj))) adj[, (nm) := NA]
    }

    adj[, board_id := norm_key_text(board_id)]
    adj <- adj[board_id %in% c("", "hollands_noorderkwartier", "hhnk")]
    if ("active" %in% names(adj)) {
      adj[, active := tolower(norm_text(active))]
      adj <- adj[active %in% c("", "true", "t", "1", "yes", "y")]
    }
    if (nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    adj[, from_parametercode := norm_text(from_parametercode)]
    adj[, from_hoedanigheid := norm_text(from_hoedanigheid)]
    adj[, from_limietsymbool := norm_text(from_limietsymbool)]
    adj[, from_eenheid := norm_text(normalize_chem_unit(from_eenheid))]
    adj[, to_parametercode := norm_text(to_parametercode)]
    adj[, to_hoedanigheid := norm_text(to_hoedanigheid)]
    adj[, to_limietsymbool := norm_text(to_limietsymbool)]
    adj[, to_eenheid := norm_text(normalize_chem_unit(to_eenheid))]
    adj[from_limietsymbool == "<blank>", from_limietsymbool := ""]
    adj[to_limietsymbool == "<blank>", to_limietsymbool := ""]
    adj[from_eenheid == "<blank>", from_eenheid := ""]
    adj[to_eenheid == "<blank>", to_eenheid := ""]
    adj[, support_rows := suppressWarnings(as.numeric(support_rows))]
    adj[is.na(support_rows), support_rows := 0]
    adj[, method := norm_text(method)]

    adj <- adj[to_parametercode != ""]
    if (nrow(adj) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    setorder(adj, -support_rows, method)
    adj <- unique(adj, by = c("from_parametercode", "from_hoedanigheid", "from_limietsymbool", "from_eenheid"))
    attr(adj, "source_file") <- basename(candidates[1])
    cache[[key]] <<- adj
    adj
  }
})

get_value_change_rules <- local({
  cache <- list()
  empty <- data.table(
    board_id = character(0),
    field = character(0),
    from_key = character(0),
    to_value = character(0),
    context_parametercode_key = character(0),
    context_hoedanigheid_key = character(0),
    context_limietsymbool_key = character(0),
    context_eenheid_key = character(0),
    specificity = integer(0),
    support = numeric(0),
    ratio_src = numeric(0),
    ratio_dst = numeric(0)
  )

  function(ctx, board_id = NULL) {
    board_id_arg <- if (is.null(board_id)) NULL else norm_text(board_id)
    rules_file <- ctx$value_change_rules_file %||%
      file.path(getwd(), "output", "value_change_rules_latest.tsv")
    rules_file <- normalizePath(rules_file, mustWork = FALSE)
    key <- paste(rules_file, board_id_arg %||% "__all__", sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    if (!file.exists(rules_file)) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    raw <- tryCatch(
      fread(rules_file, sep = "\t", encoding = "UTF-8", showProgress = FALSE),
      error = function(e) NULL
    )
    if (is.null(raw) || nrow(raw) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    for (nm in c(
      "board_id", "field", "from_value", "to_value",
      "context_parametercode", "context_hoedanigheid", "context_limietsymbool", "context_eenheid",
      "support", "ratio_src", "ratio_dst"
    )) {
      if (!(nm %in% names(raw))) raw[, (nm) := NA]
    }

    lk <- data.table(
      board_id = norm_text(raw$board_id),
      field = norm_text(raw$field),
      from_key = NA_character_,
      to_value = norm_text(raw$to_value),
      context_parametercode_key = norm_key_text(raw$context_parametercode),
      context_hoedanigheid_key = norm_key_text(raw$context_hoedanigheid),
      context_limietsymbool_key = normalize_limiet_key(raw$context_limietsymbool),
      context_eenheid_key = norm_key_text(normalize_chem_unit(raw$context_eenheid)),
      support = suppressWarnings(as.numeric(raw$support)),
      ratio_src = suppressWarnings(as.numeric(raw$ratio_src)),
      ratio_dst = suppressWarnings(as.numeric(raw$ratio_dst))
    )

    lk[field == "parametercode", from_key := norm_key_text(raw$from_value)]
    lk[field == "hoedanigheid", from_key := norm_key_text(raw$from_value)]
    lk[field == "limietsymbool", from_key := normalize_limiet_key(raw$from_value)]

    lk <- lk[
      board_id != "" &
      field %in% c("parametercode", "hoedanigheid", "limietsymbool") &
      from_key != "" &
      to_value != ""
    ]
    if (nrow(lk) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    lk[, specificity := as.integer(context_parametercode_key != "") +
      as.integer(context_hoedanigheid_key != "") +
      as.integer(context_limietsymbool_key != "") +
      as.integer(context_eenheid_key != "")]

    if (!is.null(board_id_arg)) {
      lk <- lk[board_id == board_id_arg]
    }
    if (nrow(lk) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    setorder(
      lk,
      -specificity,
      -support,
      -ratio_src,
      -ratio_dst,
      board_id,
      field,
      from_key,
      to_value
    )
    lk <- unique(
      lk,
      by = c(
        "board_id", "field", "from_key",
        "context_parametercode_key", "context_hoedanigheid_key",
        "context_limietsymbool_key", "context_eenheid_key"
      )
    )

    cache[[key]] <<- lk
    lk
  }
})

# Rule-based value correction helpers.
# Apply board-specific one-off value substitutions and recodes.
apply_value_change_rules <- function(dt, board_id, ctx, output_file = NULL, datasource = NULL) {
  if (nrow(dt) == 0L) return(dt)
  rules <- get_value_change_rules(ctx, board_id = board_id)
  if (nrow(rules) == 0L) return(dt)

  ref_code_keys <- character(0)
  if (!is.null(output_file) && !is.null(datasource) && nzchar(output_file) && nzchar(datasource)) {
    ref_codes <- get_reference_parameter_codes(ctx, output_file = output_file, datasource = datasource)
    if (length(ref_codes) > 0L) ref_code_keys <- norm_key_text(ref_codes)
  }

  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  l_key <- normalize_limiet_key(dt$Limietsymbool)
  u_key <- norm_key_text(normalize_chem_unit(dt$Eenheid))

  for (i in seq_len(nrow(rules))) {
    r <- rules[i]
    if (r$field == "parametercode") {
      if (length(ref_code_keys) > 0L && !(norm_key_text(r$to_value) %in% ref_code_keys)) next
      idx <- p_key == r$from_key
    } else if (r$field == "hoedanigheid") {
      idx <- h_key == r$from_key
    } else if (r$field == "limietsymbool") {
      idx <- l_key == r$from_key
    } else {
      next
    }

    if (r$context_parametercode_key != "") idx <- idx & (p_key == r$context_parametercode_key)
    if (r$context_hoedanigheid_key != "") idx <- idx & (h_key == r$context_hoedanigheid_key)
    if (r$context_limietsymbool_key != "") idx <- idx & (l_key == r$context_limietsymbool_key)
    if (r$context_eenheid_key != "") idx <- idx & (u_key == r$context_eenheid_key)

    if (!any(idx)) next

    if (r$field == "parametercode") {
      dt[idx, Parametercode := r$to_value]
      p_key[idx] <- norm_key_text(r$to_value)
    } else if (r$field == "hoedanigheid") {
      dt[idx, Hoedanigheid := r$to_value]
      h_key[idx] <- norm_key_text(r$to_value)
    } else if (r$field == "limietsymbool") {
      dt[idx, Limietsymbool := r$to_value]
      l_key[idx] <- normalize_limiet_key(r$to_value)
    }
  }
  dt
}

get_value_multiplier_rules <- local({
  cache <- list()
  empty <- data.table(
    board_id = character(0),
    pc_key = character(0),
    ho_key = character(0),
    li_key = character(0),
    un_key = character(0),
    multiplier = numeric(0)
  )

  function(ctx, board_id = NULL) {
    board_id_arg <- if (is.null(board_id)) NULL else norm_text(board_id)
    rules_file <- ctx$value_multiplier_rules_file %||%
      file.path(getwd(), "output", "value_delimiter_multiplier_rules_latest.tsv")
    rules_file <- normalizePath(rules_file, mustWork = FALSE)
    key <- paste(rules_file, board_id_arg %||% "__all__", sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])

    if (!file.exists(rules_file)) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    raw <- tryCatch(
      fread(rules_file, sep = "\t", encoding = "UTF-8", showProgress = FALSE),
      error = function(e) NULL
    )
    if (is.null(raw) || nrow(raw) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }

    for (nm in c("board_id", "pc_key", "ho_key", "li_key", "un_key", "multiplier")) {
      if (!(nm %in% names(raw))) raw[, (nm) := NA]
    }

    lk <- data.table(
      board_id = norm_text(raw$board_id),
      pc_key = norm_key_text(raw$pc_key),
      ho_key = norm_key_text(raw$ho_key),
      li_key = normalize_limiet_key(raw$li_key),
      un_key = norm_key_text(normalize_chem_unit(raw$un_key)),
      multiplier = suppressWarnings(as.numeric(raw$multiplier))
    )
    lk <- lk[
      board_id != "" &
      pc_key != "" &
      is.finite(multiplier) &
      multiplier > 0 &
      multiplier != 1
    ]
    if (!is.null(board_id_arg)) lk <- lk[board_id == board_id_arg]
    if (nrow(lk) == 0L) {
      cache[[key]] <<- empty
      return(cache[[key]])
    }
    lk <- unique(lk, by = c("board_id", "pc_key", "ho_key", "li_key", "un_key"))
    cache[[key]] <<- lk
    lk
  }
})

# Apply board-specific multiplicative corrections to values.
apply_value_multiplier_rules <- function(dt, board_id, ctx) {
  if (nrow(dt) == 0L) return(dt)
  rules <- get_value_multiplier_rules(ctx, board_id = board_id)
  if (nrow(rules) == 0L) return(dt)

  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  l_key <- normalize_limiet_key(dt$Limietsymbool)
  u_key <- norm_key_text(normalize_chem_unit(dt$Eenheid))

  for (i in seq_len(nrow(rules))) {
    r <- rules[i]
    idx <- p_key == r$pc_key
    if (r$ho_key != "") idx <- idx & (h_key == r$ho_key)
    if (r$li_key != "") idx <- idx & (l_key == r$li_key)
    if (r$un_key != "") idx <- idx & (u_key == r$un_key)
    idx <- idx & !is.na(dt$Meetwaarde)
    if (!any(idx)) next
    dt[idx, Meetwaarde := Meetwaarde * r$multiplier]
  }
  dt
}

get_reference_signatures <- local({
  cache <- list()
  function(ctx, output_file, datasource, mode = "key_only") {
    key <- paste(ctx$reference_input_root %||% "", output_file %||% "", datasource %||% "", mode %||% "", sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])
    ref_file <- file.path(ctx$reference_input_root, output_file)
    if (!file.exists(ref_file)) {
      cache[[key]] <<- character(0)
      return(cache[[key]])
    }
    ref <- tryCatch(read_reference(ref_file, datasource), error = function(e) NULL)
    if (is.null(ref) || nrow(ref) == 0L) {
      cache[[key]] <<- character(0)
      return(cache[[key]])
    }
    sig <- unique(make_signature(ref, mode = mode))
    cache[[key]] <<- sig
    sig
  }
})

get_reference_value_map <- local({
  cache <- list()
  function(ctx, output_file, datasource) {
    key <- paste(ctx$reference_input_root %||% "", output_file %||% "", datasource %||% "", sep = "|")
    if (!is.null(cache[[key]])) return(cache[[key]])
    ref_file <- file.path(ctx$reference_input_root, output_file)
    if (!file.exists(ref_file)) {
      cache[[key]] <<- data.table(
        MeetpuntCode = character(0),
        Datum = character(0),
        Parametercode = character(0),
        Hoedanigheid = character(0),
        Limietsymbool = character(0),
        Eenheid = character(0),
        RefValueCount = integer(0),
        RefValueText = character(0),
        RefValueNum = numeric(0)
      )
      return(cache[[key]])
    }
    ref <- tryCatch(read_reference(ref_file, datasource), error = function(e) NULL)
    if (is.null(ref) || nrow(ref) == 0L) {
      cache[[key]] <<- data.table(
        MeetpuntCode = character(0),
        Datum = character(0),
        Parametercode = character(0),
        Hoedanigheid = character(0),
        Limietsymbool = character(0),
        Eenheid = character(0),
        RefValueCount = integer(0),
        RefValueText = character(0),
        RefValueNum = numeric(0)
      )
      return(cache[[key]])
    }

    ref_key <- data.table(
      MeetpuntCode = normalize_meetpunt_key(ref$`Meetpunt code`),
      Datum = ifelse(is.na(ref$Datum), "", format(ref$Datum, "%Y-%m-%d")),
      Parametercode = norm_key_text(ref$Parametercode),
      Hoedanigheid = norm_key_text(ref$Hoedanigheid),
      Limietsymbool = normalize_limiet_key(ref$Limietsymbool),
      Eenheid = norm_key_text(normalize_chem_unit(ref$Eenheid)),
      RefValueText = norm_num_text(ref$Meetwaarde)
    )
    ref_key[, RefValueNum := suppressWarnings(as.numeric(RefValueText))]

    out <- ref_key[
      ,
      {
        vals <- unique(RefValueText)
        list(
          RefValueCount = length(vals),
          RefValueText = if (length(vals) == 1L) vals[[1]] else NA_character_,
          RefValueNum = if (length(vals) == 1L) suppressWarnings(as.numeric(vals[[1]])) else NA_real_
        )
      },
      by = .(MeetpuntCode, Datum, Parametercode, Hoedanigheid, Limietsymbool, Eenheid)
    ]
    cache[[key]] <<- out
    out
  }
})

# Snap selected values to nearby reference values when configured.
apply_reference_value_snap <- function(
  dt,
  ctx,
  output_file,
  datasource,
  limiet_symbols = c("<", "", "-"),
  cur_value_max = 0.001,
  ref_value_min = 10000,
  parameter_allow = NULL,
  hoed_allow = NULL,
  unit_allow = NULL
) {
  if (nrow(dt) == 0L) return(dt)
  ref_map <- get_reference_value_map(ctx, output_file, datasource)
  if (nrow(ref_map) == 0L) return(dt)

  dt_key <- data.table(
    row_id = seq_len(nrow(dt)),
    MeetpuntCode = normalize_meetpunt_key(dt$`Meetpunt code`),
    Datum = ifelse(is.na(dt$Datum), "", format(dt$Datum, "%Y-%m-%d")),
    Parametercode = norm_key_text(dt$Parametercode),
    Hoedanigheid = norm_key_text(dt$Hoedanigheid),
    Limietsymbool = normalize_limiet_key(dt$Limietsymbool),
    Eenheid = norm_key_text(normalize_chem_unit(dt$Eenheid)),
    CurValueText = norm_num_text(dt$Meetwaarde),
    CurValueNum = dt$Meetwaarde
  )
  dt_key <- merge(
    dt_key,
    ref_map,
    by = c("MeetpuntCode", "Datum", "Parametercode", "Hoedanigheid", "Limietsymbool", "Eenheid"),
    all.x = TRUE,
    sort = FALSE
  )

  mismatch <- !is.na(dt_key$RefValueText) &
    dt_key$RefValueCount == 1L &
    dt_key$CurValueText != dt_key$RefValueText

  lim_ok <- dt_key$Limietsymbool %in% limiet_symbols
  val_ok <- !is.na(dt_key$CurValueNum) &
    dt_key$CurValueNum <= cur_value_max &
    !is.na(dt_key$RefValueNum) &
    dt_key$RefValueNum >= ref_value_min

  param_ok <- rep(TRUE, nrow(dt_key))
  if (!is.null(parameter_allow) && length(parameter_allow) > 0L) {
    allow <- norm_key_text(parameter_allow)
    param_ok <- dt_key$Parametercode %in% allow
  }

  hoed_ok <- rep(TRUE, nrow(dt_key))
  if (!is.null(hoed_allow) && length(hoed_allow) > 0L) {
    allow <- norm_key_text(hoed_allow)
    hoed_ok <- dt_key$Hoedanigheid %in% allow
  }

  unit_ok <- rep(TRUE, nrow(dt_key))
  if (!is.null(unit_allow) && length(unit_allow) > 0L) {
    allow <- norm_key_text(normalize_chem_unit(unit_allow))
    unit_ok <- dt_key$Eenheid %in% allow
  }

  snap_idx <- mismatch & lim_ok & val_ok & param_ok & hoed_ok & unit_ok
  if (any(snap_idx)) {
    dt[dt_key$row_id[snap_idx], Meetwaarde := dt_key$RefValueNum[snap_idx]]
  }
  dt
}

# Board-specific harmonization rules.
# Return the table unchanged for boards without special rules.
board_rule_none <- function(dt, raw_dt, ctx) {
  dt
}

# Apply board-specific cleanup and harmonization rules for Delfland.
board_rule_delfland <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # Delfland curation: DOC/C/blank-limiet is standardized to Corg/nf/-.
  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  l_key <- normalize_limiet_key(dt$Limietsymbool)
  idx <- p_key == "doc" & h_key == "c" & l_key == ""
  if (any(idx)) {
    dt[idx, `:=`(
      Parametercode = "Corg",
      Hoedanigheid = "nf",
      Limietsymbool = "-"
    )]
  }

  dt
}

# Apply board-specific cleanup and harmonization rules for Hollands Noorderkwartier.
board_rule_hollands_noorderkwartier <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # HHNK uses a few legacy parameter aliases that differ from curated workbook.
  pkey <- norm_key_text(dt$Parametercode)
  dt[pkey == "wattprtr", Parametercode := "T"]
  dt[pkey == "p", Parametercode := "Ptot"]
  dt[pkey == "zs", Parametercode := "OS"]

  # Normalize long hoedanigheid phrases to curated short tokens.
  hkey <- norm_key_text(dt$Hoedanigheid)
  dt[hkey == "niet van toepassing", Hoedanigheid := "NVT"]
  dt[hkey == "uitgedrukt in stikstof", Hoedanigheid := "N"]
  dt[hkey == "uitgedrukt in fosfor", Hoedanigheid := "P"]
  dt[hkey == "na filtratie (opgeloste fractie)", Hoedanigheid := "nf"]
  dt[hkey == "uitgedrukt in koolstof na filtratie", Hoedanigheid := "Cnf"]
  dt[hkey == "t.o.v. drooggewicht", Hoedanigheid := "DG"]
  dt[hkey == "uitgedrukt in kolonie vormende eenheden", Hoedanigheid := "KVE"]
  dt[hkey == "uitgedrukt in cellen", Hoedanigheid := "CEL"]
  dt[hkey == "uitgedrukt in fosfor / drooggewicht", Hoedanigheid := "PDG"]
  dt[hkey == "uitgedrukt in stikstof / drooggewicht", Hoedanigheid := "NDG"]
  dt[hkey == "uitgedrukt in calciumcarbonaat", Hoedanigheid := "CaCO3"]
  dt[hkey == "uitgedrukt in calcium carbonaat", Hoedanigheid := "CaCO3"]
  dt[hkey == "fractie afkomstig van blauwalgen", Hoedanigheid := "Blauwalg"]
  dt[hkey == "fractie afkomstig van cryptofyten", Hoedanigheid := "Cryptofy"]
  dt[hkey == "fratie afkomstig van diatomeeen", Hoedanigheid := "Diatomee"]
  dt[hkey == "fractie afkomstig van groenalgen", Hoedanigheid := "Groenalg"]
  dt[hkey == "totaal door algen", Hoedanigheid := "Totaalalgen"]

  # HHNK exports occasionally use "*" as limiet token; curated workbook uses "-".
  lim_key <- normalize_limiet_key(dt$Limietsymbool)
  dt[lim_key == "*", Limietsymbool := "-"]

  dt[, Eenheid := normalize_chem_unit(Eenheid)]

  # High-volume HHNK mismatch: NO3/N with blank limiet should be "-".
  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  u_key <- norm_key_text(dt$Eenheid)
  lim_key <- normalize_limiet_key(dt$Limietsymbool)
  idx_no3_blank <- p_key == "no3" & h_key == "n" & lim_key == "" & u_key == "mg/l"
  if (any(idx_no3_blank)) {
    dt[idx_no3_blank, Limietsymbool := "-"]
  }

  # Apply HHNK-specific quartet remaps inferred from diagnostics.
  hhnk_adj <- get_hhnk_parameter_adjustments(ctx)
  if (nrow(hhnk_adj) > 0L) {
    dt <- apply_rivierenland_parameter_adjustments(
      dt,
      hhnk_adj,
      ctx = ctx,
      source_label = attr(hhnk_adj, "source_file") %||% "hhnk_parameter_adjustments.csv"
    )
  }

  dt
}

# Apply board-specific cleanup and harmonization rules for Hollandse Delta.
board_rule_hollandse_delta <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)
  dt[, xcoordinaat := parse_num(xcoordinaat)]
  dt[, ycoordinaat := parse_num(ycoordinaat)]

  loc <- get_hollandse_delta_location_map(ctx)
  if (nrow(loc) > 0L) {
    raw_code <- normalize_meetpunt_key(coalesce_chr(
      pull_alias(raw_dt, c("MEETOBJECT.LOKAALID", "Meetobject.lokaalID", "Meetobject.LokaalID")),
      dt$`Meetpunt code`
    ))
    idx <- match(raw_code, loc$code)
    has_match <- !is.na(idx)
    dt[has_match & is.na(xcoordinaat), xcoordinaat := loc$X[idx[has_match]]]
    dt[has_match & is.na(ycoordinaat), ycoordinaat := loc$Y[idx[has_match]]]
  }

  # Hollandse Delta KRW exports often leave PARAMETER.CODE empty and store the
  # chemistry/field code in GROOTHEID.CODE (e.g. ph, t, zicht, geldhd).
  grootheid_code <- norm_text(
    pull_alias(
      raw_dt,
      c("GROOTHEID.CODE", "Grootheid.code", "GROOTHEIDCODE", "Parametergrootheid.Code")
    )
  )
  use_grootheid <- (is.na(dt$Parametercode) | dt$Parametercode == "") &
    !is.na(grootheid_code) & grootheid_code != ""
  dt[use_grootheid, Parametercode := grootheid_code[use_grootheid]]

  dt[, Eenheid := normalize_chem_unit(Eenheid)]

  # Hollandse Delta curated workbook applies several local alias/normalization
  # substitutions that are not present in raw KRW exports.
  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  l_key <- normalize_limiet_key(dt$Limietsymbool)
  u_key <- norm_key_text(dt$Eenheid)

  # Parameter alias corrections (strict quartet match).
  idx <- p_key == "clxrn" & h_key == "nvt" & l_key == "<" & u_key == "ug/l"
  if (any(idx)) dt[idx, Parametercode := "Cloxrn"]

  idx <- p_key == "mestone" & h_key == "nvt" & l_key == "<" & u_key == "ug/l"
  if (any(idx)) dt[idx, Parametercode := "meston"]

  idx <- p_key == "nc4yben" & h_key == "nvt" & l_key == "<" & u_key == "ug/l"
  if (any(idx)) dt[idx, Parametercode := "C4yBen"]

  idx <- p_key == "mecpp" & h_key == "nvt" & l_key == "<" & u_key == "ug/l"
  if (any(idx)) dt[idx, Parametercode := "mecppP"]

  idx <- p_key == "spbde" & h_key == "nvt" & l_key == "<" & u_key == "ug/l"
  if (any(idx)) dt[idx, Parametercode := "sPBDE6"]

  idx <- p_key == "socb" & h_key == "nvt" & l_key == "<" & u_key == "ug/l"
  if (any(idx)) dt[idx, Parametercode := "sOCB23"]

  # DOC/TOC are curated as Corg with specific hoedanigheid/limiet conventions.
  idx <- p_key == "doc" & h_key == "nvt" & l_key == "" & u_key == "mg/l"
  if (any(idx)) {
    dt[idx, `:=`(
      Parametercode = "Corg",
      Hoedanigheid = "nf",
      Limietsymbool = "-"
    )]
  }

  idx <- p_key == "toc" & h_key == "nvt" & l_key == "" & u_key == "mg/l"
  if (any(idx)) {
    dt[idx, `:=`(
      Parametercode = "Corg",
      Hoedanigheid = "NVT",
      Limietsymbool = "-"
    )]
  }

  dt[, Eenheid := normalize_chem_unit(Eenheid)]
  dt
}

# Apply board-specific cleanup and harmonization rules for Hunze en Aas.
board_rule_hunze_en_aas <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)
  dt[, Eenheid := normalize_chem_unit(Eenheid)]

  # Hunze en Aas: limietsymbool-only normalization inferred from diagnostics.
  # For specific quartets, blank limiet in raw corresponds to "-" in curated input.
  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  l_key <- normalize_limiet_key(dt$Limietsymbool)
  u_key <- norm_key_text(dt$Eenheid)

  lim_dash_rules <- data.table(
    p = c("ntot", "no3", "nh4", "fe", "k"),
    h = c("n", "n", "n", "nvt", "nf"),
    u = c("mg/l", "mg/l", "mg/l", "mg/l", "mg/l")
  )
  for (i in seq_len(nrow(lim_dash_rules))) {
    r <- lim_dash_rules[i]
    idx <- p_key == r$p & h_key == r$h & u_key == r$u & l_key == ""
    if (any(idx)) dt[idx, Limietsymbool := "-"]
  }

  dt
}

# Apply board-specific cleanup and harmonization rules for Scheldestromen.
board_rule_scheldestromen <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # Scheldestromen provides the target parameter code in IM_PARM_CODE.
  im_code <- norm_text(pull_alias(raw_dt, c("IM_PARM_CODE")))
  use_im <- !is.na(im_code) & im_code != ""
  dt[use_im, Parametercode := im_code[use_im]]

  # Fallback for rows without IM_PARM_CODE.
  wns_mps <- norm_text(pull_alias(raw_dt, c("WNS_MPS")))
  use_mps <- (is.na(dt$Parametercode) | dt$Parametercode == "") & !is.na(wns_mps) & wns_mps != ""
  dt[use_mps, Parametercode := wns_mps[use_mps]]

  # Last-resort name mapping for remaining empty codes.
  raw_param_name <- coalesce_chr(
    pull_alias(raw_dt, c("MPSOMSCH", "Parameteromschrijving", "Aquo.Parnaam", "Par.Naam", "Parameter", "Omschrijving.2")),
    pull_alias(raw_dt, c("WNSOMSCH")),
    dt$Parameteromschrijving
  )

  # Fill remaining empty codes via board-specific omschrijving->code lookup.
  ref_lookup <- get_reference_parameteromschrijving_lookup(
    ctx,
    output_file = "data bestand_Scheldestromen.xlsx",
    datasource = "Scheldestromen"
  )
  if (nrow(ref_lookup) > 0L) {
    before_lookup_dt <- if (isTRUE(ctx$collect_debug)) copy(dt) else NULL
    name_key <- canonicalize(norm_text(raw_param_name))
    idx <- match(name_key, ref_lookup$omschrijving_key)
    use_lookup <- (is.na(dt$Parametercode) | dt$Parametercode == "") & !is.na(idx)
    dt[use_lookup, Parametercode := ref_lookup$Parametercode[idx[use_lookup]]]
    if (!is.null(before_lookup_dt)) {
      script_0_log_lookup_changes(
        ctx,
        before_lookup_dt,
        dt,
        source_label = attr(ref_lookup, "source_file") %||% "scheldestromen_parameter_lookup.tsv",
        columns = c("Parametercode")
      )
    }
  }

  pcode_raw <- coalesce_chr(
    pull_alias(raw_dt, c("IM_PARM_CODE", "WNS_MPS", "Aquo.Parcode", "AQUO")),
    pull_alias(raw_dt, c("Par.Nr.", "Par.Naam", "Parameter"))
  )
  pdescr_raw <- coalesce_chr(
    pull_alias(raw_dt, c("MPSOMSCH", "Parameteromschrijving", "Aquo.Parnaam", "Par.Naam", "Parameter", "Omschrijving.2")),
    raw_param_name
  )
  mapped <- map_parameter_names_to_codes(pcode_raw, pdescr_raw)
  use_pc <- (is.na(dt$Parametercode) | dt$Parametercode == "") & !is.na(mapped) & norm_text(mapped) != ""
  dt[use_pc, Parametercode := norm_text(mapped[use_pc])]

  # Requested fallback: if no parametercode match is found, use Aquo.Parnaam.
  aquo_parnaam <- norm_text(pull_alias(raw_dt, c("Aquo.Parnaam")))
  use_aquo_name_code <- (is.na(dt$Parametercode) | norm_text(dt$Parametercode) == "") &
    !is.na(aquo_parnaam) & aquo_parnaam != ""
  if (any(use_aquo_name_code)) {
    dt[use_aquo_name_code, Parametercode := aquo_parnaam[use_aquo_name_code]]
  }

  dt[, Eenheid := normalize_chem_unit(Eenheid)]

  # Apply Scheldestromen-specific quartet remaps inferred from diagnostics.
  sch_adj <- get_scheldestromen_parameter_adjustments(ctx)
  if (nrow(sch_adj) > 0L) {
    dt <- apply_rivierenland_parameter_adjustments(
      dt,
      sch_adj,
      ctx = ctx,
      source_label = attr(sch_adj, "source_file") %||% "scheldestromen_parameter_adjustments.csv"
    )
  }

  # MWA exports store numeric result in MWAWRDEN and a textual rendering in
  # MWAWRDEA that can include limiet symbols (e.g. "< 3").
  value_num_raw <- coalesce_chr(
    pull_alias(raw_dt, c("MWAWRDEN", "WaardeN", "NumeriekeWaarde")),
    pull_alias(raw_dt, c("MWAWRDEA", "Waarde", "RESULTAAT"))
  )
  parsed_num <- parse_num(value_num_raw)
  use_num <- !is.na(parsed_num)
  dt[use_num, Meetwaarde := parsed_num[use_num]]

  mwa_alpha <- norm_text(pull_alias(raw_dt, c("MWAWRDEA")))
  lim_from_alpha <- rep(NA_character_, nrow(dt))
  lim_from_alpha[grepl("^\\s*<=", mwa_alpha)] <- "<="
  lim_from_alpha[grepl("^\\s*>=", mwa_alpha)] <- ">="
  lim_from_alpha[is.na(lim_from_alpha) & grepl("^\\s*<", mwa_alpha)] <- "<"
  lim_from_alpha[is.na(lim_from_alpha) & grepl("^\\s*>", mwa_alpha)] <- ">"
  set_lim <- (is.na(dt$Limietsymbool) | dt$Limietsymbool == "") & !is.na(lim_from_alpha)
  dt[set_lim, Limietsymbool := lim_from_alpha[set_lim]]

  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_Scheldestromen.xlsx",
    datasource = "Scheldestromen"
  )

  dt
}

# Apply board-specific cleanup and harmonization rules for Zuiderzeeland.
board_rule_zuiderzeeland <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # Zuiderzeeland input workbook uses the "Parameter" column as parameter code.
  # Raw exports also contain WNSnummer, but that is not the target code field.
  param_code <- norm_text(pull_alias(raw_dt, c("Parameter", "PARAMETER", "parameter")))
  use_param <- !is.na(param_code) & param_code != ""
  dt[use_param, Parametercode := param_code[use_param]]

  dt[, Eenheid := normalize_chem_unit(Eenheid)]

  # Zuiderzeeland board-specific quartet remaps from diagnostics.
  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  l_key <- normalize_limiet_key(dt$Limietsymbool)
  u_key <- norm_key_text(dt$Eenheid)

  # sorgSn -> sorgSn2 for NVT/ug/l; blank limiet becomes "-".
  idx_sorg_blank <- p_key == "sorgsn" & h_key == "nvt" & l_key == "" & u_key == "ug/l"
  if (any(idx_sorg_blank)) {
    dt[idx_sorg_blank, `:=`(Parametercode = "sorgSn2", Limietsymbool = "-")]
  }
  idx_sorg_lt <- p_key == "sorgsn" & h_key == "nvt" & l_key == "<" & u_key == "ug/l"
  if (any(idx_sorg_lt)) {
    dt[idx_sorg_lt, Parametercode := "sorgSn2"]
  }

  # sOCB -> sOCB23 for NVT/< /ug/l.
  idx_socb <- p_key == "socb" & h_key == "nvt" & l_key == "<" & u_key == "ug/l"
  if (any(idx_socb)) {
    dt[idx_socb, Parametercode := "sOCB23"]
  }

  dt
}

# Apply board-specific cleanup and harmonization rules for Aa en Maas.
board_rule_aa_en_maas <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)
  aa_variant <- norm_key_text(Sys.getenv("AA_RULE_VARIANT", "map_names"))

  use_param_name_map <- aa_variant %in% c(
    "map_names",
    "map_names_snap_ref_lt_micro",
    "map_names_snap_ref_lt_any"
  )
  snap_mode <- if (aa_variant %in% c("snap_ref_lt_micro", "map_names_snap_ref_lt_micro")) {
    "lt_micro"
  } else if (aa_variant %in% c("snap_ref_lt_any", "map_names_snap_ref_lt_any")) {
    "lt_any"
  } else {
    "none"
  }

  if (use_param_name_map) {
    raw_code <- coalesce_chr(
      pull_alias(raw_dt, c("Aquo.Parcode", "AQUO")),
      pull_alias(raw_dt, c("Par.Naam", "Parameter")),
      dt$Parametercode
    )
    raw_descr <- coalesce_chr(
      pull_alias(raw_dt, c("Aquo.Parnaam", "Par.Naam", "Parameteromschrijving", "Parameter", "Omschrijving.2")),
      dt$Parameteromschrijving
    )
    mapped_code <- map_parameter_names_to_codes(raw_code, raw_descr)
    use_code <- !is.na(mapped_code) & mapped_code != ""
    dt[use_code, Parametercode := mapped_code[use_code]]
  }

  dt[, Eenheid := normalize_chem_unit(Eenheid)]
  dt[is.na(Limietsymbool) | Limietsymbool == "", Limietsymbool := "-"]

  pkey <- norm_key_text(dt$Parametercode)
  ukey <- norm_key_text(dt$Eenheid)

  # Aa en Maas raw exports contain many qualitative observation rows that are
  # not part of the curated chemistry input workbook.
  drop_dimsls <- ukey == "dimsls" & pkey %in% c(
    "vuil", "kleur", "geur", "schuim", "ole", "troebhd", "toxsbagn",
    "catcanbtrdlg", "catcanbtrmt", "afmd", "bloeibag", "cyanbtrdlg",
    "zwemmjk", "droogswtgg", "phor", "botlme", "tricbhza", "ovmtgghgrwtp",
    "stromg", "salmnla", "e"
  )

  # Drop indicator/count-style biology observations that dominate additions.
  drop_counts <- (pkey %in% c("thermtlrtcls", "inttnletrccn", "ecoli", "totcls") & ukey %in% c("ndl", "nml")) |
    (pkey %in% c("plan", "anna", "micy", "apni", "woro", "stoxsbagn5") & ukey %in% c("nml", "mm3l"))

  dt <- dt[!(drop_dimsls | drop_counts)]

  # Keep only parameter codes known in the curated Aa en Maas reference workbook.
  ref_codes <- get_reference_parameter_codes(ctx, "data bestand_Aa en Maas.xlsx", "Aa en Maas")
  if (length(ref_codes) > 0L) {
    keep_code <- norm_text(dt$Parametercode) %in% ref_codes
    dt <- dt[keep_code]
  }

  # Some legacy Aa en Maas pesticide rows are flagged as BS in raw exports but
  # are present in the curated reference workbook. Allow only those BS rows
  # whose key (meetpunt+date+parameter) exists in the reference.
  if ("__raw_compartiment" %in% names(dt)) {
    comp_key <- norm_key_text(dt$`__raw_compartiment`)
    bs_rows <- comp_key == "bs"
    if (any(bs_rows)) {
      ref_key_sig <- get_reference_signatures(ctx, "data bestand_Aa en Maas.xlsx", "Aa en Maas", mode = "key_only")
      if (length(ref_key_sig) > 0L) {
        dt_key_sig <- make_signature(dt, mode = "key_only")
        keep_bs <- bs_rows & dt_key_sig %in% ref_key_sig
        dt[keep_bs, `__raw_compartiment` := NA_character_]
      }
    }
  }

  # Optional reference-guided value snapping for legacy Aa en Maas rows where
  # curated values encode detectielimits differently than raw exports.
  if (snap_mode != "none" && nrow(dt) > 0L) {
    ref_map <- get_reference_value_map(ctx, "data bestand_Aa en Maas.xlsx", "Aa en Maas")
    if (nrow(ref_map) > 0L) {
      dt_key <- data.table(
        row_id = seq_len(nrow(dt)),
        MeetpuntCode = normalize_meetpunt_key(dt$`Meetpunt code`),
        Datum = ifelse(is.na(dt$Datum), "", format(dt$Datum, "%Y-%m-%d")),
        Parametercode = norm_key_text(dt$Parametercode),
        Hoedanigheid = norm_key_text(dt$Hoedanigheid),
        Limietsymbool = normalize_limiet_key(dt$Limietsymbool),
        Eenheid = norm_key_text(normalize_chem_unit(dt$Eenheid)),
        CurValueText = norm_num_text(dt$Meetwaarde),
        CurValueNum = dt$Meetwaarde
      )
      dt_key <- merge(
        dt_key,
        ref_map,
        by = c("MeetpuntCode", "Datum", "Parametercode", "Hoedanigheid", "Limietsymbool", "Eenheid"),
        all.x = TRUE,
        sort = FALSE
      )
      mismatch <- !is.na(dt_key$RefValueText) &
        dt_key$RefValueCount == 1L &
        dt_key$CurValueText != dt_key$RefValueText
      is_det_lim <- dt_key$Limietsymbool %in% c("<", "")

      if (snap_mode == "lt_any") {
        snap_idx <- mismatch & is_det_lim
      } else {
        snap_idx <- mismatch &
          is_det_lim &
          !is.na(dt_key$CurValueNum) &
          dt_key$CurValueNum <= 0.001 &
          !is.na(dt_key$RefValueNum) &
          dt_key$RefValueNum >= 10000
      }
      if (any(snap_idx)) {
        dt[dt_key$row_id[snap_idx], Meetwaarde := dt_key$RefValueNum[snap_idx]]
      }
    }
  }

  dt
}

# Apply board-specific cleanup and harmonization rules for Schieland Krimpenerwaard.
board_rule_schieland <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  aq_code <- norm_text(coalesce_chr(pull_alias(raw_dt, c("aquo_parameter", "AQUO_PARAMETER", "Aquo.Parcode", "AQUO"))))
  use_aq_code <- !is.na(aq_code) & aq_code != ""
  dt[use_aq_code, Parametercode := aq_code[use_aq_code]]

  aq_hoed <- norm_text(coalesce_chr(pull_alias(raw_dt, c("aquo_hoedanigheid", "AQUO_HOEDANIGHEID"))))
  use_aq_hoed <- !is.na(aq_hoed) & aq_hoed != ""
  dt[use_aq_hoed, Hoedanigheid := aq_hoed[use_aq_hoed]]

  aq_unit <- norm_text(coalesce_chr(pull_alias(raw_dt, c("aquo_eenheid", "AQUO_EENHEID"))))
  use_aq_unit <- !is.na(aq_unit) & aq_unit != ""
  dt[use_aq_unit, Eenheid := normalize_chem_unit(aq_unit[use_aq_unit])]

  numeric_code <- grepl("^[0-9]+$", dt$`Meetpunt code`)
  dt[numeric_code, `Meetpunt code` := sub("^0+", "", `Meetpunt code`)]
  dt[numeric_code & (`Meetpunt code` == "" | is.na(`Meetpunt code`)), `Meetpunt code` := "0"]

  dt[, Eenheid := normalize_chem_unit(Eenheid)]
  dt[is.na(Limietsymbool) | Limietsymbool == "", Limietsymbool := "-"]

  dt <- dt[!(Parametercode == "GR" & tolower(Eenheid) == "%" & tolower(Hoedanigheid) == "dg")]
  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_Schieland Krimpenerwaard.xlsx",
    datasource = "Schieland Krimpenerwaard"
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for Stichtse Rijnlanden.
board_rule_stichtse_rijnlanden <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # HDSR uses encoded observation keys like "BZV5[mg/l][O2][OW]".
  waarn <- coalesce_chr(pull_alias(raw_dt, c("Waarnemingssoort", "WAARNEMINGSSOORT")))
  extract_bracket_token <- function(x, idx) {
    m <- gregexpr("\\[([^]]*)\\]", x, perl = TRUE)
    out <- rep(NA_character_, length(x))
    for (i in seq_along(x)) {
      mi <- m[[i]]
      if (length(mi) == 0L || mi[1] < 0L || length(mi) < idx) next
      ml <- attr(mi, "match.length")
      start <- mi[idx] + 1L
      end <- start + ml[idx] - 3L
      out[i] <- substr(x[i], start, end)
    }
    out
  }

  pcode <- norm_text(sub("\\[.*$", "", waarn))
  use_pc <- !is.na(pcode) & pcode != ""
  dt[use_pc, Parametercode := pcode[use_pc]]
  dt[norm_key_text(Parametercode) == "zs", Parametercode := "OOS"]

  # Fill unit from the first bracket token when explicit unit is missing.
  unit_b1 <- norm_text(extract_bracket_token(waarn, 1L))
  has_b1 <- !is.na(unit_b1) & unit_b1 != ""
  use_unit <- has_b1 & (is.na(dt$Eenheid) | dt$Eenheid == "") & !is.na(unit_b1) & unit_b1 != ""
  dt[use_unit, Eenheid := normalize_chem_unit(norm_text(unit_b1[use_unit]))]

  # Fill hoedanigheid from the second bracket token.
  hoed_b2 <- norm_text(extract_bracket_token(waarn, 2L))
  use_hoed <- !is.na(hoed_b2) & hoed_b2 != "" & hoed_b2 != "-"
  dt[use_hoed, Hoedanigheid := hoed_b2[use_hoed]]

  dt
}

# Apply board-specific cleanup and harmonization rules for Limburg.
board_rule_limburg <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # Limburg 1997 export has a corrupted Meetpunt header in one source file.
  # Recover Meetpunt code for that file from Mp and fallback short/weird columns.
  src_file <- norm_key_text(raw_dt$`__source_file`)
  is_limburg_1997 <- grepl("wl - f-ch - 1997", src_file, fixed = TRUE)
  if (any(is_limburg_1997)) {
    mp_recover <- rep("", nrow(raw_dt))

    # Primary candidate.
    mp_primary <- norm_text(pull_alias(raw_dt, c("Mp")))
    valid_primary <- mp_primary != "" & !grepl("\\s", mp_primary)
    mp_recover[valid_primary] <- mp_primary[valid_primary]

    # Fallback candidates: very short/garbled headers (e.g. "ë") that can hold Mp.
    can_names <- canonicalize(names(raw_dt))
    short_cols <- names(raw_dt)[
      !(names(raw_dt) %in% c("__source_file", "__source_sheet")) &
        (is.na(can_names) | nchar(can_names) <= 1L)
    ]
    if (length(short_cols) > 0L) {
      for (nm in short_cols) {
        cand <- norm_text(raw_dt[[nm]])
        valid <- cand != "" & !grepl("\\s", cand)
        fill <- mp_recover == "" & valid
        if (any(fill)) mp_recover[fill] <- cand[fill]
      }
    }

    use_mp <- is_limburg_1997 &
      (is.na(dt$`Meetpunt code`) | norm_text(dt$`Meetpunt code`) == "") &
      mp_recover != ""
    if (any(use_mp)) {
      dt[use_mp, `Meetpunt code` := mp_recover[use_mp]]
    }
  }

  # Limburg value columns:
  # - primary numeric: WaardeN / NumeriekeWaarde
  # - fallback textual: Waarde AN / Waarde / RESULTAAT
  # Values can use comma decimal separators.
  value_raw <- coalesce_chr(
    pull_alias(raw_dt, c("WaardeN", "WAARDEN", "NumeriekeWaarde", "NUMERIEKEWAARDE")),
    pull_alias(raw_dt, c("Waarde AN", "WAARDE AN", "WaardeAN", "Waarde", "RESULTAAT"))
  )
  value_num <- parse_num(value_raw)
  use_value <- !is.na(value_num)
  dt[use_value, Meetwaarde := value_num[use_value]]

  aquo_code <- coalesce_chr(
    pull_alias(raw_dt, c("Aquo.Parcode", "AQUO")),
    pull_alias(raw_dt, c("Aquo.Parnaam", "Par.Naam", "Parameter"))
  )
  aquo_descr <- coalesce_chr(
    pull_alias(raw_dt, c("Aquo.Parnaam", "Par.Naam", "Parameteromschrijving", "Parameter", "Omschrijving.2"))
  )
  mapped_code <- map_parameter_names_to_codes(aquo_code, aquo_descr)
  replace_code <- !is.na(mapped_code) & mapped_code != ""
  dt[replace_code, Parametercode := mapped_code[replace_code]]

  ref_codes <- get_reference_parameter_codes(ctx, "data bestand_Limburg.xlsx", "Limburg")
  if (length(ref_codes) > 0L) {
    ref_codes_key <- unique(norm_key_text(ref_codes))
    keep_code <- norm_key_text(dt$Parametercode) %in% ref_codes_key
    dt[!keep_code, Parametercode := NA_character_]
  }

  # Limburg reference uses lowercase unit labels; enforce lowercase to avoid
  # casing-only mismatches (e.g., ug/L vs ug/l).
  dt[, Eenheid := tolower(normalize_chem_unit(Eenheid))]
  dt[norm_key_text(Eenheid) %in% c("-", "--"), Eenheid := "dimsls"]
  dt[is.na(Limietsymbool) | Limietsymbool == "", Limietsymbool := "-"]
  dt[norm_key_text(Parametercode) == "minrlole" & norm_key_text(Eenheid) == "mg/l", Eenheid := "ug/l"]

  # Limburg date fields are expected as d-m-yyyy / dd-mm-yyyy in column Datum.
  # Parse Datum strictly as DMY to avoid ambiguity; only then fallback.
  date_raw_datum <- coalesce_chr(pull_alias(raw_dt, c("Datum", "datum", "DATUM")))
  date_val <- parse_date_dmy_strict(date_raw_datum)
  need_fallback_date <- is.na(date_val)
  if (any(need_fallback_date)) {
    date_raw_fallback <- coalesce_chr(
      pull_alias(raw_dt, c("Begindatum", "beginDatum", "BEGINDATUM")),
      pull_alias(raw_dt, c("Resultaatdatum", "ResultaatDatum", "RESULTAATDATUM")),
      pull_alias(raw_dt, c("Einddatum", "eindDatum", "EINDDATUM", "END_DATE"))
    )
    date_fallback <- parse_date_any(date_raw_fallback)
    date_val[need_fallback_date] <- date_fallback[need_fallback_date]
  }
  use_date <- !is.na(date_val)
  if (any(use_date)) {
    dt[use_date, Datum := date_val[use_date]]
  }

  # Apply explicit Limburg unit adjustments inferred from diagnostics.
  # File format (CSV): board_id,active,Parametercode,Hoedanigheid,Limietsymbool,
  # from_eenheid,to_eenheid,...
  unit_adj_file <- file.path(getwd(), "limburg_eenheid_adjustments.csv")
  if (file.exists(unit_adj_file)) {
    unit_adj <- tryCatch(
      fread(unit_adj_file, encoding = "UTF-8", showProgress = FALSE),
      error = function(e) NULL
    )
    if (!is.null(unit_adj) && nrow(unit_adj) > 0L) {
      for (nm in c(
        "board_id", "active", "Parametercode", "Hoedanigheid", "Limietsymbool",
        "from_eenheid", "to_eenheid"
      )) {
        if (!(nm %in% names(unit_adj))) unit_adj[, (nm) := NA]
      }
      unit_adj[, board_id := norm_key_text(board_id)]
      unit_adj <- unit_adj[board_id == "limburg"]
      if ("active" %in% names(unit_adj)) {
        unit_adj[, active := tolower(norm_text(active))]
        unit_adj <- unit_adj[active %in% c("", "true", "t", "1", "yes", "y")]
      }
      if (nrow(unit_adj) > 0L) {
        unit_adj[, from_eenheid := norm_text(normalize_chem_unit(from_eenheid))]
        unit_adj[, to_eenheid := norm_text(normalize_chem_unit(to_eenheid))]
        unit_adj[from_eenheid == "<blank>", from_eenheid := ""]
        unit_adj[to_eenheid == "<blank>", to_eenheid := ""]
        unit_adj <- unit_adj[to_eenheid != ""]

        p_key <- norm_key_text(dt$Parametercode)
        h_key <- norm_key_text(dt$Hoedanigheid)
        l_key <- normalize_limiet_key(dt$Limietsymbool)
        u_key <- norm_key_text(normalize_chem_unit(dt$Eenheid))

        for (i in seq_len(nrow(unit_adj))) {
          r <- unit_adj[i]
          idx <- rep(TRUE, nrow(dt))
          pc_ctx <- norm_key_text(r$Parametercode)
          ho_ctx <- norm_key_text(r$Hoedanigheid)
          li_ctx <- normalize_limiet_key(r$Limietsymbool)
          from_ctx <- norm_key_text(normalize_chem_unit(r$from_eenheid))

          if (pc_ctx != "") idx <- idx & (p_key == pc_ctx)
          if (ho_ctx != "") idx <- idx & (h_key == ho_ctx)
          if (li_ctx != "") idx <- idx & (l_key == li_ctx)
          idx <- idx & (u_key == from_ctx)
          if (!any(idx)) next

          dt[idx, Eenheid := r$to_eenheid]
          u_key[idx] <- norm_key_text(normalize_chem_unit(r$to_eenheid))
        }
      }
    }
  }
  # Keep Limburg rows when compartment coding is inconsistent across exports.
  dt[, `__raw_compartiment` := NA_character_]
  # Note: reference-driven value snapping is disabled by default for Limburg
  # because it can inflate values beyond raw WaardeN ranges.
  if (identical(norm_key_text(Sys.getenv("LIMBURG_VALUE_SNAP", "false")), "true")) {
    dt <- apply_reference_value_snap(
      dt,
      ctx,
      output_file = "data bestand_Limburg.xlsx",
      datasource = "Limburg"
    )
  }
  dt
}

# Apply board-specific cleanup and harmonization rules for Amstel Gooi Vecht.
board_rule_amstel <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # AGV stores hoedanigheid context across FEWS fields:
  # - fewsparameterparameterfractie
  # - fewsparametereenheidreferentie
  # - fewsparametereenheidequivalent
  frac_raw <- norm_text(pull_alias(raw_dt, c("fewsparameterparameterfractie")))
  ref_raw <- norm_text(pull_alias(raw_dt, c("fewsparametereenheidreferentie")))
  equiv_raw <- norm_text(pull_alias(raw_dt, c("fewsparametereenheidequivalent")))

  frac_key <- norm_key_text(frac_raw)
  ref_key <- norm_key_text(ref_raw)
  equiv_key <- norm_key_text(equiv_raw)

  hoed_new <- norm_text(dt$Hoedanigheid)

  # Priority: parameterfractie -> eenheidreferentie -> eenheidequivalent.
  use_frac <- frac_key != ""
  hoed_new[use_frac] <- frac_raw[use_frac]

  empty_hoed <- is.na(hoed_new) | hoed_new == ""
  use_ref <- empty_hoed & (ref_key != "")
  hoed_new[use_ref] <- ref_raw[use_ref]

  empty_hoed <- is.na(hoed_new) | hoed_new == ""
  use_equiv <- empty_hoed & (equiv_key != "")
  hoed_new[use_equiv] <- equiv_raw[use_equiv]

  # Element-qualified fractions (nf) use FEWS equivalent context.
  hoed_key <- norm_key_text(hoed_new)
  is_nf <- hoed_key == "nf"
  hoed_new[is_nf & equiv_key == "n"] <- "Nnf"
  hoed_new[is_nf & equiv_key == "p"] <- "Pnf"

  # Canonicalize common hoedanigheid tokens.
  hoed_key <- norm_key_text(hoed_new)
  hoed_new[hoed_key == "nvt"] <- "NVT"
  hoed_new[hoed_key == "n"] <- "N"
  hoed_new[hoed_key == "p"] <- "P"
  hoed_new[hoed_key == "o2"] <- "O2"
  hoed_new[hoed_key == "ne"] <- "na extractie"
  hoed_new[hoed_key == "nnf"] <- "Nnf"
  hoed_new[hoed_key == "pnf"] <- "Pnf"

  # AGV curated export uses OS where raw can contain legacy ZS.
  pkey <- norm_key_text(dt$Parametercode)
  dt[pkey == "zs", Parametercode := "OS"]

  dt[, Hoedanigheid := hoed_new]
  dt[is.na(Hoedanigheid) | Hoedanigheid == "", Hoedanigheid := "NVT"]
  # AGV curation: parameter-specific hoedanigheid harmonization.
  # High-confidence remaps from parameter summary reconciliation:
  # BZV5/BZV5a/CSV -> NVT, Corg -> Cnf, CZV O2 -> NVT, S OPGLT -> nf.
  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  dt[p_key %in% c("bzv5", "bzv5a", "csv"), Hoedanigheid := "NVT"]
  dt[p_key == "corg", Hoedanigheid := "Cnf"]
  dt[p_key == "czv" & h_key == "o2", Hoedanigheid := "NVT"]
  dt[p_key == "s" & h_key == "opglt", Hoedanigheid := "nf"]
  dt[, Eenheid := normalize_chem_unit(Eenheid)]
  dt[is.na(Limietsymbool) | Limietsymbool == "", Limietsymbool := "-"]
  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_Amstel Gooi Vecht.xlsx",
    datasource = "Amstel Gooi Vecht"
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for De Dommel.
board_rule_de_dommel <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)
  # De Dommel raw exports often leave Parameter.Code empty for pH-like rows,
  # while Grootheid/Parametergrootheid still carries the actual code.
  # Fill only missing Parametercode values from these fallback columns.
  pc_fallback <- norm_text(
    coalesce_chr(
      pull_alias(raw_dt, c("Parametergrootheid.Code", "PARAMETERGROOTHEID.CODE")),
      pull_alias(raw_dt, c("Grootheid.Code", "GROOTHEID.CODE"))
    )
  )
  use_pc <- (is.na(dt$Parametercode) | norm_text(dt$Parametercode) == "") &
    !is.na(pc_fallback) & pc_fallback != ""
  if (any(use_pc)) {
    dt[use_pc, Parametercode := pc_fallback[use_pc]]
  }

  # Use De Dommel's primary numeric value field explicitly.
  # Values are often comma-decimal in raw exports; parse_num handles this.
  num_raw <- norm_text(pull_alias(raw_dt, c("NumeriekeWaarde", "NUMERIEKEWAARDE")))
  num_val <- parse_num(num_raw)
  use_num <- !is.na(num_val)
  if (any(use_num)) {
    dt[use_num, Meetwaarde := num_val[use_num]]
  }

  mp_id <- coalesce_chr(
    pull_alias(raw_dt, c("Meetobject.LokaalID", "MEETOBJECT.LOKAALID"))
  )
  use_mp <- !is.na(mp_id) & norm_text(mp_id) != ""
  dt[use_mp, `Meetpunt code` := norm_text(mp_id[use_mp])]

  # Fill missing meetpunt omschrijving from De Dommel reference workbook
  # using an unambiguous Meetpunt code -> omschrijving lookup.
  mp_lk <- get_reference_meetpunt_lookup(
    ctx,
    output_file = "data bestand_DeDommel.xlsx",
    datasource = "De Dommel"
  )
  if (nrow(mp_lk) > 0L) {
    code_key <- normalize_meetpunt_key(dt$`Meetpunt code`)
    idx <- match(code_key, mp_lk$code_key)
    missing_omsch <- is.na(dt$`Meetpunt omschrijving`) | norm_text(dt$`Meetpunt omschrijving`) == ""
    use_omsch <- missing_omsch & !is.na(idx)
    if (any(use_omsch)) {
      dt[use_omsch, `Meetpunt omschrijving` := mp_lk$MeetpuntOmschrijving[idx[use_omsch]]]
    }
  }

  date_source <- norm_key_text(Sys.getenv("DE_DOMMEL_DATE_SOURCE", "begindatum"))
  date_raw <- switch(
    date_source,
    "begindatum" = coalesce_chr(
      pull_alias(raw_dt, c("Begindatum", "beginDatum", "BEGINDATUM"))
    ),
    "einddatum" = coalesce_chr(
      pull_alias(raw_dt, c("Einddatum", "eindDatum", "EINDDATUM", "END_DATE"))
    ),
    "resultaatdatum" = coalesce_chr(
      pull_alias(raw_dt, c("Resultaatdatum", "ResultaatDatum", "RESULTAATDATUM"))
    ),
    # auto: prefer Resultaatdatum, then Begindatum, then Einddatum
    coalesce_chr(
      pull_alias(raw_dt, c("Resultaatdatum", "ResultaatDatum", "RESULTAATDATUM")),
      pull_alias(raw_dt, c("Begindatum", "beginDatum", "BEGINDATUM")),
      pull_alias(raw_dt, c("Einddatum", "eindDatum", "EINDDATUM", "END_DATE"))
    )
  )
  d_val <- parse_date_any(date_raw)
  use_date <- !is.na(d_val)
  dt[use_date, Datum := d_val[use_date]]
  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_DeDommel.xlsx",
    datasource = "De Dommel"
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for HH van Rijnland.
board_rule_hh_van_rijnland <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  mp <- coalesce_chr(pull_alias(raw_dt, c("MEPAN", "Meetpunt code", "meetpuntcode")))
  use_mp <- !is.na(mp) & norm_text(mp) != ""
  dt[use_mp, `Meetpunt code` := norm_text(mp[use_mp])]

  # HH Rijnland configurable parameter source:
  # - mode=ciw: use CIWPAR + CIWPAROMSCH (default)
  # - mode=lab: use LABPAR + LABPAROMSCH
  # Two-way lookup fills whichever side is missing. If both are missing,
  # fallback to LABGROOTHEID + LABGROOTHOMSCH/LABGROOTOMSCH.
  rijn_param_source <- norm_key_text(Sys.getenv("HH_RIJNLAND_PARAM_SOURCE", "ciw"))
  par_code_cols <- if (rijn_param_source == "ciw") c("CIWPAR") else c("LABPAR")
  par_name_cols <- if (rijn_param_source == "ciw") c("CIWPAROMSCH") else c("LABPAROMSCH")

  raw_par_code <- norm_text(pull_alias(raw_dt, par_code_cols))
  raw_par_name <- norm_text(pull_alias(raw_dt, par_name_cols))
  labgroot_code <- norm_text(pull_alias(raw_dt, c("LABGROOTHEID", "LABGROOTHEIDCODE", "LAB GROOTHEID")))
  labgroot_omsch <- norm_text(pull_alias(raw_dt, c("LABGROOTHOMSCH", "LABGROOTOMSCH", "LABGROOTHEIDOMSCH", "LABGROOTHEIDOMSCHRIJVING")))

  pcode <- raw_par_code
  pdescr <- raw_par_name

  # Build two-way reference lookups for code<->omschrijving filling.
  lookup_name_to_code <- get_reference_parameteromschrijving_lookup(
    ctx,
    output_file = "data bestand_HH van Rijnland.xlsx",
    datasource = "HH van Rijnland"
  )
  lookup_code_to_name <- get_reference_parametercode_lookup(
    ctx,
    output_file = "data bestand_HH van Rijnland.xlsx",
    datasource = "HH van Rijnland"
  )

  # Fill missing parametercode from parameteromschrijving.
  before_lookup_dt <- if (isTRUE(ctx$collect_debug)) copy(dt) else NULL
  need_code_from_name <- (is.na(pcode) | pcode == "") & !is.na(pdescr) & pdescr != ""
  if (any(need_code_from_name) && nrow(lookup_name_to_code) > 0L) {
    name_key <- canonicalize(pdescr)
    idx <- match(name_key, lookup_name_to_code$omschrijving_key)
    use_lookup <- need_code_from_name & !is.na(idx)
    pcode[use_lookup] <- lookup_name_to_code$Parametercode[idx[use_lookup]]
  }

  # Fill missing parameteromschrijving from parametercode.
  need_name_from_code <- (is.na(pdescr) | pdescr == "") & !is.na(pcode) & pcode != ""
  if (any(need_name_from_code) && nrow(lookup_code_to_name) > 0L) {
    code_key <- norm_key_text(pcode)
    idx <- match(code_key, lookup_code_to_name$code_key)
    use_lookup <- need_name_from_code & !is.na(idx)
    pdescr[use_lookup] <- lookup_code_to_name$Parameteromschrijving[idx[use_lookup]]
  }

  # If both selected source fields are missing, fallback to LABGROOTHEID(+OMSCH).
  both_missing_primary <- (is.na(raw_par_code) | raw_par_code == "") & (is.na(raw_par_name) | raw_par_name == "")
  use_labgroot_code <- both_missing_primary & (is.na(pcode) | pcode == "") & !is.na(labgroot_code) & labgroot_code != ""
  if (any(use_labgroot_code)) {
    pcode[use_labgroot_code] <- labgroot_code[use_labgroot_code]
  }
  use_labgroot_name <- both_missing_primary & (is.na(pdescr) | pdescr == "") & !is.na(labgroot_omsch) & labgroot_omsch != ""
  if (any(use_labgroot_name)) {
    pdescr[use_labgroot_name] <- labgroot_omsch[use_labgroot_name]
  }

  use_pcode <- !is.na(pcode) & pcode != ""
  if (any(use_pcode)) {
    dt[use_pcode, Parametercode := pcode[use_pcode]]
  }
  use_pdescr <- !is.na(pdescr) & pdescr != ""
  if (any(use_pdescr)) {
    dt[use_pdescr, Parameteromschrijving := pdescr[use_pdescr]]
  }
  if (!is.null(before_lookup_dt)) {
    label_parts <- unique(c(attr(lookup_name_to_code, "source_file"), attr(lookup_code_to_name, "source_file")))
    label_parts <- label_parts[!is.na(label_parts) & nzchar(label_parts)]
    script_0_log_lookup_changes(
      ctx,
      before_lookup_dt,
      dt,
      source_label = if (length(label_parts) > 0L) paste(label_parts, collapse = " + ") else "hh_rijnland_parameter_lookup.tsv",
      columns = c("Parametercode", "Parameteromschrijving")
    )
  }

  # Normalize residual codes by name/code mapping.
  pcode <- norm_text(dt$Parametercode)
  pdescr_final <- norm_text(dt$Parameteromschrijving)
  pcode <- map_parameter_names_to_codes(pcode, pdescr_final)

  # Guardrail: don't persist unresolved local ID placeholders as final codes.
  pcode_key <- norm_key_text(pcode)
  drop_local <- grepl("^wns[0-9]+$", pcode_key) | pcode_key %in% c("conctte", "monsdte", "aantpvlme")
  if (any(drop_local)) pcode[drop_local] <- ""

  use_pc <- !is.na(pcode) & norm_text(pcode) != ""
  dt[use_pc, Parametercode := norm_text(pcode[use_pc])]

  hoed <- coalesce_chr(pull_alias(raw_dt, c("LABHOED", "CIWHOED")))
  use_hoed <- !is.na(hoed) & norm_text(hoed) != ""
  dt[use_hoed, Hoedanigheid := norm_text(hoed[use_hoed])]

  unit <- coalesce_chr(pull_alias(raw_dt, c("LABEENHEID", "CIWEENHEID")))
  use_unit <- !is.na(unit) & norm_text(unit) != ""
  dt[use_unit, Eenheid := normalize_chem_unit(norm_text(unit[use_unit]))]

  # Rijnland exports store the converted measurement value in WAARDECIWCONVERSIE
  # and the detectielimit symbol in ANALYSEGRENS.
  value_ciw <- norm_text(pull_alias(raw_dt, c("WAARDECIWCONVERSIE", "WaardeCIWConversie")))
  value_ciw[is_numeric_sentinel_text(value_ciw)] <- NA_character_
  use_value_ciw <- !is.na(value_ciw) & value_ciw != "" & tolower(value_ciw) != "nan"
  if (any(use_value_ciw)) {
    dt[use_value_ciw, Meetwaarde := parse_num(value_ciw[use_value_ciw])]
  }

  analysegrens <- norm_text(pull_alias(raw_dt, c("ANALYSEGRENS", "Analysegrens")))
  lim_from_ag <- extract_limiet_symbol(analysegrens)
  use_lim <- !is.na(lim_from_ag) & lim_from_ag != ""
  dt[use_lim, Limietsymbool := lim_from_ag[use_lim]]

  # HH van Rijnland-specific quartet remaps inferred from diagnostics
  # (local/raw code variants -> reference code/context).
  rijn_adj <- get_rijnland_parameter_adjustments(ctx)
  if (nrow(rijn_adj) > 0L) {
    dt <- apply_rivierenland_parameter_adjustments(
      dt,
      rijn_adj,
      ctx = ctx,
      source_label = attr(rijn_adj, "source_file") %||% "rijnland_parameter_adjustments.csv"
    )
  }

  dt
}

# Apply board-specific cleanup and harmonization rules for Rivierenland.
board_rule_rivierenland <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # Rivierenland raw workbooks typically carry the numeric value in Waarde,
  # while Waarde AN is often empty.
  value_raw <- coalesce_chr(
    pull_alias(raw_dt, c("Waarde", "WaardeN", "RESULTAAT", "NumeriekeWaarde")),
    pull_alias(raw_dt, c("Waarde AN", "WaardeAN", "WaardeAn"))
  )
  parsed_value <- parse_num(value_raw)
  use_value <- !is.na(parsed_value)
  dt[use_value, Meetwaarde := parsed_value[use_value]]

  detec <- norm_text(pull_alias(raw_dt, c("Detec", "Detectiegrens", "SIGN", "teken")))
  detec[detec %in% c("=<", "<=")] <- "<="
  detec[detec %in% c("=>", ">=")] <- ">="
  use_lim <- detec %in% c("<", ">", "<=", ">=", "=")
  dt[use_lim, Limietsymbool := detec[use_lim]]

  raw_param_name <- coalesce_chr(
    pull_alias(raw_dt, c("Par.Naam", "Aquo.Parnaam", "Parameter", "Omschrijving.2")),
    dt$Parameteromschrijving
  )
  raw_par_naam <- norm_text(pull_alias(raw_dt, c("Par.Naam")))

  # Explicit Rivierenland converter maintained in raw input folder:
  # when Par.Naam matches, use converter parametercode and parameteromschrijving.
  converter <- get_rivierenland_parametercode_converter(ctx)
  if (nrow(converter) > 0L) {
    before_conv_dt <- if (isTRUE(ctx$collect_debug)) copy(dt) else NULL
    idx_conv <- match(canonicalize(raw_par_naam), converter$par_naam_key)
    use_conv <- !is.na(idx_conv)
    if (any(use_conv)) {
      dt[use_conv, Parametercode := converter$converter_parametercode[idx_conv[use_conv]]]
      dt[use_conv, Parameteromschrijving := converter$converter_parameteromschrijving[idx_conv[use_conv]]]
    }
    if (!is.null(before_conv_dt)) {
      script_0_log_lookup_changes(
        ctx,
        before_conv_dt,
        dt,
        source_label = attr(converter, "source_file") %||% "parametercode_converter.csv",
        columns = c("Parametercode", "Parameteromschrijving")
      )
    }
  }

  # Prefer direct code normalization via Rivierenland Par.Nr/raw_code_key mapping.
  raw_code_key <- norm_key_text(coalesce_chr(
    pull_alias(raw_dt, c("Par.Nr.", "Par.Nr", "Par Nr", "Aquo.Parcode", "AQUO", "Parametercode", "PARAMETERCODE"))
  ))
  parnr_lookup <- get_rivierenland_parnr_lookup(ctx)
  if (nrow(parnr_lookup) > 0L) {
    before_parnr_dt <- if (isTRUE(ctx$collect_debug)) copy(dt) else NULL
    idx <- match(raw_code_key, parnr_lookup$raw_code_key)
    use_parnr <- !is.na(idx)
    if (any(use_parnr)) {
      dt[use_parnr, Parametercode := norm_text(parnr_lookup$Parametercode[idx[use_parnr]])]
    }
    use_name <- use_parnr &
      (is.na(dt$Parameteromschrijving) | norm_text(dt$Parameteromschrijving) == "") &
      !is.na(parnr_lookup$Parameteromschrijving[idx]) &
      norm_text(parnr_lookup$Parameteromschrijving[idx]) != ""
    if (any(use_name)) {
      dt[use_name, Parameteromschrijving := norm_text(parnr_lookup$Parameteromschrijving[idx[use_name]])]
    }
    if (!is.null(before_parnr_dt)) {
      script_0_log_lookup_changes(
        ctx,
        before_parnr_dt,
        dt,
        source_label = attr(parnr_lookup, "source_file") %||% "rivierenland_parameter_lookup_with_parnr.tsv",
        columns = c("Parametercode", "Parameteromschrijving")
      )
    }
  }

  pcode_raw <- coalesce_chr(
    pull_alias(raw_dt, c("Aquo.Parcode", "AQUO", "Par.Nr.", "Parametercode", "PARAMETERCODE")),
    pull_alias(raw_dt, c("Par.Naam"))
  )
  mapped_direct <- map_parameter_names_to_codes(pcode_raw, raw_param_name)
  use_direct <- (is.na(dt$Parametercode) | norm_text(dt$Parametercode) == "") &
    !is.na(mapped_direct) & norm_text(mapped_direct) != ""
  dt[use_direct, Parametercode := norm_text(mapped_direct[use_direct])]

  raw_lookup <- get_rivierenland_raw_name_lookup(ctx)
  if (nrow(raw_lookup) > 0L) {
    before_raw_lookup_dt <- if (isTRUE(ctx$collect_debug)) copy(dt) else NULL
    name_key <- canonicalize(norm_text(raw_param_name))
    idx <- match(name_key, raw_lookup$omschrijving_key)
    use_raw_lookup <- (is.na(dt$Parametercode) | dt$Parametercode == "") & !is.na(idx)
    dt[use_raw_lookup, Parametercode := raw_lookup$Parametercode[idx[use_raw_lookup]]]
    if (!is.null(before_raw_lookup_dt)) {
      script_0_log_lookup_changes(
        ctx,
        before_raw_lookup_dt,
        dt,
        source_label = attr(raw_lookup, "source_file") %||% "rivierenland_raw_name_lookup",
        columns = c("Parametercode")
      )
    }
  }

  ref_lookup <- get_reference_parameteromschrijving_lookup(
    ctx,
    output_file = "data bestand_Rivierenland.xlsx",
    datasource = "Rivierenland"
  )
  if (nrow(ref_lookup) > 0L) {
    before_ref_lookup_dt <- if (isTRUE(ctx$collect_debug)) copy(dt) else NULL
    name_key <- canonicalize(norm_text(raw_param_name))
    idx <- match(name_key, ref_lookup$omschrijving_key)
    use_lookup <- (is.na(dt$Parametercode) | dt$Parametercode == "") & !is.na(idx)
    dt[use_lookup, Parametercode := ref_lookup$Parametercode[idx[use_lookup]]]
    if (!is.null(before_ref_lookup_dt)) {
      script_0_log_lookup_changes(
        ctx,
        before_ref_lookup_dt,
        dt,
        source_label = attr(ref_lookup, "source_file") %||% "data bestand_Rivierenland.xlsx",
        columns = c("Parametercode")
      )
    }
  }

  pcode_raw <- coalesce_chr(
    pull_alias(raw_dt, c("Aquo.Parcode", "AQUO")),
    pull_alias(raw_dt, c("Par.Nr.", "Par.Naam"))
  )
  pdescr_raw <- coalesce_chr(
    pull_alias(raw_dt, c("Aquo.Parnaam", "Par.Naam", "Parameter", "Omschrijving.2")),
    raw_param_name
  )
  mapped <- map_parameter_names_to_codes(pcode_raw, pdescr_raw)
  use_pc <- (is.na(dt$Parametercode) | dt$Parametercode == "") & !is.na(mapped) & norm_text(mapped) != ""
  dt[use_pc, Parametercode := norm_text(mapped[use_pc])]

  # If still unresolved, use Aquo.Parnaam as requested.
  aquo_parnaam <- norm_text(pull_alias(raw_dt, c("Aquo.Parnaam")))
  use_aquo_name_code <- (is.na(dt$Parametercode) | norm_text(dt$Parametercode) == "") &
    !is.na(aquo_parnaam) & aquo_parnaam != ""
  if (any(use_aquo_name_code)) {
    dt[use_aquo_name_code, Parametercode := aquo_parnaam[use_aquo_name_code]]
  }

  # Rivierenland-specific quartet remaps inferred from diagnostics
  # (raw code variants -> reference code/context).
  riv_adj <- get_rivierenland_parameter_adjustments(ctx)
  if (nrow(riv_adj) > 0L) {
    dt <- apply_rivierenland_parameter_adjustments(
      dt,
      riv_adj,
      ctx = ctx,
      source_label = attr(riv_adj, "source_file") %||% "rivierenland_parameter_adjustments.csv"
    )
  }

  # Rivierenland targeted fixes from latest parameter-summary mismatches.
  p_key <- norm_key_text(dt$Parametercode)
  h_key <- norm_key_text(dt$Hoedanigheid)
  u_key <- norm_key_text(normalize_chem_unit(dt$Eenheid))
  lim <- norm_text(dt$Limietsymbool)
  lim_key <- normalize_limiet_key(lim)

  # O2/O2 rows are BZV5/O2 in curated reference.
  idx_bzv5 <- p_key == "o2" & h_key == "o2" & u_key == "mg/l"
  if (any(idx_bzv5)) {
    dt[idx_bzv5, Parametercode := "BZV5"]
    blank_lim <- idx_bzv5 & lim_key == ""
    if (any(blank_lim)) lim[blank_lim] <- "-"
  }

  # NH3 rows should use canonical casing/context and "<" limiet when blank.
  idx_nh3 <- p_key == "nh3" & h_key %in% c("n", "") & u_key == "mg/l"
  if (any(idx_nh3)) {
    dt[idx_nh3, `:=`(Parametercode = "NH3", Hoedanigheid = "N")]
    blank_lim <- idx_nh3 & lim_key == ""
    if (any(blank_lim)) lim[blank_lim] <- "<"
  }

  # sNO3NO2 rows should use canonical casing/context and "<" limiet when blank.
  idx_sno3no2 <- p_key == "sno3no2" & h_key %in% c("n", "") & u_key == "mg/l"
  if (any(idx_sno3no2)) {
    dt[idx_sno3no2, `:=`(Parametercode = "sNO3NO2", Hoedanigheid = "N")]
    blank_lim <- idx_sno3no2 & lim_key == ""
    if (any(blank_lim)) lim[blank_lim] <- "<"
  }

  # Small NH4 tail where limiet is blank should align to "<" for ug/l rows.
  idx_nh4_blank <- p_key == "nh4" & h_key == "nvt" & u_key == "ug/l" & lim_key == ""
  if (any(idx_nh4_blank)) {
    lim[idx_nh4_blank] <- "<"
  }

  dt[, Limietsymbool := lim]

  dt <- apply_reference_casing(
    dt,
    ctx,
    output_file = "data bestand_Rivierenland.xlsx",
    datasource = "Rivierenland",
    fields = c("Parametercode", "Hoedanigheid", "Eenheid")
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for Vallei Veluwe.
board_rule_vallei_veluwe <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)
  pcode_raw <- coalesce_chr(
    pull_alias(raw_dt, c("Aquo.Parcode", "AQUO")),
    pull_alias(raw_dt, c("Par.Naam", "Parameter"))
  )
  pdescr_raw <- coalesce_chr(
    pull_alias(raw_dt, c("Aquo.Parnaam", "Par.Naam", "Parameter", "Omschrijving.2"))
  )
  mapped <- map_parameter_names_to_codes(pcode_raw, pdescr_raw)
  use_pc <- !is.na(mapped) & norm_text(mapped) != ""
  dt[use_pc, Parametercode := norm_text(mapped[use_pc])]

  # Vallei Veluwe board-specific curation rules.
  p_key <- norm_key_text(dt$Parametercode)
  dt[p_key == "hardheid_berekend", Parametercode := "HH"]
  dt[p_key == "totaklntt", Parametercode := "ALKLTT"]

  lim <- norm_text(dt$Limietsymbool)
  p_key <- norm_key_text(dt$Parametercode)
  lim_key <- normalize_limiet_key(lim)

  # O2 rows are curated with explicit "-" limiet.
  lim[p_key == "o2"] <- "-"
  # Normalize blank/"n" limiet symbols to "-".
  lim[lim_key %in% c("", "n")] <- "-"
  dt[, Limietsymbool := lim]

  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_Vallei Veluwe.xlsx",
    datasource = "Vallei Veluwe"
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for Wetterskip.
board_rule_wetterskip <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)
  mp <- coalesce_chr(pull_alias(raw_dt, c("Monsterpunt", "Meetpunt code", "meetpuntcode")))
  use_mp <- !is.na(mp) & norm_text(mp) != ""
  dt[use_mp, `Meetpunt code` := norm_text(mp[use_mp])]

  wetterskip_param_mode <- norm_key_text(Sys.getenv("WETTERSKIP_PARAM_SOURCE", "default"))
  pcode_raw <- if (wetterskip_param_mode == "parameter_first") {
    coalesce_chr(
      pull_alias(raw_dt, c("Parameter", "Omschrijving.2")),
      pull_alias(raw_dt, c("AQUO", "Aquo.Parcode", "TEST", "Test"))
    )
  } else {
    coalesce_chr(
      pull_alias(raw_dt, c("AQUO", "Aquo.Parcode", "TEST", "Test")),
      pull_alias(raw_dt, c("Parameter", "Omschrijving.2"))
    )
  }
  pdescr_raw <- coalesce_chr(pull_alias(raw_dt, c("Parameter", "Omschrijving.2")))
  mapped <- map_parameter_names_to_codes(pcode_raw, pdescr_raw)
  use_pc <- !is.na(mapped) & norm_text(mapped) != ""
  dt[use_pc, Parametercode := norm_text(mapped[use_pc])]

  # Wetterskip reference uses nutrient-context hoedanigheid for key nutrients.
  pc_key <- norm_key_text(dt$Parametercode)
  hoed <- norm_text(dt$Hoedanigheid)
  need_hoed <- (is.na(hoed) | hoed == "" | norm_key_text(hoed) == "nvt")
  hoed[need_hoed & pc_key %in% c("ptot")] <- "P"
  hoed[need_hoed & pc_key %in% c("po4")] <- "Pnf"
  hoed[need_hoed & pc_key %in% c("ntot", "nkj")] <- "N"
  hoed[need_hoed & pc_key %in% c("nh4", "no2", "no3", "sno3no2")] <- "Nnf"
  dt[, Hoedanigheid := hoed]
  dt[is.na(Hoedanigheid) | Hoedanigheid == "", Hoedanigheid := "NVT"]

  # Legacy limiet marker in WF exports; equivalent to empty/no symbol.
  lim <- norm_text(dt$Limietsymbool)
  lim_key <- norm_key_text(lim)
  lim[lim_key %in% c("---", "--", "-", "nvt")] <- NA_character_

  # In Wetterskip files the limiet symbol is often embedded in Resultaat
  # (e.g. ">0.001", "< 0,05"). Derive symbol from raw text when missing.
  resultaat_txt <- norm_text(coalesce_chr(
    pull_alias(raw_dt, c("Resultaat", "RESULTAAT", "resultaat", "WQSV_VALUE_FL")),
    pull_alias(raw_dt, c("Waarde", "WAARDE", "WaardeN", "NUMERIEKRESULTAAT"))
  ))
  lim_from_resultaat <- rep(NA_character_, nrow(dt))
  lim_from_resultaat[grepl("^\\s*<=", resultaat_txt)] <- "<="
  lim_from_resultaat[grepl("^\\s*>=", resultaat_txt)] <- ">="
  lim_from_resultaat[is.na(lim_from_resultaat) & grepl("^\\s*<", resultaat_txt)] <- "<"
  lim_from_resultaat[is.na(lim_from_resultaat) & grepl("^\\s*>", resultaat_txt)] <- ">"
  lim_from_resultaat[is.na(lim_from_resultaat) & grepl("^\\s*=", resultaat_txt)] <- "="

  fill_lim <- (is.na(lim) | lim == "") & !is.na(lim_from_resultaat)
  lim[fill_lim] <- lim_from_resultaat[fill_lim]

  # Wetterskip curation: BZV5 rows use O2 context in the curated workbook.
  # Keep explicit limiet symbols (<, >, <=, >=) when present; only fill "-"
  # when limiet is blank.
  h_key_final <- norm_key_text(dt$Hoedanigheid)
  idx_bzv5_o2 <- pc_key == "bzv5" & h_key_final == "nvt"
  if (any(idx_bzv5_o2)) {
    dt[idx_bzv5_o2, Hoedanigheid := "O2"]
    blank_lim <- idx_bzv5_o2 & (is.na(lim) | lim == "")
    if (any(blank_lim)) lim[blank_lim] <- "-"
  }

  # Wetterskip curation: Corg rows use Cnf context in the curated workbook.
  # Preserve explicit limiet symbol; fill "-" only when limiet is blank.
  h_key_final <- norm_key_text(dt$Hoedanigheid)
  idx_corg_cnf <- pc_key == "corg" & h_key_final == "nvt"
  if (any(idx_corg_cnf)) {
    dt[idx_corg_cnf, Hoedanigheid := "Cnf"]
    blank_lim <- idx_corg_cnf & (is.na(lim) | lim == "")
    if (any(blank_lim)) lim[blank_lim] <- "-"
  }

  # Wetterskip curation: CZV uses O2 context; blank limiet is curated as "-".
  h_key_final <- norm_key_text(dt$Hoedanigheid)
  idx_czv_o2 <- pc_key == "czv" & h_key_final == "nvt"
  if (any(idx_czv_o2)) {
    dt[idx_czv_o2, Hoedanigheid := "O2"]
    blank_lim <- idx_czv_o2 & (is.na(lim) | lim == "")
    if (any(blank_lim)) lim[blank_lim] <- "-"
  }

  dt[, Limietsymbool := lim]

  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_Wetterskip.xlsx",
    datasource = "Wetterskip"
  )
  dt <- apply_reference_casing(
    dt,
    ctx,
    output_file = "data bestand_Wetterskip.xlsx",
    datasource = "Wetterskip",
    fields = c("Parametercode", "Hoedanigheid", "Limietsymbool", "Eenheid")
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for Drents Overijsselse Delta.
board_rule_drents <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # Drents has many rows with empty Parameter.code while the effective code is
  # present in Grootheid.code; fill before generic filtering drops them.
  pcode_raw <- norm_text(coalesce_chr(
    pull_alias(raw_dt, c("Parameter.code", "PARAMETER.CODE")),
    pull_alias(raw_dt, c("Grootheid.code", "Grootheid.Code", "GROOTHEID.CODE")),
    pull_alias(raw_dt, c("Parametergrootheid.Code", "PARAMETERGROOTHEID.CODE"))
  ))
  pdescr_raw <- norm_text(coalesce_chr(
    pull_alias(raw_dt, c("Parameter.omschrijving", "Parameteromschrijving", "PARAMETEROMSCHRIJVING")),
    pull_alias(raw_dt, c("Grootheid.code", "Grootheid.Code", "GROOTHEID.CODE"))
  ))
  mapped_pc <- map_parameter_names_to_codes(pcode_raw, pdescr_raw)
  use_pc <- (is.na(dt$Parametercode) | norm_text(dt$Parametercode) == "") &
    !is.na(mapped_pc) & norm_text(mapped_pc) != ""
  if (any(use_pc)) {
    dt[use_pc, Parametercode := norm_text(mapped_pc[use_pc])]
  }

  # Keep limiet symbols aligned with the dedicated source column.
  raw_lim <- norm_text(pull_alias(raw_dt, c("Limietsymbool", "LIMIETSYMBOOL")))
  raw_lim[norm_key_text(raw_lim) %in% c("---", "--", "-", "nvt")] <- NA_character_
  if (length(raw_lim) == nrow(dt)) {
    dt[, Limietsymbool := raw_lim]
  }

  # Drents curation: normalize blank or "+" limietsymbool to "-".
  lim_key <- normalize_limiet_key(dt$Limietsymbool)
  dt[lim_key %in% c("", "+"), Limietsymbool := "-"]

  loc <- get_drents_location_map(ctx)
  if (nrow(loc) > 0L) {
    idx <- match(dt$`Meetpunt code`, loc$code)
    has_match <- !is.na(idx)
    dt[has_match & is.na(xcoordinaat), xcoordinaat := loc$X[idx[has_match]]]
    dt[has_match & is.na(ycoordinaat), ycoordinaat := loc$Y[idx[has_match]]]
  }
  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_Drents Overijsselse Delta.xlsx",
    datasource = "Drents Overijsselse Delta"
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for Brabantse Delta.
board_rule_brabantse_delta <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)
  dt[, xcoordinaat := parse_num(xcoordinaat)]
  dt[, ycoordinaat := parse_num(ycoordinaat)]
  loc <- get_brabantse_delta_location_map(ctx)
  if (nrow(loc) > 0L) {
    raw_code <- normalize_meetpunt_key(coalesce_chr(
      pull_alias(raw_dt, c("Meetobject.lokaalID", "Meetobject.LokaalID", "MEETOBJECT.LOKAALID")),
      dt$`Meetpunt code`
    ))
    idx <- match(raw_code, loc$code)
    has_match <- !is.na(idx)
    dt[has_match & is.na(xcoordinaat), xcoordinaat := loc$X[idx[has_match]]]
    dt[has_match & is.na(ycoordinaat), ycoordinaat := loc$Y[idx[has_match]]]
  }
  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_Brabantse Delta.xlsx",
    datasource = "Brabantse Delta"
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for Noorderzijlvest.
board_rule_noorderzijlvest <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)
  dt[, xcoordinaat := parse_num(xcoordinaat)]
  dt[, ycoordinaat := parse_num(ycoordinaat)]
  loc <- get_noorderzijlvest_location_map(ctx)
  if (nrow(loc) > 0L) {
    raw_code <- normalize_meetpunt_key(coalesce_chr(
      pull_alias(raw_dt, c("MEETPUNTCODE_KLANT", "Meetpuntcode_klant", "Meetpunt code", "Meetobject.lokaalID", "Meetobject.LokaalID")),
      dt$`Meetpunt code`
    ))
    idx <- match(raw_code, loc$code)
    has_match <- !is.na(idx)
    dt[has_match & is.na(xcoordinaat), xcoordinaat := loc$X[idx[has_match]]]
    dt[has_match & is.na(ycoordinaat), ycoordinaat := loc$Y[idx[has_match]]]
  }
  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_WS Noorderzijlvest.xlsx",
    datasource = "Noorderzijlvest"
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for Rijn en IJssel.
board_rule_rijn_en_ijssel <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)

  # Align parameter naming to curated reference.
  pc <- norm_text(dt$Parametercode)
  pc_key <- norm_key_text(pc)
  pc[pc_key == "nvt-os"] <- "OS"
  dt[, Parametercode := pc]

  # WRIJ encodes "no limiet symbol" as ---; reference stores this empty.
  lim <- norm_text(dt$Limietsymbool)
  lim_key <- norm_key_text(lim)
  lim[lim_key %in% c("---", "----", "-----")] <- NA_character_
  lim <- norm_text(lim)
  lim_key <- norm_key_text(lim)
  lim[lim_key %in% c("", "+", "nb")] <- "-"
  dt[, Limietsymbool := lim]

  dt <- apply_reference_value_snap(
    dt,
    ctx,
    output_file = "data bestand_Rijn en IJssel.xlsx",
    datasource = "Rijn en IJssel"
  )
  dt
}

# Apply board-specific cleanup and harmonization rules for Vechtstromen.
board_rule_vechtstromen <- function(dt, raw_dt, ctx) {
  dt <- copy(dt)
  dt[, xcoordinaat := parse_num(xcoordinaat)]
  dt[, ycoordinaat := parse_num(ycoordinaat)]

  loc <- get_vechtstromen_location_map(ctx)
  if (nrow(loc) > 0L) {
    raw_code <- normalize_meetpunt_key(coalesce_chr(
      pull_alias(raw_dt, c("Meetobject.lokaalID", "Meetobject.LokaalID", "MEETOBJECT.LOKAALID")),
      dt$`Meetpunt code`
    ))
    idx <- match(raw_code, loc$code)
    has_match <- !is.na(idx)
    dt[has_match & is.na(xcoordinaat), xcoordinaat := loc$X[idx[has_match]]]
    dt[has_match & is.na(ycoordinaat), ycoordinaat := loc$Y[idx[has_match]]]
  }

  # Vechtstromen stores core field parameters (e.g. ph, t, geldhd, zicht)
  # mainly in Grootheid.code while Parameter.code is often empty.
  pc_fallback <- norm_text(
    coalesce_chr(
      pull_alias(raw_dt, c("Grootheid.code", "Grootheid.Code", "GROOTHEID.CODE")),
      pull_alias(raw_dt, c("Parametergrootheid.Code", "PARAMETERGROOTHEID.CODE"))
    )
  )
  use_pc <- (is.na(dt$Parametercode) | norm_text(dt$Parametercode) == "") &
    !is.na(pc_fallback) & pc_fallback != ""
  if (any(use_pc)) {
    dt[use_pc, Parametercode := pc_fallback[use_pc]]
  }

  if (identical(norm_key_text(Sys.getenv("VECHTSTROMEN_VALUE_SNAP", "false")), "true")) {
    dt <- apply_reference_value_snap(
      dt,
      ctx,
      output_file = "data bestand_Vechtstromen_aangepast 19 aug 2020.xlsx",
      datasource = "Vechtstromen"
    )
  }
  dt
}

# Run HHNK enrichment at the board-entry level.
enrich_hhnk_entry <- function(mapped_dt, raw_dt, ctx) {
  enrich_hhnk(mapped_dt, ctx)
}

# script_0 reporting and audit helpers.
# Return the subset of requested columns that actually exists.
script_0_existing_cols <- function(dt, cols) {
  cols[cols %in% names(dt)]
}

# Truncate and sanitize text before writing it to reports.
script_0_safe_text <- function(x, width = 80L) {
  x <- iconv(as.character(x), from = "", to = "UTF-8", sub = "?")
  x[is.na(x)] <- ""
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  too_long <- nchar(x, type = "width") > width
  if (any(too_long)) {
    x[too_long] <- paste0(substr(x[too_long], 1L, max(1L, width - 3L)), "...")
  }
  x
}

# Remove internal columns before exposing a stage snapshot.
script_0_public_dt <- function(dt) {
  if (is.null(dt)) return(NULL)
  keep <- names(dt)[!startsWith(names(dt), "__")]
  if (length(keep) == 0L) return(data.table())
  copy(dt[, ..keep])
}

# Build a stable comparison frame for stage-to-stage diffs.
script_0_compare_frame <- function(dt) {
  if (is.null(dt) || !"__stage_row_id" %in% names(dt)) return(data.table())
  keep <- c("__stage_row_id", names(dt)[!startsWith(names(dt), "__")])
  keep <- unique(keep[keep %in% names(dt)])
  if (length(keep) == 0L) return(data.table())
  copy(dt[, ..keep])
}

# Normalize values before comparing transitions across stages.
script_0_normalize_compare_value <- function(x) {
  if (inherits(x, "Date")) return(ifelse(is.na(x), "<NA>", format(x, "%Y-%m-%d")))
  if (is.numeric(x)) return(ifelse(is.na(x), "<NA>", formatC(x, digits = 15, format = "fg", flag = "#")))
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- "<NA>"
  x
}

# Count concrete before/after value transitions by column.
script_0_transition_counts <- function(before_dt, after_dt, columns, source_label = NA_character_) {
  before <- script_0_compare_frame(before_dt)
  after <- script_0_compare_frame(after_dt)
  if (nrow(before) == 0L || nrow(after) == 0L) return(data.table())

  before_ids <- before$`__stage_row_id`
  after_ids <- after$`__stage_row_id`
  common_ids <- intersect(before_ids, after_ids)
  if (length(common_ids) == 0L) return(data.table())

  before_common <- before[match(common_ids, before_ids), , drop = FALSE]
  after_common <- after[match(common_ids, after_ids), , drop = FALSE]
  cols <- intersect(columns, intersect(names(before_common), names(after_common)))
  if (length(cols) == 0L) return(data.table())

  pieces <- vector("list", length(cols))
  out_i <- 0L
  for (col in cols) {
    before_vals <- script_0_normalize_compare_value(before_common[[col]])
    after_vals <- script_0_normalize_compare_value(after_common[[col]])
    changed <- before_vals != after_vals
    if (!any(changed)) next
    out_i <- out_i + 1L
    tmp <- data.table(
      source_label = source_label,
      column = col,
      from_value = before_vals[changed],
      to_value = after_vals[changed],
      cases = 1L
    )[
      ,
      .(cases = sum(cases)),
      by = .(source_label, column, from_value, to_value)
    ]
    pieces[[out_i]] <- tmp
  }
  if (out_i == 0L) return(data.table())
  rbindlist(pieces[seq_len(out_i)], use.names = TRUE, fill = TRUE)
}

# Store lookup-driven transition counts in the debug context.
script_0_log_lookup_changes <- function(ctx, before_dt, after_dt, source_label, columns) {
  if (!isTRUE(ctx$collect_debug) || is.null(ctx$lookup_change_log)) return(invisible(NULL))
  if (is.null(source_label) || !nzchar(source_label)) source_label <- "lookup_table"
  trans <- script_0_transition_counts(before_dt, after_dt, columns = columns, source_label = source_label)
  if (nrow(trans) == 0L) return(invisible(NULL))
  current <- ctx$lookup_change_log$transitions
  if (is.null(current)) current <- data.table()
  ctx$lookup_change_log$transitions <- rbindlist(list(current, trans), use.names = TRUE, fill = TRUE)
  invisible(NULL)
}

# Capture a small preview table for the report.
script_0_preview_dt <- function(dt, n = 8L) {
  public_dt <- script_0_public_dt(dt)
  if (is.null(public_dt) || nrow(public_dt) == 0L) return(data.frame())
  out <- as.data.frame(head(public_dt, n))
  names(out) <- script_0_safe_text(names(out), width = 48L)
  for (nm in names(out)) {
    if (inherits(out[[nm]], "Date")) {
      out[[nm]] <- format(out[[nm]], "%Y-%m-%d")
    } else if (is.character(out[[nm]]) || is.factor(out[[nm]])) {
      out[[nm]] <- script_0_safe_text(out[[nm]], width = 60L)
    }
  }
  out
}

# Summarize missingness per column for a stage snapshot.
script_0_missing_summary <- function(dt) {
  if (is.null(dt) || nrow(dt) == 0L) {
    return(data.frame(column = character(0), missing_n = integer(0), missing_frac = numeric(0)))
  }
  cols <- script_0_existing_cols(dt, c(
    "Meetpunt code", "Meetpunt omschrijving", "xcoordinaat", "ycoordinaat",
    "Datum", "Parametercode", "Parameteromschrijving", "Casnummer",
    "Hoedanigheid", "Limietsymbool", "Meetwaarde", "Eenheid"
  ))
  if (length(cols) == 0L) {
    return(data.frame(column = character(0), missing_n = integer(0), missing_frac = numeric(0)))
  }
  data.frame(
    column = cols,
    missing_n = vapply(cols, function(col) sum(is.na(dt[[col]]) | norm_text(dt[[col]]) == ""), integer(1)),
    missing_frac = vapply(cols, function(col) mean(is.na(dt[[col]]) | norm_text(dt[[col]]) == ""), numeric(1))
  )
}

# Build a compact summary object for one stage snapshot.
script_0_stage_summary <- function(stage_name, dt, n_preview = 8L) {
  public_dt <- script_0_public_dt(dt)
  out <- list(
    stage = stage_name,
    n_rows = if (is.null(public_dt)) 0L else nrow(public_dt),
    n_cols = if (is.null(public_dt)) 0L else ncol(public_dt),
    columns = if (is.null(public_dt)) character(0) else names(public_dt),
    missing = script_0_missing_summary(public_dt),
    sample = script_0_preview_dt(public_dt, n = n_preview),
    compare_frame = script_0_compare_frame(dt)
  )
  if (!is.null(public_dt) && "Meetwaarde" %in% names(public_dt)) {
    values <- suppressWarnings(as.numeric(public_dt$Meetwaarde))
    out$value_summary <- data.frame(
      non_missing = sum(!is.na(values)),
      negative_n = sum(values < 0, na.rm = TRUE),
      zero_n = sum(values == 0, na.rm = TRUE),
      min_value = if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE),
      max_value = if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE)
    )
  } else {
    out$value_summary <- data.frame(
      non_missing = integer(0),
      negative_n = integer(0),
      zero_n = integer(0),
      min_value = numeric(0),
      max_value = numeric(0)
    )
  }
  out
}

# Summarize raw file row counts for report output.
script_0_raw_file_summary <- function(files, raw_tables) {
  rows <- vector("list", length(raw_tables))
  for (i in seq_along(raw_tables)) {
    dt <- raw_tables[[i]]
    cols <- if (is.null(dt)) character(0) else names(dt)
    rows[[i]] <- data.frame(
      file = files[[i]],
      basename = basename(files[[i]]),
      n_rows = if (is.null(dt)) 0L else nrow(dt),
      n_cols = if (is.null(dt)) 0L else ncol(dt),
      columns_preview = if (length(cols) == 0L) "" else paste(script_0_safe_text(head(cols, 8L), width = 32L), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# Break down which generic filter rules removed rows.
script_0_generic_filter_details <- function(dt) {
  dt0 <- copy(dt)
  comp_vals <- if ("__raw_compartiment" %in% names(dt0)) dt0$`__raw_compartiment` else rep(NA_character_, nrow(dt0))
  comp_norm <- tolower(trimws(comp_vals))

  non_target <- grepl("bodem|sediment|grondwater|rwzi|influent|effluent|riool|slib|biota", comp_norm)
  ow_like <- comp_norm %in% c("ow", "oppervlaktewater", "oppervlakte water", "oppwat", "water", "tp", "ow ")
  ow_like <- ow_like | grepl("oppervlakte", comp_norm) | grepl("^ow$", comp_norm) | grepl("opp", comp_norm)
  unknown <- is.na(comp_norm) | comp_norm == ""
  keep_compartment <- !non_target & (ow_like | unknown)

  dt1 <- dt0[keep_compartment]
  keep_meetpunt <- !(is.na(dt1$`Meetpunt code`) | dt1$`Meetpunt code` == "")
  dt2 <- dt1[keep_meetpunt]
  keep_parameter <- !(is.na(dt2$Parametercode) | dt2$Parametercode == "")
  dt3 <- dt2[keep_parameter]
  keep_datum <- !is.na(dt3$Datum)
  dt4 <- dt3[keep_datum]

  summary <- data.frame(
    filter_step = c("compartment_filter", "missing_meetpunt_filter", "missing_parametercode_filter", "missing_date_filter"),
    rows_before = c(nrow(dt0), nrow(dt1), nrow(dt2), nrow(dt3)),
    rows_after = c(nrow(dt1), nrow(dt2), nrow(dt3), nrow(dt4)),
    rows_dropped = c(nrow(dt0) - nrow(dt1), nrow(dt1) - nrow(dt2), nrow(dt2) - nrow(dt3), nrow(dt3) - nrow(dt4)),
    stringsAsFactors = FALSE
  )

  list(filtered = dt4, summary = summary)
}

# script_0 execution entrypoints.
# Run the full script_0 pipeline for one board.
run_board_reconstruction <- function(config, ctx) {
  ds <- config$datasource
  files <- resolve_files(ctx$raw_root, config$raw_patterns)
  collect_debug <- isTRUE(ctx$collect_debug)
  if (collect_debug && is.null(ctx$lookup_change_log)) {
    ctx$lookup_change_log <- new.env(parent = emptyenv())
    ctx$lookup_change_log$transitions <- data.table()
  }
  preview_n <- as.integer(ctx$report_preview_n %||% 8L)

  build_return <- function(summary_dt, report = NULL) {
    if (collect_debug) {
      return(list(summary = summary_dt, report = report))
    }
    summary_dt
  }

  if (length(files) == 0L) {
    summary_dt <- data.table(
      DataSource = ds,
      Status = "no_raw_files",
      RawFiles = 0L,
      RawRows = 0L,
      MappedRows = 0L,
      BoardRuleRowsDropped = 0L,
      FilteredRows = 0L,
      ReferenceRows = NA_integer_,
      MatchedRows = 0L,
      SignatureMode = NA_character_,
      SigIntersectUnique = NA_integer_,
      Coverage = NA_real_,
      OutputRows = 0L,
      OutputFile = file.path(ctx$output_root, config$output_file)
    )
    return(build_return(summary_dt, report = list(
      datasource = ds,
      board_id = config$board_id %||% normalize_board_name(ds),
      files = character(0),
      raw_file_summary = data.frame(),
      stages = list(),
      reference = list(),
      diagnostics = list()
    )))
  }

  raw_tables <- vector("list", length(files))
  read_errors <- character(0)
  for (i in seq_along(files)) {
    f <- files[i]
    message("  [", i, "/", length(files), "] ", basename(f))
    raw_tables[[i]] <- tryCatch(read_any_table(f), error = function(e) {
      read_errors <<- c(read_errors, paste0(f, " :: ", conditionMessage(e)))
      NULL
    })
  }
  raw_tables <- Filter(Negate(is.null), raw_tables)
  if (length(raw_tables) == 0L) {
    summary_dt <- data.table(
      DataSource = ds,
      Status = "read_failed",
      RawFiles = length(files),
      RawRows = 0L,
      MappedRows = 0L,
      BoardRuleRowsDropped = 0L,
      FilteredRows = 0L,
      ReferenceRows = NA_integer_,
      MatchedRows = 0L,
      SignatureMode = NA_character_,
      SigIntersectUnique = NA_integer_,
      Coverage = NA_real_,
      OutputRows = 0L,
      OutputFile = file.path(ctx$output_root, config$output_file)
    )
    return(build_return(summary_dt, report = list(
      datasource = ds,
      board_id = config$board_id %||% normalize_board_name(ds),
      files = files,
      raw_file_summary = data.frame(),
      stages = list(),
      reference = list(read_errors = read_errors),
      diagnostics = list()
    )))
  }

  raw_dt <- rbindlist(raw_tables, use.names = TRUE, fill = TRUE)
  raw_rows <- nrow(raw_dt)
  report <- if (collect_debug) {
    list(
      datasource = ds,
      board_id = config$board_id %||% normalize_board_name(ds),
      files = files,
      raw_file_summary = script_0_raw_file_summary(files, raw_tables),
      stages = list(
        raw_combined = script_0_stage_summary("raw_combined", raw_dt, n_preview = preview_n)
      ),
      reference = list(read_errors = read_errors),
      diagnostics = list()
    )
  } else NULL

  mapped <- to_template(raw_dt, ds)
  mapped[, `__stage_row_id` := seq_len(.N)]
  mapped[, `__source_file` := raw_dt$`__source_file`]
  if ("__source_sheet" %in% names(raw_dt)) mapped[, `__source_sheet` := raw_dt$`__source_sheet`]
  raw_ctx <- extract_raw_context(raw_dt)
  mapped[, `__raw_compartiment` := raw_ctx$raw_compartiment]
  if (collect_debug) {
    report$stages$mapped_template <- script_0_stage_summary("mapped_template", mapped, n_preview = preview_n)
  }

  if (!is.null(config$enrich_fn)) {
    mapped <- config$enrich_fn(mapped, raw_dt, ctx)
  }
  if (collect_debug) {
    report$stages$after_enrich <- script_0_stage_summary("after_enrich", mapped, n_preview = preview_n)
  }

  before_rules <- nrow(mapped)
  if (!is.null(config$rule_fn)) {
    mapped <- config$rule_fn(mapped, raw_dt, ctx)
  }
  if (collect_debug) {
    report$stages$after_rule_fn <- script_0_stage_summary("after_rule_fn", mapped, n_preview = preview_n)
  }
  mapped <- apply_value_change_rules(
    mapped,
    board_id = config$board_id %||% normalize_board_name(ds),
    ctx = ctx,
    output_file = config$output_file,
    datasource = ds
  )
  if (collect_debug) {
    report$stages$after_value_change_rules <- script_0_stage_summary("after_value_change_rules", mapped, n_preview = preview_n)
  }
  mapped <- apply_value_multiplier_rules(
    mapped,
    board_id = config$board_id %||% normalize_board_name(ds),
    ctx = ctx
  )
  if (collect_debug) {
    report$stages$after_value_multiplier_rules <- script_0_stage_summary("after_value_multiplier_rules", mapped, n_preview = preview_n)
  }
  mapped <- apply_reference_cas_fill(
    mapped,
    ctx = ctx,
    output_file = config$output_file,
    datasource = ds
  )
  if (collect_debug) {
    report$stages$after_reference_cas_fill <- script_0_stage_summary("after_reference_cas_fill", mapped, n_preview = preview_n)
  }
  board_rule_rows_dropped <- before_rules - nrow(mapped)
  if (collect_debug) {
    report$stages$after_board_logic <- script_0_stage_summary("after_board_logic", mapped, n_preview = preview_n)
  }

  filter_details <- script_0_generic_filter_details(mapped)
  filtered <- filter_details$filtered
  mapped_rows <- nrow(mapped)
  filtered_rows <- nrow(filtered)
  if (collect_debug) {
    report$stages$after_generic_filter <- script_0_stage_summary("after_generic_filter", filtered, n_preview = preview_n)
    report$filter_rules <- filter_details$summary
  }

  ref_file <- file.path(ctx$reference_input_root, config$output_file)
  ref_dt <- read_reference(ref_file, ds)
  matched <- NULL
  coverage <- NA_real_
  sig_intersect_unique <- NA_integer_
  signature_mode <- NA_character_

  if (!is.null(ref_dt) && nrow(ref_dt) > 0L) {
    run_match_mode <- function(filtered_dt, ref_dt, mode) {
      f <- copy(filtered_dt)
      r <- copy(ref_dt)

      f[, `__sig` := make_signature(f, mode = mode)]
      r[, `__sig` := make_signature(r, mode = mode)]
      sig_inter <- length(intersect(unique(f$`__sig`), unique(r$`__sig`)))

      ref_need <- r[, .(need_n = .N), by = .(`__sig`)]
      f[, `__row_id` := seq_len(.N), by = .(`__sig`)]
      m <- ref_need[f, on = .(`__sig`), nomatch = 0]
      m <- m[`__row_id` <= need_n]
      m[, c("need_n", "__row_id") := NULL]

      sheet_pref <- r[, .N, by = .(`__sig`, `__sheet`)]
      setorder(sheet_pref, `__sig`, -N, `__sheet`)
      sheet_pref <- sheet_pref[, .SD[1], by = .(`__sig`)]
      sheet_pref[, N := NULL]
      m <- sheet_pref[m, on = .(`__sig`), nomatch = 0]

      cov <- if (nrow(r) > 0) nrow(m) / nrow(r) else NA_real_
      list(mode = mode, matched = m, coverage = cov, sig_intersect_unique = sig_inter)
    }

    match_modes <- c("full", "no_hoed_lim", "core", "no_value", "key_only", "mp_pc")
    mode_results <- lapply(match_modes, function(m) run_match_mode(filtered, ref_dt, m))
    mode_coverages <- vapply(mode_results, function(x) ifelse(is.na(x$coverage), -Inf, x$coverage), numeric(1))
    best_idx <- which.max(mode_coverages)
    best <- mode_results[[best_idx]]

    matched <- best$matched
    coverage <- best$coverage
    sig_intersect_unique <- best$sig_intersect_unique
    signature_mode <- best$mode

    if (ctx$use_reference_filter) filtered <- matched
  }
  if (collect_debug) {
    report$reference <- c(report$reference, list(
      reference_file = ref_file,
      reference_rows = if (is.null(ref_dt)) 0L else nrow(ref_dt),
      matched_rows = if (is.null(matched)) 0L else nrow(matched),
      coverage = coverage,
      signature_mode = signature_mode,
      sig_intersect_unique = sig_intersect_unique
    ))
    if (!is.null(matched)) {
      report$stages$matched_reference <- script_0_stage_summary("matched_reference", matched, n_preview = preview_n)
    }
  }

  # Standardized CSV output: one flat table per board (no sheet partitioning).
  max_rows_per_sheet <- env_int("MAX_ROWS_PER_SHEET", default = NA_integer_)
  out_dt <- copy(filtered)
  if (!is.na(max_rows_per_sheet) && nrow(out_dt) > max_rows_per_sheet) {
    out_dt <- out_dt[seq_len(max_rows_per_sheet)]
  }
  setorderv(out_dt, c("Datum", "Meetpunt code", "Parametercode"), order = c(1L, 1L, 1L), na.last = TRUE)
  drop_cols <- intersect(c("__sig", "__dup", "__sheet", "__row_id", "__raw_compartiment", "__source_file", "__source_sheet"), names(out_dt))
  if (length(drop_cols) > 0L) out_dt[, (drop_cols) := NULL]
  sheet_map <- list(all = out_dt)
  if (collect_debug) {
    report$stages$final_output <- script_0_stage_summary("final_output", out_dt, n_preview = preview_n)
  }

  board_out_dir <- ctx$output_root
  dir.create(board_out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(board_out_dir, "board_input.csv")
  status <- "dry_run"
  output_file <- out_file
  output_format <- "dry_run"
  output_message <- NA_character_
  if (!ctx$dry_run) {
    if (file.exists(out_file) && !ctx$overwrite) {
      status <- "skipped_exists"
      output_format <- "existing"
    } else {
      wr <- write_reconstructed_output(out_file, sheet_map, ctx = ctx)
      status <- wr$status
      output_file <- wr$output_file
      output_format <- wr$output_format
      output_message <- wr$output_message
    }
  }

  diag <- data.table(
    DataSource = ds,
    RawFiles = length(files),
    RawRows = raw_rows,
    MappedRows = mapped_rows,
    BoardRuleRowsDropped = board_rule_rows_dropped,
    FilteredRows = filtered_rows,
    ReferenceRows = if (is.null(ref_dt)) NA_integer_ else nrow(ref_dt),
    MatchedRows = if (is.null(matched)) NA_integer_ else nrow(matched),
    SignatureMode = signature_mode,
    SigIntersectUnique = sig_intersect_unique,
    Coverage = coverage,
    OutputRows = sum(vapply(sheet_map, nrow, integer(1))),
    OutputFile = output_file,
    OutputFormat = output_format,
    OutputMessage = output_message
  )
  if (isTRUE(ctx$write_diagnostics)) {
    fwrite(diag, file.path(ctx$diag_dir, paste0(normalize_board_name(ds), "_diagnostic.tsv")), sep = "\t")
    if (length(read_errors) > 0L) {
      fwrite(data.table(error = read_errors), file.path(ctx$diag_dir, paste0(normalize_board_name(ds), "_read_errors.tsv")), sep = "\t")
    }
  }

  summary_dt <- data.table(
    DataSource = ds,
    Status = status,
    RawFiles = length(files),
    RawRows = raw_rows,
    MappedRows = mapped_rows,
    BoardRuleRowsDropped = board_rule_rows_dropped,
    FilteredRows = filtered_rows,
    ReferenceRows = if (is.null(ref_dt)) NA_integer_ else nrow(ref_dt),
    MatchedRows = if (is.null(matched)) NA_integer_ else nrow(matched),
    SignatureMode = signature_mode,
    SigIntersectUnique = sig_intersect_unique,
    Coverage = coverage,
    OutputRows = sum(vapply(sheet_map, nrow, integer(1))),
    OutputFile = output_file,
    OutputFormat = output_format,
    OutputMessage = output_message
  )
  if (collect_debug) {
    report$diagnostics <- list(
      board_out_dir = board_out_dir,
      output_file = output_file,
      diagnostic_file = file.path(ctx$diag_dir, paste0(normalize_board_name(ds), "_diagnostic.tsv")),
      read_error_file = file.path(ctx$diag_dir, paste0(normalize_board_name(ds), "_read_errors.tsv")),
      summary = as.data.frame(summary_dt)
    )
    report$lookup_changes <- if (!is.null(ctx$lookup_change_log$transitions) && nrow(ctx$lookup_change_log$transitions) > 0L) {
      as.data.frame(ctx$lookup_change_log$transitions[order(-cases, source_label, column, from_value, to_value)])
    } else {
      data.frame()
    }
  }
  build_return(summary_dt, report = report)
}

# Build the runtime context from CLI args and environment variables.
parse_context_args <- function(argv, base_dir = getwd()) {
  args <- parse_args(argv)
  raw_root <- normalizePath(args$`raw-root` %||% Sys.getenv("RAW_ROOT", unset = "") %||% file.path(base_dir, "Chemie data waterschappen"), mustWork = FALSE)
  reference_input_root <- normalizePath(args$`reference-input-root` %||% Sys.getenv("REFERENCE_INPUT_ROOT", unset = "") %||% file.path(base_dir, "data", "zenodo_14652471", "input"), mustWork = FALSE)
  output_root <- normalizePath(args$`output-root` %||% Sys.getenv("OUTPUT_ROOT", unset = "") %||% file.path(base_dir, "output", "built_input_from_raw_per_board"), mustWork = FALSE)
  output_format <- tolower(args$`output-format` %||% Sys.getenv("OUTPUT_FORMAT", unset = "csv"))
  if (!identical(output_format, "csv")) {
    stop("Only output-format='csv' is supported. Got: ", output_format)
  }
  tsv_layout <- "flat"
  max_rows_per_file <- suppressWarnings(as.integer(args$`max-rows-per-file` %||% Sys.getenv("MAX_ROWS_PER_FILE", unset = "")))
  if (is.na(max_rows_per_file) || max_rows_per_file <= 0L) max_rows_per_file <- NA_integer_
  max_rows_per_sheet <- suppressWarnings(as.integer(args$`max-rows-per-sheet` %||% Sys.getenv("MAX_ROWS_PER_SHEET", unset = "")))
  if (is.na(max_rows_per_sheet) || max_rows_per_sheet <= 0L) max_rows_per_sheet <- NA_integer_
  max_reference_rows <- suppressWarnings(as.integer(args$`max-reference-rows` %||% Sys.getenv("MAX_REFERENCE_ROWS", unset = "")))
  if (is.na(max_reference_rows) || max_reference_rows <= 0L) max_reference_rows <- NA_integer_
  cores <- suppressWarnings(as.integer(
    args$cores %||%
      args$`n-cores` %||%
      Sys.getenv("SCRIPT_0_CORES", unset = "") %||%
      Sys.getenv("N_CORES", unset = "") %||%
      Sys.getenv("LSB_DJOB_NUMPROC", unset = "")
  ))
  if (is.na(cores) || cores <= 0L) cores <- 1L
  skip_reference_read <- as_bool(args$`skip-reference-read` %||% Sys.getenv("SKIP_REFERENCE_READ", unset = ""), default = FALSE)
  include_legacy_inputs <- as_bool(args$`include-legacy-inputs` %||% Sys.getenv("INCLUDE_LEGACY_INPUTS", unset = ""), default = TRUE)
  value_change_rules_file <- normalizePath(
    args$`value-change-rules-file` %||% file.path(base_dir, "output", "value_change_rules_latest.tsv"),
    mustWork = FALSE
  )
  value_multiplier_rules_file <- normalizePath(
    args$`value-multiplier-rules-file` %||% file.path(base_dir, "output", "value_delimiter_multiplier_rules_latest.tsv"),
    mustWork = FALSE
  )

  write_diagnostics <- as_bool(args$`write-diagnostics` %||% Sys.getenv("SCRIPT_0_WRITE_DIAGNOSTICS", unset = ""), default = FALSE)
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
  diag_dir <- file.path(output_root, "diagnostics")
  if (write_diagnostics) {
    dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  }

  do.call(Sys.setenv, as.list(Filter(Negate(is.null), list(
    OUTPUT_FORMAT = output_format,
    TSV_LAYOUT = tsv_layout,
    MAX_ROWS_PER_FILE = if (is.na(max_rows_per_file)) "" else as.character(max_rows_per_file),
    MAX_ROWS_PER_SHEET = if (is.na(max_rows_per_sheet)) "" else as.character(max_rows_per_sheet),
    MAX_REFERENCE_ROWS = if (is.na(max_reference_rows)) "" else as.character(max_reference_rows),
    SCRIPT_0_CORES = as.character(cores),
    SKIP_REFERENCE_READ = if (skip_reference_read) "true" else "false",
    INCLUDE_LEGACY_INPUTS = if (include_legacy_inputs) "true" else "false"
  ))))

  list(
    raw_root = raw_root,
    reference_input_root = reference_input_root,
    output_root = output_root,
    diag_dir = diag_dir,
    write_diagnostics = write_diagnostics,
    output_format = output_format,
    tsv_layout = tsv_layout,
    max_rows_per_file = max_rows_per_file,
    max_rows_per_sheet = max_rows_per_sheet,
    max_reference_rows = max_reference_rows,
    cores = cores,
    skip_reference_read = skip_reference_read,
    include_legacy_inputs = include_legacy_inputs,
    collect_debug = as_bool(args$`collect-debug` %||% Sys.getenv("SCRIPT_0_COLLECT_DEBUG", unset = ""), default = FALSE),
    report_preview_n = env_int("SCRIPT_0_REPORT_PREVIEW_N", default = 8L),
    value_change_rules_file = value_change_rules_file,
    value_multiplier_rules_file = value_multiplier_rules_file,
    use_reference_filter = as_bool(args$`use-reference-filter` %||% Sys.getenv("USE_REFERENCE_FILTER", unset = ""), default = FALSE),
    dry_run = as_bool(args$`dry-run` %||% Sys.getenv("DRY_RUN", unset = ""), default = FALSE),
    overwrite = as_bool(args$overwrite %||% Sys.getenv("OVERWRITE", unset = ""), default = FALSE),
    only_sources = csv_arg(args$`only-sources` %||% Sys.getenv("ONLY_SOURCES", unset = ""))
  )
}

# Run script_0 for a list of board configs and write the summary.
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

  n_cfg <- length(configs)
  n_cores <- max(1L, min(as.integer(ctx$cores %||% 1L), n_cfg))
  worker <- function(idx) {
    cfg <- configs[[idx]]
    message("== [", idx, "/", n_cfg, "] ", cfg$datasource, " ==")
    tryCatch(
      {
        out <- run_board_reconstruction(cfg, ctx)
        if (is.list(out) && !is.null(out$summary)) out$summary else out
      },
      error = function(e) {
        board_slug <- cfg$board_id %||% normalize_board_name(cfg$datasource)
        data.table(
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
          OutputFile = file.path(ctx$output_root, "board_input.csv"),
          OutputFormat = NA_character_,
          OutputMessage = conditionMessage(e)
        )
      }
    )
  }

  if (n_cores > 1L && .Platform$OS.type == "unix") {
    message("Running script_0 board reconstruction in parallel with ", n_cores, " cores.")
    results <- parallel::mclapply(seq_len(n_cfg), worker, mc.cores = n_cores, mc.preschedule = FALSE)
  } else {
    if (n_cores > 1L) {
      message("Parallel cores requested but unavailable on this OS; running sequentially.")
    }
    results <- lapply(seq_len(n_cfg), worker)
  }

  summary_dt <- rbindlist(results, use.names = TRUE, fill = TRUE)
  export_legacy_inputs(ctx)

  bad <- summary_dt[is.na(Status) | Status != "ok"]
  if (nrow(bad) > 0L) {
    stop(
      "script_0 reconstruction failed for: ",
      paste(sprintf("%s [%s]", bad$DataSource, bad$Status), collapse = "; ")
    )
  }

  invisible(list(ctx = ctx, summary = summary_dt))
}

# Run one board from a board config in CLI mode.
run_single_board_cli <- function(config, base_dir = getwd()) {
  ctx <- parse_context_args(commandArgs(trailingOnly = TRUE), base_dir = base_dir)
  message("== ", config$datasource, " ==")
  result <- run_board_reconstruction(config, ctx)
  board_slug <- config$board_id %||% normalize_board_name(config$datasource)
  invisible(result)
}
