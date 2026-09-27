# Loading, standardising and validating the input tables -----------------------

norm_name <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

month_start <- function(d) as.Date(format(d, "%Y-%m-01"))

days_in_month <- function(d) {
  d <- month_start(d)
  as.numeric(month_start(d + 32) - d)
}

# Parse the many date shapes found in production spreadsheets and floor to month.
parse_month <- function(x) {
  if (inherits(x, "Date")) return(month_start(x))
  if (inherits(x, "POSIXt")) return(month_start(as.Date(x)))
  if (is.numeric(x)) return(month_start(as.Date(x, origin = "1899-12-30")))  # Excel serial
  x <- trimws(as.character(x))
  out <- rep(as.Date(NA), length(x))
  try_fmt <- function(v, f) {
    d <- as.Date(v, format = f)
    yr <- as.integer(format(d, "%Y"))
    d[!is.na(yr) & (yr < 1900 | yr > 2200)] <- NA  # reject e.g. "15/04/2021" read as year 15
    d
  }
  # day-first before month-first (Latin-American / European convention)
  shapes <- list(c("", "%Y-%m-%d"), c("", "%Y/%m/%d"), c("", "%d/%m/%Y"), c("", "%d-%m-%Y"), c("", "%m/%d/%Y"),
                 c("", "%Y%m%d"), c("pre-01", "%Y-%m-%d"), c("01/", "%d/%m/%Y"), c("01-", "%d-%b-%Y"))
  for (sh in shapes) {
    todo <- is.na(out) & !is.na(x) & x != ""
    if (!any(todo)) break
    v <- if (sh[1] == "pre-01") paste0(x[todo], "-01") else paste0(sh[1], x[todo])
    out[todo] <- try_fmt(v, sh[2])
  }
  month_start(out)
}

new_issue <- function(table, severity, message) {
  data.table::data.table(table = table, severity = severity, message = message)
}

# Map a raw data.frame onto a table spec: rename aliases, coerce, check required.
standardize_table <- function(df, key) {
  spec <- wf_schema[[key]]
  dt <- data.table::as.data.table(df)
  raw <- names(dt)
  nn <- norm_name(raw)
  issues <- list()
  out <- data.table::data.table(.row = seq_len(nrow(dt)))
  for (cs in spec$cols) {
    cand <- c(cs$name, norm_name(cs$aliases))
    hit <- match(cand, nn)
    hit <- hit[!is.na(hit)][1]
    if (is.na(hit)) {
      if (cs$required) issues[[length(issues) + 1]] <- new_issue(key, "error", sprintf("Missing required column '%s'", cs$name))
      out[[cs$name]] <- switch(cs$type, chr = NA_character_, date = as.Date(NA), NA_real_)
      next
    }
    v <- dt[[hit]]
    v <- switch(cs$type,
      chr = trimws(as.character(v)),
      date = parse_month(v),
      suppressWarnings(as.numeric(gsub(",", "", as.character(v))))
    )
    if (cs$type == "date" && cs$required && any(is.na(v))) {
      issues[[length(issues) + 1]] <- new_issue(key, "warning", sprintf("%d rows with unreadable '%s' dropped", sum(is.na(v)), cs$name))
    }
    out[[cs$name]] <- v
  }
  out[, .row := NULL]
  # Drop rows missing any required key
  req_chr <- vapply(spec$cols, function(c) c$required && c$type != "num", logical(1))
  for (nm in vapply(spec$cols[req_chr], `[[`, "", "name")) {
    bad <- is.na(out[[nm]]) | (is.character(out[[nm]]) & out[[nm]] == "")
    if (any(bad)) out <- out[!bad]
  }
  list(data = out, issues = data.table::rbindlist(issues))
}

sheet_key <- function(sheet) {
  s <- norm_name(sheet)
  for (k in names(wf_schema)) {
    if (s %in% norm_name(wf_schema[[k]]$sheet_aliases)) return(k)
  }
  for (k in names(wf_schema)) {
    if (any(startsWith(s, norm_name(wf_schema[[k]]$sheet_aliases)))) return(k)
  }
  NA_character_
}

# Read a workbook with one sheet per table.
read_dataset_xlsx <- function(path) {
  sheets <- readxl::excel_sheets(path)
  raw <- list()
  for (s in sheets) {
    k <- sheet_key(s)
    if (!is.na(k) && is.null(raw[[k]])) raw[[k]] <- readxl::read_excel(path, sheet = s, guess_max = 10000)
  }
  raw
}

# Read CSV files; the file name decides the table (production.csv, stooip.csv ...).
read_dataset_csv <- function(paths, names = basename(paths)) {
  raw <- list()
  for (i in seq_along(paths)) {
    k <- sheet_key(tools::file_path_sans_ext(names[i]))
    if (!is.na(k)) raw[[k]] <- data.table::fread(paths[i], colClasses = "character")
  }
  raw
}

read_dataset_dir <- function(dir) {
  files <- list.files(dir, pattern = "\\.(csv|xlsx)$", full.names = TRUE, ignore.case = TRUE)
  raw <- list()
  for (f in files) {
    part <- if (grepl("xlsx$", f, ignore.case = TRUE)) read_dataset_xlsx(f) else read_dataset_csv(f)
    for (k in names(part)) raw[[k]] <- part[[k]]
  }
  raw
}

# Build a clean dataset (list of data.tables) + issues from raw tables.
build_dataset <- function(raw, name = "dataset") {
  ds <- list(name = name)
  issues <- list()
  for (k in names(wf_schema)) {
    if (is.null(raw[[k]])) {
      issues[[length(issues) + 1]] <- new_issue(k, if (k %in% c("production", "allocation", "stooip")) "error" else "warning",
                                                "Table not provided")
      ds[[k]] <- NULL
      next
    }
    st <- standardize_table(raw[[k]], k)
    ds[[k]] <- st$data
    if (nrow(st$issues)) issues[[length(issues) + 1]] <- st$issues
  }
  ds <- fill_defaults(ds)
  ds$issues <- rbind(data.table::rbindlist(issues), validate_dataset(ds), fill = TRUE)
  ds
}

fill_defaults <- function(ds) {
  if (is.null(ds$hierarchy) && !is.null(ds$allocation)) {
    ds$hierarchy <- unique(ds$allocation[, .(pattern, well)])
    ds$hierarchy[, `:=`(field = NA_character_, block = NA_character_, well_type = NA_character_, x = NA_real_, y = NA_real_)]
  }
  if (!is.null(ds$hierarchy)) {
    h <- ds$hierarchy
    h[is.na(field) | field == "", field := "Field"]
    h[is.na(block) | block == "", block := "Block 1"]
    h[, well_type := toupper(substr(well_type, 1, 1))]
    # Infer well type from production when missing
    if (!is.null(ds$production) && any(is.na(h$well_type) | !h$well_type %in% c("P", "I"))) {
      role <- ds$production[, .(inj = sum(bwipd, na.rm = TRUE), prd = sum(bopd + bwpd, na.rm = TRUE)), by = well]
      role[, t := ifelse(inj > prd, "I", "P")]
      h[role, on = "well", inferred := i.t]
      h[!well_type %in% c("P", "I"), well_type := data.table::fcoalesce(inferred, "P")]
      h[, inferred := NULL]
    }
    h[, well_type := ifelse(well_type == "I", "INJECTOR", "PRODUCER")]
    ds$hierarchy <- h
  }
  if (!is.null(ds$production)) {
    p <- ds$production
    for (v in c("bopd", "bwpd", "bwipd")) data.table::set(p, which(is.na(p[[v]])), v, 0)
  }
  if (!is.null(ds$fluids)) {
    f <- ds$fluids
    for (nm in names(fluid_defaults)) data.table::set(f, which(is.na(f[[nm]])), nm, fluid_defaults[[nm]])
  }
  ds
}

validate_dataset <- function(ds) {
  I <- list()
  add <- function(t, s, m) I[[length(I) + 1]] <<- new_issue(t, s, m)
  p <- ds$production; h <- ds$hierarchy; a <- ds$allocation; s <- ds$stooip
  f <- ds$fluids; pe <- ds$petrophysics

  if (!is.null(p) && nrow(p)) {
    dup <- p[, .N, by = .(well, date)][N > 1]
    if (nrow(dup)) add("production", "warning", sprintf("%d duplicated well-month rows (volumes summed)", nrow(dup)))
    neg <- p[bopd < 0 | bwpd < 0 | bwipd < 0, .N]
    if (neg) add("production", "error", sprintf("%d rows with negative rates", neg))
    both <- p[bwipd > 0 & (bopd + bwpd) > 0, uniqueN(well)]
    if (both) add("production", "info", sprintf("%d wells both produce and inject in the same month (conversions?)", both))
  }
  if (!is.null(p) && !is.null(a)) {
    miss <- setdiff(unique(p$well), a$well)
    if (length(miss)) add("allocation", "warning", sprintf("%d producing/injecting wells have no allocation (volumes not assigned to any pattern): %s",
                                                           length(miss), paste(head(miss, 8), collapse = ", ")))
    sums <- a[is.na(date), .(s = sum(coefficient)), by = well]
    off <- sums[abs(s - 1) > 0.02]
    if (nrow(off)) add("allocation", "warning", sprintf("%d wells whose coefficients do not add to 1 (e.g. %s = %.2f)",
                                                       nrow(off), off$well[1], off$s[1]))
    if (a[coefficient < 0 | coefficient > 1, .N]) add("allocation", "error", "Coefficients outside [0, 1]")
  }
  if (!is.null(a) && !is.null(s)) {
    nost <- setdiff(unique(a$pattern), s$pattern)
    if (length(nost)) add("stooip", "error", sprintf("Patterns with allocation but no STOOIP: %s", paste(head(nost, 8), collapse = ", ")))
    noal <- setdiff(unique(s$pattern), a$pattern)
    if (length(noal)) add("allocation", "info", sprintf("Patterns with STOOIP but no allocated wells: %s", paste(head(noal, 8), collapse = ", ")))
  }
  if (!is.null(s)) {
    if (s[is.na(stooip) | stooip <= 0, .N]) add("stooip", "warning", "Pattern/sand rows with missing or zero STOOIP")
    if (!is.null(f) && !"*" %in% f$sand) {
      nof <- setdiff(unique(s$sand), f$sand)
      if (length(nof)) add("fluids", "warning", sprintf("Sands without fluid data (defaults used): %s", paste(nof, collapse = ", ")))
    }
  }
  if (!is.null(pe) && !is.null(a)) {
    nop <- setdiff(unique(a$well), pe$well)
    if (length(nop)) add("petrophysics", "info", sprintf("%d wells without petrophysics: sand split falls back to STOOIP share", length(nop)))
  }
  if (!is.null(h) && !is.null(p)) {
    noh <- setdiff(unique(p$well), h$well)
    if (length(noh)) add("hierarchy", "info", sprintf("%d wells not in hierarchy", length(noh)))
  }
  data.table::rbindlist(I)
}

# Blank Excel template with one sheet per table and a README sheet.
write_template <- function(path) {
  sheets <- list()
  readme <- list()
  for (k in names(wf_schema)) {
    sp <- wf_schema[[k]]
    cols <- vapply(sp$cols, `[[`, "", "name")
    df <- as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols))
    sheets[[k]] <- df
    for (c in sp$cols) readme[[length(readme) + 1]] <- data.frame(
      sheet = k, column = c$name, required = c$required, unit = c$unit,
      accepted_aliases = paste(c$aliases, collapse = ", "), description = c$doc)
  }
  writexl::write_xlsx(c(list(README = do.call(rbind, readme)), sheets), path)
}
