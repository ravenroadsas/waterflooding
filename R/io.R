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
  out <- data.table::copy(out)
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
      if (k %in% required_tables) issues[[length(issues) + 1]] <- new_issue(k, "error", "Table not provided")
      next
    }
    st <- standardize_table(raw[[k]], k)
    ds[[k]] <- st$data
    if (nrow(st$issues)) issues[[length(issues) + 1]] <- st$issues
  }
  ds <- fill_defaults(ds)
  ds$issues <- rbind(data.table::rbindlist(issues), validate_dataset(ds), fill = TRUE)
  if (!nrow(ds$issues)) ds$issues <- data.table::data.table(table = character(), severity = character(), message = character())
  ds
}

fill_defaults <- function(ds) {
  if (!is.null(ds$wells)) {
    w <- ds$wells
    for (v in c("bopd", "bwpd", "bwipd")) data.table::set(w, which(is.na(w[[v]])), v, 0)
    ds$wells <- w[, .(bopd = mean(bopd), bwpd = mean(bwpd), bwipd = mean(bwipd)), by = .(well, date)]
  }
  if (!is.null(ds$fluids)) {
    f <- ds$fluids
    for (nm in names(fluid_defaults)) data.table::set(f, which(is.na(f[[nm]])), nm, fluid_defaults[[nm]])
    ds$fluids_relperm_given <- any(!is.na(ds$fluids$swc))
  }
  if (!is.null(ds$vol)) {
    v <- ds$vol
    v[is.na(reservoir) | reservoir == "", reservoir := "DEFAULT"]
    # HCPV is authoritative; fall back to STOIIP x Bo when missing
    if (!is.null(ds$fluids)) v[ds$fluids, on = "reservoir", bo_res := i.bo]
    if (!"bo_res" %in% names(v)) v[, bo_res := NA_real_]
    v[is.na(hcpv), hcpv := stoiip * data.table::fcoalesce(bo_res, 1.2)]
    v[, bo_res := NULL]
    v[, boi := ifelse(stoiip > 0, hcpv / stoiip, NA_real_)]
  }
  # hierarchy: from alloc when missing; well type inferred from volumes
  if (is.null(ds$hierarchy) && !is.null(ds$alloc)) {
    ds$hierarchy <- unique(ds$alloc[, .(pattern, well)])
    ds$hierarchy[, `:=`(field = NA_character_, area = NA_character_, well_type = NA_character_, x = NA_real_, y = NA_real_)]
    ds$hierarchy_inferred <- TRUE
  }
  if (!is.null(ds$hierarchy)) {
    h <- ds$hierarchy
    h[is.na(field) | field == "", field := "Field"]
    h[is.na(area) | area == "", area := "Area 1"]
    h[, well_type := toupper(substr(data.table::fcoalesce(well_type, ""), 1, 1))]
    if (!is.null(ds$wells)) {
      role <- ds$wells[, .(inj = sum(bwipd), prd = sum(bopd + bwpd)), by = well][, .(well, t = ifelse(inj > prd, "I", "P"))]
      h[role, on = "well", inferred := i.t]
      h[!well_type %in% c("P", "I"), well_type := data.table::fcoalesce(inferred, "P")]
      h[, inferred := NULL]
    }
    h[, well_type := ifelse(well_type == "I", "INJECTOR", "PRODUCER")]
    ds$hierarchy <- h
  }
  if (!is.null(ds$prototypes)) ds$prototypes[is.na(version) | version == "", version := "v1"]
  if (!is.null(ds$prototype_assign)) ds$prototype_assign[is.na(version) | version == "", version := "v1"]
  if (!is.null(ds$interventions)) {
    ds$interventions[, type := toupper(trimws(type))]
    ds$interventions[is.na(status) | status == "", status := "EXECUTED"]
    ds$interventions[, status := toupper(status)]
  }
  ds
}

validate_dataset <- function(ds) {
  I <- list()
  add <- function(t, s, m) I[[length(I) + 1]] <<- new_issue(t, s, m)
  w <- ds$wells; a <- ds$alloc; v <- ds$vol; f <- ds$fluids; isd <- ds[["injsand"]]; st <- ds$injsand_status
  if (!is.null(w) && nrow(w)) {
    if (w[bopd < 0 | bwpd < 0 | bwipd < 0, .N]) add("wells", "error", "Negative rates found")
    both <- w[bwipd > 0 & (bopd + bwpd) > 0, data.table::uniqueN(well)]
    if (both) add("wells", "info", sprintf("%d wells both produce and inject in the same month (conversions?)", both))
  }
  if (!is.null(w) && !is.null(a)) {
    miss <- setdiff(unique(w$well), a$well)
    if (length(miss)) add("alloc", "warning", sprintf("%d wells have no allocation (their volumes reach no pattern): %s",
                                                     length(miss), paste(head(miss, 6), collapse = ", ")))
    last <- a[, .(coeff = coeff[which.max(data.table::fcoalesce(date, as.Date("1800-01-01")))]), by = .(well, pattern)]
    sums <- last[, .(s = sum(coeff)), by = well][abs(s - 1) > 0.02]
    if (nrow(sums)) add("alloc", "warning", sprintf("%d wells whose latest coefficients do not add to 1 (e.g. %s = %.2f)",
                                                  nrow(sums), sums$well[1], sums$s[1]))
    if (a[coeff < 0 | coeff > 1, .N]) add("alloc", "error", "Coefficients outside [0, 1]")
  }
  if (!is.null(a) && !is.null(v)) {
    nov <- setdiff(unique(a$pattern), v$pattern)
    if (length(nov)) add("vol", "error", sprintf("Patterns with allocation but no volumetrics: %s", paste(head(nov, 6), collapse = ", ")))
  }
  if (!is.null(v)) {
    if (v[is.na(hcpv) | hcpv <= 0, .N]) add("vol", "warning", "Pattern/sand rows with missing or zero HCPV")
    if (!is.null(f)) {
      nof <- setdiff(unique(v$reservoir), f$reservoir)
      if (length(nof) && !"DEFAULT" %in% f$reservoir) add("fluids", "warning", sprintf("Reservoirs without PVT (defaults used): %s", paste(nof, collapse = ", ")))
    }
  }
  if (!is.null(f) && !isTRUE(ds$fluids_relperm_given)) add("fluids", "info", "No relative permeability: default Corey set used for theoretical curves")
  if (!is.null(isd)) {
    bad <- setdiff(unique(isd$sand), v$sand)
    if (length(bad)) add("injsand", "warning", sprintf("Sands in InjSand not found in Vol: %s", paste(bad, collapse = ", ")))
    if (!is.null(w)) {
      chk <- merge(isd[, .(prof = sum(bwipd)), by = .(well, date)], w[, .(well, date, bwipd)], by = c("well", "date"))
      chk <- chk[bwipd > 0]
      if (nrow(chk)) {
        off <- chk[abs(prof / bwipd - 1) > 0.05, .N]
        if (off) add("injsand", "info", sprintf("%d well-months where profile rates differ >5%% from Wells.BWIPD (shares are used)", off))
      }
      inj <- unique(w[bwipd > 0, well]); nopro <- setdiff(inj, isd$well)
      if (length(nopro)) add("injsand", "warning", sprintf("%d injectors without a profile: split by HCPV (%s)", length(nopro), paste(head(nopro, 5), collapse = ", ")))
    }
  } else add("injsand", "warning", "No injection profiles: unit injection split by HCPV")
  if (!is.null(st)) {
    bad <- setdiff(unique(st$sand), v$sand)
    if (length(bad)) add("injsand_status", "warning", sprintf("Sands not found in Vol: %s", paste(bad, collapse = ", ")))
  }
  if (is.null(ds$prototypes)) add("prototypes", "info", "No prototype curves: an analog prototype is built from field data")
  if (is.null(ds$baseline)) add("baseline", "info", "No baseline table: waterflood start = first injection month")
  if (isTRUE(ds$hierarchy_inferred)) add("hierarchy", "info", "No hierarchy: one area, well types inferred, maps disabled")
  data.table::rbindlist(I)
}

# Excel template with one sheet per table and a README sheet.
write_template <- function(path) {
  sheets <- list(); readme <- list()
  for (k in names(wf_schema)) {
    sp <- wf_schema[[k]]
    cols <- vapply(sp$cols, `[[`, "", "name")
    sheets[[k]] <- as.data.frame(stats::setNames(replicate(length(cols), character(), simplify = FALSE), cols))
    for (c in sp$cols) readme[[length(readme) + 1]] <- data.frame(
      sheet = k, role = sp$role, column = c$name, required = c$required, unit = c$unit,
      accepted_aliases = paste(c$aliases, collapse = ", "), description = c$doc)
  }
  writexl::write_xlsx(c(list(README = do.call(rbind, readme)), sheets), path)
}
