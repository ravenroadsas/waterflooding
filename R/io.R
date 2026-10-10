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
  # otherwise the longest alias the name starts with ("perfiles_mensuales_2026" is not InjSand "perfiles")
  best <- NA_character_; len <- 0
  for (k in names(wf_schema)) {
    al <- norm_name(wf_schema[[k]]$sheet_aliases)
    hit <- al[startsWith(s, al)]
    if (length(hit) && max(nchar(hit)) > len) { best <- k; len <- max(nchar(hit)) }
  }
  best
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
  lv <- c("orgunit", "contract", "field", "structure", "substructure", "area")
  if (is.null(ds$hierarchy) && !is.null(ds$alloc)) {
    ds$hierarchy <- unique(ds$alloc[, .(pattern, well)])
    for (v in lv) ds$hierarchy[, (v) := NA_character_]
    ds$hierarchy[, `:=`(well_type = NA_character_, x = NA_real_, y = NA_real_)]
    ds$hierarchy_inferred <- TRUE
  }
  role <- if (!is.null(ds$wells)) ds$wells[, .(inj = sum(bwipd), prd = sum(bopd + bwpd)), by = well][, .(well, t = ifelse(inj > prd, "I", "P"))] else NULL
  if (!is.null(ds$hierarchy)) {
    h <- ds$hierarchy
    for (v in lv) if (!v %in% names(h)) h[, (v) := NA_character_]
    for (v in c(lv, "pattern")) data.table::set(h, which(h[[v]] == ""), v, NA_character_)
    h[is.na(field), field := "Field"]
    h[is.na(area), area := data.table::fcoalesce(structure, "Area 1")]
    h[, well_type := toupper(substr(data.table::fcoalesce(well_type, ""), 1, 1))]
    if (!is.null(role)) {
      h[role, on = "well", inferred := i.t]
      h[!well_type %in% c("P", "I"), well_type := data.table::fcoalesce(inferred, "P")]
      h[, inferred := NULL]
    }
    h[, well_type := ifelse(well_type == "I", "INJECTOR", "PRODUCER")]
    ds$hierarchy <- h[!is.na(pattern)]
    ds$well_master <- unique(h[, c("well", "well_type", lv, "x", "y"), with = FALSE], by = "well")
  } else ds$well_master <- data.table::data.table(well = character(), well_type = character())
  # every producing / injecting well is in the master, even outside patterns (primary)
  if (!is.null(role)) {
    miss <- role[!well %in% ds$well_master$well]
    if (nrow(miss)) ds$well_master <- rbind(ds$well_master, miss[, .(well, well_type = ifelse(t == "I", "INJECTOR", "PRODUCER"),
                                                                     field = "Field", area = "Area 1")], fill = TRUE)
  }
  if (!is.null(ds$intervals)) {
    iv <- ds$intervals
    est <- iconv(tolower(trimws(data.table::fcoalesce(iv$estado, ""))), to = "ASCII//TRANSLIT")
    emap <- c(abierto = "abierto", open = "abierto", opened = "abierto", a = "abierto",
              parcial = "parcial", partial = "parcial", "partially open" = "parcial", p = "parcial",
              cerrado = "cerrado", closed = "cerrado", c = "cerrado", "no perforado" = "cerrado", "not perforated" = "cerrado")
    iv[, estado := ifelse(est == "", "cerrado", ifelse(est %in% names(emap), emap[est], est))]
    iv[is.na(qf0) & !is.na(qo0) & !is.na(qw0), qf0 := qo0 + qw0]
    iv[is.na(bsw0_pct) & is.finite(qf0) & qf0 > 0, bsw0_pct := 100 * qw0 / qf0]
    iv[is.na(np_ooip) & is.finite(ooip_stb) & ooip_stb > 0, np_ooip := np_well_stb / ooip_stb]
    iv[is.na(kh_md_ft) & is.finite(kabs_md) & is.finite(h_net_ft), kh_md_ft := kabs_md * h_net_ft]
    iv[, qa := trimws(data.table::fcoalesce(qa, ""))]
    # hierarchy labels from the intervals where the master has none
    m <- ds$well_master
    for (v in c("orgunit", "field")) {
      lab <- unique(iv[!is.na(get(v)) & get(v) != "", c("well", v), with = FALSE], by = "well")
      if (!v %in% names(m)) m[, (v) := NA_character_]
      if (nrow(lab)) m[lab, on = "well", (v) := data.table::fifelse(is.na(get(v)) | get(v) == "Field", get(paste0("i.", v)), get(v))]
    }
    nw <- setdiff(unique(iv$well), m$well)
    if (length(nw)) m <- rbind(m, unique(iv[well %in% nw, .(well, well_type = "PRODUCER", orgunit, field, area = "Area 1")], by = "well"), fill = TRUE)
    ds$well_master <- m
    ds$intervals <- iv
  }
  if (!is.null(ds$profiles)) {
    pf <- ds$profiles
    sc <- tolower(trimws(pf$scenario))
    smap <- c(bajo = "Bajo", low = "Bajo", p90 = "Bajo", pesimista = "Bajo", base = "Base", mid = "Base", p50 = "Base", medio = "Base",
              alto = "Alto", high = "Alto", p10 = "Alto", optimista = "Alto")
    pf[, scenario := ifelse(sc %in% names(smap), smap[sc], scenario)]
    pf[is.na(sand) | sand == "", sand := "-"]
    pf[is.na(interval_id) | interval_id == "", interval_id := "-"]
    pf[is.na(qf) & !is.na(qo) & !is.na(qw), qf := qo + qw]
    data.table::setorder(pf, well, sand, interval_id, scenario, month)
    ds$profiles <- pf
  }
  if (!is.null(ds$findings)) {
    fd <- ds$findings
    fd[, action := toupper(trimws(action))]
    fd[, family := toupper(substr(data.table::fcoalesce(family, ""), 1, 1))]
    fd[!family %in% c("M", "V", "U", "S", "O"), family := NA_character_]
    fd[is.na(sand) | sand == "", sand := "-"]
    fd[is.na(interval_id) | interval_id == "", interval_id := "-"]
    ds$findings <- fd
  }
  if (!is.null(ds$baseline)) ds$baseline[is.na(method) | method == "", method := "waterflood"]
  if (!is.null(ds$log_intervals)) ds$log_intervals[is.na(sand) | sand == "", sand := NA_character_]
  if (!is.null(ds$completions)) {
    cp <- ds$completions
    ty <- toupper(trimws(cp$type))
    tmap <- c(PERFORATION = "PERFORATION", PERF = "PERFORATION", PUNZADO = "PERFORATION", CANONEO = "PERFORATION", ADPERF = "PERFORATION", REPERF = "PERFORATION",
              SQUEEZE = "SQUEEZE", SQZ = "SQUEEZE", CEMENTACION = "SQUEEZE", PLUG = "PLUG", CIBP = "PLUG", TAPON = "PLUG", BRIDGE_PLUG = "PLUG",
              SLEEVE = "SLEEVE", CAMISA = "SLEEVE")
    cp[, type := ifelse(ty %in% names(tmap), tmap[ty], ty)]
    cp[is.na(base_ft), base_ft := top_ft]
    cp[, status := toupper(data.table::fcoalesce(status, ""))]
    cp[status == "", status := ifelse(type %in% c("PERFORATION", "SLEEVE"), "OPEN", "ACTIVE")]
    ds$completions <- cp
  }
  if (!is.null(ds$job_costs)) ds$job_costs[, job_type := toupper(trimws(job_type))]

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
    if (length(miss)) add("alloc", "info", sprintf("%d wells are in no waterflood pattern: treated as primary (%s)",
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
  for (x in validate_well_analysis(ds)) I[[length(I) + 1]] <- x
  if (is.null(ds$prototypes)) add("prototypes", "info", "No prototype curves: an analog prototype is built from field data")
  if (is.null(ds$baseline)) add("baseline", "info", "No baseline table: waterflood start = first injection month")
  if (isTRUE(ds$hierarchy_inferred)) add("hierarchy", "info", "No hierarchy: one area, well types inferred, maps disabled")
  data.table::rbindlist(I)
}

# Checks of the single-well analysis tables (INTERVALOS / PERFILES_MENSUALES).
# Returns a list of issue rows; the consistency rules come from the source definition.
validate_well_analysis <- function(ds, tol = 0.01) {
  I <- list(); add <- function(t, s, m) I[[length(I) + 1]] <<- new_issue(t, s, m)
  iv <- ds$intervals; pf <- ds$profiles
  if (!is.null(iv)) {
    d <- iv[, .N, by = .(well, sand, interval_id)][N > 1]
    if (nrow(d)) add("intervals", "error", sprintf("%d duplicated well / unit / interval keys (e.g. %s %s %s)", nrow(d), d$well[1], d$sand[1], d$interval_id[1]))
    bad <- iv[is.finite(top_ft) & is.finite(base_ft) & top_ft >= base_ft]
    if (nrow(bad)) add("intervals", "warning", sprintf("%d intervals with top at or below base (e.g. %s %s)", nrow(bad), bad$well[1], bad$interval_id[1]))
    ov <- iv[is.finite(top_ft) & is.finite(base_ft)][order(well, top_ft)][, .(o = any(utils::head(base_ft, -1) > utils::tail(top_ft, -1))), by = well][o == TRUE]
    if (nrow(ov)) add("intervals", "warning", sprintf("Overlapping intervals in %d wells (%s)", nrow(ov), paste(head(ov$well, 5), collapse = ", ")))
    odd <- setdiff(unique(iv$estado), c("abierto", "cerrado", "parcial"))
    if (length(odd)) add("intervals", "warning", sprintf("Unknown estado_apertura values: %s", paste(odd, collapse = ", ")))
    qq <- iv[is.finite(qf0) & is.finite(qo0) & is.finite(qw0) & abs(qo0 + qw0 - qf0) > tol * pmax(qf0, 1)]
    if (nrow(qq)) add("intervals", "warning", sprintf("%d intervals where qo + qw differs from qf", nrow(qq)))
    if (!is.null(ds$vol)) {
      us <- setdiff(unique(iv$sand), ds$vol$sand)
      if (length(us)) add("intervals", "info", sprintf("Units not in Vol (no pattern support evidence): %s", paste(head(us, 6), collapse = ", ")))
    }
    corr <- iv[nzchar(qa) & !grepl("^ok$", qa, ignore.case = TRUE), .N]
    if (corr) add("intervals", "info", sprintf("%d intervals with a QA-corrected estimate (qa_resultado not OK): shown on each record", corr))
  }
  if (!is.null(pf)) {
    k <- c("well", "sand", "interval_id")
    if (!is.null(iv)) {
      orph <- unique(pf[, ..k])[!iv, on = k]
      orph <- orph[!(sand == "-" & interval_id == "-")]
      if (nrow(orph)) add("profiles", "warning", sprintf("%d profiles without an interval (e.g. %s %s %s): ignored for intervals", nrow(orph), orph$well[1], orph$sand[1], orph$interval_id[1]))
      b <- merge(pf[scenario == "Base" & month == min(month), c(k, "qoi", "qw0", "qf"), with = FALSE], iv[, c(k, "qo0", "qw0", "qf0"), with = FALSE], by = k, suffixes = c("", ".iv"))
      off <- function(a, z) is.finite(a) & is.finite(z) & abs(a - z) > tol * pmax(abs(z), 1)
      nb <- b[off(qoi, qo0) | off(qw0, qw0.iv) | off(qf, qf0)]
      if (nrow(nb)) add("profiles", "warning", sprintf("%d Base profiles whose initial rates differ from the interval (qoi = qo_inicial, qwi = qw_inicial, qf = qf_inicial), e.g. %s %s", nrow(nb), nb$well[1], nb$interval_id[1]))
    }
    cst <- pf[is.finite(qf), .(r = diff(range(qf)) / pmax(mean(qf), 1)), by = c(k, "scenario")][r > tol]
    if (nrow(cst)) add("profiles", "warning", sprintf("%d scenarios where total liquid is not constant over the months", nrow(cst)))
    sm <- pf[is.finite(qo) & is.finite(qw) & is.finite(qf) & abs(qo + qw - qf) > tol * pmax(qf, 1), .N]
    if (sm) add("profiles", "warning", sprintf("%d rows where qo + qw differs from qf", sm))
    sc <- dcast_first(pf, k)
    if (nrow(sc)) {
      bo <- sc[is.finite(Bajo) & is.finite(Base) & is.finite(Alto) & !(Bajo <= Base + 1e-9 & Base <= Alto + 1e-9)]
      if (nrow(bo)) add("profiles", "warning", sprintf("%d profiles where initial oil is not Bajo <= Base <= Alto", nrow(bo)))
    }
    mo <- pf[, .(ok = isTRUE(all.equal(sort(unique(as.integer(month))), seq_len(max(month)))), n = max(month)), by = c(k, "scenario")]
    if (any(!mo$ok)) add("profiles", "warning", sprintf("%d scenarios whose months are not contiguous from 1", sum(!mo$ok)))
    if (mo[, data.table::uniqueN(n), by = k][V1 > 1, .N]) add("profiles", "info", "Scenarios of the same interval have different horizons")
    unk <- setdiff(unique(pf$scenario), c("Bajo", "Base", "Alto"))
    if (length(unk)) add("profiles", "warning", sprintf("Unknown scenarios: %s", paste(unk, collapse = ", ")))
  }
  for (k in c("log_intervals", "completions", "interval_rates", "interval_potential")) {
    x <- ds[[k]]; if (is.null(x)) next
    bad <- x[is.finite(top_ft) & is.finite(base_ft) & top_ft > base_ft, .N]
    if (bad) add(k, "warning", sprintf("%d rows with top below base", bad))
  }
  if (!is.null(ds$log_intervals)) add("log_intervals", "info", sprintf("%d intervals from %d algorithms on %d wells", nrow(ds$log_intervals),
                                                                       data.table::uniqueN(ds$log_intervals$algorithm), data.table::uniqueN(ds$log_intervals$well)))
  if (!is.null(ds$completions)) {
    odd <- setdiff(unique(ds$completions$type), c("PERFORATION", "SQUEEZE", "PLUG", "SLEEVE"))
    if (length(odd)) add("completions", "warning", sprintf("Unknown completion types (ignored): %s", paste(odd, collapse = ", ")))
  }
  if (!is.null(ds$intervals) && !is.null(ds$completions)) {
    st <- completion_state(ds$completions)
    iv <- ds$intervals[estado == "cerrado" & is.finite(top_ft) & is.finite(base_ft)]
    cf <- iv[, .(c = interval_conflict(st, well, top_ft, base_ft)), by = .(well, interval_id)][c == "already open"]
    if (nrow(cf)) add("intervals", "warning", sprintf("%d intervals marked closed overlap open perforations in Completions (e.g. %s %s): treated as open", nrow(cf), cf$well[1], cf$interval_id[1]))
  }
  if (is.null(ds$job_costs)) add("job_costs", "info", "No cost lookup: default standard costs used (Settings)")
  if (!is.null(ds$findings)) {
    nf <- ds$findings[is.na(family), .N]
    if (nf) add("findings", "info", sprintf("%d finding rows without an evidence family (M, V, U, S, O): kept as context only", nf))
  }
  I
}

dcast_first <- function(pf, k) {
  x <- pf[month == 1, c(k, "scenario", "qo"), with = FALSE]
  if (!nrow(x)) return(data.table::data.table())
  x <- unique(x, by = c(k, "scenario"))
  out <- data.table::dcast(x, stats::as.formula(paste(paste(k, collapse = "+"), "~ scenario")), value.var = "qo")
  if (!all(c("Bajo", "Base", "Alto") %in% names(out))) return(data.table::data.table())
  out
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
