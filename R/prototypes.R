# Prototypes: expected performance vs DWI ---------------------------------------------
#
# A prototype is a versioned reference object (methodology section 7): curves of
# expected Sec RF, DWP, utilization and WOR against DWI. Sources: uploaded curves
# (simulation / analog), analogs built in the app from chosen patterns, and a
# Buckley-Leverett curve from the rel-perm data. Assignments carry a version and
# a valid-from date so recalibration never rewrites how past months were read.

proto_key <- function(prototype, version) paste0(prototype, " | ", version)

prototype_table <- function(user, extra = NULL) {
  cols <- c("prototype", "version", "dwi", "sec_rf", "dwp", "util", "wor")
  out <- data.table::rbindlist(list(user, extra), fill = TRUE)
  if (!nrow(out)) return(data.table::data.table(prototype = character(), version = character(), dwi = numeric(),
                                                sec_rf = numeric(), dwp = numeric(), util = numeric(), wor = numeric(),
                                                source = character(), pkey = character()))
  for (c in cols) if (!c %in% names(out)) out[[c]] <- NA_real_
  if (!"source" %in% names(out)) out[, source := "uploaded"]
  out[is.na(source), source := "uploaded"]
  out[, pkey := proto_key(prototype, version)]
  data.table::setorder(out, pkey, dwi)
  out[]
}

# Analog prototype: median trajectory of the chosen (flooded) patterns on a DWI grid.
analog_prototype <- function(series, name, version, patterns = NULL, step = 0.05) {
  s <- series[dwi > 0 & !is.na(sec_rf)]
  if (length(patterns)) s <- s[entity %in% patterns]
  if (!nrow(s)) return(prototype_table(NULL))
  grid <- seq(step, max(s$dwi), by = step)
  interp <- function(x, y) {
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    if (length(unique(x)) < 3) return(rep(NA_real_, length(grid)))
    d <- data.table::data.table(x, y)[, .(y = mean(y)), by = x][order(x)]
    v <- stats::approx(d$x, d$y, xout = grid, rule = 1)$y
    v
  }
  tr <- s[, .(dwi = grid, sec_rf = interp(dwi, sec_rf), dwp = interp(dwi, dwp),
              util = interp(dwi, util6), lwor = interp(dwi, log10(pmax(wor6, 1e-3)))), by = entity]
  pr <- tr[, .(n = sum(!is.na(sec_rf)), sec_rf = stats::median(sec_rf, na.rm = TRUE), dwp = stats::median(dwp, na.rm = TRUE),
               util = stats::median(util, na.rm = TRUE), wor = 10^stats::median(lwor, na.rm = TRUE)), by = dwi]
  pr <- pr[n >= 3][, n := NULL]
  sm <- function(v) { if (sum(is.finite(v)) < 5) return(v); r <- stats::runmed(ifelse(is.finite(v), v, stats::median(v, na.rm = TRUE)), 5); as.numeric(stats::filter(r, rep(1 / 3, 3), sides = 2)) |> (\(x) ifelse(is.na(x), r, x))() }
  pr[, `:=`(util = sm(util), wor = 10^sm(log10(wor)))]
  if (!nrow(pr)) return(prototype_table(NULL))
  pr <- rbind(data.table::data.table(dwi = 0, sec_rf = 0, dwp = 0, util = NA_real_, wor = NA_real_), pr)
  pr[, `:=`(sec_rf = cummax(data.table::fcoalesce(sec_rf, 0)), dwp = cummax(data.table::fcoalesce(dwp, 0)))]
  pr[, `:=`(prototype = name, version = version, source = "analog")]
  prototype_table(pr)
}

# Theoretical prototype from HCPV-weighted rel-perm (balanced voidage).
bl_prototype <- function(sp) {
  w <- sp$hcpv / sum(sp$hcpv)
  wm <- function(x) sum(w * x)
  p <- list(swc = wm(sp$swc), sor = wm(sp$sor), krw_or = wm(sp$krw), kro_wc = wm(sp$kro), nw = wm(sp$nw),
            no = wm(sp$no), mu_o = wm(sp$mu_o), mu_w = wm(sp$mu_w))
  swi <- wm(sp$sw); bo <- wm(sp$bo); bw <- wm(sp$bw)
  cv <- welge_curve(p, swi)
  grid <- seq(0, 3, by = 0.05)
  ed <- stats::approx(cv$hcpvi, cv$ed, xout = grid, rule = 2)$y
  fw <- stats::approx(cv$hcpvi, cv$fw, xout = grid, rule = 2)$y
  slope <- c(NA, diff(ed) / diff(grid))
  out <- data.table::data.table(dwi = grid, sec_rf = ed, dwp = pmax(grid - ed, 0),
                                util = ifelse(slope > 1e-4, 1 / slope, NA_real_),
                                wor = ifelse(fw < 0.9999, fw / (1 - fw) * bo / bw, NA_real_),
                                prototype = "Buckley-Leverett", version = "theory", source = "theoretical")
  prototype_table(out)
}

# Assignment per pattern (latest valid_from on or before each month).
prototype_assignment <- function(assign_tbl, protos, patterns, override = NULL) {
  keys <- unique(protos$pkey)
  default <- keys[!grepl("^Buckley", keys)][1]
  if (is.na(default)) default <- keys[1]
  if (!is.null(override) && nzchar(override) && override %in% keys) {
    return(data.table::data.table(pattern = patterns, valid_from = as.Date("1800-01-01"), pkey = override))
  }
  a <- if (!is.null(assign_tbl) && nrow(assign_tbl)) {
    x <- assign_tbl[, .(pattern, valid_from = data.table::fcoalesce(valid_from, as.Date("1800-01-01")), pkey = proto_key(prototype, version))]
    x[pkey %in% keys]
  } else NULL
  miss <- setdiff(patterns, a$pattern)
  rbind(a, data.table::data.table(pattern = miss, valid_from = rep(as.Date("1800-01-01"), length(miss)), pkey = rep(default, length(miss))))
}

proto_interp <- function(protos, key, var, x) {
  p <- protos[protos$pkey == key & is.finite(protos[[var]])]
  if (nrow(p) < 2) return(rep(NA_real_, length(x)))
  stats::approx(p$dwi, p[[var]], xout = x, rule = 2, ties = "ordered")$y
}

apply_prototypes <- function(pm, assign, protos) {
  a <- data.table::copy(assign)[, date := valid_from]
  data.table::setkey(a, pattern, date)
  pm <- a[pm, on = .(pattern, date), roll = Inf, rollends = c(TRUE, TRUE)]
  if ("valid_from" %in% names(pm)) pm[, valid_from := NULL]
  data.table::setnames(pm, "pkey", "proto_key")
  pm[, dwi_p := ifelse(hcpv > 0, cum_winj_rb / hcpv, 0)]
  pm[, `:=`(exp_sec_rb = proto_interp(protos, proto_key[1], "sec_rf", dwi_p) * hcpv,
            exp_dwp_rb = proto_interp(protos, proto_key[1], "dwp", dwi_p) * hcpv,
            exp_util_w = proto_interp(protos, proto_key[1], "util", dwi_p) * hcpv,
            exp_lwor_w = log10(pmax(proto_interp(protos, proto_key[1], "wor", dwi_p), 1e-4)) * hcpv),
     by = .(pattern, proto_key)]
  pm[, flood_hcpv := hcpv]
  pm[, dwi_p := NULL]
  data.table::setorder(pm, pattern, date)
  pm[]
}
