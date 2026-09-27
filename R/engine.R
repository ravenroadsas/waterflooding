# Calculation engine (v2) --------------------------------------------------------
#
# Lineage (methodology section 7):
#   Wells + Alloc                      -> PatternRates
#   PatternRates + Vol + Fluids + Base -> PatternMaturity  (DWI, RF, Sec RF, DWP, DTP, Loss)
#   PatternRates + Vol                 -> PatternVelocity  (TP, Prod TP, IWR, Utilization)
#   InjSand + Alloc + Vol              -> PatternSandMetrics (unit DWI, unit TP, Cobb gap)
#   Wells + Hierarchy                  -> WellHeterogeneity (HI oil / HI water)
# Additive quantities are kept per pattern (and pattern x sand) so any level
# (area, field) is aggregated first and ratios are derived afterwards.

# 1. Well monthly volumes --------------------------------------------------------
well_volumes <- function(w) {
  w <- data.table::copy(w)
  w[, days := days_in_month(date)]
  w[, `:=`(oil = bopd * days, water = bwpd * days, winj = bwipd * days)]
  w[]
}

# 2. Allocation coefficient for every (pattern, well, month) -----------------------
# Undated rows are a constant baseline; dated rows hold until the next date.
expand_allocation <- function(alloc, wv) {
  a <- data.table::copy(alloc)[, .(pattern, well, date, coeff)]
  a[is.na(date), date := as.Date("1800-01-01")]
  a <- a[, .(coeff = coeff[.N]), by = .(pattern, well, date)]
  data.table::setkey(a, pattern, well, date)
  pairs <- unique(a[, .(pattern, well)])
  grid <- merge(pairs, unique(wv[, .(well, date)]), by = "well", allow.cartesian = TRUE)
  out <- a[grid, on = .(pattern, well, date), roll = Inf, rollends = c(TRUE, TRUE)]
  out[is.na(coeff), coeff := 0]
  out[]
}

# 3. Static properties ------------------------------------------------------------
sand_props <- function(vol, fluids) {
  v <- data.table::copy(vol)
  res <- unique(v$reservoir)
  fl <- data.table::rbindlist(lapply(res, function(r) {
    p <- reservoir_params(fluids, r)
    data.table::data.table(reservoir = r, bo = p$bo, bw = p$bw, swc = p$swc, sor = p$sor, krw = p$krw_or,
                           kro = p$kro_wc, nw = p$nw, no = p$no, mu_o = p$mu_o, mu_w = p$mu_w)
  }))
  v <- merge(v, fl, by = "reservoir", all.x = TRUE)
  v[is.na(sw), sw := swc]
  v[]
}

pattern_props <- function(sp) {
  sp[, .(hcpv = sum(hcpv), stoiip = sum(stoiip),
         bo = sum(hcpv * bo) / sum(hcpv), bw = sum(hcpv * bw) / sum(hcpv),
         swi = sum(hcpv * sw) / sum(hcpv), n_sands = .N,
         kh = if (all(is.na(k)) || all(is.na(h))) NA_real_ else sum(k * h, na.rm = TRUE)),
     by = pattern][, boi := hcpv / stoiip][]
}

# 4. Baseline (secondary recovery) -------------------------------------------------
pattern_baseline <- function(pm, baseline, method = "start") {
  first_inj <- pm[winj > 0, .(first_inj = min(date)), by = pattern]
  b <- merge(unique(pm[, .(pattern)]), first_inj, by = "pattern", all.x = TRUE)
  if (!is.null(baseline) && nrow(baseline)) {
    b <- merge(b, baseline[, .(pattern, wf_start, np_primary)], by = "pattern", all.x = TRUE)
  } else b[, `:=`(wf_start = as.Date(NA), np_primary = NA_real_)]
  b[, wf_start := data.table::fcoalesce(wf_start, first_inj)]
  b[, method := method]
  b[]
}

# 5. Run ----------------------------------------------------------------------------
run_engine <- function(ds, st = default_settings, extra_protos = NULL) {
  wv <- well_volumes(ds$wells)
  months <- sort(unique(wv$date))
  al <- expand_allocation(ds$alloc, wv)
  sp <- sand_props(ds$vol, ds$fluids)
  pp <- pattern_props(sp)

  pw <- merge(al, wv[, .(well, date, oil, water, winj)], by = c("well", "date"))
  pw[, `:=`(oil = oil * coeff, water = water * coeff, winj = winj * coeff)]

  pm <- pw[, .(oil = sum(oil), water = sum(water), winj = sum(winj)), by = .(pattern, date)]
  grid <- data.table::CJ(pattern = pp$pattern, date = months)
  pm <- pm[grid, on = .(pattern, date)]
  for (v in c("oil", "water", "winj")) data.table::set(pm, which(is.na(pm[[v]])), v, 0)
  pm <- merge(pm, pp[, .(pattern, hcpv, stoiip, bo, bw, boi)], by = "pattern")
  data.table::setorder(pm, pattern, date)
  pm[, days := days_in_month(date)]
  pm[, `:=`(oil_rb = oil * bo, water_rb = water * bw, winj_rb = winj * bw)]
  pm[, `:=`(cum_oil = cumsum(oil), cum_water = cumsum(water), cum_winj = cumsum(winj),
            cum_oil_rb = cumsum(oil_rb), cum_water_rb = cumsum(water_rb), cum_winj_rb = cumsum(winj_rb)), by = pattern]

  base <- pattern_baseline(pm, ds$baseline, st$baseline_method)
  pm <- merge(pm, base[, .(pattern, wf_start, np_primary)], by = "pattern", all.x = TRUE)
  data.table::setorder(pm, pattern, date)
  pm[, flooding := !is.na(wf_start) & date >= wf_start]
  pm[, sec_oil := {
    i <- which(flooding)[1]
    if (is.na(i)) rep(0, .N) else {
      np0 <- if (!is.na(np_primary[1])) np_primary[1] else if (i > 1) cum_oil[i - 1] else 0
      base_cum <- rep(0, .N)
      if (st$baseline_method == "decline" && i > 7) {
        pre <- max(1, i - 24):(i - 1); pre <- pre[oil[pre] > 0]
        if (length(pre) >= 6) {
          fit <- stats::lm(log(oil[pre]) ~ pre)
          d <- min(max(-stats::coef(fit)[2], 0), 0.1)
          q0 <- exp(stats::predict(fit, data.frame(pre = i - 1)))
          k <- seq_len(.N) - (i - 1)
          base_cum <- cumsum(ifelse(k >= 1, q0 * exp(-d * k), 0))
        }
      }
      pmax(cum_oil - np0 - base_cum, 0) * flooding
    }
  }, by = pattern]
  pm[, sec_oil_rb := sec_oil * bo]
  # reservoir withdrawals since the flood started (Loss = DWI - DTP on the same time basis)
  pm[, wd_wf_rb := {
    wd <- cumsum(oil_rb + water_rb); i <- which(flooding)[1]
    if (is.na(i)) rep(0, .N) else pmax(wd - (if (i > 1) wd[i - 1] else 0), 0) * flooding
  }, by = pattern]

  pat_map <- pattern_map(ds$hierarchy, pp$pattern)
  protos <- prototype_table(ds$prototypes, extra_protos)
  if (!nrow(protos)) protos <- analog_prototype(derive_metrics(data.table::copy(pm)[, entity := pattern], st), "Field analog (auto)", "auto")
  protos <- rbind(protos, bl_prototype(sp), fill = TRUE)
  assign <- prototype_assignment(ds$prototype_assign, protos, pp$pattern, st$prototype_override)
  pm <- apply_prototypes(pm, assign, protos)

  units <- unit_injection(ds, wv, al, sp, months, st)
  hi <- heterogeneity(wv, ds$hierarchy)

  list(months = months, well = wv, alloc = al, pattern_well = pw, sand_props = sp, props = pp,
       pm = pm, base = base, protos = protos, assign = assign, units = units, hi = hi,
       pat_map = pat_map, wells = ds$hierarchy, ds = ds, settings = st)
}

pattern_map <- function(h, patterns) {
  pm <- if (!is.null(h)) unique(h[, .(pattern, area, field)], by = "pattern") else data.table::data.table(pattern = character(), area = character(), field = character())
  miss <- setdiff(patterns, pm$pattern)
  if (length(miss)) pm <- rbind(pm, data.table::data.table(pattern = miss, area = "Unassigned", field = "Field"))
  pm[pattern %in% patterns]
}

# 6. Aggregation and derived metrics ---------------------------------------------------
additive_cols <- c("oil", "water", "winj", "oil_rb", "water_rb", "winj_rb", "cum_oil", "cum_water", "cum_winj",
                   "cum_oil_rb", "cum_water_rb", "cum_winj_rb", "sec_oil", "sec_oil_rb", "hcpv", "stoiip",
                   "exp_sec_rb", "exp_dwp_rb", "exp_util_w", "exp_lwor_w", "flood_hcpv", "wd_wf_rb")

entity_col <- function(level) switch(level, pattern = "pattern", area = "area", field = "field")

aggregate_level <- function(res, level = "pattern", entities = NULL, areas = NULL, st = res$settings) {
  pm <- merge(res$pm, res$pat_map, by = "pattern", all.x = TRUE)
  if (length(areas)) pm <- pm[area %in% areas]
  pm[, entity := get(entity_col(level))]
  if (length(entities)) pm <- pm[entity %in% entities]
  if (level == "pattern") {
    ag <- pm
  } else {
    ag <- pm[, c(lapply(.SD, sum), list(days = days[1], wf_start = suppressWarnings(min(wf_start, na.rm = TRUE)))),
             by = .(entity, date), .SDcols = additive_cols]
  }
  data.table::setorder(ag, entity, date)
  derive_metrics(ag, st)
}

safe_div <- function(a, b) ifelse(is.finite(a) & is.finite(b) & b > 0, a / b, NA_real_)

roll_sum <- function(x, n) data.table::frollsum(x, pmin(seq_along(x), n), adaptive = TRUE)

derive_metrics <- function(ag, st = default_settings) {
  ag <- data.table::copy(ag)
  if (!"exp_sec_rb" %in% names(ag)) ag[, c("exp_sec_rb", "exp_dwp_rb", "exp_util_w", "exp_lwor_w", "flood_hcpv", "wd_wf_rb") := NA_real_]
  ag[, `:=`(
    qo = oil / days, qw = water / days, qwi = winj / days,
    wc = safe_div(water, oil + water), wor = safe_div(water, oil),
    dwi = safe_div(cum_winj_rb, hcpv), rf = safe_div(cum_oil_rb, hcpv), sec_rf = safe_div(sec_oil_rb, hcpv),
    dwp = safe_div(cum_water_rb, hcpv), dtp = safe_div(cum_oil_rb + cum_water_rb, hcpv),
    iwr_cum = safe_div(cum_winj_rb, cum_oil_rb + cum_water_rb),
    tp = 100 * safe_div(winj_rb, hcpv) * 365 / days,
    prod_tp = 100 * safe_div(oil_rb + water_rb, hcpv) * 365 / days,
    util_cum = safe_div(cum_winj_rb, sec_oil_rb)
  )]
  ag[, `:=`(dtp_wf = safe_div(wd_wf_rb, hcpv))]
  ag[, loss := ifelse(dwi > 0, dwi - dtp_wf, NA_real_)]
  ag[, `:=`(
    tp12 = data.table::frollmean(tp, pmin(seq_len(.N), 12), adaptive = TRUE),
    prod_tp12 = data.table::frollmean(prod_tp, pmin(seq_len(.N), 12), adaptive = TRUE),
    iwr12 = safe_div(roll_sum(winj_rb, 12), roll_sum(oil_rb + water_rb, 12)),
    iwr = safe_div(winj_rb, oil_rb + water_rb),
    util3 = safe_div(roll_sum(winj_rb, 3), roll_sum(oil_rb, 3)),
    util6 = safe_div(roll_sum(winj_rb, 6), roll_sum(oil_rb, 6)),
    util12 = safe_div(roll_sum(winj_rb, 12), roll_sum(oil_rb, 12)),
    wc6 = safe_div(roll_sum(water, 6), roll_sum(oil + water, 6)),
    wor6 = safe_div(roll_sum(water, 6), roll_sum(oil, 6)),
    qo6 = roll_sum(oil, 6) / roll_sum(days, 6)
  ), by = entity]
  ag[, tp12_ago := data.table::shift(tp12, 12), by = entity]
  ag[, util := switch(as.character(st$util_window), "3" = util3, "12" = util12, util6)]
  judge <- ag$dwi >= st$judge_dwi
  ag[, `:=`(opr = ifelse(judge, safe_div(sec_oil_rb, exp_sec_rb), NA_real_),
            wpr = ifelse(judge, safe_div(cum_water_rb, exp_dwp_rb), NA_real_),
            exp_sec_rf = safe_div(exp_sec_rb, hcpv), exp_dwp = safe_div(exp_dwp_rb, hcpv),
            exp_util = safe_div(exp_util_w, flood_hcpv), exp_wor = 10^safe_div(exp_lwor_w, flood_hcpv))]
  ag[winj <= 0 & cum_winj <= 0, `:=`(iwr = NA_real_, iwr12 = NA_real_)]
  ag[]
}

snapshot_at <- function(series, asof) {
  s <- series[date <= asof]
  s[, .SD[.N], by = entity]
}

# 7. Unit (sand) injection from InjSand ---------------------------------------------------
unit_injection <- function(ds, wv, al, sp, months, st) {
  inj <- wv[winj > 0, .(well, date, winj, days)]
  if (!nrow(inj)) return(NULL)
  shares <- NULL
  if (!is.null(ds[["injsand"]]) && nrow(ds[["injsand"]])) {
    prof <- ds[["injsand"]][, .(bwipd = sum(bwipd)), by = .(well, sand, pdate = date)]
    prof[, share := bwipd / sum(bwipd), by = .(well, pdate)]
    prof <- prof[is.finite(share)]
    pd <- unique(prof[, .(well, pdate)])
    pd[, date := pdate]
    data.table::setkey(pd, well, date)
    lk <- pd[inj[, .(well, date)], on = .(well, date), roll = Inf, rollends = c(TRUE, TRUE)]
    lk <- lk[!is.na(pdate)]
    shares <- merge(lk[, .(well, date, pdate)], prof[, .(well, pdate, sand, share)], by = c("well", "pdate"), allow.cartesian = TRUE)
    shares[, method := "profile"]
  }
  # injectors without profile: HCPV share of the sands in their patterns
  noprof <- setdiff(unique(inj$well), if (is.null(shares)) character() else unique(shares$well))
  if (length(noprof)) {
    wp <- unique(al[well %in% noprof & coeff > 0, .(well, pattern)])
    hs <- merge(wp, sp[, .(pattern, sand, hcpv)], by = "pattern", allow.cartesian = TRUE)[, .(hcpv = sum(hcpv)), by = .(well, sand)]
    hs[, share := hcpv / sum(hcpv), by = well]
    fb <- merge(inj[well %in% noprof, .(well, date)], hs[, .(well, sand, share)], by = "well", allow.cartesian = TRUE)
    fb[, `:=`(pdate = as.Date(NA), method = "hcpv")]
    shares <- rbind(shares, fb, fill = TRUE)
  }
  ws <- merge(shares, inj, by = c("well", "date"))
  ws[, winj_s := winj * share]

  # valves and design rates, held until the next status date
  if (!is.null(ds$injsand_status) && nrow(ds$injsand_status)) {
    stt <- ds$injsand_status[, .(vrf = vrf[.N], cobb = cobb[.N]), by = .(well, sand, date)]
    data.table::setkey(stt, well, sand, date)
    ws <- stt[ws, on = .(well, sand, date), roll = Inf]
  } else ws[, `:=`(vrf = NA_real_, cobb = NA_real_)]

  # pattern x sand
  ia <- al[well %in% unique(ws$well), .(pattern, well, date, coeff)]
  pu <- merge(ia, ws[, .(well, date, sand, winj_s, cobb, method)], by = c("well", "date"), allow.cartesian = TRUE)
  pu <- pu[, .(winj = sum(coeff * winj_s), cobb = sum(coeff * cobb, na.rm = TRUE),
               has_cobb = any(!is.na(cobb)), assumed = any(method == "hcpv")), by = .(pattern, sand, date)]
  g <- sp[, .(date = months), by = .(pattern, sand, reservoir, hcpv, bw, k, h)]
  pu <- pu[g, on = .(pattern, sand, date)]
  pu[is.na(winj), winj := 0]
  pu[is.na(has_cobb), has_cobb := FALSE]
  pu[, days := days_in_month(date)]
  data.table::setorder(pu, pattern, sand, date)
  pu[, `:=`(winj_rb = winj * bw, cum_winj = cumsum(winj)), by = .(pattern, sand)]
  pu[, `:=`(cum_winj_rb = cumsum(winj_rb)), by = .(pattern, sand)]
  pu[, `:=`(dwi = safe_div(cum_winj_rb, hcpv), tp = 100 * safe_div(winj_rb, hcpv) * 365 / days, rate = winj / days)]
  pu[, tp12 := data.table::frollmean(tp, pmin(seq_len(.N), 12), adaptive = TRUE), by = .(pattern, sand)]
  pu[, tp12_ago := data.table::shift(tp12, 12), by = .(pattern, sand)]
  pu[, cobb := ifelse(has_cobb, cobb, NA_real_)]
  list(well_sand = ws, pattern_sand = pu)
}

# Vertical conformance proxy: overlap of cumulative injection shares and HCPV shares.
vertical_efficiency <- function(pu_snap) {
  pu_snap[, .(ve = {
    si <- if (sum(cum_winj_rb) > 0) cum_winj_rb / sum(cum_winj_rb) else rep(NA_real_, .N)
    sh <- hcpv / sum(hcpv)
    if (anyNA(si)) NA_real_ else sum(pmin(si, sh))
  }, top_sand = sand[which.max(dwi)], top_dwi = max(dwi, na.rm = TRUE),
  top_share = { s <- cum_winj_rb / sum(cum_winj_rb); s[which.max(dwi)] },
  top_hcpv_share = (hcpv / sum(hcpv))[which.max(dwi)]), by = pattern]
}

# 8. Heterogeneity index (producers, cumulative, vs area average) -----------------------
heterogeneity <- function(wv, h) {
  if (is.null(h)) return(NULL)
  prd <- unique(h[well_type == "PRODUCER", .(well, area)], by = "well")
  w <- merge(wv[, .(well, date, oil, water)], prd, by = "well")
  if (!nrow(w)) return(NULL)
  data.table::setorder(w, well, date)
  w[, `:=`(cum_oil = cumsum(oil), cum_water = cumsum(water)), by = well]
  w[, `:=`(avg_oil = mean(cum_oil[cum_oil > 0]), avg_water = mean(cum_water[cum_water > 0])), by = .(area, date)]
  w[, `:=`(hi_oil = safe_div(cum_oil, avg_oil) - 1, hi_water = safe_div(cum_water, avg_water) - 1)]
  w[]
}
