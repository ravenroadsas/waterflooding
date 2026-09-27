# Calculation engine ---------------------------------------------------------
#
#   well monthly rates --(areal allocation, per month)--> pattern
#                      --(kh split per well & sand)-----> pattern x sand
#
# Everything additive (stb, rb, cumulatives, STOOIP, pore volumes, ideal
# recovery) is computed at pattern x sand level; ratios and dimensionless
# variables are derived after aggregating to the requested level (pattern,
# block, field) and sand selection so they are always volume-consistent.

# 1. Well monthly volumes ------------------------------------------------------
well_volumes <- function(prod) {
  w <- prod[, .(bopd = sum(bopd), bwpd = sum(bwpd), bwipd = sum(bwipd)), by = .(well, date)]
  w[, days := days_in_month(date)]
  w[, `:=`(oil = bopd * days, water = bwpd * days, winj = bwipd * days)]
  w[]
}

# 2. Allocation coefficient for every (pattern, well, month) --------------------
# Undated rows are a constant baseline; dated rows hold until the next date.
# Before the first dated value the baseline (or the first dated value) applies.
expand_allocation <- function(alloc, wv) {
  a <- data.table::copy(alloc)[, .(pattern, well, date, coefficient)]
  a[is.na(date), date := as.Date("1800-01-01")]
  a <- a[, .(coefficient = coefficient[.N]), by = .(pattern, well, date)]
  data.table::setkey(a, pattern, well, date)
  pairs <- unique(a[, .(pattern, well)])
  grid <- merge(pairs, unique(wv[, .(well, date)]), by = "well", allow.cartesian = TRUE)
  out <- a[grid, on = .(pattern, well, date), roll = Inf, rollends = c(TRUE, TRUE)]
  out[is.na(coefficient), coefficient := 0]
  out[]
}

# 3. Sand split for each (pattern, well): kh share among the pattern's sands,
#    STOOIP share when the well has no petrophysics for those sands.
sand_split <- function(pairs, stooip, petro) {
  st <- stooip[, .(stooip = sum(stooip, na.rm = TRUE)), by = .(pattern, sand)]
  cand <- merge(pairs, st, by = "pattern", allow.cartesian = TRUE)
  if (!is.null(petro) && nrow(petro)) {
    pe <- data.table::copy(petro)
    pe[, kh := ifelse(is.na(permeability), net_pay, net_pay * permeability)]
    # a well with partial perm data uses h only, for consistency
    pe[, kh := if (anyNA(permeability)) net_pay else kh, by = well]
    pe <- pe[, .(kh = sum(kh, na.rm = TRUE)), by = .(well, sand)]
    cand[pe, on = .(well, sand), kh := i.kh]
  } else {
    cand[, kh := NA_real_]
  }
  cand[is.na(kh), kh := 0]
  cand[, `:=`(khs = sum(kh), sts = sum(stooip)), by = .(pattern, well)]
  cand[, method := ifelse(khs > 0, "kh", "stooip")]
  cand[, frac := ifelse(khs > 0, kh / khs, ifelse(sts > 0, stooip / sts, 1 / .N)), by = .(pattern, well)]
  cand[, .(pattern, well, sand, frac, method)]
}

# 4. Static properties per pattern x sand, joined with fluid data.
pattern_sand_props <- function(stooip, fluids) {
  s <- stooip[, .(stooip = sum(stooip, na.rm = TRUE),
                  swi = stats::weighted.mean(swi, pmax(stooip, 1), na.rm = TRUE),
                  boi = stats::weighted.mean(boi, pmax(stooip, 1), na.rm = TRUE),
                  area = sum(area, na.rm = TRUE),
                  net_pay = stats::weighted.mean(net_pay, pmax(stooip, 1), na.rm = TRUE),
                  porosity = stats::weighted.mean(porosity, pmax(stooip, 1), na.rm = TRUE),
                  permeability = stats::weighted.mean(permeability, pmax(stooip, 1), na.rm = TRUE)),
              by = .(pattern, sand)]
  fl <- data.table::rbindlist(lapply(unique(s$sand), function(sd) {
    fp <- fluid_params(fluids, sd)
    data.table::data.table(sand = sd, bo = fp$bo, bw = fp$bw, mu_o = fp$mu_o, mu_w = fp$mu_w, swc = fp$swc,
                           sor = fp$sor, krw_or = fp$krw_or, kro_wc = fp$kro_wc, nw = fp$nw, no = fp$no)
  }))
  s <- merge(s, fl, by = "sand")
  s[is.na(swi) | is.nan(swi), swi := swc]
  s[is.na(boi) | is.nan(boi), boi := bo]
  s[, hcpv := stooip * boi]
  s[, pv := hcpv / (1 - swi)]
  s[, mov := stooip * pmax(1 - swi - sor, 0) / (1 - swi)]
  s[, mobility := (krw_or / mu_w) / (kro_wc / mu_o)]
  s[, ed_max := pmax(1 - swi - sor, 0) / (1 - swi)]
  s[]
}

# 5. Run: builds the pattern x sand monthly table and pattern x well table.
run_engine <- function(ds) {
  wv <- well_volumes(ds$production)
  months <- sort(unique(wv$date))
  al <- expand_allocation(ds$allocation, wv)
  pairs <- unique(al[, .(pattern, well)])
  split <- sand_split(pairs, ds$stooip, ds$petrophysics)
  props <- pattern_sand_props(ds$stooip, ds$fluids)

  # pattern x well monthly (allocated, all sands)
  pw <- merge(al, wv[, .(well, date, oil, water, winj)], by = c("well", "date"))
  pw[, `:=`(oil = oil * coefficient, water = water * coefficient, winj = winj * coefficient)]

  # pattern x sand monthly
  ps <- merge(pw[, .(pattern, well, date, oil, water, winj)], split[, .(pattern, well, sand, frac)],
              by = c("pattern", "well"), allow.cartesian = TRUE)
  ps <- ps[, .(oil = sum(oil * frac), water = sum(water * frac), winj = sum(winj * frac)), by = .(pattern, sand, date)]

  # complete grid so cumulatives are continuous
  keys <- unique(props[, .(pattern, sand)])
  grid <- keys[, .(date = months), by = .(pattern, sand)]
  ps <- ps[grid, on = .(pattern, sand, date)]
  for (v in c("oil", "water", "winj")) data.table::set(ps, which(is.na(ps[[v]])), v, 0)
  ps <- merge(ps, props[, .(pattern, sand, stooip, boi, bo, bw, swi, hcpv, pv, mov, swc, sor, krw_or, kro_wc, nw, no, mu_o, mu_w)],
              by = c("pattern", "sand"))
  data.table::setorder(ps, pattern, sand, date)
  ps[, `:=`(oil_rb = oil * bo, water_rb = water * bw, winj_rb = winj * bw)]
  ps[, `:=`(cum_oil = cumsum(oil), cum_water = cumsum(water), cum_winj = cumsum(winj),
            cum_oil_rb = cumsum(oil_rb), cum_water_rb = cumsum(water_rb), cum_winj_rb = cumsum(winj_rb)),
     by = .(pattern, sand)]
  # waterflood start and incremental (post-injection) oil
  ps[, wf_started := cum_winj > 0]
  ps[, np_start := {
    i <- which(winj > 0)[1]
    if (is.na(i)) NA_real_ else cum_oil[i] - oil[i]
  }, by = .(pattern, sand)]
  # incremental waterflood oil = production since flood start minus the primary
  # decline extrapolated (exponential fit on the last 24 pre-injection months)
  ps[, np_wf := {
    i <- which(winj > 0)[1]
    if (is.na(i)) rep(0, .N) else {
      base <- rep(0, .N)
      pre <- max(1, i - 24):(i - 1)
      pre <- pre[pre >= 1 & oil[pre] > 0]
      if (i > 1 && length(pre) >= 6) {
        fit <- stats::lm(log(oil[pre]) ~ pre)
        d <- min(max(-stats::coef(fit)[2], 0), 0.1)
        q0 <- exp(stats::predict(fit, data.frame(pre = i - 1)))
        k <- seq_len(.N) - (i - 1)
        base <- ifelse(k >= 1, q0 * exp(-d * k), 0)
      }
      pmax(cum_oil - np_start - cumsum(base), 0) * wf_started
    }
  }, by = .(pattern, sand)]
  # reservoir withdrawals since the waterflood started (for VRR since flood start)
  ps[, wd_wf_rb := {
    wd <- cumsum(oil_rb + water_rb)
    i <- which(winj > 0)[1]
    if (is.na(i)) rep(0, .N) else pmax(wd - (wd[i] - oil_rb[i] - water_rb[i]), 0) * wf_started
  }, by = .(pattern, sand)]
  ps[, hcpvi_ps := ifelse(hcpv > 0, cum_winj_rb / hcpv, 0)]
  # ideal (100 % volumetric sweep) waterflood oil from Buckley-Leverett
  ps[, ideal_np := {
    p <- list(swc = swc[1], sor = sor[1], krw_or = krw_or[1], kro_wc = kro_wc[1], nw = nw[1], no = no[1],
              mu_o = mu_o[1], mu_w = mu_w[1])
    stooip[1] * ed_fun(p, swi[1])(hcpvi_ps) * boi[1] / bo[1]
  }, by = .(pattern, sand)]
  ps[, c("np_start", "hcpvi_ps") := NULL]

  h <- ds$hierarchy
  pat_map <- if (!is.null(h)) unique(h[, .(pattern, block, field)], by = "pattern") else data.table::data.table(pattern = character(), block = character(), field = character())
  miss <- setdiff(unique(props$pattern), pat_map$pattern)
  if (length(miss)) pat_map <- rbind(pat_map, data.table::data.table(pattern = miss, block = "Unassigned", field = "Field"))

  list(months = months, well = wv, alloc = al, pattern_well = pw, split = split, props = props,
       ps = ps, pat_map = pat_map, wells = h)
}

# 6. Aggregate to a level and sand selection, then derive metrics ---------------
additive_cols <- c("oil", "water", "winj", "oil_rb", "water_rb", "winj_rb", "cum_oil", "cum_water",
                   "cum_winj", "cum_oil_rb", "cum_water_rb", "cum_winj_rb", "np_wf", "ideal_np", "wd_wf_rb")
static_cols <- c("stooip", "hcpv", "pv", "mov")

entity_col <- function(level) switch(level, pattern = "pattern", block = "block", field = "field")

aggregate_level <- function(res, level = "pattern", sands = NULL, entities = NULL) {
  ps <- res$ps
  if (length(sands)) ps <- ps[sand %in% sands]
  ps <- merge(ps, res$pat_map, by = "pattern", all.x = TRUE)
  ec <- entity_col(level)
  ps[, entity := get(ec)]
  if (length(entities)) ps <- ps[entity %in% entities]
  ag <- ps[, c(lapply(.SD, sum), list(days = days_in_month(date[1]))), by = .(entity, date), .SDcols = c(additive_cols, static_cols)]
  data.table::setorder(ag, entity, date)
  derive_metrics(ag)
}

safe_div <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, NA_real_)

derive_metrics <- function(ag) {
  ag[, `:=`(
    qo = oil / days, qw = water / days, qwi = winj / days,
    ql = (oil + water) / days,
    wc = safe_div(water, oil + water),
    wor = safe_div(water, oil),
    vrr = safe_div(winj_rb, oil_rb + water_rb),
    cum_vrr = safe_div(cum_winj_rb, cum_oil_rb + cum_water_rb),
    vrr_wf = safe_div(cum_winj_rb, wd_wf_rb),
    rf = safe_div(cum_oil, stooip),
    rf_wf = safe_div(np_wf, stooip),
    hcpvi = safe_div(cum_winj_rb, hcpv),
    pvi = safe_div(cum_winj_rb, pv),
    rf_ideal = safe_div(ideal_np, stooip),
    ev = safe_div(np_wf, ideal_np),
    mi = safe_div(cum_oil, mov),
    inj_eff = safe_div(oil, winj),
    wuf = safe_div(cum_winj, np_wf),
    remaining = stooip - cum_oil,
    remaining_mov = pmax(mov - cum_oil, 0)
  )]
  ag[, ev := pmin(ev, 1.5)]
  ag[cum_winj <= 0, `:=`(vrr = NA_real_, vrr_wf = NA_real_)]  # no flood yet
  ag[]
}

# Snapshot of the entities at one month (last row <= asof).
snapshot_at <- function(series, asof) {
  s <- series[date <= asof]
  s[, .SD[.N], by = entity]
}

# Theoretical recovery curve (ED vs HCPVI) for an aggregate, using STOOIP-weighted
# pattern x sand properties.
aggregate_props <- function(res, sands = NULL, patterns = NULL) {
  pr <- res$props
  if (length(sands)) pr <- pr[sand %in% sands]
  if (length(patterns)) pr <- pr[pattern %in% patterns]
  w <- pmax(pr$stooip, 1)
  wm <- function(x) stats::weighted.mean(x, w)
  list(swc = wm(pr$swc), sor = wm(pr$sor), krw_or = wm(pr$krw_or), kro_wc = wm(pr$kro_wc), nw = wm(pr$nw),
       no = wm(pr$no), mu_o = wm(pr$mu_o), mu_w = wm(pr$mu_w), swi = wm(pr$swi), bo = wm(pr$bo), boi = wm(pr$boi),
       bw = wm(pr$bw), stooip = sum(pr$stooip))
}

theoretical_curve <- function(res, sands = NULL, patterns = NULL) {
  p <- aggregate_props(res, sands, patterns)
  cv <- welge_curve(p, p$swi)
  cv[, rf := ed * p$boi / p$bo]
  cv[hcpvi <= 3]
}

# Pattern x sand metrics at one month (for the pattern x sand matrix).
pattern_sand_snapshot <- function(res, asof, patterns = NULL) {
  ps <- res$ps[date == max(date[date <= asof])]
  if (length(patterns)) ps <- ps[pattern %in% patterns]
  ag <- ps[, c(lapply(.SD, sum), list(days = days_in_month(date[1]))), by = .(pattern, sand, date),
           .SDcols = c(additive_cols, static_cols)]
  ag[, entity := pattern]
  derive_metrics(ag)
}

# Wells of a pattern with coefficient and allocated cumulative volumes at asof.
pattern_wells <- function(res, pat, asof) {
  pw <- res$pattern_well[pattern == pat & date <= asof]
  if (!nrow(pw)) return(data.table::data.table())
  cur <- pw[, .SD[.N], by = well][, .(well, coefficient)]
  tot <- pw[, .(cum_oil = sum(oil), cum_water = sum(water), cum_winj = sum(winj),
                last_qo = sum(oil[date == max(date)]) / days_in_month(max(date)),
                last_qwi = sum(winj[date == max(date)]) / days_in_month(max(date))), by = well]
  out <- merge(cur, tot, by = "well")
  if (!is.null(res$wells)) out <- merge(out, unique(res$wells[, .(well, well_type)], by = "well"), by = "well", all.x = TRUE)
  wtot <- res$well[date <= asof, .(well_cum_oil = sum(oil), well_cum_winj = sum(winj)), by = well]
  out <- merge(out, wtot, by = "well", all.x = TRUE)
  data.table::setorder(out, well_type, well)
  out[]
}
