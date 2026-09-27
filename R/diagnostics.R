# Maturity stages, diagnostics and forecasts ---------------------------------

default_settings <- list(
  wor_limit = 49,          # economic WOR (98 % water cut)
  wor_fit_months = 24,     # window for log(WOR) vs Np fit
  early_hcpvi = 0.10,      # below: early response
  mature_hcpvi = 0.50,     # above: mature
  mature_wc = 0.80,        # above: mature
  late_wc = 0.95,          # above: late life
  late_hcpvi = 1.20,       # above: late life
  vrr_low = 0.90,          # under-injection threshold (6-month mean)
  vrr_high = 1.30,         # over-injection threshold
  wc_jump = 0.15,          # water-cut increase in 12 months flagged as fast rise
  ev_low = 0.35,           # poor apparent sweep
  quad_hcpvi = 0.50,       # opportunity quadrant split, x
  quad_ev = 0.50           # opportunity quadrant split, y
)

stage_levels <- c("Primary", "Early response", "Developing", "Mature", "Late life")
stage_colors <- c("Primary" = "#8b98a9", "Early response" = "#60a5fa", "Developing" = "#2dd4bf",
                  "Mature" = "#fbbf24", "Late life" = "#f87171")

classify_stage <- function(hcpvi, wc, st = default_settings) {
  hcpvi[is.na(hcpvi)] <- 0
  wc[is.na(wc)] <- 0
  out <- ifelse(hcpvi <= 0, "Primary",
         ifelse(wc >= st$late_wc | hcpvi >= st$late_hcpvi, "Late life",
         ifelse(hcpvi >= st$mature_hcpvi | wc >= st$mature_wc, "Mature",
         ifelse(hcpvi >= st$early_hcpvi, "Developing", "Early response"))))
  factor(out, levels = stage_levels)
}

quadrant_levels <- c("Accelerate", "Harvest", "Conformance", "Investigate", "Convert / start flood")
quadrant_colors <- c("Accelerate" = "#2dd4bf", "Harvest" = "#60a5fa", "Conformance" = "#f87171",
                     "Investigate" = "#fbbf24", "Convert / start flood" = "#8b98a9")
quadrant_action <- c(
  "Accelerate" = "Good sweep, low throughput: raise injection / add injectors",
  "Harvest" = "Good sweep, mature: keep balance, manage lift & water handling",
  "Conformance" = "Poor sweep after high throughput: profile control, water shut-off, redirect water",
  "Investigate" = "Low sweep early on: check allocation, connectivity, injector integrity",
  "Convert / start flood" = "No injection yet: candidate for conversion / pattern start-up"
)

classify_quadrant <- function(hcpvi, ev, st = default_settings) {
  out <- ifelse(is.na(hcpvi) | hcpvi <= 0, "Convert / start flood",
         ifelse(hcpvi < st$quad_hcpvi,
                ifelse(!is.na(ev) & ev >= st$quad_ev, "Accelerate", "Investigate"),
                ifelse(!is.na(ev) & ev >= st$quad_ev, "Harvest", "Conformance")))
  factor(out, levels = quadrant_levels)
}

# log10(WOR) vs Np straight-line extrapolation to the economic WOR.
wor_forecast <- function(s, st = default_settings) {
  s <- s[oil > 0 & !is.na(wor) & wor > 0.02]
  s <- utils::tail(s, st$wor_fit_months)
  none <- list(eur = NA_real_, a = NA_real_, b = NA_real_, n = nrow(s))
  if (nrow(s) < 6) return(none)
  fit <- tryCatch(stats::lm(log10(wor) ~ cum_oil, data = s), error = function(e) NULL)
  if (is.null(fit)) return(none)
  a <- unname(stats::coef(fit)[1]); b <- unname(stats::coef(fit)[2])
  if (!is.finite(b) || b <= 0) return(none)
  eur <- (log10(st$wor_limit) - a) / b
  list(eur = max(eur, max(s$cum_oil)), a = a, b = b, n = nrow(s))
}

# Chan diagnostic series: WOR and its time derivative vs days on production.
chan_series <- function(s) {
  s <- s[oil + water > 0]
  if (!nrow(s)) return(s[, .(t = numeric(), wor = numeric(), dwor = numeric())])
  t <- cumsum(s$days)
  wor <- s$wor
  sm <- stats::filter(wor, rep(1 / 3, 3), sides = 2)
  dwor <- c(NA, diff(as.numeric(sm)) / diff(t))
  data.table::data.table(date = s$date, t = t, wor = wor, dwor = dwor)
}

# Flags for one entity series up to asof. Returns data.table(code, severity, message, action).
entity_flags <- function(s, st = default_settings) {
  out <- list()
  add <- function(code, sev, msg, act) out[[length(out) + 1]] <<- data.table::data.table(code = code, severity = sev, message = msg, action = act)
  empty <- data.table::data.table(code = character(), severity = character(), message = character(), action = character())
  n <- nrow(s)
  if (n < 3) return(empty)
  last <- s[n]
  l6 <- utils::tail(s, 6)
  prev6 <- if (n >= 12) s[(n - 11):(n - 6)] else NULL
  flooding <- isTRUE(last$cum_winj > 0)

  if (flooding && all(utils::tail(s$winj, 3) == 0) && any(s$winj > 0))
    add("INJ_STOPPED", "critical", "No injection in the last 3 months", "Check injector status / facilities")
  vrr6 <- sum(l6$winj_rb) / max(sum(l6$oil_rb + l6$water_rb), 1e-9)
  if (flooding && sum(l6$winj) > 0 && vrr6 < st$vrr_low)
    add("UNDER_INJ", if (vrr6 < 0.7) "critical" else "warning",
        sprintf("Voidage under-replaced: VRR(6m) = %.2f", vrr6), "Increase injection or reduce withdrawals")
  if (flooding && vrr6 > st$vrr_high)
    add("OVER_INJ", "warning", sprintf("Over-injection: VRR(6m) = %.2f", vrr6), "Check for out-of-pattern losses / fractures; rebalance")
  if (flooding && isTRUE(last$vrr_wf < 0.85) && isTRUE(last$hcpvi > 0.1))
    add("CUM_DEFICIT", "warning", sprintf("Voidage deficit since flood start: cum VRR = %.2f", last$vrr_wf),
        "Reservoir pressure likely below target; prioritise injection")
  if (!is.null(prev6)) {
    wc_now <- sum(l6$water) / max(sum(l6$oil + l6$water), 1e-9)
    old <- if (n >= 18) s[(n - 17):(n - 12)] else prev6
    wc_old <- sum(old$water) / max(sum(old$oil + old$water), 1e-9)
    if (wc_now - wc_old > st$wc_jump)
      add("WC_RISE", "warning", sprintf("Fast water-cut rise: %+.0f pts in 12 months", 100 * (wc_now - wc_old)),
          "Review Chan plot for channeling / coning; check injector-producer connectivity")
    qo6 <- mean(l6$qo); qo_prev <- mean(prev6$qo)
    if (qo_prev > 0 && qo6 / qo_prev < 0.7)
      add("OIL_DECLINE", "warning", sprintf("Oil rate down %.0f %% vs previous 6 months", 100 * (1 - qo6 / qo_prev)),
          "Check lift, well downtime and injection support")
  }
  if (flooding && isTRUE(last$hcpvi > 0.3) && isTRUE(last$ev < st$ev_low))
    add("LOW_SWEEP", "warning", sprintf("Low apparent sweep: Ev = %.2f at HCPVI %.2f", last$ev, last$hcpvi),
        "Conformance / profile control candidate")
  if (flooding && isTRUE(last$hcpvi < st$quad_hcpvi) && isTRUE(last$ev >= 0.6))
    add("HIGH_RESPONSE", "opportunity", sprintf("Strong response: Ev = %.2f at only %.2f HCPVI", last$ev, last$hcpvi),
        "Increase injection rate to accelerate recovery")
  if (!flooding && sum(s$oil) > 0)
    add("NO_FLOOD", "info", "Pattern still on primary depletion", "Evaluate injector conversion")
  # Chan: WOR derivative climbing steeply on log-log = channeling signature
  if (flooding && n >= 24) {
    cs <- chan_series(utils::tail(s, 24))
    cs <- cs[is.finite(dwor) & dwor > 0 & wor > 0]
    if (nrow(cs) >= 8) {
      sl <- tryCatch(unname(stats::coef(stats::lm(log10(dwor) ~ log10(t), data = cs))[2]), error = function(e) NA)
      if (is.finite(sl) && sl > 1.5 && utils::tail(cs$wor, 1) > 1)
        add("CHANNELING", "warning", sprintf("Chan WOR' slope %.1f: possible channeling", sl),
            "Consider injector profile modification / producer water shut-off")
    }
  }
  if (!length(out)) return(empty)
  data.table::rbindlist(out)
}

sev_rank <- c(critical = 1, warning = 2, opportunity = 3, info = 4)

# Full snapshot table with stage, quadrant, EUR and flags for all entities.
build_snapshot <- function(series, asof, st = default_settings) {
  ents <- unique(series$entity)
  rows <- lapply(ents, function(e) {
    s <- series[entity == e & date <= asof]
    if (!nrow(s)) return(NULL)
    last <- data.table::copy(s[.N])
    l6 <- utils::tail(s, 6)
    last[, `:=`(
      vrr6 = sum(l6$winj_rb) / max(sum(l6$oil_rb + l6$water_rb), 1e-9),
      qo6 = mean(l6$qo), qwi6 = mean(l6$qwi), wc6 = sum(l6$water) / max(sum(l6$oil + l6$water), 1e-9)
    )]
    fc <- wor_forecast(s, st)
    # cap at movable oil: a flat WOR trend cannot recover more than (1 - Sor)
    eur <- min(fc$eur, last$mov)
    last[, `:=`(eur = eur, rf_eur = eur / stooip, wor_a = fc$a, wor_b = fc$b)]
    fl <- entity_flags(s, st)
    if (nrow(fl)) fl[, entity := e]
    list(row = last, flags = fl)
  })
  rows <- Filter(Negate(is.null), rows)
  snap <- data.table::rbindlist(lapply(rows, `[[`, "row"), fill = TRUE)
  flags <- data.table::rbindlist(lapply(rows, `[[`, "flags"), fill = TRUE)
  if (!nrow(snap)) return(list(snap = snap, flags = flags))
  snap[, stage := classify_stage(hcpvi, wc6, st)]
  snap[, quadrant := classify_quadrant(hcpvi, ev, st)]
  snap[, action := unname(quadrant_action[as.character(quadrant)])]
  if (nrow(flags)) {
    flags[, rank := sev_rank[severity]]
    fs <- flags[, .(n_flags = .N, worst = severity[which.min(rank)]), by = entity]
    snap <- merge(snap, fs, by = "entity", all.x = TRUE)
  } else {
    snap[, `:=`(n_flags = 0L, worst = NA_character_)]
  }
  snap[is.na(n_flags), n_flags := 0L]
  list(snap = snap, flags = flags)
}
