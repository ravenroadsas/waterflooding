# Wellbore: candidates from the log algorithms, completion state, water offenders, potential gaps ---
#
# Log algorithms return intervals only (no rates). Intervals of several algorithms that overlap merge
# into one candidate; agreement = number of algorithms that found it. Each candidate is checked against
# the completion history (open perforations, squeezes, plugs) and matched to the single-well analysis
# (INTERVALOS) for its potential. Open intervals carry rates (water offender analysis) and a theoretical
# potential (stimulation / re-perforation gap). Interval potentials add up.

overlap_ft <- function(t1, b1, t2, b2) pmax(0, pmin(b1, b2) - pmax(t1, t2))
overlap_frac <- function(t1, b1, t2, b2) overlap_ft(t1, b1, t2, b2) / pmax(pmin(b1 - t1, b2 - t2), 1e-6)

# Current state of the wellbore: the latest event per depth wins. A perforation is open unless a later
# squeeze covers it or it sits below an active plug.
completion_state <- function(cp, asof = NULL) {
  empty <- data.table::data.table(well = character(), top_ft = numeric(), base_ft = numeric(), date = as.Date(character()))
  if (is.null(cp) || !nrow(cp)) return(list(open = empty, squeeze = empty, plug = empty))
  x <- data.table::copy(cp)
  if (!is.null(asof)) x <- x[is.na(date) | date <= asof]
  x[, d := data.table::fcoalesce(date, as.Date("1900-01-01"))]
  data.table::setorder(x, well, d)
  last <- x[, .SD[.N], by = .(well, top_ft, base_ft, type)]
  plug <- last[type == "PLUG" & !status %in% c("REMOVED", "DRILLED", "RETRIEVED")]
  sq <- last[type == "SQUEEZE" & !status %in% c("REMOVED", "DRILLED")]
  op <- last[type %in% c("PERFORATION", "SLEEVE") & status == "OPEN"]
  if (nrow(op)) {
    keep <- vapply(seq_len(nrow(op)), function(i) {
      o <- op[i]
      s <- sq[well == o$well & d >= o$d & overlap_frac(top_ft, base_ft, o$top_ft, o$base_ft) >= 0.5]
      p <- plug[well == o$well & top_ft <= o$top_ft]
      !nrow(s) && !nrow(p)
    }, TRUE)
    op <- op[keep]
  }
  list(open = op, squeeze = sq, plug = plug)
}

# "" = free to perforate; otherwise the reason it cannot be offered.
interval_conflict <- function(st, w, top, base) {
  mapply(function(w, t, b) {
    p <- st$plug[well == w & top_ft <= t]
    if (nrow(p)) return(sprintf("below plug at %s ft", fmt_int(min(p$top_ft))))
    if (nrow(st$squeeze[well == w & overlap_frac(top_ft, base_ft, t, b) >= 0.3])) return("squeezed")
    if (nrow(st$open[well == w & overlap_frac(top_ft, base_ft, t, b) >= 0.5])) return("already open")
    ""
  }, w, top, base, USE.NAMES = FALSE)
}

# Merge the algorithms' intervals per well: overlapping intervals form one candidate (their union).
merge_log_intervals <- function(li) {
  if (is.null(li) || !nrow(li)) return(NULL)
  x <- data.table::copy(li)[is.finite(top_ft) & is.finite(base_ft) & base_ft > top_ft]
  data.table::setorder(x, well, top_ft)
  x[, grp := { g <- integer(.N); cur <- 0L; bmax <- -Inf
    for (i in seq_len(.N)) { if (top_ft[i] > bmax) { cur <- cur + 1L; bmax <- base_ft[i] } else bmax <- max(bmax, base_ft[i]); g[i] <- cur }
    g }, by = well]
  n_alg <- x[, data.table::uniqueN(algorithm), by = well]
  c <- x[, .(top_ft = min(top_ft), base_ft = max(base_ft), n = data.table::uniqueN(algorithm),
             algorithms = paste(sort(unique(algorithm)), collapse = ", "), sand = stats::na.omit(sand)[1],
             score = if (all(is.na(score))) NA_real_ else mean(score, na.rm = TRUE)), by = .(well, grp)]
  c <- merge(c, n_alg[, .(well, n_of = V1)], by = "well")
  c[, cand_id := sprintf("L%s", round(top_ft))]
  c[, grp := NULL]
  c[]
}

# Candidates of all wells, matched to INTERVALOS (potential) and checked against completions.
log_candidates <- function(ds, asof = NULL) {
  c <- merge_log_intervals(ds$log_intervals)
  if (is.null(c)) return(NULL)
  st <- completion_state(ds$completions, asof)
  iv <- ds$intervals
  c[, `:=`(interval_id = NA_character_, iv_sand = NA_character_)]
  if (!is.null(iv) && nrow(iv)) {
    for (i in seq_len(nrow(c))) {
      m <- iv[well == c$well[i] & is.finite(top_ft) & overlap_frac(top_ft, base_ft, c$top_ft[i], c$base_ft[i]) >= 0.5]
      if (nrow(m)) { m <- m[which.max(overlap_ft(top_ft, base_ft, c$top_ft[i], c$base_ft[i]))]
        data.table::set(c, i, c("interval_id", "iv_sand"), list(m$interval_id, m$sand)) }
    }
  }
  c[, sand := data.table::fcoalesce(sand, iv_sand)][, iv_sand := NULL]
  c[, conflict := interval_conflict(st, well, top_ft, base_ft)]
  c[, target := data.table::fcoalesce(interval_id, cand_id)]
  c[]
}

# Open intervals ranked by their share of the well's water (latest rates at or before asof).
water_offenders <- function(ds, asof = NULL) {
  ir <- ds$interval_rates
  if (is.null(ir) || !nrow(ir)) return(NULL)
  x <- data.table::copy(ir)
  if (!is.null(asof)) x <- x[is.na(date) | date <= asof]
  if (!nrow(x)) return(NULL)
  x[, d := data.table::fcoalesce(date, as.Date("1900-01-01"))]
  x <- x[, .SD[d == max(d)], by = well]
  st <- completion_state(ds$completions, asof)
  if (nrow(st$open)) x <- x[vapply(seq_len(.N), function(i) !nrow(st$open[well == x$well[i]]) ||
                                     nrow(st$open[well == x$well[i] & overlap_frac(top_ft, base_ft, x$top_ft[i], x$base_ft[i]) >= 0.5]) > 0, TRUE)]
  x[, `:=`(share = qw / pmax(sum(qw), 1e-9), wc = qw / pmax(qo + qw, 1e-9), rank = data.table::frank(-qw, ties.method = "first")), by = well]
  x[, target := data.table::fcoalesce(interval_id, sprintf("R%s", round(top_ft)))]
  x[, d := NULL]
  x[order(well, rank)]
}

# Theoretical potential vs current rate per open interval.
potential_gaps <- function(ds, asof = NULL) {
  ip <- ds$interval_potential
  if (is.null(ip) || !nrow(ip)) return(NULL)
  ir <- water_offenders(ds, asof)
  x <- data.table::copy(ip)
  x[, `:=`(qo_now = NA_real_, qw_now = NA_real_)]
  if (!is.null(ir)) for (i in seq_len(nrow(x))) {
    m <- ir[well == x$well[i] & ((!is.na(interval_id) & interval_id %in% x$interval_id[i]) | overlap_frac(top_ft, base_ft, x$top_ft[i], x$base_ft[i]) >= 0.5)]
    if (nrow(m)) data.table::set(x, i, c("qo_now", "qw_now"), list(m$qo[1], m$qw[1]))
  }
  x[, gap := qo_theo - qo_now]
  x[, target := data.table::fcoalesce(interval_id, sprintf("P%s", round(top_ft)))]
  x[]
}

# Deepest depth known for a well (cost lookup).
well_depth <- function(ds, w) {
  d <- c(ds$completions[well == w, base_ft], ds$intervals[well == w, base_ft], ds$log_intervals[well == w, base_ft])
  d <- d[is.finite(d)]
  if (length(d)) max(d) else NA_real_
}

# Everything the ADPERF workbench shows for one well, with the opportunity key of each element.
well_workbench <- function(res, op, w, asof) {
  ds <- res$ds; s <- op$summary
  okey <- function(a, tg) { if (!nrow(s)) return(rep(NA_character_, length(tg))); vapply(tg, function(t) { k <- s[well == w & action == a & interval %in% t, key]; if (length(k)) k[1] else NA_character_ }, "") }
  st <- completion_state(ds$completions, asof)
  st <- lapply(st, function(x) x[well == w])
  lg <- if (!is.null(ds$log_intervals)) ds$log_intervals[well == w] else NULL
  c <- log_candidates(ds, asof); c <- if (!is.null(c)) c[well == w] else NULL
  iv <- ds$intervals; iv <- if (!is.null(iv)) iv[well == w & estado %in% c("cerrado", "parcial") & is.finite(top_ft)] else NULL
  if (!is.null(iv) && nrow(iv)) {
    extra <- iv[!interval_id %in% c$interval_id]
    if (nrow(extra)) c <- rbind(c, extra[, .(well, top_ft, base_ft, n = 0L, algorithms = "single-well analysis only", sand, score = NA_real_,
                                             n_of = if (!is.null(c) && nrow(c)) c$n_of[1] else 0L, cand_id = interval_id, interval_id,
                                             conflict = interval_conflict(completion_state(ds$completions, asof), well, top_ft, base_ft), target = interval_id)], fill = TRUE)
    if (!is.null(c)) c[estado_of(iv, interval_id) == "parcial" & conflict == "already open", conflict := ""]
  }
  if (!is.null(c) && nrow(c)) {
    c[, key := okey("ADPERF", target)]
    m <- if (nrow(s)) s[well == w & action == "ADPERF", .(target = interval, qo_bajo = qo1_Bajo, qo_base = qo1_Base, qo_alto = qo1_Alto, status = as.character(status), gain)] else NULL
    if (!is.null(m) && nrow(m)) c <- merge(c, m, by = "target", all.x = TRUE) else c[, `:=`(qo_bajo = NA_real_, qo_base = NA_real_, qo_alto = NA_real_, status = NA_character_, gain = NA_real_)]
    c[!is.finite(qo_base) & is.finite(gain), qo_base := gain]
    data.table::setorder(c, top_ft)
  }
  of <- water_offenders(ds, asof); of <- if (!is.null(of)) of[well == w] else NULL
  if (!is.null(of) && nrow(of)) of[, key := okey("WSO", target)]
  gp <- potential_gaps(ds, asof); gp <- if (!is.null(gp)) gp[well == w] else NULL
  if (!is.null(gp) && nrow(gp)) gp[, key := okey("REPERF", target)]
  deps <- c(st$open$top_ft, st$open$base_ft, st$squeeze$base_ft, st$plug$top_ft, lg$top_ft, lg$base_ft, c$top_ft, c$base_ft, of$base_ft, gp$base_ft)
  deps <- deps[is.finite(deps)]
  list(well = w, state = st, log = lg, cands = c, offenders = of, gaps = gp,
       depth_min = if (length(deps)) min(deps) else NA_real_, depth_max = if (length(deps)) max(deps) + 15 else NA_real_,
       qmax = max(c(c$qo_base, 1), na.rm = TRUE))
}
estado_of <- function(iv, ids) iv$estado[match(ids, iv$interval_id)]

# Wells that have something to show in the workbench.
workbench_wells <- function(ds) {
  w <- unique(c(ds$log_intervals$well, ds$intervals[estado %in% c("cerrado", "parcial"), well], ds$interval_rates$well, ds$interval_potential$well))
  sort(w[!is.na(w)])
}
