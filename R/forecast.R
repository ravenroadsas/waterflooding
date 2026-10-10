# Forecasts for well opportunities ----------------------------------------------------------
#
# Three sources, best first:
#   PROFILE  Bajo / Base / Alto monthly profiles delivered by an analysis (PERFILES_MENSUALES)
#   SF_FIT   waterflood-fit gain of a pattern rule (bopd, see sf.R)
#   ARPS     decline fit of the well's own history (stimulation, lift, reactivation, job baselines)
# Profiles are joined on well + unit + interval; "-" stands for the whole well.

days_per_month <- 30.4375

arps_q <- function(qi, di, b, t) {
  if (!is.finite(b) || b <= 1e-6) qi * exp(-di * t) else qi / (1 + b * di * t)^(1 / b)
}

# Fit q(t) = qi / (1 + b di t)^(1/b) on log rates; b from a small grid (0 = exponential).
arps_fit <- function(q, t = seq_along(q) - 1) {
  ok <- is.finite(q) & q > 0
  if (sum(ok) < 6) return(NULL)
  q <- q[ok]; t <- t[ok]
  best <- NULL
  for (b in c(0, 0.3, 0.5, 0.8)) {
    f <- function(p) sum((log(q) - log(arps_q(exp(p[1]), exp(p[2]), b, t)))^2)
    o <- tryCatch(stats::optim(c(log(max(q[1], 1e-3)), log(0.02)), f), error = function(e) NULL)
    if (!is.null(o) && (is.null(best) || o$value < best$sse)) best <- list(qi = exp(o$par[1]), di = exp(o$par[2]), b = b, sse = o$value, t0 = 0)
  }
  if (!is.null(best) && best$di > 1) best$di <- 1
  best
}
arps_predict <- function(fit, t) if (is.null(fit)) rep(NA_real_, length(t)) else arps_q(fit$qi, fit$di, fit$b, t)

profile_key <- function(well, sand, interval) paste(well, data.table::fcoalesce(sand, "-"), data.table::fcoalesce(interval, "-"), sep = "|")

# One row per profile key with the numbers the ranking and the record need.
profile_summary <- function(pf, horizon = 360) {
  if (is.null(pf) || !nrow(pf)) return(NULL)
  p <- data.table::copy(pf)
  p[, pkey := profile_key(well, sand, interval_id)]
  per <- p[, {
    n12 <- month <= 12
    last <- .SD[which.max(month)]
    # extend beyond the delivered horizon with the delivered b and Di
    ext <- if (is.finite(last$qo) && is.finite(last$di) && last$di > 0 && max(month) < horizon) {
      tt <- seq_len(horizon - max(month)); sum(arps_q(last$qo, last$di, data.table::fcoalesce(last$b, 0), tt)) * days_per_month
    } else 0
    .(qo1 = qo[which.min(month)], qw1 = qw[which.min(month)], qf1 = qf[which.min(month)],
      np12 = sum(qo[n12], na.rm = TRUE) * days_per_month, wp12 = sum(qw[n12], na.rm = TRUE) * days_per_month,
      np_t = sum(qo, na.rm = TRUE) * days_per_month, np_ext = sum(qo, na.rm = TRUE) * days_per_month + ext,
      months = max(month), b = last$b, di = last$di)
  }, by = .(pkey, scenario)]
  w <- data.table::dcast(per, pkey ~ scenario, value.var = c("qo1", "qw1", "qf1", "np12", "wp12", "np_t", "np_ext", "months", "b", "di"))
  for (sc in c("Bajo", "Base", "Alto")) for (v in c("qo1", "qw1", "qf1", "np12", "wp12", "np_t", "np_ext", "months", "b", "di")) {
    nm <- paste(v, sc, sep = "_"); if (!nm %in% names(w)) w[, (nm) := NA_real_]
  }
  w[, unc := ifelse(is.finite(qo1_Base) & qo1_Base > 0 & is.finite(qo1_Alto) & is.finite(qo1_Bajo), (qo1_Alto - qo1_Bajo) / qo1_Base, NA_real_)]
  w[]
}

# A job's forecast: intervals add up, so Bajo / Base / Alto of the job are the sums of its
# opportunities' scenarios month by month (a shorter profile simply stops contributing).
combine_forecasts <- function(fcs) {
  fcs <- Filter(function(x) !is.null(x) && nrow(x), fcs)
  if (!length(fcs)) return(NULL)
  x <- data.table::rbindlist(lapply(fcs, function(f) f[, .(scenario, month = as.integer(month), qo, qw, qf)]))
  x[, .(qo = sum(qo, na.rm = TRUE), qw = sum(qw, na.rm = TRUE), qf = sum(qf, na.rm = TRUE)), by = .(scenario, month)][order(scenario, month)]
}

profile_rows <- function(pf, pkey) {
  if (is.null(pf) || !nrow(pf)) return(NULL)
  x <- pf[profile_key(well, sand, interval_id) == pkey]
  if (nrow(x)) x else NULL
}

# Monthly well rates (calendar-day) up to a date.
well_history <- function(res, w, to = max(res$months)) {
  x <- res$well[well == w & date <= to, .(date, bopd, bwpd, bwipd)]
  data.table::setorder(x, date)
  x
}

# Decline anomaly: the last `recent` months against the well's own decline over the
# `fit_months` before them. Returns NULL when there is not enough history.
decline_check <- function(h, recent = 6, fit_months = 18) {
  h <- h[bopd > 0 | seq_len(.N) > .N - recent]
  if (nrow(h) < recent + 8) return(NULL)
  n <- nrow(h)
  pre <- h[max(1, n - recent - fit_months + 1):(n - recent)]
  fit <- arps_fit(pre$bopd)
  if (is.null(fit)) return(NULL)
  tt <- nrow(pre) - 1 + seq_len(recent)
  exp_q <- mean(arps_predict(fit, tt)); act <- mean(utils::tail(h$bopd, recent))
  list(expected = exp_q, actual = act, ratio = act / max(exp_q, 1e-6), fit = fit, last_pre = pre$bopd[nrow(pre)])
}

# Post-job evaluation for a well opportunity: incremental oil = actual - pre-job decline,
# compared month by month with the forecast frozen at validation (Bajo / Base / Alto).
evaluate_well_job <- function(res, w, d0, frozen = NULL, asof = max(res$months), pre_months = 12) {
  h <- well_history(res, w, asof)
  pre <- h[date < d0][max(1, .N - pre_months + 1):.N]
  post <- h[date > d0]
  if (!nrow(post)) return(NULL)
  fit <- if (nrow(pre) >= 6) arps_fit(pre$bopd) else NULL
  tt <- nrow(pre) - 1 + seq_len(nrow(post))
  base <- if (is.null(fit)) rep(if (nrow(pre)) mean(utils::tail(pre$bopd, 3)) else 0, nrow(post)) else arps_predict(fit, tt)
  out <- data.table::data.table(month = seq_len(nrow(post)), date = post$date, actual_qo = post$bopd, baseline_qo = base,
                                inc_qo = post$bopd - base, actual_qw = post$bwpd)
  if (!is.null(frozen) && nrow(frozen)) {
    f <- data.table::dcast(frozen, month ~ scenario, value.var = "qo")
    for (sc in c("Bajo", "Base", "Alto")) if (!sc %in% names(f)) f[, (sc) := NA_real_]
    out <- merge(out, f[, .(month, fc_bajo = Bajo, fc_base = Base, fc_alto = Alto)], by = "month", all.x = TRUE)
  } else out[, `:=`(fc_bajo = NA_real_, fc_base = NA_real_, fc_alto = NA_real_)]
  out[]
}

job_verdict <- function(ev) {
  if (is.null(ev) || !nrow(ev)) return(list(verdict = "no data", ratio = NA_real_))
  x <- ev[is.finite(fc_base)]
  if (!nrow(x)) return(list(verdict = "no forecast", ratio = NA_real_))
  inc <- sum(x$inc_qo); b <- sum(x$fc_base)
  v <- if (all(is.finite(x$fc_alto)) && inc > sum(x$fc_alto)) "above Alto"
       else if (all(is.finite(x$fc_bajo)) && inc < sum(x$fc_bajo)) "below Bajo" else "within range"
  list(verdict = v, ratio = inc / max(b, 1e-6), months = nrow(x))
}
