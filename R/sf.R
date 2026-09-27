# Simmons & Falls (SPE-96469) waterflood fit ---------------------------------------------
#
#   Sec RF = A * (1 - exp(-C * DWI))
# A throughput-based fit: remaining waterflood oil depends on future injected
# volume, not on time, so it survives rate changes. Used for remaining WF oil,
# forecasts at any target TP and as the baseline to judge interventions.
# A is found in closed form for a given C (appendix A, eq. A-4); C by 1-D search.

sf_fit <- function(dwi, sec_rf, min_points = 6) {
  ok <- is.finite(dwi) & is.finite(sec_rf) & dwi > 0.02
  x <- dwi[ok]; y <- sec_rf[ok]
  if (length(x) < min_points || diff(range(x)) < 0.05) return(NULL)
  a_of <- function(C) { e <- 1 - exp(-C * x); sum(y * e) / sum(e^2) }
  sse <- function(C) { A <- a_of(C); sum((y - A * (1 - exp(-C * x)))^2) }
  o <- stats::optimize(sse, c(0.02, 20))
  C <- o$minimum; A <- a_of(C)
  sst <- sum((y - mean(y))^2)
  list(A = A, C = C, r2 = if (sst > 0) 1 - o$objective / sst else NA_real_, n = length(x), dwi_max = max(x))
}

sf_predict <- function(fit, dwi) fit$A * (1 - exp(-fit$C * dwi))

# Remaining waterflood recovery (fraction of HCPV) beyond the current DWI.
sf_remaining <- function(fit, dwi) fit$A * exp(-fit$C * dwi)

# Monthly forecast anchored at the current point, at a constant injection TP (%/yr).
sf_forecast <- function(fit, dwi0, sec0, tp_pct, hcpv, bo, start, months = 120) {
  t <- seq_len(months)
  d <- dwi0 + tp_pct / 100 * t / 12
  sec <- sec0 + (sf_predict(fit, d) - sf_predict(fit, dwi0))
  oil_rb <- diff(c(sec0, sec)) * hcpv
  data.table::data.table(date = seq(start, by = "month", length.out = months + 1)[-1], dwi = d, sec_rf = sec,
                         qo = oil_rb / bo / 30.44)
}

# Oil rate change (stb/d) for a change in injection TP at the current maturity.
sf_rate_gain <- function(fit, dwi, tp_from, tp_to, hcpv, bo) {
  fo <- fit$C * sf_remaining(fit, dwi)        # dSecRF / dDWI
  fo * (tp_to - tp_from) / 100 * hcpv / bo / 365
}
