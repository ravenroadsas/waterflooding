# Relative permeability, fractional flow and Buckley-Leverett / Welge --------

corey_kr <- function(sw, p) {
  s <- pmin(pmax((sw - p$swc) / (1 - p$swc - p$sor), 0), 1)
  list(krw = p$krw_or * s^p$nw, kro = p$kro_wc * (1 - s)^p$no)
}

frac_flow <- function(sw, p) {
  kr <- corey_kr(sw, p)
  mw <- kr$krw / p$mu_w
  mo <- kr$kro / p$mu_o
  ifelse(mw + mo > 0, mw / (mw + mo), 0)
}

mobility_ratio <- function(p) (p$krw_or / p$mu_w) / (p$kro_wc / p$mu_o)

# Displacement efficiency ED (fraction of initial oil) as a function of HCPVI
# (hydrocarbon pore volumes of water injected), 1-D Welge construction starting
# at initial water saturation swi. Returned as a data.table (hcpvi, ed, fw).
welge_curve <- function(p, swi = p$swc, n = 600, max_hcpvi = 20) {
  swi <- max(swi, p$swc)
  sw_max <- 1 - p$sor
  if (sw_max <= swi + 1e-6) return(data.table::data.table(hcpvi = c(0, max_hcpvi), ed = 0, fw = 1))
  sw <- seq(swi, sw_max, length.out = n)
  fw <- frac_flow(sw, p)
  fwi <- fw[1]
  # Shock front: tangent from (swi, fwi)
  slope <- (fw[-1] - fwi) / (sw[-1] - swi)
  k <- which.max(slope) + 1
  m <- slope[k - 1]
  pvi_bt <- 1 / m
  # Post-breakthrough: outlet saturation sw2 >= swf
  dfw <- c(diff(fw) / diff(sw), NA)
  idx <- seq(k, n - 1)
  idx <- idx[is.finite(dfw[idx]) & dfw[idx] > 1e-9]
  pvi2 <- 1 / dfw[idx]
  swavg2 <- sw[idx] + (1 - fw[idx]) * pvi2
  keep <- c(TRUE, diff(pvi2) > 0)  # enforce monotonic PVI
  pvi2 <- pvi2[keep]; swavg2 <- swavg2[keep]; fw2 <- fw[idx][keep]
  pvi_pre <- seq(0, pvi_bt, length.out = 60)
  swavg_pre <- swi + pvi_pre * (1 - fwi)
  pvi <- c(pvi_pre, pvi2)
  swavg <- c(swavg_pre, swavg2)
  fwo <- c(rep(fwi, length(pvi_pre)), fw2)
  ed <- (swavg - swi) / (1 - swi)
  out <- data.table::data.table(hcpvi = pvi / (1 - swi), ed = pmin(ed, (1 - p$sor - swi) / (1 - swi)), fw = fwo)
  out <- out[hcpvi <= max_hcpvi]
  out <- rbind(out, data.table::data.table(hcpvi = max_hcpvi, ed = max(out$ed), fw = max(out$fw)))
  unique(out, by = "hcpvi")
}

.ed_cache <- new.env()

# Memoised ED(hcpvi) interpolator for a parameter set.
ed_fun <- function(p, swi) {
  key <- paste(signif(c(p$swc, p$sor, p$krw_or, p$kro_wc, p$nw, p$no, p$mu_o, p$mu_w, swi), 5), collapse = "|")
  f <- .ed_cache[[key]]
  if (is.null(f)) {
    cv <- welge_curve(p, swi)
    f <- stats::approxfun(cv$hcpvi, cv$ed, rule = 2, ties = "ordered")
    assign(key, f, envir = .ed_cache)
  }
  f
}

# Relative-permeability / viscosity parameter list for one reservoir (v2 Fluids row).
reservoir_params <- function(fluids, reservoir) {
  d <- fluid_defaults
  row <- if (!is.null(fluids)) fluids[fluids$reservoir == reservoir] else NULL
  if (is.null(row) || !nrow(row)) row <- if (!is.null(fluids) && nrow(fluids)) fluids[1] else NULL
  g <- function(nm) { v <- if (!is.null(row)) row[[nm]][1] else NA; if (is.null(v) || is.na(v)) d[[nm]] %||% NA_real_ else v }
  list(bo = if (!is.null(row)) row$bo[1] else 1.2, bw = g("bw"), mu_o = g("visco"), mu_w = g("viscw"),
       swc = g("swc"), sor = g("sor"), krw_or = g("krw"), kro_wc = g("kro"), nw = g("nw"), no = g("no"))
}

# Displacement efficiency implied by a producing water cut (reservoir conditions):
# the Welge average saturation behind the front for the outlet saturation whose
# fractional flow equals fw. Used for Evol(FF) in the conformance method 2.
ed_from_fw <- function(p, swi, fw) {
  cv <- welge_curve(p, swi)
  post <- cv[fw > min(cv$fw) + 1e-6]
  if (nrow(post) < 2) return(rep(NA_real_, length(fw)))
  stats::approx(post$fw, post$ed, xout = pmin(pmax(fw, min(post$fw)), max(post$fw)), ties = "ordered")$y
}

`%||%` <- function(a, b) if (is.null(a)) b else a
