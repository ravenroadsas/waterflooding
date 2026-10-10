# Configuration: operating targets and screening limits ------------------------------------
# Values marked LCI are the La Cira-Infantas references from the methodology;
# they are defaults for this asset, not universal limits.

default_settings <- list(
  util_window = 6,          # months (3, 6 or 12)
  judge_dwi = 0.15,         # OPR / WPR not judged below this DWI (SPE-96469)
  baseline_method = "start",# "start" = Np at waterflood start, "decline" = + primary decline extrapolation
  prototype_override = "",  # "" = use assignments
  target_tp = 11.5,         # %HCPV/yr (LCI)
  iwr_low = 0.8,            # under-balanced below (LCI)
  iwr_high = 1.2,           # over-balanced above (LCI)
  tp_drop = 0.30,           # injectivity loss: TP down more than this in 12 months
  tp_off = 0.30,            # rate change: TP more than this off target
  unit_dwi_q = 0.75,        # unit DWI quantile flagged as swept (injection control)
  loss_high = 0.20,         # Loss = DWI - DTP above this: out of zone / area (fig. 25)
  evr_low = 0.80,           # Evol(MB)/Evol(FF) below this: thief zone (fig. 25)
  dfl_high = 1000,          # ft of fluid above pump considered high
  early_dwi = 0.10, mature_dwi = 0.50, mature_wc = 0.80, late_dwi = 1.50, late_wc = 0.95,
  # single-well analysis (any drive)
  int_npooip_max = 0.10,    # interval little depleted: Np / OOIP at or below
  int_sw_max = 0.55,        # interval with mobile oil: Sw actual at or below
  int_bsw_max = 95,         # new interval useful while initial BSW is below (%)
  wso_bsw = 97,             # open interval watered out: initial BSW at or above (%)
  decline_drop = 0.25,      # rate this far below the well's own decline -> stimulation / lift
  shutin_months = 3,        # months without production -> reactivation candidate
  min_rate = 5,             # bopd below which well rules are not raised
  w_evidence = 0.40, w_gain = 0.40, w_stake = 0.20,  # priority score weights
  w_unc = 0.10,             # penalty for the Bajo-Alto spread relative to Base
  lci = c("target_tp", "iwr_low", "iwr_high")
)

stage_levels <- c("Primary", "Early response", "Developing", "Mature", "Late life")
stage_colors <- c("Primary" = "#8b98a9", "Early response" = "#60a5fa", "Developing" = "#2dd4bf",
                  "Mature" = "#fbbf24", "Late life" = "#f87171")

classify_stage <- function(dwi, wc, st = default_settings) {
  dwi[is.na(dwi)] <- 0; wc[is.na(wc)] <- 0
  out <- ifelse(dwi <= 0, "Primary",
         ifelse(wc >= st$late_wc | dwi >= st$late_dwi, "Late life",
         ifelse(dwi >= st$mature_dwi | wc >= st$mature_wc, "Mature",
         ifelse(dwi >= st$early_dwi, "Developing", "Early response"))))
  factor(out, levels = stage_levels)
}

# Chan diagnostic series: WOR and its time derivative vs days on production.
chan_series <- function(s) {
  s <- s[oil + water > 0]
  if (!nrow(s)) return(data.table::data.table(date = as.Date(character()), t = numeric(), wor = numeric(), dwor = numeric()))
  t <- cumsum(s$days)
  sm <- stats::filter(s$wor, rep(1 / 3, 3), sides = 2)
  data.table::data.table(date = s$date, t = t, wor = s$wor, dwor = c(NA, diff(as.numeric(sm)) / diff(t)))
}
