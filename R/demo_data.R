# Synthetic demo field in the v2 table layout ---------------------------------------------
#
# 4 x 4 inverted five-spots (16 injectors, 25 shared producers), two areas, five
# units in two reservoirs, primary from 2000 and a phased waterflood. Each
# pattern x unit is flooded Buckley-Leverett style with its own sweep, fed by the
# injector's vertical profile, so the tables carry realistic signals:
#   P02  thief zone in unit A (59 % of injection, poor sweep)      -> conformance (E)
#   P13  injector loses injectivity in late 2025, valve wide open  -> stimulation (B)
#   P04  efficient, young, injected below target                   -> raise rate (F+)
#   P07  over-balanced, producers with high fluid levels           -> extraction (C)
#   P12  most mature, high throughput and utilization              -> cut rate (F-) / isolation (A)
#   P09  ADPERF in unit D (Jun 2025) with little unit-D injection  -> support (D)
#   P16  still on primary
# plus a primary satellite field (SAT-01..06, no patterns; SAT-02 below its decline, SAT-05 shut in),
# a producer analysis (Intervalos + Perfiles_Mensuales; SAT-01 has five intervals with potential;
# PRD-07 B INT001 = the example of the source
# definition: 150 / 1,350 / 1,500) and an injector review delivering Findings.

make_demo_data <- function(seed = 7, start = as.Date("2000-01-01"), end = as.Date("2026-08-01")) {
  set.seed(seed)
  months <- seq(start, end, by = "month"); nm <- length(months); days <- days_in_month(months)
  sp <- 1000
  units <- data.table::data.table(
    sand = c("A", "B", "C", "D", "E"), reservoir = c("RES-1", "RES-1", "RES-1", "RES-2", "RES-2"),
    hshare = c(0.27, 0.21, 0.28, 0.14, 0.10), k = c(900, 450, 600, 250, 150), h = c(30, 22, 28, 15, 12),
    phi = c(0.25, 0.23, 0.24, 0.21, 0.20), sw = c(0.30, 0.32, 0.30, 0.35, 0.36))
  fluids <- data.table::data.table(reservoir = c("RES-1", "RES-2"), api = c(22, 24), rs = c(150, 170), bo = c(1.12, 1.14),
                                   bw = c(1.02, 1.02), visco = c(12, 8), viscw = c(0.5, 0.5), swc = c(0.25, 0.28),
                                   sor = c(0.30, 0.30), krw = c(0.35, 0.30), kro = c(0.85, 0.80), nw = c(2.2, 2.4), no = c(2.0, 2.2))

  pats <- data.table::CJ(i = 0:3, j = 0:3)
  pats[, pattern := sprintf("P%02d", .I)][, injector := sprintf("INJ-%02d", .I)]
  pats[, area := ifelse(j >= 2, "North", "South")]
  pats[, hcpv := c(7.9, 8.2, 8.6, 6.1, 9.4, 8.8, 7.2, 6.8, 7.7, 8.1, 8.9, 9.8, 7.0, 6.5, 7.3, 7.6)[.I] * 1e6]
  wf <- c(P05 = "2007-01", P06 = "2007-01", P12 = "2007-06",
          P01 = "2011-01", P02 = "2011-01", P03 = "2011-06", P07 = "2011-06", P10 = "2011-01", P11 = "2011-06",
          P08 = "2016-01", P09 = "2016-01", P13 = "2016-06", P14 = "2016-06", P15 = "2016-01", P04 = "2021-06")
  pats[, wf_start := as.Date(paste0(wf[pattern], "-01"))]
  pats[, tp := c(P01 = 10.8, P02 = 11.9, P03 = 9.6, P04 = 6.2, P05 = 12.8, P06 = 13.9, P07 = 14.2, P08 = 11.0, P09 = 8.9,
                 P10 = 7.1, P11 = 12.1, P12 = 17.4, P13 = 10.8, P14 = 10.2, P15 = 11.4, P16 = NA)[pattern]]
  pats[, vrr := c(P07 = 1.6, P10 = 0.66)[pattern]][is.na(vrr), vrr := stats::runif(.N, 0.97, 1.05)]
  pats[, sweep := stats::runif(.N, 0.62, 0.82)]
  pats[pattern == "P12", sweep := 0.55]
  pats[pattern == "P02", sweep := 0.5]

  # vertical injection profile per injector (shares by unit)
  prof_base <- function(pat) {
    b <- units$hshare * units$k / sum(units$hshare * units$k)
    if (pat == "P02") b <- c(0.59, 0.154, 0.192, 0.038, 0.026)
    if (pat == "P06") b <- c(0.52, 0.16, 0.20, 0.07, 0.05)
    if (pat == "P12") b <- c(0.46, 0.18, 0.22, 0.08, 0.06)
    if (pat == "P09") b <- c(0.36, 0.22, 0.32, 0.05, 0.05)
    b / sum(b)
  }

  ps <- merge(pats[, .(k1 = 1, pattern, hcpv_p = hcpv, sweep, wf_start, tp, vrr)], units[, .(k1 = 1, sand, reservoir, hshare, k, h, phi, sw)],
              by = "k1", allow.cartesian = TRUE)[, k1 := NULL]
  ps <- merge(ps, fluids[, .(reservoir, bo, bw, visco, viscw, swc, sor, krw, kro, nw, no)], by = "reservoir")
  ps[, hcpv := round(hcpv_p * hshare * stats::runif(.N, 0.9, 1.1), -3)]
  ps[, stoiip := round(hcpv / bo, -3)]
  ps[, ev := pmin(sweep * c(A = 1.0, B = 0.95, C = 1.0, D = 0.85, E = 0.8)[sand], 0.95)]
  ps[pattern == "P02" & sand == "A", ev := 0.22]

  # pattern injection (rb) by month
  inj_series <- function(pat, wfs, tp, hcpv) {
    if (is.na(wfs)) return(rep(0, nm))
    on <- months >= wfs; k <- cumsum(on)
    rate <- tp / 100 * hcpv / 365 * pmin(k / 6, 1)
    if (pat == "P13") rate[months >= as.Date("2025-10-01")] <- rate[months >= as.Date("2025-10-01")] * 0.35
    if (pat == "P08") rate[months >= as.Date("2024-03-01") & months < as.Date("2025-02-01")] <- rate[months >= as.Date("2024-03-01") & months < as.Date("2025-02-01")] * 0.4
    if (pat == "P04") rate[months >= as.Date("2024-01-01")] <- rate[months >= as.Date("2024-01-01")] * 1.0
    ifelse(on, rate * days * exp(stats::rnorm(nm, 0, 0.05)), 0)
  }
  pinj <- pats[, .(date = months, winj_rb = inj_series(pattern, wf_start, tp, hcpv)), by = pattern]
  # profile shares drift slowly; unit D in P09 stays starved even after ADPERF
  sh <- pats[, {
    b <- prof_base(pattern)
    data.table::rbindlist(lapply(seq_along(units$sand), function(u) data.table::data.table(
      sand = units$sand[u], date = months, share = b[u] * exp(0.08 * sin(seq_len(nm) / 30 + u)))))
  }, by = pattern]
  sh[, share := share / sum(share), by = .(pattern, date)]
  sim <- merge(sh, pinj, by = c("pattern", "date"))
  sim <- merge(sim, ps, by = c("pattern", "sand"))
  data.table::setorder(sim, pattern, sand, date)
  sim[, winj_s := winj_rb * share]

  sim <- sim[, {
    p <- list(swc = swc[1], sor = sor[1], krw_or = krw[1], kro_wc = kro[1], nw = nw[1], no = no[1], mu_o = visco[1], mu_w = viscw[1])
    edf <- ed_fun(p, sw[1])
    t_yr <- (seq_len(nm) - 1) / 12
    prim <- stoiip[1] * 0.011 / 365 * exp(-0.12 * t_yr) * pmin(1, t_yr * 4 + 0.25)
    dwi <- cumsum(winj_s) / hcpv[1]
    fill <- 0.04
    evt <- ev[1] * (1 - exp(-pmax(dwi - fill, 0) / 0.15))
    np_wf <- stoiip[1] * evt * edf(pmax(dwi - fill, 0)) * sw[1] / sw[1]
    .(date = date, winj_s = winj_s, oil = prim * days + pmax(c(0, diff(np_wf)), 0), bo = bo[1], bw = bw[1])
  }, by = .(pattern, sand)]

  pat_tot <- sim[, .(oil = sum(oil), winj_rb = sum(winj_s), oil_rb = sum(oil * bo), bw = mean(bw)), by = .(pattern, date)]
  pat_tot <- merge(pat_tot, pats[, .(pattern, vrr, hcpv)], by = "pattern")
  data.table::setorder(pat_tot, pattern, date)
  pat_tot[, water := {
    inj6 <- data.table::frollmean(winj_rb, 6, align = "right"); inj6[is.na(inj6)] <- winj_rb[is.na(inj6)]
    dwi <- cumsum(winj_rb) / hcpv[1]
    bt <- 1 - exp(-(dwi / 0.10)^2)
    t_yr <- (seq_len(.N) - 1) / 12
    wprim <- oil * (0.05 + 0.10 * pmin(t_yr / 20, 1))
    pmax(wprim, pmax(inj6 / vrr[1] - oil_rb, 0) * bt / bw[1])
  }, by = pattern]
  pat_tot[pattern == "P02", water := water * 1.12]

  # producers: corners share each pattern's production equally
  prods <- data.table::CJ(i = 0:4, j = 0:4)
  prods[, well := sprintf("PRD-%02d", .I)][, `:=`(x = i * sp, y = j * sp)]
  prods[, start_m := month_start(start + sample(0:24, .N, replace = TRUE) * 31)]
  corners <- pats[, .(di = c(0, 1, 0, 1), dj = c(0, 0, 1, 1)), by = .(pattern, i, j)][, `:=`(pi = i + di, pj = j + dj)]
  corners <- merge(corners, prods[, .(pi = i, pj = j, well)], by = c("pi", "pj"))
  prd <- merge(corners[, .(pattern, well)], pat_tot[, .(pattern, date, oil, water)], by = "pattern", allow.cartesian = TRUE)
  prd <- prd[, .(oil = sum(oil) / 4, water = sum(water) / 4), by = .(well, date)]
  prd <- merge(prd, prods[, .(well, start_m)], by = "well")[date >= start_m]
  prd[, dd := days_in_month(date)]
  noise <- exp(stats::rnorm(nrow(prd), 0, 0.06)); up <- ifelse(stats::runif(nrow(prd)) < 0.02, 0.5, 1)
  prd[, `:=`(bopd = round(oil / dd * noise * up, 1), bwpd = round(water / dd * noise * up, 1), bwipd = 0)]

  inj <- merge(pats[, .(pattern, well = injector, wf_start)], pat_tot[, .(pattern, date, winj_rb, bw)], by = "pattern")
  inj <- inj[!is.na(wf_start) & date >= wf_start]
  inj[, `:=`(bopd = 0, bwpd = 0, bwipd = round(winj_rb / bw / days_in_month(date), 1))]
  wells <- rbind(prd[, .(Well = well, Date = date, BOPD = bopd, BWPD = bwpd, BWIPD = bwipd)],
                 inj[, .(Well = well, Date = date, BOPD = bopd, BWPD = bwpd, BWIPD = bwipd)])
  data.table::setorder(wells, Well, Date)

  # allocation: injectors 1.0; producers 1/n corners; PRD-13 re-allocated in 2020 (valid-from example)
  # coefficients calibrated to each pattern's long-term contribution to the producer
  contrib <- merge(corners[, .(pattern, well)], pat_tot[, .(liq = sum(oil + water)), by = pattern], by = "pattern")
  contrib[, Coeff := round(liq / sum(liq), 4), by = well]
  alloc_p <- contrib[, .(Well = well, Pattern = pattern, Date = as.Date(NA), Coeff)]
  alloc <- rbind(pats[, .(Well = injector, Pattern = pattern, Date = as.Date(NA), Coeff = 1)], alloc_p)
  p13 <- alloc[Well == "PRD-13"]
  if (nrow(p13) == 4) alloc <- rbind(alloc, p13[, .(Well, Pattern, Date = as.Date("2020-01-01"), Coeff = round(Coeff * c(1.1, 0.9, 1.1, 0.9) / sum(Coeff * c(1.1, 0.9, 1.1, 0.9)), 4))])

  vol <- ps[, .(Pattern = pattern, Reservoir = reservoir, Sand = sand, STOIIP = stoiip, HCPV = hcpv,
                H = round(h * stats::runif(.N, 0.85, 1.15), 1), Phi = round(phi * stats::runif(.N, 0.95, 1.05), 3), Sw = sw,
                K = round(k * exp(stats::rnorm(.N, 0, 0.25))))]

  # injection profiles every 6 months (shares x rate, measurement noise)
  pdates <- months[format(months, "%m") %in% c("01", "07")]
  isd <- merge(sh[date %in% pdates], inj[, .(pattern, well, date, bwipd)], by = c("pattern", "date"))
  isd <- isd[, .(Well = well, Sand = sand, Date = date, BWIPD = round(bwipd * share * exp(stats::rnorm(.N, 0, 0.04)), 1))]
  data.table::setorder(isd, Well, Date, Sand)

  # valves (VRF size, 1/64 in) and design rates (Cobb, bbl/d) per injector x unit, quarterly
  qdates <- months[format(months, "%m") %in% c("01", "04", "07", "10") & months >= as.Date("2012-01-01")]
  des <- merge(pats[!is.na(wf_start), .(pattern, well = injector, wf_start)], ps[, .(pattern, sand, hcpv, bw)], by = "pattern")
  des[, cobb := round(0.115 * hcpv / 365 / bw)]
  stt <- des[, .(Date = qdates[qdates >= wf_start]), by = .(pattern, Well = well, Sand = sand, cobb)]
  stt[, VRF := c(A = 8, B = 6, C = 8, D = 4, E = 4)[Sand]]
  stt[pattern == "P13", VRF := 12]
  stt[pattern == "P02" & Sand == "A", VRF := 10]
  stt[, `:=`(Cobb = cobb, pattern = NULL, cobb = NULL)]

  hierarchy <- rbind(
    pats[, .(Field = "Demo Field", Area = area, Pattern = pattern, Well = injector, Well_Type = "INJECTOR", X = (i + 0.5) * sp, Y = (j + 0.5) * sp)],
    merge(merge(corners[, .(pattern, well)], pats[, .(pattern, area)], by = "pattern"), prods[, .(well, x, y)], by = "well")[
      , .(Field = "Demo Field", Area = area, Pattern = pattern, Well = well, Well_Type = "PRODUCER", X = x, Y = y)])
  hierarchy[, `:=`(Orgunit = "DEMO-OU", Contract = "CT-ALPHA", Structure = paste(Area, "dome"), Substructure = ifelse(Area == "North", "N1", NA_character_))]
  data.table::setorder(hierarchy, Area, Pattern, Well_Type, Well)

  baseline <- pats[!is.na(wf_start), .(Pattern = pattern, WF_Start = wf_start)]

  interventions <- data.table::data.table(
    Well = c("INJ-08", "PRD-11", "INJ-04", "INJ-06"),
    Date = as.Date(c("2025-02-01", "2025-06-01", "2024-01-01", "2026-09-01")),
    Type = c("STIM", "ADPERF", "RATE", "ISOLATION"),
    Sand = c(NA, "D", NA, "A"),
    Status = c("EXECUTED", "EXECUTED", "EXECUTED", "PLANNED"),
    Notes = c("Acid job, scale in mandrels", "Additional perforations unit D", "Rate increase to 6 % HCPV/yr", "Candidate from 2026 review"))

  lvl_p <- corners[pattern == "P07", unique(well)]
  ws <- prods[, .(Date = months[months >= as.Date("2025-01-01") & format(months, "%m") %in% c("01", "04", "07", "10")]), by = .(Well = well)]
  ws[, `:=`(Status = "ACTIVE", Lift = sample(c("BEAM", "PCP", "ESP"), .N, TRUE, prob = c(.7, .15, .15)),
            DFL = round(ifelse(Well %in% lvl_p, stats::runif(.N, 1300, 1800), stats::runif(.N, 150, 600))))]

  # ---- primary satellite field (no patterns): hyperbolic declines, one well losing rate, one shut in ----
  sat <- data.table::data.table(well = sprintf("SAT-%02d", 1:6), start_m = as.Date(c("2012-01-01", "2013-06-01", "2015-01-01", "2016-03-01", "2018-01-01", "2019-07-01")),
                                qi = c(420, 380, 300, 520, 260, 340), di = c(0.035, 0.03, 0.04, 0.045, 0.03, 0.04), b = c(0.6, 0.5, 0.7, 0.5, 0.4, 0.6),
                                wc0 = c(0.15, 0.2, 0.1, 0.25, 0.1, 0.2), x = c(6200, 6900, 7500, 6500, 7200, 7900), y = c(800, 1300, 900, 2100, 2400, 1800))
  satp <- sat[, {
    m <- months[months >= start_m]; t <- seq_along(m) - 1
    q <- qi / (1 + b * di * t)^(1 / b) * exp(stats::rnorm(length(m), 0, 0.05))
    wc <- pmin(wc0 + 0.012 * t / 12 * 6, 0.92)
    if (well == "SAT-02") q[m >= as.Date("2026-02-01")] <- q[m >= as.Date("2026-02-01")] * 0.5   # damage: below its decline
    if (well == "SAT-05") q[m >= as.Date("2026-04-01")] <- 0                                   # shut in (rod failure)
    .(Date = m, BOPD = round(q, 1), BWPD = round(ifelse(q > 0, q * wc / (1 - wc), 0), 1), BWIPD = 0)
  }, by = .(Well = well)]
  wells <- rbind(wells, satp)
  data.table::setorder(wells, Well, Date)

  hierarchy <- rbind(hierarchy, sat[, .(Field = "Demo Satellite", Area = "Satellite", Pattern = NA_character_, Well = well, Well_Type = "PRODUCER",
                                         X = x, Y = y, Orgunit = "DEMO-OU", Contract = "CT-BETA", Structure = "Satellite high", Substructure = NA_character_)])
  data.table::setcolorder(hierarchy, c("Orgunit", "Contract", "Field", "Structure", "Substructure", "Area", "Pattern", "Well", "Well_Type", "X", "Y"))

  # ---- single-well analysis (INTERVALOS + PERFILES_MENSUALES) ----
  wa <- demo_well_analysis(wells, months)

  # ---- another analysis delivering findings: an injector review ----
  findings <- data.table::data.table(
    Source = "Injector review 2026", Well = c("INJ-09", "INJ-09", "INJ-13"), Unit = c("D", "D", NA), Interval_ID = NA_character_,
    Action = c("SUPPORT_INJ", "SUPPORT_INJ", "STIM_INJ"), Family = c("O", "U", "O"),
    Metric = c("Mandrel unit D", "Unit D kh share", "Fall-off skin"), Value = c(4, 0.08, 12), Reference = c(8, 0.15, 2),
    Units = c("1/64 in", "fraction", "-"), Comment = c("Valve undersized for the design rate", "Low share of injection for its kh", "Damage confirmed by fall-off test"),
    Gain_bopd = c(NA, NA, NA), Date = as.Date("2026-06-01"))

  list(Wells = wells, Alloc = alloc, Vol = vol, Fluids = fluids[, .(Reservoir = reservoir, API = api, Rs = rs, Bo = bo, Bw = bw,
       visco, viscw, Swc = swc, Sor = sor, Krw = krw, Kro = kro, Nw = nw, No = no)],
       InjSand = isd, InjSand_status = stt, Hierarchy = hierarchy, Baseline = baseline,
       Interventions = interventions, WellStatus = ws, Intervalos = wa$intervals, Perfiles_Mensuales = wa$profiles, Findings = findings)
}

# Producer analysis for the demo: intervals proposed from the logs (closed = never perforated),
# open intervals, and Bajo / Base / Alto monthly profiles with constant liquid per scenario.
demo_well_analysis <- function(wells, months) {
  spec <- data.table::data.table(
    ORGUNIT = "DEMO-OU",
    FIELD = c(rep("Demo Field", 9), rep("Demo Satellite", 9)),
    WELL = c("PRD-07", "PRD-07", "PRD-08", "PRD-12", "PRD-12", "PRD-13", "PRD-19", "PRD-11", "PRD-24",
             "SAT-01", "SAT-01", "SAT-03", "SAT-04", "SAT-06", "SAT-01", "SAT-01", "SAT-01", "SAT-01"),
    UNIT = c("B", "C", "D", "B", "A", "C", "B", "E", "A", "B", "C", "B", "A", "C", "A", "A", "C", "D"),
    intervalo_id = c("INT001", "INT002", "INT001", "INT001", "INT002", "INT001", "INT001", "INT003", "INT001", "INT001", "INT002", "INT001", "INT001", "INT001", "INT003", "INT004", "INT005", "INT006"),
    estado_apertura = c("cerrado", "abierto", "cerrado", "parcial", "abierto", "cerrado", "cerrado", "cerrado", "abierto", "cerrado", "abierto", "cerrado", "abierto", "cerrado", "cerrado", "cerrado", "parcial", "cerrado"),
    top_ft = c(5210, 5262, 5410, 5196, 5120, 5280, 5225, 5520, 5130, 4820, 4880, 4790, 4710, 4905, 4705, 4745, 4925, 4990),
    h_net_ft = c(26, 18, 14, 22, 30, 20, 12, 9, 28, 24, 16, 18, 26, 14, 18, 12, 20, 10),
    kabs_md = c(90, 60, 25, 70, 130, 55, 20, 12, 140, 120, 80, 40, 150, 35, 110, 45, 70, 20),
    phi = c(0.23, 0.22, 0.20, 0.23, 0.25, 0.22, 0.21, 0.19, 0.25, 0.24, 0.22, 0.21, 0.24, 0.2, 0.24, 0.21, 0.22, 0.19),
    sw_las = c(0.33, 0.32, 0.36, 0.34, 0.31, 0.33, 0.35, 0.38, 0.30, 0.30, 0.33, 0.36, 0.29, 0.37, 0.31, 0.36, 0.33, 0.39),
    area_ac = c(31, 31, 28, 35, 35, 30, 18, 26, 22, 48, 48, 40, 44, 52, 48, 48, 48, 48),
    np_ooip_ratio = c(0.08, 0.31, 0.05, 0.12, 0.42, 0.07, 0.22, 0.04, 0.45, 0.06, 0.24, 0.09, 0.28, 0.03, 0.05, 0.08, 0.11, 0.04),
    sw_actual = c(0.41, 0.66, 0.40, 0.47, 0.74, 0.43, 0.58, 0.42, 0.78, 0.38, 0.55, 0.44, 0.60, 0.39, 0.37, 0.44, 0.46, 0.43),
    qo_inicial_bopd = c(150, 40, 60, 95, 18, 120, 30, 25, 12, 210, 70, 85, 60, 110, 140, 55, 90, 30),
    qw_inicial_bwpd = c(1350, 900, 340, 760, 1180, 880, 970, 75, 1450, 290, 330, 215, 540, 140, 160, 145, 310, 60),
    qa_resultado = c("OK", "OK", "Corrected: Sw from offset well", "OK", "OK", "OK", "OK", "Corrected: kh capped at P90", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "Corrected: Sw from offset well"))
  spec[, base_ft := top_ft + round(h_net_ft * 1.15)]
  spec[, `:=`(kh_md_ft = kabs_md * h_net_ft, qf_inicial_bfpd = qo_inicial_bopd + qw_inicial_bwpd)]
  spec[, bsw_inicial_pct := round(100 * qw_inicial_bwpd / qf_inicial_bfpd, 1)]
  spec[, ooip_stb := round(7758 * area_ac * h_net_ft * phi * (1 - sw_las) / 1.12, -2)]
  spec[, rf := 0.18][, eur_stb := round(ooip_stb * rf * (1 - np_ooip_ratio / 0.5), -2)]
  npw <- wells[, .(np = sum(BOPD * days_in_month(Date))), by = .(WELL = Well)]
  spec <- merge(spec, npw, by = "WELL", all.x = TRUE)
  spec[, np_total_pozo_stb := round(data.table::fcoalesce(np, 0))][, np := NULL]
  intervals <- spec[, .(ORGUNIT, FIELD, WELL, UNIT, intervalo_id, top_ft, base_ft, estado_apertura, h_net_ft, kabs_md, phi, sw_las, kh_md_ft,
                        area_ac, ooip_stb, rf, eur_stb, np_total_pozo_stb, np_ooip_ratio, sw_actual, bsw_inicial_pct, qo_inicial_bopd,
                        qw_inicial_bwpd, qf_inicial_bfpd, qa_resultado)]
  # monthly profiles for the closed / partly open intervals (iXpantia-style hyperbolic decline, Q liquid constant)
  T <- 36
  cand <- spec[estado_apertura != "abierto"]
  sc <- data.table::data.table(escenario = c("Bajo", "Base", "Alto"), fq = c(2 / 3, 1, 4 / 3), fo = c(1 / 3, 1, 2), b = c(0.6, 0.5, 0.4), di = c(0.06, 0.045, 0.035))
  profiles <- cand[, {
    data.table::rbindlist(lapply(seq_len(nrow(sc)), function(k) {
      z <- sc[k]
      qf <- round(qf_inicial_bfpd * z$fq); qoi <- round(min(qo_inicial_bopd * z$fo, 0.9 * qf), 1); qwi <- qf - qoi
      t <- seq_len(T) - 1
      qo <- round(qoi / (1 + z$b * z$di * t)^(1 / z$b), 1)
      data.table::data.table(escenario = z$escenario, mes = seq_len(T), qoi_bopd = qoi, qwi_bwpd = qwi, qo_perfil_bopd = qo,
                             qw_perfil_bwpd = qf - qo, qf_perfil_bfpd = qf, b = z$b, di_por_mes = z$di)
    }))
  }, by = .(ORGUNIT, FIELD, WELL, UNIT, intervalo_id)]
  list(intervals = intervals, profiles = profiles)
}

# Prototype curves for the demo: the median of the well-behaved patterns, like
# an analog built from a mature area.
demo_prototypes <- function(res) {
  good <- setdiff(sprintf("P%02d", 1:15), c("P02", "P12", "P13"))
  ser <- aggregate_level(res, "pattern")
  a <- analog_prototype(ser, "Demo analog", "v1", good)
  a[, .(Prototype = prototype, Version = version, DWI = dwi, Sec_RF = sec_rf, DWP = dwp, Util = util, WOR = wor)]
}

write_demo_data <- function(dir = "data/demo") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(dir, full.names = TRUE))
  d <- make_demo_data()
  for (k in names(d)) data.table::fwrite(d[[k]], file.path(dir, paste0(k, ".csv")))
  # prototypes from a first engine pass, then derived tables for the reconciliation demo
  ds <- build_dataset(read_dataset_dir(dir), "demo")
  res <- run_engine(ds, default_settings)
  pr <- demo_prototypes(res)
  data.table::fwrite(pr, file.path(dir, "Prototypes.csv"))
  data.table::fwrite(data.table::data.table(Pattern = res$props$pattern, Prototype = "Demo analog", Version = "v1"),
                     file.path(dir, "Prototype_Assign.csv"))
  ds <- build_dataset(read_dataset_dir(dir), "demo")
  res <- run_engine(ds, default_settings)
  s <- aggregate_level(res, "pattern")
  jit <- function(x, sd = 0.002) x * exp(stats::rnorm(length(x), 0, sd))
  data.table::fwrite(s[, .(Pattern = entity, Date = date, Np = round(jit(cum_oil)), Nw = round(jit(cum_water)),
                           Nwi = round(cum_winj), IWRCUM = round(iwr_cum, 3), OPR = round(opr, 3), WPR = round(wpr, 3))],
                     file.path(dir, "Patterns_Mat.csv"))
  data.table::fwrite(s[, .(Pattern = entity, Date = date, WOR = round(wor, 3), Util = round(util12, 2), TP = round(tp, 2),
                           IWR = round(iwr, 3))], file.path(dir, "Patterns_Vel.csv"))
  invisible(d)
}
