# Synthetic demo field -------------------------------------------------------
#
# 4 x 4 inverted five-spot patterns (16 injectors, 25 shared producers), two
# blocks, three sands, primary from 2008 and a phased waterflood. Oil is driven
# by Buckley-Leverett x a per-pattern sweep so the diagnostics have something
# realistic to find (an under-injected pattern, an over-injected one, a
# channeling one and a pattern still on primary).

make_demo_data <- function(seed = 42, start = as.Date("2008-01-01"), end = as.Date("2026-08-01")) {
  set.seed(seed)
  months <- seq(start, end, by = "month")
  nm <- length(months)
  days <- days_in_month(months)
  sp <- 1000  # well spacing (m)

  sands <- data.table::data.table(
    sand = c("A", "B", "C"),
    share = c(0.47, 0.35, 0.18), k = c(260, 200, 150), h = c(28, 22, 12), phi = c(0.24, 0.21, 0.18),
    swi = c(0.24, 0.27, 0.31), bo = c(1.18, 1.16, 1.14), mu_o = c(2.5, 4, 7), swc = c(0.22, 0.25, 0.28),
    sor = c(0.28, 0.30, 0.32), krw_or = c(0.32, 0.28, 0.22), kro_wc = c(0.85, 0.8, 0.75), nw = c(2.2, 2.5, 2.8), no = c(2.0, 2.2, 2.4)
  )
  fluids <- sands[, .(sand, bo, bw = 1.02, mu_o, mu_w = 0.45, swc, sor, krw_or, kro_wc, nw, no)]

  # pattern grid
  pats <- data.table::CJ(i = 0:3, j = 0:3)
  pats[, pattern := sprintf("P%02d", .I)]
  pats[, injector := sprintf("INJ-%02d", .I)]
  pats[, block := ifelse(j >= 2, "North", "South")]
  pats[, `:=`(x = (i + 0.5) * sp, y = (j + 0.5) * sp)]
  # waterflood phases
  phase <- c(P06 = "2012-01-01", P07 = "2012-01-01",
             P01 = "2014-06-01", P02 = "2014-06-01", P05 = "2014-06-01", P10 = "2014-06-01", P11 = "2014-06-01", P03 = "2014-06-01",
             P09 = "2017-03-01", P13 = "2017-03-01", P14 = "2017-03-01", P15 = "2017-03-01", P08 = "2017-03-01",
             P04 = "2021-09-01", P12 = "2021-09-01")  # P16 still primary
  pats[, wf_start := as.Date(phase[pattern])]
  pats[, sweep := stats::runif(.N, 0.6, 0.85)]
  pats[, vrr_target := stats::runif(.N, 0.95, 1.1)]
  pats[pattern == "P10", vrr_target := 0.62]            # under-injected
  pats[pattern == "P07", vrr_target := 1.45]            # over-injected (out-of-pattern loss)
  pats[pattern == "P02", sweep := 0.28]                 # channeling
  pats[, size := stats::runif(.N, 0.75, 1.3)]

  prods <- data.table::CJ(i = 0:4, j = 0:4)
  prods[, well := sprintf("PRD-%02d", .I)]
  prods[, `:=`(x = i * sp, y = j * sp)]
  prods[, start := start + sample(0:30, .N, replace = TRUE) * 31]
  prods[, start := month_start(start)]

  # pattern x sand volumes and simulation
  ps <- merge(pats[, .(k1 = 1, pattern, size, sweep, vrr_target, wf_start)], sands[, .(k1 = 1, sand, share, k, h, phi, swi, bo, mu_o, swc, sor, krw_or, kro_wc, nw, no)],
              by = "k1", allow.cartesian = TRUE)[, k1 := NULL]
  ps[, stooip := round(6.0e6 * size * share * stats::runif(.N, 0.85, 1.15), -3)]
  ps[, hcpv := stooip * bo]
  # thief zone: sand A of channeling pattern sweeps poorly
  ps[, ev_max := pmin(sweep * ifelse(sand == "A", 1.05, ifelse(sand == "B", 0.95, 0.8)), 0.95)]

  sim <- ps[, {
    p <- list(swc = swc, sor = sor, krw_or = krw_or, kro_wc = kro_wc, nw = nw, no = no, mu_o = mu_o, mu_w = 0.45)
    edf <- ed_fun(p, swi)
    t_yr <- (seq_len(nm) - 1) / 12
    # primary: exponential decline to ~7 % RF
    qi <- stooip * 0.013 / 365  # stb/d
    prim <- qi * exp(-0.16 * t_yr) * pmin(1, t_yr * 4 + 0.25)
    started <- !is.na(wf_start) & months >= wf_start
    k_on <- cumsum(started)
    # injection: ramp over 6 months to a rate giving ~0.07 HCPV/yr
    target_rb <- hcpv * stats::runif(1, 0.055, 0.085) / 365
    ramp <- pmin(k_on / 6, 1)
    winj_rb <- ifelse(started, target_rb * ramp * days * exp(stats::rnorm(nm, 0, 0.06)), 0)
    hcpvi <- cumsum(winj_rb) / hcpv
    fill <- 0.04
    ev <- ev_max * (1 - exp(-pmax(hcpvi - fill, 0) / 0.12))
    np_wf <- stooip * ev * edf(pmax(hcpvi - fill, 0))
    oil_wf <- c(0, diff(np_wf))
    oil <- prim * days + pmax(oil_wf, 0)
    # water: withdrawals follow injection / VRR; primary water cut ~ 8-20 %
    wprim <- oil * (0.05 + 0.10 * pmin(t_yr / 15, 1))
    # gas fill-up and breakthrough delay: little flood water back before ~0.1 HCPVI
    bt <- 1 - exp(-(hcpvi / 0.12)^2)
    wr <- ifelse(started, pmax(winj_rb / vrr_target - oil * bo, 0) * bt / 1.02, 0)
    water <- pmax(wprim, wr)
    .(date = months, oil = oil, water = water, winj = winj_rb / 1.02)
  }, by = .(pattern, sand)]

  pat_tot <- sim[, .(oil = sum(oil), water = sum(water), winj = sum(winj)), by = .(pattern, date)]

  # producers: each pattern's production shared equally by its 4 corner producers
  corners <- pats[, .(di = c(0, 1, 0, 1), dj = c(0, 0, 1, 1)), by = .(pattern, i, j)]
  corners[, `:=`(pi = i + di, pj = j + dj)]
  corners <- merge(corners, prods[, .(pi = i, pj = j, well)], by = c("pi", "pj"))
  prd <- merge(corners[, .(pattern, well)], pat_tot, by = "pattern", allow.cartesian = TRUE)
  prd <- prd[, .(oil = sum(oil) / 4, water = sum(water) / 4), by = .(well, date)]
  prd <- merge(prd, prods[, .(well, start)], by = "well")
  prd <- prd[date >= start]
  prd[, days := days_in_month(date)]
  noise <- exp(stats::rnorm(nrow(prd), 0, 0.07))
  up <- ifelse(stats::runif(nrow(prd)) < 0.03, stats::runif(nrow(prd), 0, 0.4), 1)  # workover / downtime months
  prd[, `:=`(bopd = round(oil / days * noise * up, 1), bwpd = round(water / days * noise * up, 1), bwipd = 0)]

  inj <- merge(pats[, .(pattern, well = injector, wf_start)], pat_tot, by = "pattern")
  inj <- inj[!is.na(wf_start) & date >= wf_start]
  inj[, days := days_in_month(date)]
  up <- ifelse(stats::runif(nrow(inj)) < 0.03, 0.2, 1)
  inj[, `:=`(bopd = 0, bwpd = 0, bwipd = round(winj / days * up, 1))]
  # P13 injector down for the last 4 months
  inj[well == "INJ-13" & date > end - 120, bwipd := 0]

  production <- rbind(prd[, .(well, date, bopd, bwpd, bwipd)], inj[, .(well, date, bopd, bwpd, bwipd)])
  data.table::setorder(production, well, date)

  # hierarchy and allocation
  shares <- corners[, .N, by = well]
  alloc_p <- merge(corners[, .(pattern, well)], shares, by = "well")[, .(pattern, well, coefficient = round(1 / N, 4))]
  alloc_i <- pats[, .(pattern, well = injector, coefficient = 1)]
  allocation <- rbind(alloc_i, alloc_p)
  hierarchy <- rbind(
    merge(alloc_i[, .(pattern, well)], pats[, .(pattern, block, x, y)], by = "pattern")[, .(field = "Demo Field", block, pattern, well, well_type = "INJECTOR", x, y)],
    merge(merge(alloc_p[, .(pattern, well)], pats[, .(pattern, block)], by = "pattern"), prods[, .(well, x, y)], by = "well")[
      , .(field = "Demo Field", block, pattern, well, well_type = "PRODUCER", x, y)]
  )
  data.table::setorder(hierarchy, block, pattern, well_type, well)

  stooip <- ps[, .(pattern, sand, stooip, area = round(sp^2 / 4046.86 * 1.0, 1), net_pay = round(h * stats::runif(.N, 0.8, 1.2), 1),
                   porosity = round(phi * stats::runif(.N, 0.95, 1.05), 3), swi, permeability = round(k * exp(stats::rnorm(.N, 0, 0.3))), boi = bo)]

  # petrophysics per well / sand
  wells <- unique(hierarchy$well)
  petro <- data.table::CJ(well = wells, sand = sands$sand)
  petro <- merge(petro, sands[, .(sand, h, phi, swi, k)], by = "sand")
  petro[, `:=`(net_pay = round(h * stats::runif(.N, 0.7, 1.3), 1), porosity = round(phi * stats::runif(.N, 0.93, 1.07), 3),
               sw = round(swi * stats::runif(.N, 0.95, 1.05), 3), permeability = round(k * exp(stats::rnorm(.N, 0, 0.35))))]
  petro <- petro[, .(well, sand, net_pay, porosity, sw, permeability)]
  data.table::setorder(petro, well, sand)

  list(production = production, hierarchy = hierarchy, stooip = stooip, fluids = fluids,
       petrophysics = petro, allocation = allocation)
}

write_demo_data <- function(dir = "data/demo") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  d <- make_demo_data()
  for (k in names(d)) data.table::fwrite(d[[k]], file.path(dir, paste0(k, ".csv")))
  invisible(d)
}
