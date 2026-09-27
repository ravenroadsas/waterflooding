# Opportunities: evidence-based candidates (methodology section 8) ------------------------
#
# Each rule has one screening signal and gathers evidence in five families:
#   M maturity   V velocity   U unit / vertical   S spatial   O operations
# One family = screening_only. Two or more, including maturity or velocity,
# = candidate. Engineers move candidates on (validated, executed, evaluated)
# and those decisions are kept in the store, keyed by type + pattern + unit.

opp_types <- c(A = "Injection control / isolation", B = "Injector stimulation", C = "More extraction / lift",
               D = "Support for new perforations", E = "Conformance / channeling", F = "Rate change")
opp_colors <- c(A = "#fbbf24", B = "#60a5fa", C = "#a78bfa", D = "#e2e8f0", E = "#f87171", F = "#2dd4bf")
status_levels <- c("screening_only", "candidate", "validated_candidate", "executed", "outcome_evaluated", "dismissed")
status_colors <- c(screening_only = "#fbbf24", candidate = "#22d3ee", validated_candidate = "#34d399",
                   executed = "#a78bfa", outcome_evaluated = "#e2e8f0", dismissed = "#64748b")
families <- c(M = "maturity", V = "velocity", U = "unit / vertical", S = "spatial", O = "operations")

# ---- pattern snapshot with everything the rules need ----------------------------------
pattern_snapshot <- function(res, series, asof, st = res$settings) {
  sn <- snapshot_at(series, asof)
  sn <- merge(sn, res$pat_map[, .(entity = pattern, area)], by = "entity", all.x = TRUE, suffixes = c("", ".m"))
  if ("area.m" %in% names(sn)) { sn[, area := data.table::fcoalesce(area, area.m)]; sn[, area.m := NULL] }
  sn[, stage := classify_stage(dwi, wc6, st)]
  sn[, flooded := dwi > 0]
  # waterflood fit per pattern
  fits <- lapply(sn$entity, function(e) {
    s <- series[entity == e & date <= asof & flooding %in% TRUE]
    f <- sf_fit(s$dwi, s$sec_rf)
    if (is.null(f)) return(data.table::data.table(entity = e, sf_a = NA_real_, sf_c = NA_real_, sf_r2 = NA_real_))
    data.table::data.table(entity = e, sf_a = f$A, sf_c = f$C, sf_r2 = f$r2)
  })
  sn <- merge(sn, data.table::rbindlist(fits), by = "entity")
  sn[, sf_remaining := ifelse(is.finite(sf_a), sf_a * exp(-sf_c * dwi), NA_real_)]
  sn[, remaining_stb := sf_remaining * hcpv / (cum_oil_rb / pmax(cum_oil, 1))]
  # vertical efficiency and top unit
  if (!is.null(res$units)) {
    pu <- res$units$pattern_sand[date == max(date[date <= asof])]
    ve <- vertical_efficiency(pu[hcpv > 0])
    sn <- merge(sn, ve, by.x = "entity", by.y = "pattern", all.x = TRUE)
  }
  # volumetric efficiency ratio (method 2): Sec RF vs displacement implied by water cut
  sp <- res$sand_props
  sn[, evr := {
    x <- sp[pattern == entity]
    if (!nrow(x) || !isTRUE(flooded) || !is.finite(wc6)) NA_real_ else {
      w <- x$hcpv / sum(x$hcpv)
      p <- list(swc = sum(w * x$swc), sor = sum(w * x$sor), krw_or = sum(w * x$krw), kro_wc = sum(w * x$kro),
                nw = sum(w * x$nw), no = sum(w * x$no), mu_o = sum(w * x$mu_o), mu_w = sum(w * x$mu_w))
      fw_res <- wc6 * sum(w * x$bw) / (wc6 * sum(w * x$bw) + (1 - wc6) * sum(w * x$bo))
      ed <- ed_from_fw(p, sum(w * x$sw), fw_res)
      if (is.finite(ed) && ed > 0) sec_rf / ed else NA_real_
    }
  }, by = entity]
  # producers: heterogeneity and fluid levels
  if (!is.null(res$hi)) {
    hi <- res$hi[date == max(date[date <= asof])]
    pw <- unique(res$alloc[coeff > 0 & date == max(date[date <= asof]), .(entity = pattern, well)])
    ph <- merge(pw, hi[, .(well, hi_oil, hi_water)], by = "well")
    worst <- ph[, .SD[which.max(hi_water - hi_oil)], by = entity][, .(entity, worst_well = well, worst_hi_oil = hi_oil, worst_hi_water = hi_water)]
    sn <- merge(sn, worst, by = "entity", all.x = TRUE)
  }
  ws <- res$ds$well_status
  if (!is.null(ws) && nrow(ws) && any(is.finite(ws$dfl))) {
    lastws <- ws[date <= asof][, .SD[.N], by = well]
    pw <- unique(res$alloc[coeff > 0, .(entity = pattern, well)])
    d <- merge(pw, lastws[, .(well, dfl)], by = "well")[, .(dfl_mean = mean(dfl, na.rm = TRUE)), by = entity]
    sn <- merge(sn, d, by = "entity", all.x = TRUE)
  } else sn[, dfl_mean := NA_real_]
  # area references
  ar <- sn[flooded == TRUE, .(med_dwi = stats::median(dwi), med_util = stats::median(util, na.rm = TRUE),
                              q75_util = stats::quantile(util, 0.75, na.rm = TRUE), med_opr = stats::median(opr, na.rm = TRUE)), by = area]
  sn <- merge(sn, ar, by = "area", all.x = TRUE)
  sn[]
}

# ---- rules -------------------------------------------------------------------------------
fmtn <- function(x, d = 2) ifelse(is.finite(x), formatC(x, format = "f", digits = d), "n/a")
fmtp <- function(x, d = 1) ifelse(is.finite(x), paste0(formatC(x, format = "f", digits = d), " %"), "n/a")

new_opp <- function(type, pattern, sand = NA_character_, well = NA_character_, fam, evidence, text, gain = NA_real_, stake = NA_real_) {
  list(key = paste(type, pattern, ifelse(is.na(sand), "-", sand), sep = "|"), type = type, pattern = pattern, sand = sand, well = well,
       fam = fam, evidence = evidence, text = text, gain = gain, stake = stake)
}

ev_row <- function(metric, value, reference, unit, comment) data.table::data.table(metric = metric, value = value, reference = reference, unit = unit, comment = comment)

dominant_injector <- function(res, pat, asof) {
  a <- res$alloc[pattern == pat & date == max(date[date <= asof]) & coeff > 0]
  a <- a[well %in% res$wells[well_type == "INJECTOR", well]]
  if (!nrow(a)) return(NA_character_)
  a$well[which.max(a$coeff)]
}

generate_opportunities <- function(res, series, asof, st = res$settings) {
  sn <- pattern_snapshot(res, series, asof, st)
  pu <- if (!is.null(res$units)) res$units$pattern_sand[date == max(date[date <= asof])] else NULL
  if (!is.null(pu)) {
    pu <- merge(pu, res$pat_map[, .(pattern, area)], by = "pattern")
    pu[, `:=`(q_dwi = stats::quantile(dwi[dwi > 0], st$unit_dwi_q, na.rm = TRUE), med_udwi = stats::median(dwi[dwi > 0], na.rm = TRUE)), by = area]
  }
  vrf_now <- if (!is.null(res$units)) res$units$well_sand[date == max(date[date <= asof])] else NULL
  T <- st$target_tp
  out <- list()
  add <- function(o) out[[length(out) + 1]] <<- o

  for (i in seq_len(nrow(sn))) {
    p <- sn[i]
    if (!isTRUE(p$flooded)) next
    fit <- if (is.finite(p$sf_a)) list(A = p$sf_a, C = p$sf_c) else NULL
    bo <- p$cum_oil_rb / max(p$cum_oil, 1)
    inj <- dominant_injector(res, p$entity, asof)
    stake <- if (is.finite(p$remaining_stb)) p$remaining_stb else NA_real_
    common_m <- ev_row("DWI", p$dwi, p$med_dwi, "HCPV", sprintf("area median %s", fmtn(p$med_dwi)))

    # ---- E: conformance / channeling ----
    if (is.finite(p$opr) && p$opr < 1 && p$dwi >= p$med_dwi) {
      fam <- c(M = TRUE, V = FALSE, U = FALSE, S = FALSE, O = FALSE)
      ev <- list(common_m, ev_row("OPR", p$opr, 1, "-", sprintf("Sec RF %s vs %s expected", fmtp(100 * p$sec_rf), fmtp(100 * p$exp_sec_rf))))
      if (is.finite(p$wpr)) ev[[length(ev) + 1]] <- ev_row("WPR", p$wpr, 1, "-", "water vs prototype at the same DWI")
      if (is.finite(p$util) && (p$util >= p$q75_util || (is.finite(p$exp_util) && p$util > 1.3 * p$exp_util))) {
        fam["V"] <- TRUE; ev[[length(ev) + 1]] <- ev_row(sprintf("Utilization (%d m)", st$util_window), p$util, p$exp_util, "rb/rb", sprintf("area P75 %s", fmtn(p$q75_util, 1)))
      }
      if (is.finite(p$ve) && (p$ve < 0.75 || (is.finite(p$top_share) && p$top_share > 1.5 * p$top_hcpv_share))) {
        fam["U"] <- TRUE; ev[[length(ev) + 1]] <- ev_row(paste("Injection share, unit", p$top_sand), 100 * p$top_share, 100 * p$top_hcpv_share, "%", sprintf("unit DWI %s; vertical conformance %s", fmtn(p$top_dwi), fmtn(p$ve)))
      }
      if ((is.finite(p$worst_hi_water) && p$worst_hi_water > 0.3 && p$worst_hi_oil < 0) || (is.finite(p$evr) && p$evr < st$evr_low)) {
        fam["S"] <- TRUE
        if (is.finite(p$worst_hi_water)) ev[[length(ev) + 1]] <- ev_row(paste("HI water / oil,", p$worst_well), p$worst_hi_water, 0, "-", sprintf("HI oil %s", fmtn(p$worst_hi_oil)))
        if (is.finite(p$evr)) ev[[length(ev) + 1]] <- ev_row("Evol(MB)/Evol(FF)", p$evr, st$evr_low, "-", sprintf("Loss %s", fmtn(p$loss)))
      }
      gain <- if (is.finite(p$util) && is.finite(p$exp_util) && p$util > p$exp_util) min(p$qwi * 1.02 * (1 / p$exp_util - 1 / p$util) / bo, p$qo6) else NA_real_
      thief <- is.finite(p$loss) && p$loss < st$loss_high
      add(new_opp("E", p$entity, if (isTRUE(fam["U"])) p$top_sand else NA_character_, inj, fam, data.table::rbindlist(ev), list(
        maturity = sprintf("OPR %s at DWI %s (Sec RF %s vs %s expected); WPR %s.", fmtn(p$opr), fmtn(p$dwi), fmtp(100 * p$sec_rf), fmtp(100 * p$exp_sec_rf), fmtn(p$wpr)),
        velocity = sprintf("Utilization %s rb/rb (prototype %s, area P75 %s); Inj TP %s/yr; IWR %s.", fmtn(p$util, 1), fmtn(p$exp_util, 1), fmtn(p$q75_util, 1), fmtp(p$tp12), fmtn(p$iwr12)),
        vertical = if (is.finite(p$ve)) sprintf("Unit %s holds %s of injection for %s of HCPV (unit DWI %s); vertical conformance %s.", p$top_sand, fmtp(100 * p$top_share, 0), fmtp(100 * p$top_hcpv_share, 0), fmtn(p$top_dwi), fmtn(p$ve)) else "No injection profile.",
        spatial = sprintf("%s: HI water %s, HI oil %s. Loss %s, Evol ratio %s.", data.table::fcoalesce(p$worst_well, "n/a"), fmtn(p$worst_hi_water), fmtn(p$worst_hi_oil), fmtn(p$loss), fmtn(p$evr)),
        mechanism = if (thief) sprintf("Water recycling through a high-permeability path (thief zone%s) inside the pattern.", if (isTRUE(fam["U"])) paste(" in unit", p$top_sand) else "")
                    else "Injection leaving the pattern or the target interval (out of zone / area): Loss is high.",
        alternatives = c("Allocation coefficients overstate this pattern's water", "Injection out of zone behind casing", "Prototype not representative of this rock type"),
        gaps = c(if (!is.finite(p$ve)) "Injection profile", "Tracer or interference test between injector and the worst producer"),
        validation = c(sprintf("Run a new injection profile in %s", data.table::fcoalesce(inj, "the injector")), "Check allocation of the shared producers",
                       "Casing / cement integrity log", "Remaining reserves by unit"),
        action = sprintf("Restrict %s in %s (smaller VRF) and redirect water to under-swept units; evaluate water shut-off in %s.",
                         if (isTRUE(fam["U"])) paste("unit", p$top_sand) else "the high-intake interval", data.table::fcoalesce(inj, "the injector"), data.table::fcoalesce(p$worst_well, "the worst producer")),
        outcome = sprintf("Utilization toward %s and WOR down 20-30 %% within 3-6 months; OPR trend turns up.", fmtn(p$exp_util, 0)),
        window = "3-6 months"), gain, stake))
    }

    # ---- B: injector stimulation (loss of injectivity) ----
    if (is.finite(p$tp12) && ((is.finite(p$tp12_ago) && p$tp12 < (1 - st$tp_drop) * p$tp12_ago) || p$tp12 < 0.5 * T)) {
      fam <- c(M = FALSE, V = TRUE, U = FALSE, S = FALSE, O = FALSE)
      ev <- list(ev_row("Inj TP 12 m", p$tp12, p$tp12_ago, "%HCPV/yr", sprintf("12 months ago %s; target %s", fmtp(p$tp12_ago), fmtp(T))))
      if (is.finite(p$util) && p$util <= p$med_util) { fam["M"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("Utilization", p$util, p$med_util, "rb/rb", "efficient: area median or better") }
      if (p$dwi < p$med_dwi) { fam["M"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("DWI", p$dwi, p$med_dwi, "HCPV", "still immature for its area") }
      low_units <- character(); ops <- ""
      if (!is.null(pu)) {
        u <- pu[pattern == p$entity & dwi > 0]
        lu <- u[is.finite(tp12_ago) & tp12 < (1 - st$tp_drop) * tp12_ago]
        if (nrow(lu)) { fam["U"] <- TRUE; low_units <- lu$sand
          ev[[length(ev) + 1]] <- ev_row("Units losing injectivity", nrow(lu), nrow(u), "units", paste(lu$sand, collapse = ", ")) }
        if (!is.null(vrf_now) && !is.na(inj)) {
          v <- vrf_now[well == inj & is.finite(vrf) & is.finite(cobb)]
          open <- v[vrf >= stats::median(vrf_now$vrf, na.rm = TRUE) & winj_s / days < 0.6 * cobb]
          if (nrow(open)) { fam["O"] <- TRUE
            ops <- sprintf("VRF sizes %s (not restrictive) yet rate below 60 %% of Cobb in unit(s) %s.", paste(unique(open$vrf), collapse = "/"), paste(open$sand, collapse = ", "))
            ev[[length(ev) + 1]] <- ev_row(paste("Rate vs Cobb,", inj), sum(open$winj_s / open$days), sum(open$cobb), "bbl/d", "valve open, admission low: restriction beyond the valve") }
        }
      }
      tp_to <- if (is.finite(p$tp12_ago)) min(p$tp12_ago, T) else T
      gain <- if (!is.null(fit)) max(sf_rate_gain(fit, p$dwi, p$tp12, tp_to, p$hcpv, bo), 0) else NA_real_
      add(new_opp("B", p$entity, NA_character_, inj, fam, data.table::rbindlist(ev), list(
        maturity = sprintf("DWI %s (area median %s); OPR %s; utilization %s.", fmtn(p$dwi), fmtn(p$med_dwi), fmtn(p$opr), fmtn(p$util, 1)),
        velocity = sprintf("Inj TP %s/yr now vs %s 12 months ago (target %s).", fmtp(p$tp12), fmtp(p$tp12_ago), fmtp(T)),
        vertical = if (length(low_units)) paste("Units losing injectivity:", paste(low_units, collapse = ", ")) else "No unit-level drop detected.",
        spatial = "Check neighbouring injectors for the same trend (common cause: water quality).",
        ops = ops,
        mechanism = "Loss of injectivity: scale / precipitates, fines migration or mandrel restriction.",
        alternatives = c("Valve closed or undersized (check VRF)", "Low permeability in the target units", "High reservoir pressure (check IWR and fluid levels)"),
        gaps = c(if (!isTRUE(fam["O"])) "Current VRF / Cobb for the injector", "Water-quality and scale lab analysis"),
        validation = c("Mandrel and valve check (slickline)", "Laboratory analysis: scale, fines", "Step-rate or fall-off test", "Rule out low permeability in the unit"),
        action = sprintf("Stimulate %s (acid / scale treatment) in the affected units and restore rate to about %s/yr.", data.table::fcoalesce(inj, "the injector"), fmtp(tp_to)),
        outcome = sprintf("Inj TP back near %s/yr within 1-3 months; oil response in 3-12 months.", fmtp(tp_to)),
        window = "1-12 months"), gain, stake))
    }

    # ---- C: more extraction / lift ----
    if (is.finite(p$iwr12) && p$iwr12 > st$iwr_high) {
      fam <- c(M = FALSE, V = TRUE, U = FALSE, S = FALSE, O = FALSE)
      ev <- list(ev_row("IWR 12 m", p$iwr12, st$iwr_high, "-", "over-balanced"))
      if (is.finite(p$util) && p$util <= p$med_util) { fam["M"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("Utilization", p$util, p$med_util, "rb/rb", "efficient flood") }
      gaps <- character()
      if (is.finite(p$dfl_mean)) {
        if (p$dfl_mean >= st$dfl_high) { fam["O"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("Fluid level (mean)", p$dfl_mean, st$dfl_high, "ft", "producers are not pumped off") }
      } else gaps <- c(gaps, "Dynamic fluid levels")
      alts <- c("Allocation: injection credited to this pattern feeds neighbours", "Pressure build-up raising the risk of fracturing")
      if (is.finite(p$loss) && p$loss >= st$loss_high) alts <- c("Injection lost out of zone / area (Loss is high)", alts)
      else if (is.finite(p$loss)) { fam["S"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("Loss", p$loss, st$loss_high, "HCPV", "low: fluid stays in the pattern") }
      wd_now <- p$qwi * 1.02 / max(p$iwr12, 0.01)
      gain <- max((p$qwi * 1.02 - wd_now) * (1 - p$wc6) / bo, 0)
      add(new_opp("C", p$entity, NA_character_, NA_character_, fam, data.table::rbindlist(ev), list(
        maturity = sprintf("Utilization %s rb/rb (area median %s); OPR %s.", fmtn(p$util, 1), fmtn(p$med_util, 1), fmtn(p$opr)),
        velocity = sprintf("IWR %s over 12 months: Inj TP %s vs Prod TP %s /yr.", fmtn(p$iwr12), fmtp(p$tp12), fmtp(p$prod_tp12)),
        vertical = "-", spatial = sprintf("Loss %s.", fmtn(p$loss)),
        ops = if (is.finite(p$dfl_mean)) sprintf("Mean dynamic fluid level %s ft above pump.", fmtn(p$dfl_mean, 0)) else "No fluid-level data.",
        mechanism = "Producers limited by lift capacity: the pattern pressurizes.",
        alternatives = alts, gaps = c(gaps, "Pump capacity and efficiency"),
        validation = c("Fluid levels and dynamometer cards", "Pump / lift capacity review", "Check Loss and neighbour balance"),
        action = "Upsize lift (pump / speed) in the pattern producers with high fluid levels, keeping IWR near 1.",
        outcome = "Prod TP up, IWR toward 1, oil up within 1-3 months.", window = "1-3 months"), gain, stake))
    }

    # ---- F: rate change ----
    if (is.finite(p$tp12) && !(is.finite(p$tp12_ago) && p$tp12 < (1 - st$tp_drop) * p$tp12_ago)) {
      up <- p$tp12 < (1 - st$tp_off) * T; down <- p$tp12 > (1 + st$tp_off) * T
      if (up || down) {
        fam <- c(M = FALSE, V = TRUE, U = FALSE, S = FALSE, O = FALSE)
        ev <- list(ev_row("Inj TP 12 m", p$tp12, T, "%HCPV/yr", if (up) "below target" else "above target"))
        if (up && p$dwi < p$med_dwi && is.finite(p$util) && p$util <= p$med_util && (!is.finite(p$opr) || p$opr >= 0.9)) {
          fam["M"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("DWI / utilization", p$dwi, p$med_dwi, "HCPV", sprintf("immature and efficient (util %s)", fmtn(p$util, 1))) }
        if (down && p$dwi >= p$med_dwi && is.finite(p$util) && p$util >= p$q75_util) {
          fam["M"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("DWI / utilization", p$dwi, p$med_dwi, "HCPV", sprintf("mature and water cycling (util %s)", fmtn(p$util, 1))) }
        if (is.finite(p$iwr12) && ((up && p$iwr12 < st$iwr_low) || (down && p$iwr12 > st$iwr_high))) {
          fam["S"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("IWR 12 m", p$iwr12, 1, "-", if (up) "under-balanced" else "over-balanced") }
        gain <- if (!is.null(fit) && up) max(sf_rate_gain(fit, p$dwi, p$tp12, T, p$hcpv, bo), 0) else if (down) 0 else NA_real_
        gap_bwipd <- (T - p$tp12) / 100 * p$hcpv / 365 / 1.02
        add(new_opp("F", p$entity, NA_character_, inj, fam, data.table::rbindlist(ev), list(
          maturity = sprintf("DWI %s (area median %s), utilization %s, OPR %s.", fmtn(p$dwi), fmtn(p$med_dwi), fmtn(p$util, 1), fmtn(p$opr)),
          velocity = sprintf("Inj TP %s/yr vs target %s; IWR %s.", fmtp(p$tp12), fmtp(T), fmtn(p$iwr12)),
          vertical = "-", spatial = "-", ops = sprintf("Rate gap %+.0f bwipd to reach the target TP.", gap_bwipd),
          mechanism = if (up) "Flood running slower than its efficiency allows." else "Mature pattern recycling water at high throughput.",
          alternatives = c("Injector or facility constraint", "Target TP not suited to this rock type"),
          gaps = character(),
          validation = c("Injection pressure vs fracture limit", "Water availability / plant capacity", "Balance with neighbour patterns"),
          action = if (up) sprintf("Raise injection by about %.0f bwipd toward %s/yr.", gap_bwipd, fmtp(T)) else sprintf("Cut injection by about %.0f bwipd and redirect to immature patterns.", -gap_bwipd),
          outcome = if (up) "Oil up with utilization stable; IWR in band." else "Utilization down with oil flat.",
          window = "3-6 months"), gain, stake))
      }
    }
  }

  # ---- A: injection control per unit, D: ADPERF support ----
  if (!is.null(pu)) {
    snx <- sn[, .(entity, util, med_util, wc6, med_dwi, pat_dwi = dwi)]
    ua <- merge(pu[dwi > 0 & dwi >= q_dwi], snx, by.x = "pattern", by.y = "entity")
    ua <- ua[dwi > 1.2 * pat_dwi]   # swept relative to the rest of its own pattern
    for (i in seq_len(nrow(ua))) {
      u <- ua[i]
      fam <- c(M = FALSE, V = FALSE, U = TRUE, S = FALSE, O = FALSE)
      ev <- list(ev_row(paste("DWI unit", u$sand), u$dwi, u$q_dwi, "HCPV", "top quartile of the area's units"))
      if (is.finite(u$tp12) && u$tp12 > 1.2 * T) { fam["V"] <- TRUE; ev[[length(ev) + 1]] <- ev_row(paste("Inj TP unit", u$sand), u$tp12, T, "%HCPV/yr", "above target") }
      if (is.finite(u$cobb) && u$rate > 1.1 * u$cobb) { fam["O"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("Rate vs Cobb", u$rate, u$cobb, "bbl/d", "above the design rate") }
      if (is.finite(u$util) && (u$util >= u$med_util || u$wc6 >= 0.95)) { fam["M"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("Pattern utilization", u$util, u$med_util, "rb/rb", sprintf("WC %s", fmtp(100 * u$wc6, 0))) }
      inj <- dominant_injector(res, u$pattern, asof)
      add(new_opp("A", u$pattern, u$sand, inj, fam, data.table::rbindlist(ev), list(
        maturity = sprintf("Pattern utilization %s (area median %s), water cut %s.", fmtn(u$util, 1), fmtn(u$med_util, 1), fmtp(100 * u$wc6, 0)),
        velocity = sprintf("Unit %s TP %s/yr (target %s); rate %.0f bbl/d vs Cobb %s.", u$sand, fmtp(u$tp12), fmtp(T), u$rate, fmtn(u$cobb, 0)),
        vertical = sprintf("Unit %s DWI %s vs area P%d %s.", u$sand, fmtn(u$dwi), round(100 * st$unit_dwi_q), fmtn(u$q_dwi)),
        spatial = "Confirm with producer water cuts around the injector and the KH map of the unit.",
        mechanism = sprintf("Unit %s is swept; continued high injection mostly recycles water.", u$sand),
        alternatives = c("HCPV of the unit underestimated", "Old injection profile no longer representative"),
        gaps = c("Remaining reserves by unit"),
        validation = c("Recent injection profile", "Remaining reserves in the unit", "Check the water is redirected, not just cut"),
        action = sprintf("Reduce unit %s in %s (smaller VRF / isolation) and redirect to under-swept units.", u$sand, data.table::fcoalesce(inj, "the injector")),
        outcome = "Utilization and WOR fall within 3-6 months, oil flat or up.", window = "3-6 months"),
        NA_real_, NA_real_))
    }
    iv <- res$ds$interventions
    if (!is.null(iv) && nrow(iv)) {
      ad <- iv[type == "ADPERF" & !is.na(sand) & date <= asof & date >= asof - 3 * 365]
      for (i in seq_len(nrow(ad))) {
        j <- ad[i]
        pats <- unique(res$alloc[well == j$well & coeff > 0, pattern])
        for (pt in pats) {
          u <- pu[pattern == pt & sand == j$sand]
          if (!nrow(u)) next
          fam <- c(M = FALSE, V = FALSE, U = FALSE, S = FALSE, O = TRUE)
          ev <- list(ev_row("ADPERF", 1, NA, "job", sprintf("%s unit %s, %s", j$well, j$sand, format(j$date, "%b %Y"))))
          if (is.finite(u$tp12) && u$tp12 < 0.5 * T) { fam["V"] <- TRUE; ev[[length(ev) + 1]] <- ev_row(paste("Inj TP unit", j$sand), u$tp12, T, "%HCPV/yr", "little injection support") }
          if (u$dwi < u$med_udwi) { fam["U"] <- TRUE; ev[[length(ev) + 1]] <- ev_row(paste("DWI unit", j$sand), u$dwi, u$med_udwi, "HCPV", "immature unit") }
          if (!any(fam[c("V", "U")])) next
          inj <- dominant_injector(res, pt, asof)
          add(new_opp("D", pt, j$sand, inj, fam, data.table::rbindlist(ev), list(
            maturity = sprintf("Unit %s DWI %s (area median %s).", j$sand, fmtn(u$dwi), fmtn(u$med_udwi)),
            velocity = sprintf("Unit %s TP %s/yr vs target %s.", j$sand, fmtp(u$tp12), fmtp(T)),
            vertical = sprintf("New perforations in %s unit %s since %s.", j$well, j$sand, format(j$date, "%b %Y")),
            spatial = "-", ops = sprintf("%s: %s.", j$type, data.table::fcoalesce(j$notes, "")),
            mechanism = "Producer opened to a unit that receives little injection: response limited by support.",
            alternatives = c("Unit not connected between injector and producer", "Profile understates injection into the unit"),
            gaps = c("Injector completion status in the unit"),
            validation = c(sprintf("Injector %s completed in unit %s", data.table::fcoalesce(inj, "?"), j$sand), "Mandrel / VRF available for the unit"),
            action = sprintf("Open or enlarge the unit %s mandrel in %s (or perforate) and raise unit injection toward the design rate.", j$sand, data.table::fcoalesce(inj, "the injector")),
            outcome = sprintf("Unit %s TP up in 1-3 months; producer response in 3-9 months.", j$sand), window = "3-9 months"),
            NA_real_, NA_real_))
        }
      }
    }
  }
  finalize_opportunities(out, st)
}

finalize_opportunities <- function(out, st) {
  if (!length(out)) return(list(summary = data.table::data.table(), records = list()))
  keys <- vapply(out, `[[`, "", "key")
  out <- out[!duplicated(keys)]
  fam_n <- vapply(out, function(o) sum(o$fam), 0)
  mv <- vapply(out, function(o) any(o$fam[c("M", "V")]), TRUE)
  gains <- vapply(out, function(o) if (is.finite(o$gain)) o$gain else 0, 0)
  stakes <- vapply(out, function(o) if (is.finite(o$stake)) o$stake else 0, 0)
  gmax <- max(gains, 1e-9); smax <- max(stakes, 1e-9)
  score <- round(100 * (st$w_evidence * fam_n / 5 + st$w_gain * gains / gmax + st$w_stake * stakes / smax))
  summ <- data.table::data.table(
    okey = vapply(out, `[[`, "", "key"), type = vapply(out, `[[`, "", "type"),
    pattern = vapply(out, `[[`, "", "pattern"), sand = vapply(out, function(o) o$sand, ""),
    well = vapply(out, function(o) o$well, ""), families = vapply(out, function(o) paste(names(o$fam)[o$fam], collapse = ""), ""),
    n_fam = fam_n, auto_status = ifelse(fam_n >= 2 & mv, "candidate", "screening_only"),
    gain = vapply(out, function(o) o$gain, 0), stake = vapply(out, function(o) o$stake, 0), score = score)
  data.table::setnames(summ, "okey", "key")
  data.table::setorder(summ, -score)
  list(summary = summ, records = stats::setNames(out, vapply(out, `[[`, "", "key")))
}

# Combine engine status with the engineers' decisions from the store.
apply_states <- function(summ, states) {
  if (!nrow(summ)) return(summ)
  s <- data.table::copy(summ)
  s[, status := auto_status]
  if (!is.null(states) && nrow(states)) {
    s[states, on = "key", `:=`(status = i.status, notes = i.notes)]
    s[is.na(status), status := auto_status]
  }
  if (!"notes" %in% names(s)) s[, notes := NA_character_]
  s[, status := factor(status, levels = status_levels)]
  s[]
}

# ---- conformance ranking, method 1: points against the area average -------------------------
conformance_ranking <- function(sn) {
  f <- sn[flooded == TRUE]
  if (!nrow(f)) return(f)
  vars <- list(util = f$util, util_cum = f$util_cum, wor = log10(pmax(f$wor6, 1e-3)), wc = f$wc6, dwi = f$dwi,
               ve = if ("ve" %in% names(f)) 1 - f$ve else rep(NA_real_, nrow(f)))
  z <- data.table::as.data.table(lapply(vars, function(v) {
    zz <- ave(v, f$area, FUN = function(x) (x - mean(x, na.rm = TRUE)) / ifelse(stats::sd(x, na.rm = TRUE) > 0, stats::sd(x, na.rm = TRUE), 1))
    pmin(pmax(zz, -2), 3)
  }))
  data.table::setnames(z, paste0("pt_", names(z)))
  out <- cbind(f[, .(entity, area, util, util_cum, wor6, wc6, dwi, ve = if ("ve" %in% names(f)) ve else NA_real_, opr, loss, evr)], z)
  out[, score := rowSums(.SD, na.rm = TRUE), .SDcols = patterns("^pt_")]
  out[, method2 := ifelse(!is.finite(evr) | !is.finite(loss), NA_character_,
                   ifelse(loss < default_settings$loss_high,
                          ifelse(evr < default_settings$evr_low, "Reservoir conformance: thief zone", "Efficient"),
                          ifelse(evr < default_settings$evr_low, "Reservoir & well conformance", "Out of zone / area")))]
  data.table::setorder(out, -score)
  out[]
}

# ---- intervention outcome: before / after on the variables that justified the job -----------
evaluate_intervention <- function(res, series, iv_row, asof, window = 6) {
  pats <- unique(res$alloc[well == iv_row$well & coeff > 0, pattern])
  d0 <- iv_row$date
  rows <- lapply(pats, function(pt) {
    s <- series[entity == pt]
    pre <- s[date < d0 & date >= d0 - window * 31]
    post <- s[date > d0 & date <= min(asof, d0 + (window + 1) * 31)]
    if (!nrow(pre) || !nrow(post)) return(NULL)
    fit <- sf_fit(s[date < d0 & flooding %in% TRUE]$dwi, s[date < d0 & flooding %in% TRUE]$sec_rf)
    inc <- NA_real_
    if (!is.null(fit)) {
      last <- post[.N]; first <- pre[.N]
      pred <- first$sec_rf + (sf_predict(fit, last$dwi) - sf_predict(fit, first$dwi))
      inc <- (last$sec_rf - pred) * last$hcpv / (last$cum_oil_rb / max(last$cum_oil, 1))
    }
    data.table::data.table(pattern = pt, metric = c("Inj TP (%/yr)", "Oil rate (bopd)", "Utilization", "IWR", "WOR"),
      before = c(mean(pre$tp), mean(pre$qo), mean(pre$util, na.rm = TRUE), mean(pre$iwr, na.rm = TRUE), mean(pre$wor, na.rm = TRUE)),
      after = c(mean(post$tp), mean(post$qo), mean(post$util, na.rm = TRUE), mean(post$iwr, na.rm = TRUE), mean(post$wor, na.rm = TRUE)),
      incremental_oil_stb = inc, months_after = nrow(post))
  })
  data.table::rbindlist(rows)
}
