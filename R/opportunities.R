# Opportunities: well interventions supported by evidence (methodology section 8) ----------
#
# Every opportunity is a job on a well: key = action | well | unit | interval.
# Findings come from lenses and merge on that key:
#   PATTERN  rules A-F on waterflood patterns (dimensionless maturity / velocity), projected
#            onto the injector or producer that carries them
#   WELL     single-well analysis: new intervals (ADPERF), water shut-off, decline anomaly,
#            shut-in wells. Runs in any drive (primary, waterflood, other methods)
#   other    findings delivered by other analyses (table Findings), one lens per source
# Evidence families: M maturity, V velocity, U unit / vertical, S spatial, O operations.
# One family = screening_only; two or more including M or V = candidate. Engineers move
# candidates on (validated, executed, evaluated); decisions are kept in the store.

opp_types <- c(A = "Injection control / isolation", B = "Injector stimulation", C = "More extraction / lift",
               D = "Support for new perforations", E = "Conformance / channeling", F = "Rate change")
opp_colors <- c(A = "#fbbf24", B = "#60a5fa", C = "#a78bfa", D = "#e2e8f0", E = "#f87171", F = "#2dd4bf")
well_rules <- c(W1 = "New interval (additional perforations)", W2 = "Water shut-off in an open interval",
                W3 = "Rate below the well's own decline", W4 = "Shut-in well with remaining potential")

# Action catalog: what is done in the well. Unknown actions from other analyses are kept as given.
actions <- data.table::data.table(
  action = c("ADPERF", "WSO", "STIM_PROD", "LIFT", "REACTIVATE", "ISOLATE", "STIM_INJ", "CONFORMANCE", "RATE", "SUPPORT_INJ"),
  label = c("Additional perforations", "Water shut-off", "Producer stimulation", "Lift / extraction", "Reactivate well",
            "Isolate / restrict unit", "Injector stimulation", "Conformance / profile modification", "Injection rate change",
            "Injection support for new perforations"),
  job = c("ADPERF", "ISOLATION", "STIM", "LIFT", "REACTIVATION", "ISOLATION", "STIM", "CONFORMANCE", "RATE", "RATE"),
  color = c("#22d3ee", "#60a5fa", "#a78bfa", "#c084fc", "#94a3b8", "#fbbf24", "#3b82f6", "#f87171", "#2dd4bf", "#e2e8f0"))
action_label <- function(a) { l <- actions$label[match(a, actions$action)]; ifelse(is.na(l), a, l) }
action_color <- function(a) { l <- actions$color[match(a, actions$action)]; ifelse(is.na(l), "#e2e8f0", l) }
action_job <- function(a) { l <- actions$job[match(a, actions$action)]; ifelse(is.na(l), a, l) }
lens_colors <- c(PATTERN = "#a78bfa", WELL = "#22d3ee", OTHER = "#fbbf24")
lens_group <- function(l) ifelse(l %in% c("PATTERN", "WELL"), l, "OTHER")
pattern_rule_action <- c(A = "ISOLATE", B = "STIM_INJ", C = "LIFT", D = "SUPPORT_INJ", E = "CONFORMANCE", F = "RATE")

status_levels <- c("screening_only", "candidate", "validated_candidate", "executed", "outcome_evaluated", "dismissed")
status_colors <- c(screening_only = "#fbbf24", candidate = "#22d3ee", validated_candidate = "#34d399",
                   executed = "#a78bfa", outcome_evaluated = "#e2e8f0", dismissed = "#64748b")
families <- c(M = "maturity", V = "velocity", U = "unit / vertical", S = "spatial", O = "operations")
no_fam <- function() c(M = FALSE, V = FALSE, U = FALSE, S = FALSE, O = FALSE)

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

opp_key <- function(action, well, sand = NA_character_, interval = NA_character_)
  paste(action, well, ifelse(is.na(sand) | sand == "", "-", sand), ifelse(is.na(interval) | interval == "", "-", interval), sep = "|")

# A finding: one lens, one rule, one target well (NA = could not be projected onto a well).
new_finding <- function(lens, rule, action, well, sand = NA_character_, interval = NA_character_, fam, evidence, text,
                        gain = NA_real_, stake = NA_real_, pattern = NA_character_, gain_src = NA_character_, legacy_key = NA_character_) {
  evidence <- data.table::copy(evidence)
  if (nrow(evidence)) evidence[, lens := lens]
  list(key = if (is.na(well)) NA_character_ else opp_key(action, well, sand, interval), lens = lens, rule = rule, action = action,
       well = well, sand = sand, interval = interval, pattern = pattern, fam = fam, evidence = evidence, text = text,
       gain = gain, stake = stake, gain_src = gain_src, legacy_key = legacy_key)
}

# Pattern rules keep their v2 form; the target well is set when the finding is projected.
new_opp <- function(type, pattern, sand = NA_character_, well = NA_character_, fam, evidence, text, gain = NA_real_, stake = NA_real_) {
  new_finding("PATTERN", type, pattern_rule_action[[type]], well, sand, NA_character_, fam, evidence, text, gain, stake,
              pattern = pattern, gain_src = if (is.finite(gain)) "SF_FIT" else NA_character_,
              legacy_key = paste(type, pattern, ifelse(is.na(sand), "-", sand), sep = "|"))
}

ev_row <- function(metric, value, reference, unit, comment) data.table::data.table(metric = metric, value = value, reference = reference, unit = unit, comment = comment)

dominant_injector <- function(res, pat, asof) {
  a <- res$alloc[pattern == pat & date == max(date[date <= asof]) & coeff > 0]
  a <- a[well %in% res$wells[well_type == "INJECTOR", well]]
  if (!nrow(a)) return(NA_character_)
  a$well[which.max(a$coeff)]
}

# Producer carrying a pattern's extraction finding: the largest share of the pattern's
# withdrawals, preferring wells whose fluid level is high (not pumped off).
pick_producer <- function(res, pat, asof, dfl_high = Inf) {
  a <- res$alloc[pattern == pat & date == max(date[date <= asof]) & coeff > 0]
  a <- a[well %in% res$wells[well_type == "PRODUCER", well]]
  if (!nrow(a)) return(NA_character_)
  q <- res$well[date == max(date[date <= asof]), .(well, liq = bopd + bwpd)]
  a <- merge(a, q, by = "well", all.x = TRUE)
  a[, w := coeff * data.table::fcoalesce(liq, 0)]
  ws <- res$ds$well_status
  if (!is.null(ws) && nrow(ws) && any(is.finite(ws$dfl))) {
    d <- ws[date <= asof][, .SD[.N], by = well][, .(well, dfl)]
    a <- merge(a, d, by = "well", all.x = TRUE)
    hi <- a[is.finite(dfl) & dfl >= dfl_high]
    if (nrow(hi)) a <- hi
  }
  a$well[which.max(a$w)]
}

pattern_findings <- function(res, series, asof, st = res$settings) {
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
      add(new_opp("C", p$entity, NA_character_, pick_producer(res, p$entity, asof, st$dfl_high), fam, data.table::rbindlist(ev), list(
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
  out
}

# ---- drive context: a well (and unit) is flooded when it is allocated to a pattern under injection ----
well_patterns <- function(res, asof) {
  a <- res$alloc[date == max(date[date <= asof]) & coeff > 0, .(well, pattern, coeff)]
  a <- merge(a, res$base[, .(pattern, wf_start, mechanism)], by = "pattern", all.x = TRUE)
  a[, flooded := !is.na(wf_start) & wf_start <= asof & mechanism != "primary"]
  data.table::setorder(a, well, -coeff)
  a[]
}

# Drive per (well, unit): mechanism of the flooded pattern with the largest coefficient that holds the
# unit; everything outside the waterflood patterns is primary.
drive_context <- function(targets, res, asof) {
  wp <- well_patterns(res, asof)
  ps <- unique(res$sand_props[, .(pattern, sand)])
  t <- unique(targets[, .(well, sand)])
  out <- t[, {
    x <- wp[well == .BY$well]
    xf <- x[flooded == TRUE]
    if (!is.na(.BY$sand) && .BY$sand != "-") xf <- xf[pattern %in% ps[sand == .BY$sand, pattern]]
    .(drive = if (nrow(xf)) xf$mechanism[1] else "primary",
      ctx_pattern = if (nrow(xf)) xf$pattern[1] else if (nrow(x)) x$pattern[1] else NA_character_,
      patterns = paste(x$pattern, collapse = ", "))
  }, by = .(well, sand)]
  out[]
}

# ---- WELL lens --------------------------------------------------------------------------------
qa_ok <- function(qa) is.na(qa) | !nzchar(qa) | grepl("^ok$", qa, ignore.case = TRUE)

well_findings <- function(res, asof, st = res$settings) {
  out <- list(); add <- function(o) out[[length(out) + 1]] <<- o
  iv <- res$ds$intervals; psum <- profile_summary(res$ds$profiles)
  wm <- res$well_master
  ivh <- res$ds$interventions
  if (!is.null(iv) && nrow(iv)) {
    iv <- merge(data.table::copy(iv), wm[, .(well, fld = field)], by = "well", all.x = TRUE)
    iv[, grp := data.table::fcoalesce(field, fld, "Field")]
    iv[, `:=`(kh_p50 = stats::median(kh_md_ft, na.rm = TRUE), area_p50 = stats::median(area_ac, na.rm = TRUE),
              kh_pct = ifelse(is.finite(kh_md_ft), round(100 * data.table::frank(kh_md_ft, na.last = "keep") / sum(is.finite(kh_md_ft))), NA_real_)),
       by = .(grp, sand)]
    for (i in seq_len(nrow(iv))) {
      r <- iv[i]
      pk <- profile_key(r$well, r$sand, r$interval_id)
      ps <- if (!is.null(psum)) psum[pkey == pk] else NULL
      qa_corr <- !qa_ok(r$qa)
      depth <- if (is.finite(r$top_ft) && is.finite(r$base_ft)) sprintf("%s-%s ft", fmt_int(r$top_ft), fmt_int(r$base_ft)) else "depth n/a"
      qa_ev <- if (qa_corr) list(ev_row("QA result", NA_real_, NA_real_, "-", sprintf("estimate corrected by the QA function: %s", r$qa))) else list()
      qa_chk <- if (qa_corr) sprintf("Review the QA-corrected estimate (qa_resultado: %s)", r$qa) else character()
      q_txt <- sprintf("qo %s bopd, qw %s bwpd, qf %s bfpd (BSW %s)", fmt_int(r$qo0), fmt_int(r$qw0), fmt_int(r$qf0), fmtp(r$bsw0_pct, 0))
      if (r$estado %in% c("cerrado", "parcial")) {
        # ---- W1: interval with potential, never or partly opened -> additional perforations ----
        fam <- no_fam(); ev <- list()
        m1 <- is.finite(r$np_ooip) && r$np_ooip <= st$int_npooip_max
        m2 <- is.finite(r$sw_act) && r$sw_act <= st$int_sw_max
        if (is.finite(r$np_ooip)) ev[[length(ev) + 1]] <- ev_row("Np / OOIP", r$np_ooip, st$int_npooip_max, "fraction", if (m1) "little depletion" else "already depleted")
        if (is.finite(r$sw_act)) ev[[length(ev) + 1]] <- ev_row("Sw actual", r$sw_act, st$int_sw_max, "fraction", sprintf("Sw LAS %s", fmtn(r$sw_las)))
        if (m1 || m2) fam["M"] <- TRUE
        u1 <- is.finite(r$kh_md_ft) && is.finite(r$kh_p50) && r$kh_md_ft >= r$kh_p50
        u2 <- !is.finite(r$bsw0_pct) || r$bsw0_pct < st$int_bsw_max
        if (is.finite(r$kh_md_ft)) ev[[length(ev) + 1]] <- ev_row("kh", r$kh_md_ft, r$kh_p50, "mD.ft", sprintf("P%s of unit %s in %s", fmt_int(r$kh_pct), r$sand, r$grp))
        if (is.finite(r$bsw0_pct)) ev[[length(ev) + 1]] <- ev_row("Initial BSW", r$bsw0_pct, st$int_bsw_max, "%", if (u2) "below the limit" else "high: little oil per barrel lifted")
        if (u1 && u2) fam["U"] <- TRUE
        if (is.finite(r$area_ac) && is.finite(r$area_p50)) {
          ev[[length(ev) + 1]] <- ev_row("Drainage area (Voronoi)", r$area_ac, r$area_p50, "acre", "unit median in the field")
          if (r$area_ac >= r$area_p50) fam["S"] <- TRUE
        }
        ev <- c(ev, qa_ev)
        gain <- if (!is.null(ps) && nrow(ps) && is.finite(ps$qo1_Base)) ps$qo1_Base else r$qo0
        stake <- if (is.finite(r$eur_stb)) r$eur_stb else if (is.finite(r$ooip_stb) && is.finite(r$rf)) r$ooip_stb * r$rf else NA_real_
        verb <- if (r$estado == "parcial") "Complete the perforations of" else "Perforate"
        o <- new_finding("WELL", "W1", "ADPERF", r$well, r$sand, r$interval_id, fam,
          if (length(ev)) data.table::rbindlist(ev) else data.table::data.table(), list(
          maturity = sprintf("Np/OOIP %s; Sw actual %s (Sw LAS %s); OOIP %s stb, RF %s, EUR %s stb; well Np %s stb.", fmtn(r$np_ooip), fmtn(r$sw_act),
                             fmtn(r$sw_las), fmt_num(r$ooip_stb), fmtn(r$rf), fmt_num(r$eur_stb), fmt_num(r$np_well_stb)),
          velocity = "Primary depletion: no injection support evaluated.",
          vertical = sprintf("Unit %s, %s, %s: h net %s ft, k %s mD, phi %s, kh %s mD.ft (P%s of the unit).", r$sand, r$interval_id, depth,
                             fmtn(r$h_net_ft, 0), fmt_int(r$kabs_md), fmtn(r$phi), fmt_int(r$kh_md_ft), fmt_int(r$kh_pct)),
          spatial = sprintf("Voronoi area %s acre (unit median %s).", fmtn(r$area_ac, 0), fmtn(r$area_p50, 0)),
          ops = sprintf("Interval %s. Initial %s.%s", r$estado, q_txt, if (qa_corr) sprintf(" QA: %s.", r$qa) else ""),
          mechanism = "Interval with mobile oil identified in the logs and not (or partly) open to production.",
          alternatives = c("Sw from the Np/OOIP scenario understates sweep by offset wells", "Interval connected to a water source (high BSW)",
                           "Commingled interval reduces the rate of the open intervals"),
          gaps = c(if (!is.finite(r$sw_act)) "Current saturation of the interval", if (is.null(ps) || !nrow(ps)) "Bajo / Base / Alto profiles"),
          validation = c(sprintf("Confirm %s is not already open (completion diagram)", r$interval_id), "Cement / casing integrity across the interval",
                         sprintf("Lift capacity for %s bfpd", fmt_int(r$qf0)), sprintf("Water handling for %s bwpd", fmt_int(r$qw0)), qa_chk),
          action = sprintf("%s unit %s %s (%s) in %s.", verb, r$sand, r$interval_id, depth, r$well),
          outcome = sprintf("Initial %s.%s", q_txt, if (!is.null(ps) && nrow(ps)) sprintf(" Bajo / Base / Alto oil %s / %s / %s bopd.", fmt_int(ps$qo1_Bajo), fmt_int(ps$qo1_Base), fmt_int(ps$qo1_Alto)) else ""),
          window = "1-3 months"), gain, stake, gain_src = if (!is.null(ps) && nrow(ps)) "PROFILE" else "INTERVAL")
        o$meta <- list(sw_act = r$sw_act, qa = r$qa, estado = r$estado, top = r$top_ft, base = r$base_ft)
        add(o)
      } else if (r$estado == "abierto" && is.finite(r$bsw0_pct) && r$bsw0_pct >= st$wso_bsw) {
        # ---- W2: open interval that mostly brings water -> water shut-off ----
        fam <- no_fam(); fam["U"] <- TRUE
        ev <- list(ev_row("Initial BSW of the interval", r$bsw0_pct, st$wso_bsw, "%", q_txt))
        if (is.finite(r$sw_act) && r$sw_act >= st$int_sw_max) { fam["M"] <- TRUE; ev[[length(ev) + 1]] <- ev_row("Sw actual", r$sw_act, st$int_sw_max, "fraction", "swept") }
        h <- well_history(res, r$well, asof)
        if (nrow(h) >= 6) {
          wc <- sum(utils::tail(h$bwpd, 6)) / max(sum(utils::tail(h$bopd + h$bwpd, 6)), 1e-6)
          ev[[length(ev) + 1]] <- ev_row("Well water cut (6 m)", 100 * wc, st$wso_bsw, "%", "")
          if (100 * wc >= st$wso_bsw - 5) fam["O"] <- TRUE
        }
        ev <- c(ev, qa_ev)
        add(new_finding("WELL", "W2", "WSO", r$well, r$sand, r$interval_id, fam, data.table::rbindlist(ev), list(
          maturity = sprintf("Sw actual %s (Sw LAS %s); Np/OOIP %s.", fmtn(r$sw_act), fmtn(r$sw_las), fmtn(r$np_ooip)),
          velocity = "-", vertical = sprintf("Unit %s %s (%s), kh %s mD.ft.", r$sand, r$interval_id, depth, fmt_int(r$kh_md_ft)),
          spatial = "-", ops = sprintf("Interval open. %s.", q_txt),
          mechanism = "Open interval watered out: it adds water and little oil.",
          alternatives = c("Water from another interval behind casing", "Water cut from the analysis, not measured (no PLT)"),
          gaps = c("Production log (PLT) of the well"),
          validation = c("Production log confirms the water entry", "Mechanical isolation option (plug, packer, squeeze)", qa_chk),
          action = sprintf("Shut off unit %s %s (%s) in %s.", r$sand, r$interval_id, depth, r$well),
          outcome = sprintf("Water down by about %s bwpd with oil loss below %s bopd.", fmt_int(r$qw0), fmt_int(r$qo0)),
          window = "1 month"), NA_real_, NA_real_, gain_src = NA_character_))
      }
    }
  }

  # ---- W3 / W4 from the well's own history (any drive) ----
  prods <- intersect(wm[well_type == "PRODUCER", well], unique(res$well$well))
  ws <- res$ds$well_status
  lastws <- if (!is.null(ws) && nrow(ws)) ws[date <= asof][, .SD[.N], by = well] else NULL
  for (w in prods) {
    h <- well_history(res, w, asof)
    if (nrow(h) < 12) next
    n <- nrow(h); off <- utils::tail(h$bopd + h$bwpd, st$shutin_months)
    prior <- h[max(1, n - st$shutin_months - 24 + 1):max(1, n - st$shutin_months)]
    dfl <- if (!is.null(lastws)) lastws[well == w, dfl] else numeric()
    dfl <- if (length(dfl) && is.finite(dfl[1])) dfl[1] else NA_real_
    if (all(off <= 0) && max(prior$bopd) > st$min_rate) {
      # ---- W4: shut-in well with history ----
      fam <- no_fam(); fam["O"] <- TRUE
      q_last <- mean(utils::tail(prior$bopd[prior$bopd > 0], 6))
      ev <- list(ev_row("Months without production", st$shutin_months, st$shutin_months, "months", sprintf("last oil rate %s bopd", fmt_int(q_last))))
      ivw <- if (!is.null(iv)) iv[well == w & estado %in% c("abierto", "parcial")] else NULL
      if (!is.null(ivw) && nrow(ivw) && any(is.finite(ivw$sw_act))) {
        sw <- mean(ivw$sw_act, na.rm = TRUE)
        ev[[length(ev) + 1]] <- ev_row("Sw actual, open intervals", sw, st$int_sw_max, "fraction", "")
        if (sw <= st$int_sw_max) fam["M"] <- TRUE
      } else {
        medq <- stats::median(res$well[date == max(h$date[h$bopd > 0]) & bopd > 0, bopd])
        ev[[length(ev) + 1]] <- ev_row("Last rate vs field median", q_last, medq, "bopd", "")
        if (is.finite(medq) && q_last >= medq) fam["M"] <- TRUE
      }
      add(new_finding("WELL", "W4", "REACTIVATE", w, NA_character_, NA_character_, fam, data.table::rbindlist(ev), list(
        maturity = sprintf("Before shut-in the well made %s bopd.", fmt_int(q_last)), velocity = "-", vertical = "-", spatial = "-",
        ops = sprintf("No production in the last %d months.%s", st$shutin_months, if (!is.na(dfl)) sprintf(" Last fluid level %s ft.", fmt_int(dfl)) else ""),
        mechanism = "Well down for a mechanical or surface reason while its intervals keep potential.",
        alternatives = c("Shut in on purpose (high water cut, facilities)", "Casing or downhole failure that is not economic to repair"),
        gaps = c("Reason for the shut-in"),
        validation = c("Reason for the shut-in and repair scope", "Surface facilities available", "Remaining reserves of the open intervals"),
        action = sprintf("Repair and return %s to production.", w),
        outcome = sprintf("Oil back near %s bopd within 1 month, then the previous decline.", fmt_int(0.8 * q_last)), window = "1 month"),
        0.8 * q_last, NA_real_, gain_src = "ARPS"))
      next
    }
    if (h$bopd[n] <= 0) next
    dc <- decline_check(h, recent = 6)
    if (is.null(dc) || dc$expected < st$min_rate || dc$ratio >= 1 - st$decline_drop) next
    # ---- W3: rate below the well's own decline -> stimulation or lift ----
    fam <- no_fam(); fam["V"] <- TRUE
    ev <- list(ev_row("Oil rate vs own decline (6 m)", dc$actual, dc$expected, "bopd", sprintf("%s of the decline forecast", fmtp(100 * dc$ratio, 0))))
    lift <- is.finite(dfl) && dfl >= st$dfl_high
    if (is.finite(dfl)) { ev[[length(ev) + 1]] <- ev_row("Fluid level", dfl, st$dfl_high, "ft", if (lift) "not pumped off" else "pumped off"); if (lift) fam["O"] <- TRUE }
    ivw <- if (!is.null(iv)) iv[well == w & estado %in% c("abierto", "parcial")] else NULL
    if (!is.null(ivw) && nrow(ivw) && any(is.finite(ivw$sw_act))) {
      sw <- mean(ivw$sw_act, na.rm = TRUE); ev[[length(ev) + 1]] <- ev_row("Sw actual, open intervals", sw, st$int_sw_max, "fraction", "")
      if (sw <= st$int_sw_max) fam["M"] <- TRUE
    }
    wc_now <- sum(utils::tail(h$bwpd, 6)) / max(sum(utils::tail(h$bopd + h$bwpd, 6)), 1e-6)
    wc_pre <- sum(h$bwpd[(n - 17):(n - 6)]) / max(sum(h$bopd[(n - 17):(n - 6)] + h$bwpd[(n - 17):(n - 6)]), 1e-6)
    ev[[length(ev) + 1]] <- ev_row("Water cut now vs 12 m before", 100 * wc_now, 100 * wc_pre, "%", if (wc_now > wc_pre + 0.05) "rising: check water breakthrough" else "stable")
    act <- if (lift) "LIFT" else "STIM_PROD"
    add(new_finding("WELL", "W3", act, w, NA_character_, NA_character_, fam, data.table::rbindlist(ev), list(
      maturity = "Remaining potential from the open intervals (see Sw actual when the interval analysis exists).",
      velocity = sprintf("Oil %s bopd over the last 6 months vs %s bopd expected from the well's decline (b = %s, Di = %s /month).",
                         fmt_int(dc$actual), fmt_int(dc$expected), fmtn(dc$fit$b, 1), fmtn(dc$fit$di, 3)),
      vertical = "-", spatial = "-",
      ops = if (is.finite(dfl)) sprintf("Fluid level %s ft above pump.", fmt_int(dfl)) else "No fluid level.",
      mechanism = if (lift) "Pump not keeping up with the inflow: fluid level high." else "Near-wellbore damage (scale, fines, paraffin) or a mechanical restriction.",
      alternatives = c("Water breakthrough replacing oil (check water cut)", "Lower reservoir pressure (offset injection changes)", "Test or allocation error"),
      gaps = c(if (!is.finite(dfl)) "Fluid level / dynamometer card", "Recent well test"),
      validation = c("Recent well test confirms the rate", if (lift) "Pump capacity and efficiency" else "Damage diagnosis (skin, scale or paraffin samples)",
                     "Offset wells do not show the same drop"),
      action = if (lift) sprintf("Resize or speed up the lift in %s.", w) else sprintf("Stimulate %s (treatment for the diagnosed damage).", w),
      outcome = sprintf("Oil back toward %s bopd within 1-2 months.", fmt_int(dc$expected)), window = "1-2 months"),
      dc$expected - dc$actual, NA_real_, gain_src = "ARPS"))
  }
  out
}

# ---- other analyses (table Findings): one lens per source ----------------------------------------
external_findings <- function(res, asof) {
  fd <- res$ds$findings
  if (is.null(fd) || !nrow(fd)) return(list())
  fd <- fd[is.na(date) | date <= asof]
  if (!nrow(fd)) return(list())
  g <- split(fd, by = c("source", "action", "well", "sand", "interval_id"), keep.by = TRUE)
  lapply(g, function(x) {
    fam <- no_fam(); fm <- unique(stats::na.omit(x$family)); fam[fm] <- TRUE
    ev <- x[, .(metric = data.table::fcoalesce(metric, "finding"), value, reference, unit = data.table::fcoalesce(unit_label, ""),
                comment = data.table::fcoalesce(comment, ""))]
    sand <- if (x$sand[1] == "-") NA_character_ else x$sand[1]; itv <- if (x$interval_id[1] == "-") NA_character_ else x$interval_id[1]
    g1 <- suppressWarnings(max(x$gain_bopd, na.rm = TRUE))
    new_finding(x$source[1], x$source[1], x$action[1], x$well[1], sand, itv, fam, ev, list(
      maturity = "-", velocity = "-", vertical = "-", spatial = "-", ops = "-",
      mechanism = paste(unique(stats::na.omit(x$comment)), collapse = " "),
      alternatives = character(), gaps = character(),
      validation = sprintf("Review the %s analysis behind this finding", x$source[1]),
      action = sprintf("%s in %s%s.", action_label(x$action[1]), x$well[1], if (!is.na(sand)) paste(" unit", sand) else ""),
      outcome = "-", window = "-"), if (is.finite(g1)) g1 else NA_real_, NA_real_, gain_src = if (is.finite(g1)) "SOURCE" else NA_character_)
  })
}

# ---- pattern support for well-interval findings (former rule D) ---------------------------------
attach_support <- function(fs, res, asof, st) {
  pu <- if (!is.null(res$units)) res$units$pattern_sand[date == max(date[date <= asof])] else NULL
  wp <- well_patterns(res, asof)
  T <- st$target_tp
  lapply(fs, function(f) {
    if (!identical(f$action, "ADPERF") || is.na(f$well) || is.na(f$sand)) return(f)
    x <- wp[well == f$well & flooded == TRUE]
    if (!nrow(x) || is.null(pu)) return(f)
    u <- merge(x[, .(pattern, coeff, mechanism)], pu[sand == f$sand, .(pattern, dwi, tp12, hcpv)], by = "pattern")
    if (!nrow(u)) { f$text$velocity <- sprintf("%s is in waterflood pattern(s) %s, but unit %s is not part of their volumetrics.", f$well, paste(x$pattern, collapse = ", "), f$sand); return(f) }
    data.table::setorder(u, -coeff)
    b <- u[1]
    sup <- is.finite(b$tp12) && b$tp12 >= 0.5 * T && b$dwi > 0
    f$evidence <- rbind(f$evidence, data.table::data.table(metric = sprintf("Injection support, unit %s in %s", f$sand, b$pattern), value = b$tp12,
                         reference = T, unit = "%HCPV/yr", comment = sprintf("unit DWI %s, allocation %s", fmtn(b$dwi), fmtn(b$coeff)), lens = "PATTERN"), fill = TRUE)
    if (sup) f$fam["V"] <- TRUE
    f$pattern <- b$pattern
    f$lens_extra <- unique(c(f$lens_extra, "PATTERN"))
    f$text$velocity <- sprintf("%s: unit %s in %s receives TP %s/yr (target %s), unit DWI %s.%s", b$mechanism, f$sand, b$pattern, fmtp(b$tp12), fmtp(T), fmtn(b$dwi),
                               if (nrow(u) > 1) sprintf(" Also in %s.", paste(u$pattern[-1], collapse = ", ")) else "")
    sw <- f$meta$sw_act
    if (is.finite(b$dwi) && b$dwi >= st$mature_dwi && is.finite(sw) && sw <= st$int_sw_max) {
      f$text$validation <- c(f$text$validation, sprintf("Check the interval is not swept: unit DWI %s in %s vs Sw actual %s", fmtn(b$dwi), b$pattern, fmtn(sw)))
      f$text$alternatives <- c(f$text$alternatives, "Injection has already swept the unit; Sw from Np/OOIP ignores injected water")
    }
    if (!sup) f$text$gaps <- c(f$text$gaps, sprintf("Injection into unit %s from the pattern injector (TP %s/yr)", f$sand, fmtp(b$tp12)))
    f
  })
}

# ---- merge findings on the target key ------------------------------------------------------------
merge_findings <- function(fs) {
  ok <- vapply(fs, function(f) !is.na(f$key), TRUE)
  unassigned <- if (any(!ok)) data.table::rbindlist(lapply(fs[!ok], function(f) data.table::data.table(
    lens = f$lens, rule = f$rule, action = f$action, pattern = f$pattern, families = paste(names(f$fam)[f$fam], collapse = ""), note = f$text$action))) else data.table::data.table()
  fs <- fs[ok]
  if (!length(fs)) return(list(records = list(), unassigned = unassigned))
  keys <- vapply(fs, `[[`, "", "key")
  src_rank <- c(PROFILE = 1, SF_FIT = 2, ARPS = 3, SOURCE = 4, INTERVAL = 5)
  recs <- lapply(split(fs, factor(keys, levels = unique(keys))), function(g) {
    nf <- vapply(g, function(f) sum(f$fam), 0)
    g <- g[order(-nf)]
    p <- g[[1]]
    fam <- Reduce(`|`, lapply(g, `[[`, "fam"))
    gs <- vapply(g, function(f) if (is.na(f$gain_src)) 99 else src_rank[[f$gain_src]] %||% 50, 0)
    best <- g[[which.min(gs)]]
    tx <- p$text
    tx$validation <- unique(unlist(lapply(g, function(f) f$text$validation)))
    tx$gaps <- unique(unlist(lapply(g, function(f) f$text$gaps)))
    tx$alternatives <- unique(unlist(lapply(g, function(f) f$text$alternatives)))
    tx$others <- if (length(g) > 1) vapply(g[-1], function(f) sprintf("%s %s: %s", f$lens, f$rule, f$text$action), "") else character()
    stakes <- vapply(g, function(f) if (is.finite(f$stake)) f$stake else NA_real_, 0)
    list(key = p$key, action = p$action, well = p$well, sand = p$sand, interval = p$interval,
         lenses = unique(c(unlist(lapply(g, function(f) c(f$lens, f$lens_extra))))), rules = unique(vapply(g, `[[`, "", "rule")),
         pattern = { pp <- stats::na.omit(vapply(g, function(f) f$pattern %||% NA_character_, "")); if (length(pp)) pp[[1]] else NA_character_ },
         fam = fam, evidence = data.table::rbindlist(lapply(g, `[[`, "evidence"), fill = TRUE), text = tx,
         gain = best$gain, gain_src = best$gain_src, stake = if (any(is.finite(stakes))) max(stakes, na.rm = TRUE) else NA_real_,
         legacy_key = { lk <- stats::na.omit(vapply(g, function(f) f$legacy_key %||% NA_character_, "")); if (length(lk)) lk[[1]] else NA_character_ },
         meta = p$meta %||% list())
  })
  list(records = recs, unassigned = unassigned)
}

generate_opportunities <- function(res, series, asof, st = res$settings) {
  st <- utils::modifyList(default_settings, st)
  pf <- tryCatch(pattern_findings(res, series, asof, st), error = function(e) { warning("pattern rules: ", conditionMessage(e)); list() })
  # project pattern findings without a well: injector actions to the dominant injector
  pf <- lapply(pf, function(f) {
    if (is.na(f$well) && f$action %in% c("ISOLATE", "STIM_INJ", "CONFORMANCE", "RATE", "SUPPORT_INJ")) {
      f$well <- dominant_injector(res, f$pattern, asof)
      if (!is.na(f$well)) f$key <- opp_key(f$action, f$well, f$sand, f$interval)
    }
    f
  })
  wf <- well_findings(res, asof, st)
  xf <- external_findings(res, asof)
  all <- attach_support(c(pf, wf, xf), res, asof, st)
  m <- merge_findings(all)
  out <- finalize_opportunities(m$records, res, asof, st)
  out$unassigned <- m$unassigned
  out
}

finalize_opportunities <- function(recs, res, asof, st) {
  empty <- list(summary = data.table::data.table(), records = list())
  if (!length(recs)) return(empty)
  g <- function(v, na = NA_character_) vapply(recs, function(r) { x <- r[[v]]; if (is.null(x) || !length(x)) na else as.character(x[[1]]) }, "")
  num <- function(v) vapply(recs, function(r) { x <- r[[v]]; if (is.null(x) || !length(x)) NA_real_ else as.numeric(x) }, 0)
  s <- data.table::data.table(okey = g("key"), action = g("action"), well = g("well"), sand = g("sand"), interval = g("interval"),
    pattern = g("pattern"), lenses = vapply(recs, function(r) paste(r$lenses, collapse = ", "), ""),
    rules = vapply(recs, function(r) paste(r$rules, collapse = ", "), ""),
    families = vapply(recs, function(r) paste(names(r$fam)[r$fam], collapse = ""), ""),
    n_fam = vapply(recs, function(r) sum(r$fam), 0), mv = vapply(recs, function(r) any(r$fam[c("M", "V")]), TRUE),
    gain = num("gain"), gain_src = g("gain_src"), stake = num("stake"), legacy_key = g("legacy_key"))
  data.table::setnames(s, "okey", "key")
  s[, fkey := profile_key(well, sand, interval)]
  # forecasts: profiles first
  ps <- profile_summary(res$ds$profiles)
  if (!is.null(ps)) {
    s <- merge(s, ps[, .(fkey = pkey, qo1_Bajo, qo1_Base, qo1_Alto, np12 = np12_Base, wp12 = wp12_Base, np_ext = np_ext_Base, unc)], by = "fkey", all.x = TRUE)
    s[is.finite(qo1_Base), `:=`(gain = qo1_Base, gain_src = "PROFILE")]
  } else s[, `:=`(qo1_Bajo = NA_real_, qo1_Base = NA_real_, qo1_Alto = NA_real_, np12 = NA_real_, wp12 = NA_real_, np_ext = NA_real_, unc = NA_real_)]
  s[!is.finite(np12) & is.finite(gain), np12 := gain * 365 * 0.85]   # indicative: 12 months with a mild decline
  s[is.finite(np_ext) & !is.finite(stake), stake := np_ext]
  # context: hierarchy, drive, patterns
  ctx <- drive_context(s[, .(well, sand)], res, asof)
  s <- merge(s, ctx, by = c("well", "sand"), all.x = TRUE)
  s[is.na(pattern), pattern := ctx_pattern]
  wm <- res$well_master
  lv <- intersect(c("well_type", "orgunit", "contract", "field", "structure", "substructure", "area"), names(wm))
  s <- merge(s, wm[, c("well", lv), with = FALSE], by = "well", all.x = TRUE)
  for (v in setdiff(c("well_type", "orgunit", "contract", "field", "structure", "substructure", "area"), names(s))) s[, (v) := NA_character_]
  s[, auto_status := ifelse(n_fam >= 2 & mv, "candidate", "screening_only")]
  gmax <- max(c(s$gain[is.finite(s$gain) & s$gain > 0], 1e-9)); smax <- max(c(s$stake[is.finite(s$stake)], 1e-9))
  s[, score := round(100 * pmax(0, st$w_evidence * n_fam / 5 + st$w_gain * pmax(data.table::fcoalesce(gain, 0), 0) / gmax +
                                   st$w_stake * data.table::fcoalesce(stake, 0) / smax - st$w_unc * pmin(data.table::fcoalesce(unc, 0), 2) / 2))]
  s[, c("mv", "ctx_pattern") := NULL]
  s <- econ_apply(s, res)
  data.table::setorder(s, -score)
  names(recs) <- vapply(recs, `[[`, "", "key")
  list(summary = s, records = recs)
}

# Opportunities that touch a pattern: found on it, or targeting one of its wells.
opps_for_pattern <- function(summary, res, pat, asof = max(res$months)) {
  if (!nrow(summary)) return(summary)
  w <- unique(res$alloc[pattern == pat & coeff > 0 & date <= asof, well])
  summary[pattern %in% pat | well %in% w]
}

# Save a job: the combination of the opportunities the engineer picked on one well. Each picked
# opportunity gets a row with the same job id and, if executed, moves to `executed`; profiles not
# frozen at validation are frozen now so the job is judged against what it executed. Opportunities
# not picked are untouched and stay as identified.
log_job <- function(con, op, keys, date, status = "EXECUTED", notes = "", job = "", profiles = NULL) {
  keys <- unique(keys)
  if (!length(keys)) stop("no opportunities selected")
  s <- op$summary[key %in% keys]
  if (data.table::uniqueN(s$well) != 1) stop("a job is done on one well")
  if (is.null(job) || !nzchar(trimws(job))) job <- sprintf("JOB-%s-%s", s$well[1], format(Sys.time(), "%Y%m%d%H%M%S"))
  for (k in keys) {
    x <- op$records[[k]]; row <- s[key == k]
    if (is.null(store_frozen(con, k)) && identical(row$gain_src, "PROFILE")) store_freeze_forecast(con, k, profile_rows(profiles, row$fkey))
    store_add_intervention(con, x$well, row$pattern, x$sand, date, action_job(x$action), status, notes, k, x$interval, job)
    if (toupper(status) == "EXECUTED") store_set_state(con, k, status = "executed",
      comment = sprintf("%s %s, %s (%d of the job)", action_job(x$action), format(as.Date(date)), job, length(keys)))
  }
  job
}

# Move decisions stored under v2 keys (type|pattern|unit) to the well keys.
store_migrate_keys <- function(con, summary) {
  if (!nrow(summary)) return(invisible(0))
  m <- summary[!is.na(legacy_key), .(old = legacy_key, new = key)]
  m <- m[!duplicated(old)]
  n <- 0
  for (i in seq_len(nrow(m))) {
    have_old <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM opp_state WHERE key = ?", params = list(m$old[i]))$n
    have_new <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM opp_state WHERE key = ?", params = list(m$new[i]))$n
    if (have_old > 0 && have_new == 0) {
      for (tb in c("opp_state", "opp_history", "outcomes", "ai_drafts")) DBI::dbExecute(con, sprintf("UPDATE %s SET key = ? WHERE key = ?", tb), params = list(m$new[i], m$old[i]))
      DBI::dbExecute(con, "UPDATE interventions SET opp_key = ? WHERE opp_key = ?", params = list(m$new[i], m$old[i]))
      n <- n + 1
    }
  }
  invisible(n)
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
