# Jobs: one rig visit on one well, combining the opportunities the engineer picks ----------------
#
# A job is proposed by an engineer and approved by a lead; it is then executed and evaluated against
# the forecast frozen at approval. The job potential is the sum of its items (intervals add up):
#   ADPERF   Bajo / Base / Alto profile, or the interval's initial rates with a standard decline
#   WSO      isolation removes the interval's oil and water (measured / modelled rates, all scenarios)
#   REPERF   gap to the theoretical potential with a standard decline
#   others   the opportunity's gain rate with a standard decline
# Cost = standard cost lookup by job type and well depth (rig visit + one cost per item).

job_status_levels <- c("proposed", "approved", "executed", "evaluated", "rejected", "cancelled")
# Only candidates go into a job; screening opportunities are promoted first.
jobable_statuses <- "candidate"
job_status_colors <- c(proposed = "#fbbf24", approved = "#34d399", executed = "#a78bfa", evaluated = "#e2e8f0", rejected = "#64748b", cancelled = "#64748b")

default_job_costs <- data.table::data.table(
  job_type = c("RIG", "RIG", "ADPERF", "ISOLATION", "STIM", "REPERF", "ALS_CHANGE", "REACTIVATION", "CONFORMANCE", "RATE", "LIFT"),
  depth_min_ft = c(0, 6000, 0, 0, 0, 0, 0, 0, 0, 0, 0), depth_max_ft = c(6000, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999, 99999),
  cost_usd = c(150000, 220000, 45000, 60000, 50000, 40000, 180000, 80000, 70000, 5000, 60000))

cost_of <- function(costs, type, depth) {
  ct <- if (!is.null(costs) && nrow(costs)) costs else default_job_costs
  d <- if (is.finite(depth)) depth else 5000
  x <- ct[job_type == toupper(type) & data.table::fcoalesce(depth_min_ft, 0) <= d & data.table::fcoalesce(depth_max_ft, Inf) >= d]
  if (!nrow(x) && !identical(ct, default_job_costs)) return(cost_of(default_job_costs, type, depth))
  if (nrow(x)) x$cost_usd[1] else NA_real_
}

# Cost breakdown of a job: a rig visit unless every item is a surface rate change, then one cost per item.
job_cost <- function(ds, w, types, als_change = FALSE) {
  depth <- well_depth(ds, w)
  types <- toupper(types)
  rows <- c(if (!all(types %in% "RATE")) "RIG", types, if (als_change) "ALS_CHANGE")
  data.table::data.table(item = rows, depth_ft = depth, cost_usd = vapply(rows, function(t) cost_of(ds$job_costs, t, depth), 0))
}

std_decline <- function(q, months = 36, b = 0.5, di = 0.045) q / (1 + b * di * (seq_len(months) - 1))^(1 / b)

# Monthly Bajo / Base / Alto of one item (incremental rates of the well; negative = removed).
item_forecast <- function(row, rec, profiles, st, months = 36) {
  m <- rec$meta %||% list()
  sc <- function(qo_b, qw_b, lo, hi) data.table::rbindlist(lapply(c("Bajo", "Base", "Alto"), function(s) {
    f <- switch(s, Bajo = lo, Base = 1, Alto = hi)
    data.table::data.table(scenario = s, month = seq_len(months), qo = qo_b * f, qw = qw_b * f)
  }))[, qf := qo + qw][]
  if (identical(row$gain_src, "PROFILE")) { p <- profile_rows(profiles, row$fkey); if (!is.null(p)) return(p[, .(scenario, month = as.integer(month), qo, qw, qf)]) }
  a <- row$action
  if (a == "WSO" && is.finite(m$qo %||% NA) && is.finite(m$qw %||% NA)) {
    return(sc(rep(m$qo, months), rep(m$qw, months), 1, 1))
  }
  if (a == "ADPERF" && is.finite(m$qo %||% NA)) {
    qo <- std_decline(m$qo, months); qf <- m$qo + data.table::fcoalesce(m$qw, 0)
    return(sc(qo, qf - qo, st$noprof_lo, st$noprof_hi))
  }
  if (a == "REPERF" && is.finite(m$qo %||% NA)) return(sc(std_decline(m$qo, months), rep(data.table::fcoalesce(m$qw, 0), months), st$noprof_lo, 1))
  if (is.finite(row$gain) && row$gain > 0) return(sc(std_decline(row$gain, months), rep(0, months), st$noprof_lo, st$noprof_hi))
  NULL
}

# P(success) per action: settings until at least five outcomes of that job type exist, then the
# share of outcomes not below Bajo.
ps_of <- function(action, st, outcomes = NULL) {
  base <- switch(action_job(action), ADPERF = st$ps_adperf, ISOLATION = st$ps_isolation, STIM = st$ps_stim, REPERF = st$ps_stim, st$ps_other)
  if (!is.null(outcomes) && nrow(outcomes)) {
    o <- outcomes[startsWith(key, paste0(action, "|")) & verdict %in% c("above Alto", "within range", "below Bajo")]
    if (nrow(o) >= 5) return(mean(o$verdict != "below Bajo"))
  }
  base
}

# Opportunity triggers of a well: lift near the end of its life, repeated failures, well down.
job_triggers <- function(ds, res, w, asof, st) {
  out <- character()
  ls <- ds$lift_status
  if (!is.null(ls) && nrow(ls)) {
    x <- ls[well == w]
    if (nrow(x)) {
      x <- x[which.max(install_date)]
      used <- as.numeric(asof - x$install_date) / x$expected_runlife_days
      if (is.finite(used) && used >= st$runlife_trigger) out <- c(out, sprintf("%s run life %s", data.table::fcoalesce(x$lift_type, "lift"), fmtp(100 * used, 0)))
      if (is.finite(x$failures_12m) && x$failures_12m >= 2) out <- c(out, sprintf("%d lift failures in 12 months", as.integer(x$failures_12m)))
    }
  }
  h <- well_history(res, w, asof)
  if (nrow(h) >= st$shutin_months && all(utils::tail(h$bopd + h$bwpd, st$shutin_months) <= 0) && any(h$bopd > 0)) out <- c(out, "well down")
  out
}

lift_check <- function(ds, res, w, asof, dqf) {
  h <- well_history(res, w, asof)
  now <- if (nrow(h)) mean(utils::tail(h$bopd + h$bwpd, 3)) else NA_real_
  ls <- ds$lift_status
  x <- if (!is.null(ls)) ls[well == w] else NULL
  cap <- if (!is.null(x) && nrow(x)) x[which.max(install_date)]$capacity_bfpd else NA_real_
  life <- if (!is.null(x) && nrow(x)) { y <- x[which.max(install_date)]; as.numeric(asof - y$install_date) / y$expected_runlife_days } else NA_real_
  list(liquid_now = now, liquid_after = now + dqf, capacity = cap, over = is.finite(cap) && is.finite(now) && now + dqf > cap, runlife_used = life)
}

# Everything the job screens need about a set of opportunities on one well.
job_proposal <- function(op, keys, res, asof, st, outcomes = NULL, als_change = FALSE) {
  s <- op$summary[key %in% keys]
  if (!nrow(s)) return(NULL)
  w <- s$well[1]
  fc <- lapply(s$key, function(k) item_forecast(s[key == k], op$records[[k]], res$ds$profiles, st))
  items <- s[, .(key, action, sand, interval, gain_src, gain)]
  items[, ps := vapply(action, ps_of, 0, st = st, outcomes = outcomes)]
  items[, `:=`(qo1 = vapply(fc, function(f) if (is.null(f)) NA_real_ else f[scenario == "Base" & month == 1, qo], 0),
               qw1 = vapply(fc, function(f) if (is.null(f)) NA_real_ else f[scenario == "Base" & month == 1, qw], 0),
               np12 = vapply(fc, function(f) if (is.null(f)) NA_real_ else f[scenario == "Base" & month <= 12, sum(qo)] * days_per_month, 0))]
  total <- combine_forecasts(fc)
  cost <- job_cost(res$ds, w, action_job(s$action), als_change)
  b1 <- function(scn, v) if (is.null(total)) NA_real_ else total[scenario == scn & month == 1][[v]]
  pos <- items[is.finite(np12) & np12 > 0]
  ps_job <- if (nrow(pos)) sum(pos$np12 * pos$ps) / sum(pos$np12) else NA_real_
  np12 <- if (is.null(total)) NA_real_ else total[scenario == "Base" & month <= 12, sum(qo)] * days_per_month
  lc <- lift_check(res$ds, res, w, asof, data.table::fcoalesce(b1("Base", "qf"), 0))
  list(well = w, items = items, forecast = total, cost = cost, cost_usd = sum(cost$cost_usd, na.rm = TRUE),
       qo = c(Bajo = b1("Bajo", "qo"), Base = b1("Base", "qo"), Alto = b1("Alto", "qo")), qw = b1("Base", "qw"), qf = b1("Base", "qf"),
       np12 = np12, ps = ps_job, risked_np12 = np12 * data.table::fcoalesce(ps_job, 1),
       unc = if (is.finite(b1("Base", "qo")) && b1("Base", "qo") > 0) (b1("Alto", "qo") - b1("Bajo", "qo")) / b1("Base", "qo") else NA_real_,
       triggers = job_triggers(res$ds, res, w, asof, st), lift = lc, missing_forecast = items[!is.finite(qo1), key])
}

# Portfolio score: risked oil per cost scaled to the best job, opportunity bonus, uncertainty penalty.
score_jobs <- function(j, st) {
  if (!nrow(j)) return(j)
  eff <- j$risked_np12 / pmax(j$cost_usd, 1)
  emax <- max(c(eff[is.finite(eff)], 1e-9))
  j[, score := round(pmax(0, 100 * data.table::fcoalesce(eff / emax, 0) * (1 + ifelse(nzchar(data.table::fcoalesce(triggers, "")), st$opportunity_bonus, 0)) -
                            10 * pmin(data.table::fcoalesce(unc, 0), 3)))]
  j[]
}

# Who may approve: users listed in WF_LEADS (comma separated); everyone when it is not set.
job_can_approve <- function(user) {
  leads <- trimws(strsplit(Sys.getenv("WF_LEADS"), ",")[[1]])
  leads <- leads[nzchar(leads)]
  !length(leads) || tolower(user) %in% tolower(leads)
}

job_propose <- function(con, op, keys, res, asof, st, user, name = "", notes = "", als_change = FALSE) {
  pr <- job_proposal(op, keys, res, asof, st, store_outcomes(con), als_change)
  if (is.null(pr)) stop("no opportunities selected")
  if (data.table::uniqueN(op$summary[key %in% keys, well]) != 1) stop("a job is done on one well")
  id <- store_job_create(con, pr$well, if (nzchar(trimws(name))) trimws(name) else sprintf("JOB-%s-%s", pr$well, format(Sys.time(), "%Y%m%d%H%M%S")),
                         pr$items, pr$cost_usd, paste(pr$triggers, collapse = "; "), als_change, user, notes)
  store_freeze_forecast(con, paste0("JOB:", id), pr$forecast)
  id
}

job_set_status <- function(con, id, to, user, comment = "", op = NULL, res = NULL, date = NULL) {
  j <- store_job(con, id)
  if (is.null(j)) stop("unknown job")
  allowed <- list(proposed = c("approved", "rejected", "cancelled"), approved = c("executed", "cancelled", "proposed"), executed = c("evaluated"),
                  rejected = c("proposed"), cancelled = character(), evaluated = character())
  if (!to %in% allowed[[j$status]]) stop(sprintf("a %s job cannot move to %s", j$status, to))
  if (to %in% c("approved", "rejected") && !job_can_approve(user)) stop("only a lead (WF_LEADS) can approve or reject")
  if (to == "approved") {
    # the forecast the decision is based on: frozen again at approval
    pr <- if (!is.null(op) && !is.null(res)) job_proposal(op, store_job_items(con, id)$opp_key, res, max(res$months), res$settings) else NULL
    if (!is.null(pr$forecast)) store_freeze_forecast(con, paste0("JOB:", id), pr$forecast)
  }
  if (to == "executed") {
    it <- store_job_items(con, id)
    log_job(con, op, it$opp_key, date, "EXECUTED", comment, j$name, res$ds$profiles)
  }
  store_job_update(con, id, to, user, comment, date)
  invisible(TRUE)
}

job_evaluate <- function(con, res, id, asof) {
  j <- store_job(con, id)
  if (is.null(j) || is.na(j$exec_date)) return(NULL)
  ev <- evaluate_well_job(res, j$well, as.Date(j$exec_date), store_frozen(con, paste0("JOB:", id)), asof)
  list(eval = ev, verdict = job_verdict(ev))
}

# Trigger scan of every well with lift data or production: one row per well and trigger.
all_triggers <- function(res, asof, st) {
  ds <- res$ds
  wells <- unique(c(ds$lift_status$well, res$well_master[well_type == "PRODUCER", well]))
  wells <- intersect(wells, unique(res$well$well))
  x <- data.table::rbindlist(lapply(wells, function(w) { t <- job_triggers(ds, res, w, asof, st); if (length(t)) data.table::data.table(well = w, trigger = t) }))
  if (!nrow(x)) data.table::data.table(well = character(), trigger = character()) else x
}

trigger_rules <- function(st) data.table::data.table(
  Rule = c("Lift near the end of its run life", "Repeated lift failures", "Well down"),
  Condition = c(sprintf("run life used >= %s %% (install date and expected run life from CDF)", round(100 * st$runlife_trigger)),
                "2 or more failures in the last 12 months (CDF)", sprintf("no production for %d months", st$shutin_months)),
  Effect = "the well's open opportunities are flagged; a job built on the well gets the opportunity bonus")
