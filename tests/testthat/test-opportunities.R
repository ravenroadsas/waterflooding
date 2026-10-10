root <- normalizePath(file.path(getwd(), "..", ".."))
if (!dir.exists(file.path(root, "data/demo"))) root <- getwd()
# v3 well-centric opportunities on the demo field (pattern lens, well lens, other analyses)

demo <- local({
  ds <- build_dataset(read_dataset_dir(file.path(root, "data/demo")), "demo")
  res <- run_engine(ds, default_settings)
  ser <- aggregate_level(res, "pattern")
  list(ds = ds, res = res, op = generate_opportunities(res, ser, max(res$months)))
})

test_that("new tables load through their source names and aliases", {
  expect_equal(sheet_key("INTERVALOS"), "intervals")
  expect_equal(sheet_key("PERFILES_MENSUALES"), "profiles")
  expect_equal(sheet_key("perfiles_mensuales_2026"), "profiles")
  expect_equal(sheet_key("perfiles"), "injsand")
  ds <- demo$ds
  expect_true(all(c("qo0", "qw0", "qf0", "sw_act", "np_ooip", "qa", "estado") %in% names(ds$intervals)))
  expect_setequal(unique(ds$profiles$scenario), c("Bajo", "Base", "Alto"))
  expect_false(any(ds$issues$severity == "error"))
  expect_false(any(ds$issues$table %in% c("intervals", "profiles") & ds$issues$severity == "warning"))
  # hierarchy levels and primary wells outside patterns
  expect_true(all(c("orgunit", "contract", "structure") %in% names(ds$well_master)))
  expect_equal(ds$well_master[well == "SAT-01", field], "Demo Satellite")
})

test_that("profile checks catch a Base scenario that does not match the interval", {
  ds <- data.table::copy(demo$ds)
  ds$profiles <- data.table::copy(ds$profiles)
  ds$profiles[well == "PRD-07" & scenario == "Base" & month == 1, `:=`(qoi = 999, qo = 999)]
  iss <- data.table::rbindlist(validate_well_analysis(ds))
  expect_true(any(grepl("Base profiles whose initial rates differ", iss$message)))
  expect_true(any(grepl("not constant|qo \\+ qw", iss$message)))
})

test_that("every opportunity targets a well and keys are action|well|unit|interval", {
  s <- demo$op$summary
  expect_gt(nrow(s), 10)
  expect_false(any(is.na(s$well)))
  expect_true(all(lengths(strsplit(s$key, "|", fixed = TRUE)) == 4))
  expect_equal(anyDuplicated(s$key), 0)
})

test_that("single-well analysis: ADPERF with profile forecast and pattern support", {
  s <- demo$op$summary
  r <- s[key == "ADPERF|PRD-07|B|INT001"]
  expect_equal(nrow(r), 1)
  expect_equal(r$gain_src, "PROFILE")
  expect_equal(r$gain, 150)
  expect_equal(c(r$qo1_Bajo, r$qo1_Base, r$qo1_Alto), c(50, 150, 300))
  expect_match(r$lenses, "PATTERN")      # injection support of unit B from its waterflood pattern
  expect_match(r$families, "V")
  expect_equal(r$drive, "waterflood")
  # open intervals are never proposed for perforation
  expect_false(any(s$action == "ADPERF" & s$interval == "INT002" & s$well == "PRD-07"))
})

test_that("primary wells get well-lens opportunities only", {
  s <- demo$op$summary[grepl("^SAT-", well)]
  expect_true(all(s$drive == "primary"))
  expect_false(any(grepl("PATTERN", s$lenses)))
  expect_true("STIM_PROD|SAT-02|-|-" %in% s$key)   # rate below its own decline
  expect_true("REACTIVATE|SAT-05|-|-" %in% s$key)  # shut in
  expect_true("ADPERF|SAT-01|B|INT001" %in% s$key)
})

test_that("findings from other analyses merge with the pattern rules on the same target", {
  r <- demo$op$summary[key == "STIM_INJ|INJ-13|-|-"]
  expect_equal(nrow(r), 1)
  expect_match(r$lenses, "PATTERN"); expect_match(r$lenses, "Injector review 2026")
  rec <- demo$op$records[["STIM_INJ|INJ-13|-|-"]]
  expect_true(any(rec$evidence$lens == "Injector review 2026"))
  expect_true(any(grepl("Injector review", rec$text$validation)))
})

test_that("profile summary and Arps helpers", {
  pf <- data.table::data.table(well = "W", sand = "A", interval_id = "I1", scenario = "Base", month = 1:12, qo = 100, qw = 50, qf = 150, b = 0, di = 0.01)
  ps <- profile_summary(pf)
  expect_equal(ps$np12_Base, 100 * 12 * days_per_month)
  expect_equal(ps$wp12_Base, 50 * 12 * days_per_month)
  expect_gt(ps$np_ext_Base, ps$np_t_Base)
  q <- arps_q(200, 0.03, 0.5, 0:47)
  f <- arps_fit(q)
  expect_equal(f$b, 0.5); expect_equal(f$di, 0.03, tolerance = 1e-3)
})

test_that("post-job evaluation compares incremental oil with the frozen forecast", {
  months <- seq(as.Date("2024-01-01"), by = "month", length.out = 24)
  q <- 100 * exp(-0.02 * (0:23)); q[13:24] <- q[13:24] + 40
  res <- list(well = data.table::data.table(well = "W", date = months, bopd = q, bwpd = 10, bwipd = 0), months = months)
  fr <- data.table::data.table(scenario = rep(c("Bajo", "Base", "Alto"), each = 11), month = rep(1:11, 3), qo = rep(c(20, 40, 80), each = 11))
  ev <- evaluate_well_job(res, "W", months[12], fr, max(months))
  expect_equal(nrow(ev), 12)
  v <- job_verdict(ev)
  expect_equal(v$verdict, "within range")
  expect_equal(v$ratio, 1, tolerance = 0.05)
})

test_that("store freezes forecasts and moves v2 pattern keys to well keys", {
  con <- store_open(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  pr <- profile_rows(demo$ds$profiles, "PRD-07|B|INT001")
  store_freeze_forecast(con, "ADPERF|PRD-07|B|INT001", pr)
  expect_equal(nrow(store_frozen(con, "ADPERF|PRD-07|B|INT001")), nrow(pr))
  s <- demo$op$summary[!is.na(legacy_key)][1]
  store_set_state(con, s$legacy_key, status = "validated_candidate", comment = "v2 decision")
  expect_equal(store_migrate_keys(con, demo$op$summary), 1)
  expect_equal(store_get_state(con, s$key)$status, "validated_candidate")
  expect_null(store_get_state(con, s$legacy_key))
  store_add_intervention(con, "PRD-07", NA, "B", as.Date("2026-08-01"), "ADPERF", "EXECUTED", "", "ADPERF|PRD-07|B|INT001", "INT001", "JOB-1")
  expect_equal(store_interventions(con)$interval_id, "INT001")
})

test_that("a job executes only the opportunities picked; the rest stay identified", {
  con <- store_open(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  op <- demo$op
  sat <- op$summary[well == "SAT-01" & action == "ADPERF"]
  expect_equal(nrow(sat), 5)                       # five intervals with potential in one well
  pick <- c("ADPERF|SAT-01|B|INT001", "ADPERF|SAT-01|A|INT003")
  job <- log_job(con, op, pick, as.Date("2026-08-01"), "EXECUTED", "open the two best", "JOB-SAT01-A", demo$ds$profiles)
  expect_equal(job, "JOB-SAT01-A")
  s <- apply_states(op$summary, store_states(con))[well == "SAT-01" & action == "ADPERF"]
  expect_setequal(s[status == "executed", key], pick)
  expect_true(all(as.character(s[!key %in% pick, status]) == as.character(s[!key %in% pick, auto_status])))
  iv <- store_interventions(con)
  expect_equal(nrow(iv), 2); expect_equal(unique(iv$job), "JOB-SAT01-A")
  # each executed opportunity carries its frozen forecast; the job forecast is their sum
  fr <- combine_forecasts(lapply(pick, function(k) store_frozen(con, k)))
  b1 <- fr[scenario == "Base" & month == 1]
  expect_equal(b1$qo, 210 + 140)
  expect_equal(b1$qf, (210 + 290) + (140 + 160))
  expect_error(log_job(con, op, c("ADPERF|SAT-01|B|INT001", "ADPERF|PRD-07|B|INT001"), as.Date("2026-08-01")), "one well")
})

test_that("log algorithms merge into candidates checked against the completion history", {
  ds <- demo$ds
  c <- log_candidates(ds)[well == "SAT-01"]
  expect_equal(c[target == "INT003", n], 3)                       # all three algorithms agree
  expect_equal(c[cand_id == "L4905", conflict], "squeezed")        # algorithm C on the 2019 squeeze
  expect_match(c[cand_id == "L5015", conflict], "below plug")      # algorithm A below the plug at 5,010 ft
  st <- completion_state(ds$completions)
  expect_false(any(st$open$well == "SAT-01" & st$open$top_ft == 4905))   # perforated 2012, squeezed 2019
  s <- demo$op$summary
  expect_false(any(grepl("L4905|L5015", s$key)))                  # blocked candidates are not offered
  expect_true("ADPERF|PRD-07|-|L5300" %in% s$key)                  # found by two algorithms, no potential yet
  expect_true(is.na(s[key == "ADPERF|PRD-07|-|L5300", gain]))
  expect_match(s[key == "ADPERF|SAT-01|A|INT003", families], "U")
})

test_that("water offenders and potential gaps become isolation and re-perforation opportunities", {
  of <- water_offenders(demo$ds)[well == "SAT-01"]
  expect_equal(of[rank == 1, target], "A3")
  expect_equal(of[rank == 1, share], 230 / 360)
  expect_true("WSO|SAT-01|A|A3" %in% demo$op$summary$key)
  pg <- potential_gaps(demo$ds)
  expect_equal(pg[target == "INT002", gap], 75)
  r <- demo$op$summary[key == "REPERF|SAT-01|C|INT002"]
  expect_equal(r$gain, 75); expect_equal(r$gain_src, "POTENTIAL")
})

test_that("a job combines intervals, isolation, cost lookup and the lift trigger", {
  st <- default_settings
  k <- c("ADPERF|SAT-01|B|INT001", "ADPERF|SAT-01|A|INT003", "ADPERF|SAT-01|C|INT005", "WSO|SAT-01|A|A3")
  p <- job_proposal(demo$op, k, demo$res, max(demo$res$months), st, als_change = TRUE)
  expect_equal(unname(p$qo["Base"]), 210 + 140 + 90 - 4)          # intervals add up, the isolation removes its oil
  expect_equal(p$qw, 290 + 160 + 310 - 230)
  expect_equal(p$cost_usd, 190000 + 3 * 48000 + 70000 + 200000)   # rig + items + lift change, depth band > 5,000 ft
  expect_match(paste(p$triggers, collapse = " "), "ESP run life")
  expect_false(p$lift$over)
  j <- score_jobs(data.table::data.table(risked_np12 = c(100, 100), cost_usd = c(1, 1), unc = c(0, 0), triggers = c("", "ESP run life 96 %")), st)
  expect_gt(j$score[2], j$score[1])                                # opportunity bonus
})

test_that("engineers propose, a lead approves, and only allowed transitions happen", {
  con <- store_open(tempfile(fileext = ".sqlite")); on.exit(DBI::dbDisconnect(con))
  old <- Sys.getenv("WF_LEADS"); Sys.setenv(WF_LEADS = "lead1"); on.exit(Sys.setenv(WF_LEADS = old), add = TRUE)
  k <- c("ADPERF|SAT-01|B|INT001", "WSO|SAT-01|A|A3")
  id <- job_propose(con, demo$op, k, demo$res, max(demo$res$months), default_settings, "eng1")
  expect_equal(store_job(con, id)$status, "proposed")
  expect_error(job_set_status(con, id, "approved", "eng1", "", demo$op, demo$res), "lead")
  expect_error(job_set_status(con, id, "executed", "lead1", "", demo$op, demo$res, as.Date("2026-08-01")), "cannot move")
  job_set_status(con, id, "approved", "lead1", "go", demo$op, demo$res)
  expect_equal(store_job(con, id)$approved_by, "lead1")
  expect_false(is.null(store_frozen(con, paste0("JOB:", id))))
  job_set_status(con, id, "executed", "eng1", "", demo$op, demo$res, as.Date("2026-08-01"))
  s <- apply_states(demo$op$summary, store_states(con))[well == "SAT-01"]
  expect_setequal(s[status == "executed", key], k)
  expect_true(all(s[!key %in% k, as.character(status)] == s[!key %in% k, auto_status]))
  expect_equal(store_job_history(con, id)$to_status, c("proposed", "approved", "executed"))
})

test_that("opportunity flow: screening -> candidate -> in a job -> executed, dismissed aside", {
  con <- store_open(tempfile(fileext = ".sqlite")); on.exit(DBI::dbDisconnect(con))
  s0 <- demo$op$summary
  scr <- s0[auto_status == "screening_only", key][1]
  store_set_state(con, scr, status = "candidate", comment = "promoted: offset well responded")
  store_set_state(con, "ADPERF|SAT-01|D|INT006", status = "dismissed", comment = "thin")
  k <- c("ADPERF|SAT-01|B|INT001", "WSO|SAT-01|A|A3")
  id <- job_propose(con, demo$op, k, demo$res, max(demo$res$months), default_settings, "eng1")
  st <- function() apply_states(s0, store_states(con), store_job_map(con))
  s <- st()
  expect_equal(as.character(s[key == scr, status]), "candidate")
  expect_equal(as.character(s[key == "ADPERF|SAT-01|D|INT006", status]), "dismissed")
  expect_true(all(s[key %in% k, status] == "in_job")); expect_true(all(s[key %in% k, job_id] == id))
  expect_equal(as.character(s[key == "ADPERF|SAT-01|A|INT003", status]), "candidate")      # not picked: stays identified
  job_set_status(con, id, "rejected", "lead", "not now", demo$op, demo$res)
  expect_true(all(st()[key %in% k, status] == "candidate"))                                  # a rejected job releases them
  job_set_status(con, id, "proposed", "eng1", "again", demo$op, demo$res)
  job_set_status(con, id, "approved", "lead", "go", demo$op, demo$res)
  job_set_status(con, id, "executed", "eng1", "", demo$op, demo$res, as.Date("2026-08-01"))
  expect_true(all(st()[key %in% k, status] == "executed"))
  expect_setequal(levels(s$status), c("screening_only", "candidate", "in_job", "executed", "dismissed"))
})

test_that("trigger scan flags the wells to intervene at once", {
  t <- all_triggers(demo$res, max(demo$res$months), default_settings)
  expect_true(any(t$well == "SAT-01" & grepl("ESP run life", t$trigger)))
  expect_true(any(t$well == "SAT-05" & t$trigger == "well down"))
  expect_true(any(t$well == "PRD-09" & grepl("failures", t$trigger)))
})
