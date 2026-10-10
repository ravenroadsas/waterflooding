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
