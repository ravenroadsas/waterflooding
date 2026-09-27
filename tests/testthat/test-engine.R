root <- normalizePath(file.path(getwd(), "..", ".."))
if (!dir.exists(file.path(root, "data/demo"))) root <- getwd()
# v2 engine tests: formulas from the methodology on a tiny hand-checkable field
mini <- function(dated = FALSE, profile = TRUE) {
  months <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)
  wells <- rbind(
    data.table(Well = "PR1", Date = months, BOPD = 100, BWPD = 50, BWIPD = 0),
    data.table(Well = "IN1", Date = months, BOPD = 0, BWPD = 0, BWIPD = c(rep(0, 6), rep(300, 18))))
  alloc <- data.table(Well = c("PR1", "PR1", "IN1"), Pattern = c("P1", "P2", "P1"), Date = as.Date(NA), Coeff = c(0.5, 0.5, 1))
  if (dated) alloc <- rbind(alloc, data.table(Well = c("PR1", "PR1"), Pattern = c("P1", "P2"), Date = months[13], Coeff = c(1, 0)))
  vol <- data.table(Pattern = c("P1", "P1", "P2"), Reservoir = "R1", Sand = c("A", "B", "A"),
                    STOIIP = c(1e6, 1e6, 2e6), HCPV = c(1.2e6, 1.2e6, 2.4e6), H = 10, Phi = 0.2, Sw = 0.25)
  fluids <- data.table(Reservoir = "R1", Bo = 1.2, Bw = 1.0, visco = 2, viscw = 0.5)
  raw <- list(wells = wells, alloc = alloc, vol = vol, fluids = fluids)
  if (profile) raw$injsand <- data.table(Well = "IN1", Sand = c("A", "B"), Date = months[7], BWIPD = c(225, 75))
  raw$injsand_status <- data.table(Well = "IN1", Sand = c("A", "B"), Date = months[7], VRF = c(8, 4), Cobb = c(150, 150))
  build_dataset(raw, "mini")
}

test_that("allocation conserves volumes and dated coefficients step", {
  res <- run_engine(mini(dated = TRUE))
  expect_equal(sum(res$pm$oil), sum(res$well$oil))
  al <- res$alloc[pattern == "P1" & well == "PR1"]
  expect_equal(al[date < as.Date("2021-01-01"), unique(coeff)], 0.5)
  expect_equal(al[date >= as.Date("2021-01-01"), unique(coeff)], 1)
})

test_that("maturity variables follow the methodology definitions", {
  res <- run_engine(mini())
  s <- aggregate_level(res, "pattern")[entity == "P1"][.N]
  hcpv <- 2.4e6
  expect_equal(s$dwi, s$cum_winj * 1.0 / hcpv)
  expect_equal(s$rf, s$cum_oil * 1.2 / hcpv)
  expect_equal(s$dwp, s$cum_water * 1.0 / hcpv)
  expect_equal(s$dtp, (s$cum_oil * 1.2 + s$cum_water) / hcpv)
  # Sec RF: oil since the first injection month (month 7)
  np0 <- 6 * sum(50 * days_in_month(seq(as.Date("2020-01-01"), by = "month", length.out = 6))) / 6
  expect_equal(s$sec_rf, (s$cum_oil - np0) * 1.2 / hcpv, tolerance = 1e-9)
})

test_that("velocity variables: TP, IWR and utilization", {
  res <- run_engine(mini())
  s <- aggregate_level(res, "pattern")[entity == "P1"]
  l <- s[.N]
  expect_equal(l$tp, 100 * 300 * l$days / 2.4e6 * 365 / l$days, tolerance = 1e-9)   # %HCPV/yr
  expect_equal(l$tp12, mean(tail(s$tp, 12)))
  l12 <- tail(s, 12)
  expect_equal(l$iwr12, sum(l12$winj_rb) / sum(l12$oil_rb + l12$water_rb))
  l6 <- tail(s, 6)
  expect_equal(l$util6, sum(l6$winj_rb) / sum(l6$oil_rb))
})

test_that("InjSand profile shares split injection by unit and sum to the well total", {
  res <- run_engine(mini())
  pu <- res$units$pattern_sand[pattern == "P1" & date == max(date)]
  expect_equal(sum(pu$winj), 300 * pu$days[1])
  expect_equal(pu[sand == "A", winj] / sum(pu$winj), 0.75)
  expect_equal(pu[sand == "A", dwi], pu[sand == "A", cum_winj_rb] / 1.2e6)
  expect_equal(pu[sand == "A", cobb], 150)
  # without a profile the split falls back to HCPV
  res2 <- run_engine(mini(profile = FALSE))
  pu2 <- res2$units$pattern_sand[pattern == "P1" & date == max(date)]
  expect_equal(pu2[sand == "A", winj] / sum(pu2$winj), 0.5)
})

test_that("OPR / WPR use the prototype at the same DWI", {
  ds <- mini()
  ds$prototypes <- data.table(prototype = "T", version = "v1", dwi = c(0, 1, 2), sec_rf = c(0, 0.2, 0.3),
                              dwp = c(0, 0.5, 1.2), util = NA_real_, wor = NA_real_)
  res <- run_engine(ds, utils::modifyList(default_settings, list(judge_dwi = 0)))
  s <- aggregate_level(res, "pattern", st = utils::modifyList(default_settings, list(judge_dwi = 0)))[entity == "P1"][.N]
  expect_equal(s$opr, s$sec_rf / (0.2 * s$dwi), tolerance = 1e-6)
  expect_equal(s$wpr, s$dwp / (0.5 * s$dwi), tolerance = 1e-6)
})

test_that("Simmons & Falls fit recovers known parameters", {
  x <- seq(0.05, 2, by = 0.05); y <- 0.25 * (1 - exp(-1.3 * x))
  f <- sf_fit(x, y)
  expect_equal(f$A, 0.25, tolerance = 1e-3); expect_equal(f$C, 1.3, tolerance = 1e-2)
  expect_equal(sf_remaining(f, 1), 0.25 * exp(-1.3), tolerance = 1e-3)
})

test_that("Welge ED is piston-like before breakthrough and inverts from water cut", {
  p <- list(swc = 0.2, sor = 0.3, krw_or = 0.3, kro_wc = 0.9, nw = 2, no = 2, mu_o = 2, mu_w = 0.5)
  f <- ed_fun(p, 0.2)
  expect_equal(f(0.1), 0.1, tolerance = 1e-6)
  e1 <- ed_from_fw(p, 0.2, 0.8); e2 <- ed_from_fw(p, 0.2, 0.95)
  expect_true(e2 > e1 && e2 <= 0.5 / 0.8 + 1e-9)
})

test_that("column aliases, sheet names and date shapes are understood", {
  expect_equal(sheet_key("Patterns_Mat"), "patterns_mat")
  expect_equal(sheet_key("InjSand_status"), "injsand_status")
  expect_equal(sheet_key("Wells"), "wells")
  st <- standardize_table(data.frame(Pattern = "P1", Reservoir = "R", Sand = "A", STOIIP = "1,000", HCPV = 1200), "vol")
  expect_equal(st$data$stoiip, 1000)
  st <- standardize_table(data.frame(Well = "W", Date = c("2021-03", "15/04/2021"), BOPD = 1, BWPD = 1, BWIPD = 0), "wells")
  expect_equal(st$data$date, as.Date(c("2021-03-01", "2021-04-01")))
})

test_that("demo field: opportunities, clusters, reconciliation and store work end to end", {
  ds <- build_dataset(read_dataset_dir(file.path(root, "data/demo")), "demo")
  expect_false(any(ds$issues$severity == "error"))
  res <- run_engine(ds); s <- aggregate_level(res, "pattern"); a <- max(res$months)
  op <- generate_opportunities(res, s, a)
  expect_true(all(c("A", "B", "C", "E", "F") %in% op$summary$type))
  expect_true("B|P13|-" %in% op$summary$key)                     # injectivity loss planted in the demo
  expect_true(all(op$summary[auto_status == "candidate", n_fam] >= 2))
  sn <- pattern_snapshot(res, s, a)
  ml <- ml_cluster(sn); expect_true(ml$k >= 2 && nrow(ml$assign) == 15)
  rc <- reconcile(res, s); expect_true(all(rc$summary[variable %in% c("Np", "Nwi"), within] > 0.99))
  con <- store_open(tempfile(fileext = ".sqlite"))
  store_set_state(con, "E|P02|A", status = "validated_candidate", checks = c("a", "b"))
  expect_equal(store_checks(con, "E|P02|A"), c("a", "b"))
  s2 <- apply_states(op$summary, store_states(con))
  if ("E|P02|A" %in% s2$key) expect_equal(as.character(s2[key == "E|P02|A", status]), "validated_candidate")
  DBI::dbDisconnect(con)
})
