mini <- function(coef_p1 = 0.5, dated = FALSE) {
  months <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  prod <- rbind(
    data.table(well = "PR1", date = months, bopd = 100, bwpd = 10, bwipd = 0),
    data.table(well = "IN1", date = months, bopd = 0, bwpd = 0, bwipd = c(rep(0, 3), rep(200, 9)))
  )
  alloc <- data.table(pattern = c("P1", "P2", "P1"), well = c("PR1", "PR1", "IN1"),
                      coefficient = c(coef_p1, 1 - coef_p1, 1), date = as.Date(NA))
  if (dated) alloc <- rbind(alloc, data.table(pattern = c("P1", "P2"), well = "PR1", coefficient = c(1, 0), date = months[7]))
  stooip <- data.table(pattern = c("P1", "P1", "P2"), sand = c("A", "B", "A"), stooip = c(1e6, 1e6, 2e6),
                       swi = 0.25, boi = 1.2)
  fluids <- data.table(sand = "*", bo = 1.2, bw = 1.0, mu_o = 2, mu_w = 0.5, swc = 0.2, sor = 0.3,
                       krw_or = 0.3, kro_wc = 0.9, nw = 2, no = 2)
  petro <- data.table(well = c("PR1", "PR1"), sand = c("A", "B"), net_pay = c(30, 10), permeability = c(100, 100),
                      porosity = 0.2, sw = 0.25)
  hier <- data.table(field = "F", block = "B1", pattern = c("P1", "P1", "P2"), well = c("PR1", "IN1", "PR1"),
                     well_type = c("PRODUCER", "INJECTOR", "PRODUCER"), x = NA_real_, y = NA_real_)
  raw <- list(production = prod, allocation = alloc, stooip = stooip, fluids = fluids, petrophysics = petro, hierarchy = hier)
  build_dataset(raw, "mini")
}

test_that("allocation conserves volumes when coefficients add to one", {
  res <- run_engine(mini())
  expect_equal(sum(res$ps$oil), sum(res$well$oil))
  expect_equal(sum(res$ps$winj), sum(res$well$winj))
})

test_that("areal coefficient and kh split are applied", {
  res <- run_engine(mini(0.3))
  p1 <- res$ps[pattern == "P1", sum(oil)]
  expect_equal(p1, 0.3 * sum(res$well[well == "PR1", oil]))
  # kh split 30*100 : 10*100 -> 75 % sand A in P1
  a <- res$ps[pattern == "P1" & sand == "A", sum(oil)]
  expect_equal(a / p1, 0.75)
})

test_that("dated allocation steps from its month on", {
  res <- run_engine(mini(0.5, dated = TRUE))
  al <- res$alloc[pattern == "P1" & well == "PR1"]
  expect_equal(al[date < as.Date("2020-07-01"), unique(coefficient)], 0.5)
  expect_equal(al[date >= as.Date("2020-07-01"), unique(coefficient)], 1)
})

test_that("dimensionless variables are consistent", {
  res <- run_engine(mini())
  s <- aggregate_level(res, "pattern")[entity == "P1"][.N]
  expect_equal(s$rf, s$cum_oil / 2e6)
  expect_equal(s$hcpvi, s$cum_winj_rb / (2e6 * 1.2))
  expect_equal(s$pvi, s$hcpvi * (1 - 0.25), tolerance = 1e-9)
  expect_equal(s$cum_vrr, s$cum_winj_rb / (s$cum_oil_rb + s$cum_water_rb))
  expect_true(s$wc > 0 && s$wc < 1)
})

test_that("Welge curve: piston-like before breakthrough, bounded by movable oil", {
  p <- list(swc = 0.2, sor = 0.3, krw_or = 0.3, kro_wc = 0.9, nw = 2, no = 2, mu_o = 2, mu_w = 0.5)
  f <- ed_fun(p, 0.2)
  expect_equal(f(0.1), 0.1, tolerance = 1e-6)          # RF = HCPVI before breakthrough
  expect_lte(f(20), (1 - 0.3 - 0.2) / (1 - 0.2) + 1e-9)
  expect_true(all(diff(f(seq(0, 5, 0.1))) >= -1e-12))  # monotonic
})

test_that("column aliases and date shapes are understood", {
  df <- data.frame(Pozo = c("W1", "W1"), Fecha = c("2021-03", "15/04/2021"), QO = c("1,000", "900"), BWPD = 1, BWIPD = 0)
  st <- standardize_table(df, "production")
  expect_equal(st$data$date, as.Date(c("2021-03-01", "2021-04-01")))
  expect_equal(st$data$bopd, c(1000, 900))
})

test_that("stages and quadrants classify sensibly", {
  expect_equal(as.character(classify_stage(c(0, 0.05, 0.3, 0.7, 1.5), c(0.1, 0.1, 0.3, 0.6, 0.9))),
               c("Primary", "Early response", "Developing", "Mature", "Late life"))
  expect_equal(as.character(classify_quadrant(c(0.2, 0.8, 0.8, 0.2), c(0.7, 0.7, 0.2, 0.2))),
               c("Accelerate", "Harvest", "Conformance", "Investigate"))
})

test_that("demo dataset runs end to end with diagnostics", {
  ds <- build_dataset(make_demo_data(), "demo")
  expect_false(any(ds$issues$severity == "error"))
  res <- run_engine(ds)
  sn <- build_snapshot(aggregate_level(res, "pattern"), max(res$months))
  expect_equal(nrow(sn$snap), 16)
  expect_true("INJ_STOPPED" %in% sn$flags$code)
  expect_true(all(sn$snap$rf < 0.7))
})
