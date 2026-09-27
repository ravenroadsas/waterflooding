# Run with: Rscript tests/testthat.R   (from the repo root)
suppressPackageStartupMessages({ library(testthat); library(data.table) })
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
