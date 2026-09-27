# One-off: install the R packages the app needs
pkgs <- c("shiny", "bslib", "plotly", "DT", "readxl", "data.table", "writexl", "testthat")
install.packages(setdiff(pkgs, rownames(installed.packages())))
