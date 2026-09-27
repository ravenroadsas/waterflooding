# One-off: install the R packages the app needs
pkgs <- c("shiny", "bslib", "plotly", "DT", "readxl", "data.table", "writexl", "RSQLite", "DBI", "cluster", "httr2", "jsonlite", "testthat")
install.packages(setdiff(pkgs, rownames(installed.packages())))
