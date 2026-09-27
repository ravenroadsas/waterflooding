# Regenerate the demo dataset: Rscript scripts/make_demo.R
suppressPackageStartupMessages(library(data.table))
for (f in list.files("R", full.names = TRUE)) source(f)
write_demo_data("data/demo")
write_template("data/wf_template.xlsx")
cat("Demo data written to data/demo, template to data/wf_template.xlsx\n")
