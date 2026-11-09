###############################################################
# Real-data benchmark analysis
#
# Output:
#   paper/data/source/output/benchmark_results.csv
###############################################################

library(kmaudit)
library(survival)
library(cmprsk)
library(MASS)

source(file.path("paper", "analysis", "benchmark_helpers.R"))

output_dir <- file.path("paper", "data", "source", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

specifications <- list(
  prepare_pbc(),
  prepare_melanoma(),
  prepare_udca(),
  prepare_myeloid(),
  prepare_mgus(),
  prepare_transplant()
)

benchmark_results <- do.call(rbind, lapply(specifications, function(x)
  benchmark_metrics(x$data, x$interest, x$competing, x$horizon, x$name)
))

write.csv( benchmark_results, file.path(output_dir, "benchmark_results.csv"),
           row.names = FALSE )

print(benchmark_results, row.names = FALSE, digits = 4)