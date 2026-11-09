###############################################################
# Table 1: Real-data competing-risks benchmarks
#
# Output:
#   paper/data/source/output/table1.csv
###############################################################

if (!exists("benchmark_results"))
  source(file.path("paper", "analysis", "run_benchmarks.R"))

output_dir <- file.path("paper", "data", "source", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

table1 <- data.frame(
  Dataset = toupper(benchmark_results$dataset),
  N = benchmark_results$n,
  Censoring = benchmark_results$censoring_frequency,
  `Competing Events` = benchmark_results$competing_frequency,
  `Order Index` = benchmark_results$oi,
  `KM Risk` = benchmark_results$km_risk,
  CIF = benchmark_results$cif,
  `KM--CIF` = benchmark_results$cause_specific_overestimation,
  TOD = benchmark_results$tod,
  check.names = FALSE
)

table1[, -(1:2)] <- round(table1[, -(1:2)], 3)

write.csv( table1, file.path(output_dir, "table1.csv"), row.names = FALSE )

print(table1, row.names = FALSE, digits = 3)