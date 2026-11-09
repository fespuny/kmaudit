###############################################################
# Reproduce manuscript results
###############################################################

CLEAN_FIRST <- FALSE
if (CLEAN_FIRST) source("paper/clean_reproduce.R")

fig_dir <- "paper/figures/output"
out_dir <- "paper/data/source/output"
sim_file <- "paper/data/simulation_results.csv.gz"

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

source("paper/analysis/run_benchmarks.R")
source("paper/analysis/table1.R")

source("paper/figures/figure1.R")
source("paper/figures/figure2_mgus.R")

if (file.exists(sim_file)) {
  source("paper/figures/figure3.R")
  source("paper/analysis/characterise_cause_specific_overestimation.R")
} else {
  warning("Skipping Figure 3 and Tables 2/B1/B2: missing ", sim_file)
}

source("paper/analysis/censoring_stress_test.R")

capture.output( sessionInfo(), file = file.path(out_dir, "reproduction_sessionInfo.txt") )

message("Manuscript reproduction complete.")