# Reproducing the manuscript

This directory contains the code and archived analysis data required to
reproduce the figures, tables, model fits, and numerical validation results
reported in the accompanying methodological article.

## Reproduce all results

From the root of the `kmaudit` repository:

```r
devtools::load_all()
source("paper/reproduce.R")
```

Generated figures are written to:

```text
paper/figures/output/
```

Generated machine-readable source data and tables are written to:

```text
paper/data/source/output/
```

Both directories contain derived outputs and are excluded from version
control.

## Clean generated results

To remove all generated reproduction outputs:

```r
source("paper/clean_reproduce.R")
```

This removes only generated outputs. The archived analysis inputs

```text
paper/data/simulation_results.csv.gz
paper/data/censoring_stress_test_results.csv.gz
```

are retained.

## Reproduce individual analyses

| Manuscript output | Script |
|---|---|
| Figure 1 | `paper/figures/figure1.R` |
| Figure 2 | `paper/figures/figure2_mgus.R` |
| Figure 3 | `paper/figures/figure3.R` |
| Table 1 | `paper/analysis/table1.R` |
| Tables 2, B1 and B2 | `paper/analysis/characterise_cause_specific_overestimation.R` |
| Figure 4, Figure B1 and Table B3 | `paper/analysis/censoring_stress_test.R` |

The real-data benchmark preparation shared by Table 1 and the external
validation analyses is defined in `paper/analysis/benchmark_helpers.R` and
executed by `paper/analysis/run_benchmarks.R`.

## Archived simulation results

`paper/data/simulation_results.csv.gz` contains the released simulation-level
results used for Figure 3 and the empirical characterisation reported in
Tables 2, B1 and B2.

`paper/data/censoring_stress_test_results.csv.gz` contains the Monte Carlo
results used for Figure 4, Figure B1 and Table B3.

Normal reproduction uses these archived results. The censoring stress test
can additionally be regenerated from the supplied code by setting:

```r
RUN_SIMULATION <- TRUE
```

in `paper/analysis/censoring_stress_test.R`.

## Reproducibility boundary

The released materials reproduce the reported manuscript figures, tables,
model fits, and validation results.

The upstream calibration, response-space targeting, occupancy-weighted
sampling, and optimisation infrastructure used to develop the simulation
design is not required for reproduction and is not included in the release.

## Licensing

See the repository-level `LICENSING.md` for the applicable software, data,
and documentation licences.