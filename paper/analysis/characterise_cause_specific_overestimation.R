###############################################################
# Empirical Characterisation of Cause-Specific Overestimation
#
# Outputs: Table 2, Table B1, Table B2
###############################################################

sim_file <- file.path("paper", "data", "simulation_results.csv.gz")
out_dir <- file.path("paper", "data", "source", "output")

if (!file.exists(sim_file)) stop("Missing ", sim_file)
if (!exists("benchmark_results"))
  source(file.path("paper", "analysis", "run_benchmarks.R"))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sim <- subset(read.csv(sim_file), OI_Defined)

validation <- data.frame(
  Dataset = toupper(benchmark_results$dataset),
  ObservedFrequency = benchmark_results$competing_frequency,
  GeneratedOrderingIndex = benchmark_results$oi,
  TOD = benchmark_results$tod,
  Observed = benchmark_results$cause_specific_overestimation
)

###############################################################
# Models
###############################################################

formulas <- list(
  frequency = Overestimation1 ~ ObservedFrequency,
  oi = Overestimation1 ~ GeneratedOrderingIndex,
  tod = Overestimation1 ~ TOD,
  frequency_oi_add = Overestimation1 ~ ObservedFrequency + GeneratedOrderingIndex,
  frequency_oi = Overestimation1 ~ ObservedFrequency * GeneratedOrderingIndex,
  tod_frequency_add = Overestimation1 ~ TOD + ObservedFrequency,
  tod_frequency = Overestimation1 ~ TOD * ObservedFrequency,
  tod_oi_add = Overestimation1 ~ TOD + GeneratedOrderingIndex,
  tod_oi = Overestimation1 ~ TOD * GeneratedOrderingIndex,
  additive = Overestimation1 ~ TOD + ObservedFrequency + GeneratedOrderingIndex,
  tod_plus_frequency_oi = Overestimation1 ~ TOD + ObservedFrequency * GeneratedOrderingIndex,
  tod_frequency_plus_oi = Overestimation1 ~ TOD * ObservedFrequency + GeneratedOrderingIndex,
  tod_oi_plus_frequency = Overestimation1 ~ TOD * GeneratedOrderingIndex + ObservedFrequency,
  full = Overestimation1 ~ TOD * ObservedFrequency * GeneratedOrderingIndex
)

labels <- c(
  frequency = "Frequency", oi = "OI", tod = "TOD",
  frequency_oi_add = "Frequency + OI",
  frequency_oi = "Frequency x OI",
  tod_frequency_add = "TOD + Frequency",
  tod_frequency = "TOD x Frequency",
  tod_oi_add = "TOD + OI",
  tod_oi = "TOD x OI",
  additive = "TOD + Frequency + OI",
  tod_plus_frequency_oi = "TOD + Frequency x OI",
  tod_frequency_plus_oi = "TOD x Frequency + OI",
  tod_oi_plus_frequency = "TOD x OI + Frequency",
  full = "TOD x Frequency x OI"
)

models <- lapply(formulas, lm, data = sim)
pred <- lapply(models, function(x) pmax(predict(x, validation), 0))

corr <- vapply(pred, function(x) cor(x, validation$Observed), numeric(1))
mae <- vapply(pred, function(x) mean(abs(x - validation$Observed)), numeric(1))

###############################################################
# Model results
###############################################################

terms <- c(
  Constant = "(Intercept)", Freq = "ObservedFrequency",
  OI = "GeneratedOrderingIndex", TOD = "TOD",
  Freq_x_OI = "ObservedFrequency:GeneratedOrderingIndex",
  TOD_x_Freq = "TOD:ObservedFrequency",
  TOD_x_OI = "TOD:GeneratedOrderingIndex",
  TOD_x_Freq_x_OI = "TOD:ObservedFrequency:GeneratedOrderingIndex"
)

get_coef <- function(model) {
  b <- coef(model)
  out <- setNames(rep(NA_real_, length(terms)), names(terms))
  hit <- terms %in% names(b)
  out[hit] <- b[terms[hit]]
  out
}

results <- data.frame(
  ModelID = names(models),
  Model = unname(labels[names(models)]),
  do.call(rbind, lapply(models, get_coef)),
  Adjusted_R2 = vapply(models, function(x) summary(x)$adj.r.squared, numeric(1)),
  Validation_Correlation = corr,
  Validation_MAE = mae,
  check.names = FALSE
)

###############################################################
# Table B1
###############################################################

B1cols <- c( "Model", "Constant", "Freq", "OI", "TOD", "Freq_x_OI", 
             "TOD_x_Freq", "TOD_x_OI", "TOD_x_Freq_x_OI","Adjusted_R2", 
             "Validation_Correlation", "Validation_MAE" )

tableB1 <- results[ , B1cols ]

names(tableB1) <- gsub( "_", " ", B1cols)

tableB1[-1] <- round(tableB1[-1], 3)

###############################################################
# Table 2
###############################################################

keep <- c( "frequency", "oi", "tod", "frequency_oi_add", "tod_frequency_add", 
           "tod_frequency", "tod_oi_add", "tod_oi", "additive", "full" )

table2 <- results[ match(keep, results$ModelID), 
                   c("Model", "Adjusted_R2", "Validation_Correlation", "Validation_MAE") ]

names(table2) <- c( "Linear Model predicting B_k", "Adjusted R2", 
                    "Validation Correlation", "Validation MAE" )

table2[-1] <- round(table2[-1], 3)

###############################################################
# Table B2
###############################################################

selected <- c( tod = "TOD", tod_oi = "TOD x OI", full = "TOD x Frequency x OI" )

tableB2 <- data.frame( Dataset = validation$Dataset, 
                       Observed = validation$Observed, check.names = FALSE )

for (id in names(selected)) {
  label <- selected[[id]]
  tableB2[[paste(label, "Prediction")]] <- pred[[id]]
  tableB2[[paste(label, "Absolute Error")]] <- abs(pred[[id]] - validation$Observed)
}

mae_row <- tableB2[1, ]
mae_row[] <- NA
mae_row$Dataset <- "Mean Absolute Error"

for (id in names(selected))
  mae_row[[paste(selected[[id]], "Absolute Error")]] <- mae[[id]]

tableB2 <- rbind(tableB2, mae_row)
tableB2[-1] <- round(tableB2[-1], 3)

###############################################################
# Save and print
###############################################################

write.csv(table2, file.path(out_dir, "table2.csv"), row.names = FALSE)
write.csv(tableB1, file.path(out_dir, "tableB1.csv"), row.names = FALSE, na = "")
write.csv(tableB2, file.path(out_dir, "tableB2.csv"), row.names = FALSE, na = "-")

cat("\nTable 2\n");  print(table2, row.names = FALSE)
cat("\nTable B1\n"); print(tableB1, row.names = FALSE)
cat("\nTable B2\n"); print(tableB2, row.names = FALSE, na.print = "-")
cat("\nCharacterisation based on", nrow(sim), "simulations with defined OI.\n")