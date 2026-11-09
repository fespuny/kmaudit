###############################################################
# Censoring Stress Test
#
# Archived input:
#   paper/data/censoring_stress_test_results.csv.gz
#
# Generated outputs:
#   paper/data/source/output/tableB3.csv
#   paper/figures/output/figure4.png
#   paper/figures/output/figureB1.png
###############################################################

library(survival)
library(ggplot2)
library(patchwork)
library(cowplot)

RUN_SIMULATION <- FALSE
SEED <- 42

N_REPLICATIONS <- 2000L
N_COHORT <- 1000L
T_GRID <- 1:5

lambda_interest <- 0.15
lambda_competing <- 0.10
lambda_total <- lambda_interest + lambda_competing

data_dir <- file.path("paper", "data")
out_dir <- file.path("paper", "data", "source", "output")
fig_dir <- file.path("paper", "figures", "output")
results_file <- file.path(data_dir, "censoring_stress_test_results.csv.gz")

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

true_cif1 <- lambda_interest / lambda_total * (1 - exp(-lambda_total * T_GRID)) 
true_cif2 <- lambda_competing / lambda_total * (1 - exp(-lambda_total * T_GRID))

true_survival <- exp(-lambda_total * T_GRID)

###############################################################
# Censoring regimes
###############################################################

REGIMES <- list(
  "No censoring"                     = list("uncensored", FALSE, NA),
  "Administrative only"              = list("admin", TRUE, NA),
  "Moderate random"                  = list("uncensored", FALSE, 0.20),
  "Administrative + moderate random" = list("admin", TRUE, 0.20),
  "Administrative + heavy random"    = list("admin", TRUE, 0.80)
)

regime_order <- names(REGIMES)

censor_cols <- c(
  "No censoring"                     = "black",
  "Administrative only"              = "#1f4e79",
  "Moderate random"                  = "#33a3dc",
  "Administrative + moderate random" = "#f0ad4e",
  "Administrative + heavy random"    = "#d62728"
)

###############################################################
# Monte Carlo simulation
###############################################################

run_censoring_stress_test <- function() {
  
  set.seed(SEED)
  
  generate_uncensored <- function() {
    data.frame( t_interest = rexp(N_COHORT, lambda_interest),
                t_competing = rexp(N_COHORT, lambda_competing), c_admin = Inf )
  }
  
  generate_admin <- function() {
    data.frame( t_interest = rexp(N_COHORT, lambda_interest),
                t_competing = rexp(N_COHORT, lambda_competing), 
                c_admin = 10 - runif(N_COHORT, 0, 5) )
  }
  
  apply_censoring <- function(latent, use_admin, lambda_cens) {
    n <- nrow(latent)
    c_admin <- if (use_admin) latent$c_admin else rep(Inf, n)
    c_random <- if (is.na(lambda_cens)) rep(Inf, n) else rexp(n, lambda_cens)
    
    censor_time <- pmin(c_admin, c_random)
    event_time <- pmin(latent$t_interest, latent$t_competing)
    
    data.frame(
      time = pmin(event_time, censor_time),
      status = ifelse( event_time <= censor_time,
                       ifelse(latent$t_interest < latent$t_competing, 1L, 2L), 0L )
    )
  }
  
  get_survival <- function(fit) {
    approx( c(0, fit$time), c(1, fit$surv), xout = T_GRID, 
            method = "constant", f = 0, rule = 2 )$y
  }
  
  evaluate <- function(data, regime, replication) {
    
    s_km1 <- get_survival( survfit(Surv(time, status == 1) ~ 1, data = data) )
    
    s_km2 <- get_survival( survfit(Surv(time, status == 2) ~ 1, data = data) )
    
    s_pl <- get_survival( survfit(Surv(time, status != 0) ~ 1, data = data) )
    
    b_total <- ((1 - s_km1) - true_cif1) + ((1 - s_km2) - true_cif2)
    
    tod <- s_pl - s_km1 - s_km2 + 1 
    diagnostic_error <- tod - b_total
    survival_error <- s_pl - true_survival
    
    data.frame(
      Replication = replication,
      Regime = regime,
      Time = T_GRID,
      Btotal = b_total,
      TOD = tod,
      DiagnosticError = diagnostic_error,
      SurvivalError = survival_error,
      IdentityError = diagnostic_error - survival_error
    )
  }
  
  output <- vector("list", N_REPLICATIONS * length(REGIMES))
  k <- 1L
  pb <- txtProgressBar(min = 0, max = N_REPLICATIONS, style = 3)
  
  for (i in seq_len(N_REPLICATIONS)) {
    
    latent_unc <- generate_uncensored()
    latent_admin <- generate_admin()
    
    for (regime in names(REGIMES)) {
      
      cfg <- REGIMES[[regime]]
      latent <- if (cfg[[1]] == "uncensored") latent_unc else latent_admin
      
      data <- apply_censoring( latent, use_admin = cfg[[2]], lambda_cens = cfg[[3]] )
      
      output[[k]] <- evaluate(data, regime, i)
      k <- k + 1L
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  do.call(rbind, output)
}

###############################################################
# Load existing results or regenerate
###############################################################

if (RUN_SIMULATION || !file.exists(results_file)) {
  message(if (RUN_SIMULATION)
    "Regenerating censoring stress-test results..."
    else
      "No saved results found; running censoring stress test..."
  )
  
  results <- run_censoring_stress_test()
  write.csv(results, gzfile(results_file), row.names = FALSE)
  message("Saved simulation results to ", results_file)
  
} else {
  message("Using existing simulation results: ", results_file)
  results <- read.csv(results_file)
}

###############################################################
# Validate results
###############################################################

required <- c( "Replication", "Regime", "Time", "Btotal", "TOD", 
               "DiagnosticError", "SurvivalError", "IdentityError"
)

stopifnot(
  all(required %in% names(results)),
  nrow(results) == N_REPLICATIONS * length(REGIMES) * length(T_GRID)
)

results$Regime <- factor(results$Regime, levels = regime_order)

cat( "Censoring stress test:", length(unique(results$Replication)), "replications,",
     length(unique(results$Regime)), "settings,", nrow(results), "evaluations.\n" )

###############################################################
# Table B3
###############################################################

split_results <- split( results, list(results$Regime, results$Time), drop = TRUE )

tableB3 <- do.call(rbind, lapply(split_results, function(x) {
  
  sd_diag <- sd(x$DiagnosticError)
  sd_surv <- sd(x$SurvivalError)
  
  data.frame(
    Setting = as.character(x$Regime[1]),
    Time = x$Time[1],
    Mean_Total_Overestimation = mean(x$Btotal),
    SD_Btotal = sd(x$Btotal),
    Mean_Diagnostic_Error = mean(x$DiagnosticError),
    SD_Diagnostic_Error = sd_diag,
    Mean_Survival_Error = mean(x$SurvivalError),
    SD_Survival_Error = sd_surv,
    Max_SD_Difference = abs(sd_diag - sd_surv),
    Correlation_Error = cor(x$DiagnosticError, x$SurvivalError),
    Mean_Identity_Error = mean(x$IdentityError),
    Mean_Absolute_Identity_Error = mean(abs(x$IdentityError)),
    Max_Identity_Error = max(abs(x$IdentityError))
  )
}))

tableB3$Setting <- factor(tableB3$Setting, levels = regime_order)
tableB3 <- tableB3[order(tableB3$Setting, tableB3$Time), ]
tableB3$Setting <- as.character(tableB3$Setting)
rownames(tableB3) <- NULL

write.csv( tableB3, file.path(out_dir, "tableB3.csv"), row.names = FALSE )

###############################################################
# Figure 4
###############################################################

box_df <- results
box_df$Time <- factor(box_df$Time)

sd_df <- aggregate( Btotal ~ Regime + Time, data = results, FUN = sd )

names(sd_df)[3] <- "SD"

p_box <- ggplot( box_df, aes(x = Time, y = Btotal, fill = Regime, colour = Regime) ) +
  geom_boxplot( width = 0.55, position = position_dodge(width = 0.75), 
                outlier.shape = NA, alpha = 0.75 ) +
  scale_fill_manual(values = censor_cols, name = "Censoring setting:") +
  scale_colour_manual(values = censor_cols, name = "Censoring setting:") +
  scale_y_continuous( breaks = seq(-0.2, 0.8, 0.2), 
                      labels = sprintf("%.1f", seq(-0.2, 0.8, 0.2)), expand = c(0, 0) ) +
  coord_cartesian(ylim = c(-0.2, 0.8)) +
  theme_bw() +
  labs(x = "Years", y = expression(B[total])) +
  theme( axis.title = element_text(size = 14), axis.text = element_text(size = 12),
         panel.grid.minor = element_blank() ) +
  annotate( "text", x = 0.6, y = 0.72, 
            label = "A. Distribution of Total Overestimation",
            fontface = "bold", size = 4.8, hjust = 0 )

p_sd <- ggplot(sd_df, aes(x = Time, y = SD, colour = Regime)) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = censor_cols, name = "Censoring setting:") +
  scale_x_continuous( limits = c(1, 5), breaks = 1:5, labels = as.character(1:5) ) +
  scale_y_continuous( limits = c(0, 0.125), breaks = seq(0, 0.125, 0.025), 
                      labels = sprintf("%.3f", seq(0, 0.125, 0.025)), expand = c(0, 0) ) +
  theme_bw() +
  labs(x = "Years", y = expression(SD(B[total]))) +
  theme( axis.title = element_text(size = 14), axis.text = element_text(size = 12),
    panel.grid.minor = element_blank() ) +
  annotate( "text", x = 1, y = 0.115, label = "B. Variability of Total Overestimation",
            fontface = "bold", size = 4.8, hjust = 0 )

top_row <- ( p_box + theme(legend.position = "none") ) + 
  ( p_sd + theme(legend.position = "none") ) +
  plot_layout(widths = c(1, 1))

legend <- get_legend(
  p_sd +
    guides( colour = guide_legend( title = "Censoring setting:", nrow = 1, byrow = TRUE,
                                   override.aes = list(linewidth = 0.8, size = 2) ) ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.background = element_blank(),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 11),
      legend.key = element_blank(),
      legend.key.width = unit(0.7, "cm"),
      legend.key.height = unit(0.4, "cm"),
      legend.spacing.x = unit(0.18, "cm"),
      plot.margin = margin(t = -15, r = 0, b = 0, l = 0)
    )
)

figure4 <- plot_grid( top_row, legend, ncol = 1, rel_heights = c(1, 0.10) )

ggsave( file.path(fig_dir, "figure4.png"), figure4,
        width = 12, height = 5, units = "in", dpi = 600, bg = "white" )

###############################################################
# Figure B1: Product Limit Error Equivalence
###############################################################

error_sd <- do.call(rbind, lapply(split_results, function(x) {
  data.frame(
    Regime = as.character(x$Regime[1]),
    Time = x$Time[1],
    DiagnosticSD = sd(x$DiagnosticError),
    ProductLimitSD = sd(x$SurvivalError)
  )
}))

error_sd$Regime <- factor(error_sd$Regime, levels = regime_order)

figureB1 <- ggplot(error_sd, aes(Time)) +
  geom_line( aes(y = DiagnosticSD, colour = Regime), linewidth = 0.85 ) +
  geom_point( aes(y = ProductLimitSD, colour = Regime), shape = 1, size = 2.5, stroke = 1.1 ) +
  scale_colour_manual(values = censor_cols, name = NULL) +
  scale_y_continuous( limits = c(0, 0.085), breaks = seq(0, 0.08, 0.02),
                      labels = sprintf("%.2f", seq(0, 0.08, 0.02)), expand = c(0, 0) ) +
  scale_x_continuous( limits = c(1, 5), breaks = 1:5, 
                      labels = as.character(1:5), expand = c(0.02, 0) ) +
  theme_bw() +
  labs(x = "Years", y = "SD of estimation error") +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.box.just = "center",
    legend.text = element_text(size = 16),
    legend.key.width = unit(1, "cm"),
    legend.spacing.x = unit(0.15, "cm")
  ) +
  guides(colour = guide_legend(title = NULL, nrow = 2, byrow = TRUE)) +
  annotate( "text", x = 1.10, y = 0.079, 
            label = "Finite Sample Validation of Product Limit Error Equivalence",
            fontface = "bold", size = 7, hjust = 0 ) +
  annotate( "rect", xmin = 1.10, xmax = 2.15, ymin = 0.066, ymax = 0.075,
            fill = alpha("white", 0.92), colour = "black", linewidth = 0.20 ) +
  annotate( "segment", x = 1.15, xend = 1.32, y = 0.0723, yend = 0.0723, linewidth = 0.45 ) +
  annotate( "text", x = 1.38, y = 0.0723, 
            label = "Diagnostic error", hjust = 0, size = 4.8 ) +
  annotate( "point", x = 1.235, y = 0.0682, shape = 1, size = 2.2, stroke = 0.90 ) +
  annotate( "text", x = 1.38, y = 0.0682, 
            label = "Product limit error", hjust = 0, size = 4.8 )

ggsave( file.path(fig_dir, "figureB1.png"), figureB1, width = 12, height = 8,
        units = "in", dpi = 600, bg = "white" )

###############################################################
# Output
###############################################################

cat("\nTable B3\n")
print(tableB3, row.names = FALSE, digits = 4)

cat(
  "\nMaximum identity residual:",
  format(max(abs(results$IdentityError)), scientific = TRUE),
  "\nMaximum SD difference:",
  format(max(tableB3$Max_SD_Difference), scientific = TRUE),
  "\nMinimum correlation:",
  format(min(tableB3$Correlation_Error), digits = 16),
  "\n"
)