###############################################################
# Figure 1: Pepe-Mori event-ordering example
#
# Generated outputs:
#   paper/figures/output/figure1_panel_a.png
#   paper/figures/output/figure1_panel_b.png
#   paper/data/source/output/figure1_scenario_a.csv
#   paper/data/source/output/figure1_scenario_b.csv
#   paper/data/source/output/figure1_panel_a.csv
#   paper/data/source/output/figure1_panel_b.csv
###############################################################

library(cmprsk)
library(survival)
library(ggsurvfit)
library(ggplot2)

set.seed(11235813)

n <- 100
target_time <- 6
relapse_rate <- 0.30
death_rate <- 0.40
censor_rate <- 0.30

fig_dir <- file.path("paper", "figures", "output")
data_dir <- file.path("paper", "data", "source", "output")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################
# Common event times
###############################################################

base <- data.frame( id = seq_len(n), time = sort(runif(n, 0, target_time)), status = 0L )

###############################################################
# Scenario A: deaths before relapses
###############################################################

scenario_a <- base

scenario_a$status[ scenario_a$time < quantile(scenario_a$time, relapse_rate + death_rate) ] <- 1L

scenario_a$status[ scenario_a$time < quantile(scenario_a$time, death_rate) ] <- 2L

###############################################################
# Scenario B: relapses before deaths
###############################################################

scenario_b <- base

scenario_b$status[ scenario_b$time < quantile(scenario_b$time, relapse_rate + death_rate) ] <- 2L

scenario_b$status[ scenario_b$time < quantile(scenario_b$time, relapse_rate) ] <- 1L

stopifnot(
  relapse_rate + death_rate + censor_rate == 1,
  identical(as.integer(table(scenario_a$status)), c(30L, 30L, 40L)),
  identical(as.integer(table(scenario_b$status)), c(30L, 30L, 40L))
)

write.csv(scenario_a, file.path(data_dir, "figure1_scenario_a.csv"), row.names = FALSE)
write.csv(scenario_b, file.path(data_dir, "figure1_scenario_b.csv"), row.names = FALSE)

###############################################################
# Helpers
###############################################################

tidy_km <- function(fit, label) {
  data.frame( time = fit$time, est = fit$surv, 
              low = fit$lower, hi = fit$upper, outcome = label )
}

tidy_cif <- function(fit, cause, label) {
  obj <- fit[[paste("1", cause)]]
  se <- sqrt(obj$var)
  
  data.frame( time = obj$time, est = obj$est, 
              low = pmax(0, obj$est - 1.96 * se), hi = pmin(1, obj$est + 1.96 * se),
              outcome = label )
}

plot_scenario <- function(data, panel, label) {
  
  cif_fit <- cuminc(ftime = data$time, fstatus = data$status)
  
  event_free_fit <- survfit(Surv(time, status != 0) ~ 1, data = data)
  relapse_fit <- survfit(Surv(time, status == 1) ~ 1, data = data)
  death_fit <- survfit(Surv(time, status == 2) ~ 1, data = data)
  
  relapse_risk <- relapse_fit
  relapse_risk$surv <- 1 - relapse_fit$surv
  relapse_risk$lower <- 1 - relapse_fit$upper
  relapse_risk$upper <- 1 - relapse_fit$lower
  
  death_risk <- death_fit
  death_risk$surv <- 1 - death_fit$surv
  death_risk$lower <- 1 - death_fit$upper
  death_risk$upper <- 1 - death_fit$lower
  
  relapse_cif <- tidy_cif(cif_fit, 1, "Relapse (CIF)")
  relapse_km <- tidy_km(relapse_risk, "Relapse (KM)")
  death_cif <- tidy_cif(cif_fit, 2, "Death (CIF)")
  death_km <- tidy_km(death_risk, "Death (KM)")
  
  plot_data <- rbind(relapse_cif, relapse_km, death_cif, death_km)
  
  write.csv( plot_data,
    file.path(data_dir, paste0("figure1_panel_", panel, ".csv")), row.names = FALSE )
  
  plot_data$outcome <- factor( plot_data$outcome,
    levels = c("Relapse (CIF)", "Relapse (KM)", "Death (CIF)", "Death (KM)") )
  
  cols <- c( "Relapse (CIF)" = "#1F78B4", "Relapse (KM)" = "#1F78B4",
             "Death (CIF)" = "#D8A000", "Death (KM)" = "#D8A000")
  
  lty <- c( "Relapse (CIF)" = "solid", "Relapse (KM)" = "dotdash", 
            "Death (CIF)" = "solid", "Death (KM)" = "dotdash" )
  
  alphas <- c( "Relapse (CIF)" = 0.20, "Relapse (KM)" = 0.06,
               "Death (CIF)" = 0.20, "Death (KM)" = 0.06 )
  
  p <- event_free_fit |>
    ggsurvfit(linewidth = 0) +
    add_risktable( 
      risktable_stats = "n.risk",
      stats_label = list(n.risk = "At Risk"),
      risktable_height = 0.10, size = 6,
      theme = list(
        theme_risktable_default(axis.text.y.size = 16),
        theme(
          axis.text.y = element_text( face = "bold", size = 16, margin = margin(r = 20) ),
          plot.margin = margin(10, 10, 10, 0)
        )
      )
    ) +
    geom_ribbon(
      data = plot_data,
      aes(x = time, ymin = low, ymax = hi, fill = outcome, alpha = outcome),
      inherit.aes = FALSE
    ) +
    geom_step(
      data = plot_data,
      aes(x = time, y = est, colour = outcome, linetype = outcome),
      linewidth = 1.4, inherit.aes = FALSE
    ) +
    scale_alpha_manual(values = alphas, guide = "none") +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    scale_linetype_manual(values = lty) +
    scale_x_continuous(
      limits = c(0, target_time), breaks = seq(0, target_time, 1),
      labels = as.character(seq(0, target_time, 1)), expand = c(0.02, 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1), breaks = seq(0, 1, 0.2),
      labels = sprintf("%.1f", seq(0, 1, 0.2)), expand = c(0, 0)
    ) +
    labs(x = "Years", y = "Probability", colour = NULL, fill = NULL) +
    guides(fill = "none", alpha = "none") +
    theme_bw() +
    theme(
      legend.position = c(0.42, 0.865),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = alpha("white", 0.9), colour = NA),
      legend.key.width = unit(2, "cm"),
      legend.text = element_text(size = 18),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(size = 18, margin = margin(r = -30))
    ) +
    annotate( "text", x = 1.2, y = 0.95, label = label, fontface = "bold", size = 8 )
  
  ggsave(
    file.path(fig_dir, paste0("figure1_panel_", panel, ".png")), p,
    width = 12, height = 8, units = "in", dpi = 600, bg = "white"
  )
  
  data.frame(
    Panel = toupper(panel),
    Scenario = label,
    `Relapse CIF` = tail(relapse_cif$est, 1),
    `Relapse KM` = tail(relapse_km$est, 1),
    `Relapse KM--CIF` = tail(relapse_km$est, 1) - tail(relapse_cif$est, 1),
    `Death CIF` = tail(death_cif$est, 1),
    `Death KM` = tail(death_km$est, 1),
    `Death KM--CIF` = tail(death_km$est, 1) - tail(death_cif$est, 1),
    check.names = FALSE
  )
}

###############################################################
# Generate Figure 1 panels
###############################################################

summary_a <- plot_scenario( scenario_a, "a", "A. Deaths before Relapses" )

summary_b <- plot_scenario( scenario_b, "b", "B. Relapses before Deaths" )

figure1_summary <- rbind(summary_a, summary_b)

print(figure1_summary, row.names = FALSE, digits = 3)

###############################################################
# Manuscript result checks
###############################################################

stopifnot(
  isTRUE(all.equal(summary_a$`Relapse CIF`, 0.30, tolerance = 1e-12)),
  isTRUE(all.equal(summary_a$`Relapse KM`, 0.50, tolerance = 1e-12)),
  isTRUE(all.equal(summary_a$`Relapse KM--CIF`, 0.20, tolerance = 1e-12)),
  isTRUE(all.equal(summary_b$`Relapse CIF`, 0.30, tolerance = 1e-12)),
  isTRUE(all.equal(summary_b$`Relapse KM`, 0.30, tolerance = 1e-12)),
  isTRUE(all.equal(summary_b$`Relapse KM--CIF`, 0.00, tolerance = 1e-12))
)