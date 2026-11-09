###############################################################
# Figure 2: MGUS competing-risks example
###############################################################

library(survival)
library(cmprsk)
library(ggsurvfit)
library(ggplot2)
library(kmaudit)

target_time <- 25
fig_dir <- file.path("paper", "figures", "output")
data_dir <- file.path("paper", "data", "source", "output")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################
# Data
###############################################################

mgus <- survival::mgus

mgus$time <- ifelse(
  is.na(mgus$pctime), mgus$futime,
  pmin(mgus$pctime, mgus$futime)
) / 365.25

mgus$ev <- ifelse( !is.na(mgus$pctime) & mgus$pctime <= mgus$futime, 1L,
                   ifelse(mgus$death == 1, 2L, 0L) )

mgus$ev[mgus$time > target_time] <- 0L
mgus$time <- pmin(mgus$time, target_time)

###############################################################
# Panel A: KM risks and CIFs
###############################################################

km_pcm <- survfit(Surv(time, ev == 1) ~ 1, data = mgus)
km_death <- survfit(Surv(time, ev == 2) ~ 1, data = mgus)
km_event_free <- survfit(Surv(time, ev != 0) ~ 1, data = mgus)

km_pcm_risk <- km_pcm
km_pcm_risk$surv <- 1 - km_pcm$surv
km_pcm_risk$lower <- 1 - km_pcm$upper
km_pcm_risk$upper <- 1 - km_pcm$lower

km_death_risk <- km_death
km_death_risk$surv <- 1 - km_death$surv
km_death_risk$lower <- 1 - km_death$upper
km_death_risk$upper <- 1 - km_death$lower

cif_fit <- cuminc(mgus$time, mgus$ev)

tidy_km <- function(fit, label) {
  data.frame( time = fit$time, est = fit$surv, 
              low = fit$lower, hi = fit$upper, outcome = label )
}

tidy_cif <- function(code, label) {
  obj <- cif_fit[[paste("1", code)]]
  se <- sqrt(obj$var)
  
  data.frame( time = obj$time, est = obj$est, 
              low = pmax(0, obj$est - 1.96 * se),
              hi = pmin(1, obj$est + 1.96 * se), outcome = label )
}

panel_a_data <- rbind( tidy_km(km_pcm_risk, "PCM (KM)"), tidy_cif(1, "PCM (CIF)"),
                       tidy_km(km_death_risk, "Death (KM)"), tidy_cif(2, "Death (CIF)") )

write.csv( panel_a_data, file.path(data_dir, "figure2_panel_a.csv"), row.names = FALSE )

panel_a_data$outcome <- factor( 
  panel_a_data$outcome,
  levels = c("PCM (KM)", "PCM (CIF)", "Death (KM)", "Death (CIF)")
)

cols <- c( "PCM (KM)" = "#1F78B4", "PCM (CIF)" = "#1F78B4",
           "Death (KM)" = "#D8A000", "Death (CIF)" = "#D8A000" )

lty <- c( "PCM (KM)" = "dotdash", "PCM (CIF)" = "solid",
          "Death (KM)" = "dotdash", "Death (CIF)" = "solid" )

alphas <- c( "PCM (KM)" = 0.06, "PCM (CIF)" = 0.20, 
             "Death (KM)" = 0.06, "Death (CIF)" = 0.20 )

panel_a <- km_event_free |>
  ggsurvfit(linewidth = 0) +
  add_risktable(
    risktable_stats = "n.risk", stats_label = list(n.risk = "At Risk"),
    risktable_height = 0.10, size = 6,
    theme = list(
      theme_risktable_default(axis.text.y.size = 16),
      theme(axis.text.y = element_text( face = "bold", size = 16, margin = margin(r = 20) ) )
    )
  ) +
  geom_ribbon( data = panel_a_data,
               aes(time, ymin = low, ymax = hi, fill = outcome, alpha = outcome),
               inherit.aes = FALSE ) +
  geom_step( data = panel_a_data,
             aes(time, est, colour = outcome, linetype = outcome),
             linewidth = 1.4, inherit.aes = FALSE ) +
  scale_alpha_manual(values = alphas, guide = "none") +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  scale_linetype_manual(values = lty) +
  scale_x_continuous( limits = c(0, target_time), breaks = seq(0, target_time, 5) ) +
  scale_y_continuous( limits = c(0, 1), breaks = seq(0, 1, 0.25), 
                      labels = sprintf("%.2f", seq(0, 1, 0.25)), expand = c(0, 0) ) +
  labs(x = "Years", y = "Probability", colour = NULL, fill = NULL) +
  guides(fill = "none") +
  theme_bw() +
  theme(
    legend.position = c(0.42, 0.80),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = alpha("white", 0.9), colour = NA),
    legend.key.width = unit(1.2, "cm"),
    legend.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = -25))
  ) +
  annotate( "text", x = 0, y = 0.9, label = "A. MGUS data", 
            fontface = "bold", size = 9, hjust = 0 )

ggsave( file.path(fig_dir, "figure2_panel_a.png"), panel_a,
        width = 12, height = 8, units = "in", dpi = 600, bg = "white" )

###############################################################
# Panel B: Total Overestimation Diagnostic
###############################################################

tod_fit <- tod(mgus$time, mgus$ev)
tod_result <- summary(tod_fit)

panel_b_data <- data.frame( time = tod_result$time, est = tod_result$TOD,
                            low = tod_result$TOD_lower, hi = tod_result$TOD_upper )

write.csv( panel_b_data, file.path(data_dir, "figure2_panel_b.csv"), row.names = FALSE )

panel_b <- plot( tod_fit, xlab = "Years", x_breaks = seq(0, 25, 5), 
                 y_breaks = seq(0, 0.5, 0.1), ylim = c(0, 0.5) ) +
  annotate( "text", x = 0, y = 0.45, label = "B. Total Overestimation Diagnostic",
            colour = "red", fontface = "bold", size = 9, hjust = 0 )

ggsave( file.path(fig_dir, "figure2_panel_b.png"), panel_b,
        width = 12, height = 8, units = "in", dpi = 600, bg = "white" )

###############################################################
# Manuscript result checks
###############################################################

horizon <- summary(tod_fit, times = target_time)

figure2_summary <- data.frame(
  Mean_TOD = mean(tod_result$TOD),
  TOD_25 = horizon$TOD,
  TOD_Lower_25 = horizon$TOD_lower,
  TOD_Upper_25 = horizon$TOD_upper,
  OI = oi( mgus$time, mgus$ev, interest = 1, competing = 2 )
)

print(figure2_summary, digits = 4, row.names = FALSE)