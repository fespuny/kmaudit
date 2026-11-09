###############################################################
# Figure 3: Simulation response surfaces
#
# Outputs:
#   paper/figures/figure3_panel_a.png
#   paper/figures/figure3_panel_b.png
#   paper/data/figure3_panel_a.csv
#   paper/data/figure3_panel_b.csv
###############################################################

library(dplyr)
library(ggplot2)

data_file <- file.path("paper", "data", "simulation_results.csv.gz")
fig_dir <- file.path("paper", "figures", "output")
source_dir <- file.path("paper", "data", "source", "output")

if (!file.exists(data_file)) {
  stop("Missing paper/data/simulation_results.csv.gz")
}

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)

d <- read.csv(data_file)

required <- c( "ObservedFrequency", "ObservedOrderingIndex", 
               "Overestimation1", "TOD", "OI_Defined" )
stopifnot(all(required %in% names(d)))

d <- subset(d, OI_Defined)

nbins <- 10L
breaks <- seq(0, 1, 0.2)
labels <- sprintf("%.1f", breaks)

d$FreqBin <- cut( d$ObservedFrequency, 
                  breaks = seq(0, 1, length.out = nbins + 1), include.lowest = TRUE )

d$OIBin <- cut( d$ObservedOrderingIndex,
                breaks = seq(0, 1, length.out = nbins + 1), include.lowest = TRUE )

aggregate_surface <- function(var) {
  z <- d |>
    group_by(FreqBin, OIBin) |>
    summarise(value = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  
  z$x <- rep(seq(0.05, 0.95, 0.1), each = nbins)
  z$y <- rep(seq(0.05, 0.95, 0.1), times = nbins)
  z
}

make_panel <- function(data, legend, high, panel) {
  ggplot(data, aes(x, y, fill = value)) +
    geom_tile(width = 0.1, height = 0.1) +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    scale_x_continuous(breaks = breaks, labels = labels) +
    scale_y_continuous(breaks = breaks, labels = labels) +
    scale_fill_gradient( low = "white", high = high, limits = c(0, 1),
      breaks = breaks, labels = labels, name = legend ) +
    labs( x = "Competing event frequency", y = "Order index", tag = panel ) +
    theme_bw() +
    theme( 
      axis.title.x = element_text(size = 18, margin = margin(t = 15)),
      axis.title.y = element_text(size = 18, margin = margin(r = 10)),
      axis.text = element_text(size = 16),
      legend.title = element_text(size = 16, margin = margin(b = 15)),
      legend.text = element_text(size = 14),
      legend.key.height = unit(1, "cm"),
      legend.key.width = unit(0.6, "cm"),
      plot.tag = element_text(face = "bold", size = 30),
      plot.tag.position = c(-0.04, 0.96)
    )
}

panel_a_data <- aggregate_surface("Overestimation1")
panel_b_data <- aggregate_surface("TOD")

write.csv( panel_a_data, file.path(source_dir, "figure3_panel_a.csv"), row.names = FALSE )

write.csv( panel_b_data, file.path(source_dir, "figure3_panel_b.csv"), row.names = FALSE )

panel_a <- make_panel( panel_a_data, expression(hat(B)[1](t)), "#1F78B4", "A" )

panel_b <- make_panel( panel_b_data, "TOD", "red", "B" )

ggsave( file.path(fig_dir, "figure3_panel_a.png"),
  panel_a, width = 8, height = 6, units = "in", dpi = 600, bg = "white" )

ggsave( file.path(fig_dir, "figure3_panel_b.png"),
  panel_b, width = 8, height = 6, units = "in", dpi = 600, bg = "white" )

cat("Figure 3 reproduced from", nrow(d), "simulations with defined OI.\n")