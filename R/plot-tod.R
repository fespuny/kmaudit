#' Plot a Total Overestimation Diagnostic
#'
#' @param x A `tod` object.
#' @param type Either `"tod"` or `"curves"`.
#' @param ci Show the TOD confidence band.
#' @param show_mean Show mean TOD as a dashed horizontal line.
#' @param colour Colour used for TOD.
#' @param ribbon_alpha Confidence-band opacity.
#' @param linewidth Step-curve width.
#' @param xlab Horizontal-axis label.
#' @param ylab Optional vertical-axis label.
#' @param mean_label Mean-line annotation style. `"normal"` displays
#'   `"Mean over follow-up"` and `"short"` displays `"Mean"`.
#' @param x_breaks Optional x-axis breaks.
#' @param y_breaks Optional y-axis breaks.
#' @param ylim Optional y-axis limits.
#' @param ... Additional arguments, currently ignored.
#'
#' @return A `ggplot` object.
#' @export
plot.tod <- function(
    x,
    type = c("tod", "curves"),
    ci = TRUE,
    show_mean = TRUE,
    colour = "red",
    ribbon_alpha = 0.07,
    linewidth = 1.4,
    xlab = "Time",
    ylab = NULL,
    mean_label = c("normal", "short"),
    x_breaks = NULL,
    y_breaks = NULL,
    ylim = NULL,
    ...) {
  
  type <- match.arg(type)
  d <- x$estimates
  
  if (type == "curves") {
    
    curve_data <- rbind(
      data.frame(
        time = d$time,
        estimate = d$S_PL,
        curve = "Overall product-limit survival"
      ),
      do.call(rbind, lapply(seq_along(x$causes), function(j) {
        data.frame(
          time = d$time,
          estimate = d[[paste0("S_KM_", x$causes[j])]],
          curve = paste("Cause", x$causes[j], "KM survival")
        )
      }))
    )
    
    return(
      ggplot2::ggplot(
        curve_data,
        ggplot2::aes( x = .data$time, y = .data$estimate, colour = .data$curve, linetype = .data$curve )
      ) +
        ggplot2::geom_step(linewidth = linewidth) +
        ggplot2::scale_y_continuous(
          limits = c(0, 1),
          breaks = seq(0, 1, 0.2)
        ) +
        ggplot2::labs(
          x = xlab,
          y = if (is.null(ylab)) "Survival probability" else ylab,
          colour = NULL,
          linetype = NULL
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom"
        )
    )
  }
  
  plot_data <- data.frame(
    time = d$time,
    estimate = d$TOD,
    lower = d$TOD_lower,
    upper = d$TOD_upper
  )
  
  mean_tod <- mean(plot_data$estimate)
  
  if (is.null(x_breaks)) {
    x_breaks <- pretty(c(0, max(plot_data$time)), n = 6)
  }
  
  if (is.null(ylim)) {
    ymax <- max(plot_data$upper, na.rm = TRUE)
    ylim <- c(0, max(0.1, ceiling(10 * ymax) / 10))
  }
  
  if (is.null(y_breaks)) {
    y_breaks <- pretty(ylim, n = 5)
  }
  
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes( x = .data$time, y = .data$estimate )
  )
  
  if (ci) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        fill = colour,
        alpha = ribbon_alpha
      )
  }
  
  p <- p +
    ggplot2::geom_step(
      colour = colour,
      linewidth = linewidth
    )
  
  mean_label <- match.arg(mean_label)
  
  if (show_mean) {
    
    label <- if (mean_label == "short") {
      sprintf("Mean = %.1f%%", 100 * mean_tod)
    } else {
      sprintf("Mean over follow-up = %.1f%%", 100 * mean_tod)
    }
    
    p <- p +
      ggplot2::geom_hline(
        yintercept = mean_tod,
        colour = colour,
        linetype = "dashed",
        linewidth = 0.9
      ) +
      ggplot2::annotate(
        "text",
        x = min(plot_data$time) + 0.2,
        y = mean_tod + 0.03 * diff(ylim),
        label = label,
        colour = colour,
        fontface = "bold",
        size = 6,
        hjust = 0
      )
  }
  
  p +
    ggplot2::scale_x_continuous(
      limits = c(0, max(plot_data$time)),
      breaks = x_breaks
    ) +
    ggplot2::scale_y_continuous(
      limits = ylim,
      breaks = y_breaks,
      labels = sprintf("%.1f", y_breaks),
      expand = c(0, 0.01)
    ) +
    ggplot2::labs(
      x = xlab,
      y = if (is.null(ylab)) {
        expression(hat(B)[total](t))
      } else {
        ylab
      }
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 18),
      axis.text = ggplot2::element_text(size = 16),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 10)
      )
    )
}