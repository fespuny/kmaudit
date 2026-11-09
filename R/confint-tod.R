#' Confidence interval for TOD
#'
#' Returns the confidence interval obtained by transferring the asymmetric
#' confidence deviations of the overall product-limit survival estimator to
#' TOD under Product Limit Error Equivalence. The lower TOD limit is truncated
#' at zero; the upper limit is not truncated.
#'
#' @param object A `tod` object.
#' @param parm Ignored.
#' @param level Confidence level. It must match the level used to create
#'   `object`; otherwise the object is refitted from its stored inputs.
#' @param times Optional evaluation times.
#' @param ... Additional arguments, currently ignored.
#' @return A data frame containing time, TOD and confidence limits.
#' @export
confint.tod <- function(object, parm, level = object$conf_level,
                        times = object$estimates$time, ...) {
  if (!inherits(object, "tod")) stop("`object` must inherit from class 'tod'.", call. = FALSE)
  if (!isTRUE(all.equal(level, object$conf_level))) {
    object <- tod(object$input$time, object$input$status,
                  censor = object$censor, times = times, conf_level = level)
  }
  z <- summary(object, times = times)
  data.frame(
    time = z$time,
    estimate = z$TOD,
    lower = z$TOD_lower,
    upper = z$TOD_upper,
    level = level
  )
}

#' @rdname confint.tod
#' @export
tod_ci <- function(object, level = object$conf_level,
                   times = object$estimates$time) {
  stats::confint(object, level = level, times = times)
}
