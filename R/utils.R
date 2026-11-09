.validate_inputs <- function(time, status, censor) {
  if (!is.numeric(time)) stop("`time` must be numeric.", call. = FALSE)
  if (length(time) != length(status)) stop("`time` and `status` must have equal length.", call. = FALSE)
  if (!length(time)) stop("Inputs must not be empty.", call. = FALSE)
  if (anyNA(time) || anyNA(status)) stop("Missing values are not allowed.", call. = FALSE)
  if (any(!is.finite(time)) || any(time < 0)) stop("`time` must contain finite, non-negative values.", call. = FALSE)
  if (length(censor) != 1L || is.na(censor)) stop("`censor` must be one non-missing value.", call. = FALSE)
  causes <- sort(unique(status[status != censor]))
  if (length(causes) < 2L) stop("At least two realised event types are required.", call. = FALSE)
  list(time = time, status = status, causes = causes)
}

.step_surv <- function(fit, times) {
  unname(summary(fit, times = times, extend = TRUE)$surv)
}

.step_surv_ci <- function(fit, times) {
  z <- summary(fit, times = times, extend = TRUE)
  data.frame(
    estimate = unname(z$surv),
    lower = unname(z$lower),
    upper = unname(z$upper)
  )
}
