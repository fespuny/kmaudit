#' Total Overestimation Diagnostic
#'
#' Computes the Total Overestimation Diagnostic (TOD) from the overall
#' product-limit survival curve and the cause-specific Kaplan-Meier curves.
#' Censoring is coded explicitly; all other realised status values are treated
#' as event causes.
#'
#' @param time Numeric follow-up times.
#' @param status Event indicator. `censor` denotes censoring and every other
#'   realised value denotes an event cause.
#' @param censor Value denoting censoring. Defaults to 0.
#' @param times Optional evaluation times. By default, all distinct times in
#'   the overall product-limit fit are used.
#' @param conf_level Confidence level used by `survival::survfit()` for the
#'   overall product-limit curve. Defaults to 0.95.
#' @return An object of class `tod` containing TOD, its transferred
#'   product-limit confidence limits, fitted curves, and original inputs.
#' @export
#' @examples
#' time <- c(1, 2, 3, 4, 5, 6)
#' status <- c(1, 2, 0, 1, 2, 0)
#' fit <- tod(time, status)
#' fit
#' summary(fit, times = c(2, 4, 6))
tod <- function(time, status, censor = 0, times = NULL, conf_level = 0.95) {
  x <- .validate_inputs(time, status, censor)
  time <- x$time
  status <- x$status
  causes <- x$causes
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be one number strictly between 0 and 1.", call. = FALSE)
  }

  overall_fit <- survival::survfit(
    survival::Surv(time, status != censor) ~ 1,
    conf.int = conf_level
  )
  cause_fits <- lapply(causes, function(cause) {
    survival::survfit(survival::Surv(time, status == cause) ~ 1)
  })
  names(cause_fits) <- as.character(causes)

  if (is.null(times)) {
    times <- overall_fit$time
  } else {
    if (!is.numeric(times) || anyNA(times) || any(!is.finite(times)) || any(times < 0)) {
      stop("`times` must contain finite, non-negative numeric values.", call. = FALSE)
    }
    times <- sort(unique(times))
  }

  overall <- .step_surv_ci(overall_fit, times)
  s_k <- vapply(cause_fits, .step_surv, numeric(length(times)), times = times)
  if (is.null(dim(s_k))) s_k <- matrix(s_k, ncol = length(causes))
  colnames(s_k) <- paste0("S_KM_", causes)
  tod_value <- overall$estimate - rowSums(s_k) + length(causes) - 1

  # Product-limit error equivalence: transfer the asymmetric confidence
  # deviations of S_PL directly to TOD, truncating only the lower limit at 0.
  delta_lower <- overall$estimate - overall$lower
  delta_upper <- overall$upper - overall$estimate
  tod_lower <- pmax(0, tod_value - delta_lower)
  tod_upper <- tod_value + delta_upper

  estimates <- data.frame(
    time = times,
    S_PL = overall$estimate,
    s_k,
    TOD = tod_value,
    TOD_lower = tod_lower,
    TOD_upper = tod_upper,
    check.names = FALSE
  )
  out <- list(
    estimates = estimates,
    causes = causes,
    censor = censor,
    conf_level = conf_level,
    fits = list(overall = overall_fit, causes = cause_fits),
    input = data.frame(time = time, status = status)
  )
  class(out) <- "tod"
  out
}

#' @export
print.tod <- function(x, ...) {
  cat("Kaplan-Meier overestimation audit\n")
  cat("Observations:", nrow(x$input), "\n")
  cat("Event causes:", paste(x$causes, collapse = ", "), "\n")
  cat("Confidence level:", format(x$conf_level), "\n")
  cat("Maximum TOD:", format(max(x$estimates$TOD), digits = 4), "\n")
  invisible(x)
}

#' @export
summary.tod <- function(object, times = NULL, ...) {
  if (is.null(times)) return(object$estimates)
  if (!is.numeric(times) || anyNA(times) || any(!is.finite(times)) || any(times < 0)) {
    stop("`times` must contain finite, non-negative numeric values.", call. = FALSE)
  }
  cause_cols <- paste0("S_KM_", object$causes)
  overall <- .step_surv_ci(object$fits$overall, times)
  s_k <- vapply(object$fits$causes, .step_surv, numeric(length(times)), times = times)
  if (is.null(dim(s_k))) s_k <- matrix(s_k, ncol = length(object$causes))
  colnames(s_k) <- cause_cols
  value <- overall$estimate - rowSums(s_k) + length(object$causes) - 1
  lower <- pmax(0, value - (overall$estimate - overall$lower))
  upper <- value + (overall$upper - overall$estimate)
  data.frame(time = times, S_PL = overall$estimate, s_k, TOD = value,
             TOD_lower = lower, TOD_upper = upper, check.names = FALSE)
}
