#' Order Index
#'
#' Computes the proportion of interest-competing event pairs in which the
#' competing event occurs before the event of interest. A value near 1 means
#' competing events tend to occur earlier; a value near 0 means they tend to
#' occur later.
#'
#' @param time Numeric follow-up times.
#' @param status Event indicator.
#' @param interest Status value identifying the event of interest.
#' @param competing Optional vector of competing event values. By default, all
#'   non-censoring values other than `interest` are pooled as competing events.
#' @param censor Value denoting censoring. Defaults to 0.
#' @param ties Tie handling: `"average"` gives tied cross-cause pairs weight
#'   0.5; `"error"` rejects cross-cause ties; `"interest_first"` gives them
#'   weight 0; `"competing_first"` gives them weight 1.
#' @return A numeric scalar in [0, 1], or `NA_real_` if either group is absent.
#' @export
#' @examples
#' oi(c(1, 2, 3, 4), c(2, 1, 2, 1), interest = 1)
oi <- function(time, status, interest, competing = NULL, censor = 0,
               ties = c("average", "error", "interest_first", "competing_first")) {
  ties <- match.arg(ties)
  if (!is.numeric(time) || length(time) != length(status) || anyNA(time) || anyNA(status)) {
    stop("`time` must be numeric and have the same non-missing length as `status`.", call. = FALSE)
  }
  if (is.null(competing)) competing <- setdiff(unique(status), c(censor, interest))
  is_i <- status == interest
  is_c <- status %in% competing
  ti <- time[is_i]
  tc <- time[is_c]
  if (!length(ti) || !length(tc)) return(NA_real_)

  cmp <- outer(tc, ti, FUN = "-")
  tied <- cmp == 0
  if (ties == "error" && any(tied)) {
    stop("Cross-cause event-time ties found; select an explicit tie convention.", call. = FALSE)
  }
  score <- cmp < 0
  tie_weight <- switch(ties, average = 0.5, error = 0, interest_first = 0,
                       competing_first = 1)
  mean(score + tied * tie_weight)
}
