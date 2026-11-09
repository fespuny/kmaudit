test_that("TOD confidence deviations equal product-limit deviations", {
  fit <- tod(c(1, 2, 3, 4, 5, 6, 7, 8), c(1, 2, 0, 1, 2, 0, 1, 2))
  z <- fit$estimates
  pl <- summary(fit$fits$overall, times = z$time, extend = TRUE)
  expect_equal(z$TOD - z$TOD_lower,
               pmin(z$TOD, unname(pl$surv - pl$lower)))
  expect_equal(z$TOD_upper - z$TOD,
               unname(pl$upper - pl$surv))
})

test_that("tod_ci and confint agree", {
  fit <- tod(c(1, 2, 3, 4, 5, 6), c(1, 2, 0, 1, 2, 0))
  expect_equal(tod_ci(fit), confint(fit))
})
