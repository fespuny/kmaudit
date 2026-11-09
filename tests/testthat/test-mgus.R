test_that("MGUS reproduces the published results", {
  
  d <- survival::mgus
  horizon <- 25
  
  d$time <- ifelse( is.na(d$pctime), d$futime, pmin(d$pctime, d$futime) ) / 365.25
  
  d$ev <- ifelse( !is.na(d$pctime) & d$pctime <= d$futime, 1L,
                  ifelse(d$death == 1, 2L, 0L) )
  
  d$ev[d$time > horizon] <- 0L
  d$time <- pmin(d$time, horizon)
  
  fit <- tod(d$time, d$ev)
  x <- summary(fit, times = horizon)
  
  expect_equal(x$TOD, 0.28714512315589968, tolerance = 1e-12)
  expect_equal(x$TOD_lower, 0.24723923137901616, tolerance = 1e-12)
  expect_equal(x$TOD_upper, 0.34057291373637771, tolerance = 1e-12)
  expect_equal(100 * mean(summary(fit)$TOD), 8.3069968451652052, tolerance = 1e-12)
  expect_equal( oi(d$time, d$ev, interest = 1, competing = 2), 0.50166468489892979, tolerance = 1e-12 )
})