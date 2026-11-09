test_that("TOD equals S_PL minus sum S_KM plus K minus one", {
  fit <- tod(c(1, 2, 3, 4, 5, 6), c(1, 2, 0, 1, 2, 0))
  d <- fit$estimates
  expect_equal(d$TOD, d$S_PL - d$S_KM_1 - d$S_KM_2 + 1)
})

test_that("TOD is invariant to row order", {
  time <- c(1, 2, 3, 4, 5, 6)
  status <- c(1, 2, 0, 1, 2, 0)
  a <- tod(time, status)$estimates
  id <- c(6, 2, 4, 1, 5, 3)
  b <- tod(time[id], status[id])$estimates
  expect_equal(a, b)
})

test_that("plot.tod returns a ggplot object", {
  fit <- tod(
    time = c(1, 2, 3, 4, 5, 6),
    status = c(1, 2, 0, 1, 2, 0)
  )
  
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(plot(fit, type = "curves"), "ggplot")
})