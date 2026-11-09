test_that("OI identifies the two extreme orderings", {
  expect_equal(oi(1:4, c(2, 2, 1, 1), interest = 1), 1)
  expect_equal(oi(1:4, c(1, 1, 2, 2), interest = 1), 0)
})

test_that("OI handles cross-cause ties explicitly", {
  expect_equal(oi(c(1, 1), c(1, 2), interest = 1), 0.5)
  expect_error(oi(c(1, 1), c(1, 2), interest = 1, ties = "error"), "ties")
})
