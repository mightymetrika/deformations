test_that("When a = b then deaprps matches dcaprps", {
  expect_equal(deaprps(3:10, a = 1, b = 1), dcaprps(3:10))
})
