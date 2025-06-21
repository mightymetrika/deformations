test_that("When a = b then deaprpa matches dcaprpa", {
  expect_equal(deaprpa(4, a = 1, b = 1), dcaprpa(4))
})
