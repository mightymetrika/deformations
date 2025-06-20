test_that("test that dcaprps give valid solutions with n= 3 and n = 4", {

  # Test n = 3
  dcaprps3 <- function(r = 1){
    2*r*sqrt(pi)*(3^(-1/4) - sqrt(pi)/3)
  }

  expect_equal(dcaprps3(), dcaprps(3))

  # Test n = 4
  dcaprps4 <- function(r = 1){
    r*sqrt(pi)*(1 - sqrt(pi)/2)
  }

  expect_equal(dcaprps4(), dcaprps(4))
})
