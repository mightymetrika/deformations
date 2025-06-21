test_that("measure_koch_deformity basic functionality works", {

  # Test basic function call
  koch_result <- measure_koch_deformity(side_length = 1, max_iterations = 5)

  # Check that result has correct class
  expect_s3_class(koch_result, "koch_deformity")
  expect_s3_class(koch_result, "data.frame")

  # Check that result has correct dimensions
  expect_equal(nrow(koch_result), 6)  # 0 to 5 iterations = 6 rows
  expect_equal(ncol(koch_result), 6)  # iteration, perimeter, area, r_perimeter, r_area, deformity

  # Check that required columns exist
  expected_cols <- c("iteration", "perimeter", "area", "r_perimeter", "r_area", "deformity")
  expect_equal(names(koch_result), expected_cols)

  # Check that iterations are correct sequence
  expect_equal(koch_result$iteration, 0:5)
})

test_that("Koch deformity mathematical properties hold", {

  koch_result <- measure_koch_deformity(side_length = 1, max_iterations = 3)

  # Perimeter should increase with each iteration (for Koch snowflake)
  expect_true(all(diff(koch_result$perimeter) > 0))

  # Area should increase with each iteration but stabilize
  expect_true(all(diff(koch_result$area) >= 0))

  # Initial iteration (k=0) should be equilateral triangle
  expect_equal(koch_result$perimeter[1], 3)  # side_length = 1, so perimeter = 3
  expect_equal(koch_result$area[1], sqrt(3)/4, tolerance = 1e-10)  # equilateral triangle area
})

test_that("S3 methods work without error", {

  koch_result <- measure_koch_deformity(side_length = 1, max_iterations = 4)

  # Test print method doesn't error
  expect_output(print(koch_result), "Koch Snowflake Deformity Analysis")

  # Test plot method doesn't error (capture plot to avoid displaying)
  expect_silent({
    png(tempfile())
    plot(koch_result)
    dev.off()
  })

  # Test summary method works
  koch_summary <- summary(koch_result)
  expect_s3_class(koch_summary, "summary.koch_deformity")
})
