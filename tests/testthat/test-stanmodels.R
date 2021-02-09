test_that("stanmodels contains the right models", {
  expect_identical(names(stanmodels), c("vtf","deming"))
  # will only print when the package is installed properly
  expect_output(stanmodels$deming$print())
  expect_output(stanmodels$vtf$print())
})
