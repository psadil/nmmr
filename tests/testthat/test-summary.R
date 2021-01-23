test_that("sem checks for weird inputs", {

  expect_warning(sem(NA))
  expect_warning(sem(c(1, NA, 2)))
  expect_warning(sem(c(NA, rep(2, 10))))
  expect_equal(sem(rep(2, 10)), 0)
  expect_equal(sem(c(NA, rep(2, 10)), na.rm = TRUE), 0)

})

test_that("wisesummary runs w/ and w/o tidy eval", {

  expect_s3_class(WISEsummary(sub02, y, withinvars = c(contrast, orientation), idvar = voxel), class = "data.frame")
  expect_s3_class(WISEsummary(sub02, y, withinvars = c("contrast", orientation), idvar = voxel), class = "data.frame")
  expect_s3_class(WISEsummary(sub02, "y", withinvars = c(contrast, orientation), idvar = voxel), class = "data.frame")
  expect_s3_class(WISEsummary(sub02, y, withinvars = c(contrast, orientation), idvar = "voxel"), class = "data.frame")


  # runs with differnet combinations of inputs
  expect_silent(WISEsummary(sub02, y, withinvars = orientation, betweenvars = contrast, idvar = voxel))
  expect_silent(WISEsummary(sub02, y, withinvars = c(contrast, orientation)))
  expect_silent(WISEsummary(sub02, y, withinvars = c(contrast, orientation), betweenvars = voxel))
  expect_silent(WISEsummary(sub02, y, withinvars = c(contrast), idvar = voxel))

  # runs on other dataset
  expect_silent(WISEsummary(ChickWeight, dependentvars = weight, withinvars = Time, idvar = Chick))

})
