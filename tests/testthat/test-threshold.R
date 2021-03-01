test_that("can calculate thresholds", {
  sub02_wide <- sub02 %>%
    tidyr::pivot_wider(names_from = contrast, values_from = y)

  # can run with bare and quoted names
  testthat::expect_silent(cross_threshold(sub02_wide, voxel, low, high))
  testthat::expect_silent(cross_threshold(sub02_wide, "voxel", low, high))
  testthat::expect_silent(cross_threshold(sub02_wide, voxel, "low", high))
  testthat::expect_silent(cross_threshold(sub02_wide, "voxel", "low", "high"))

  # correct number of rows at a given threshold
  remaining <- cross_threshold(sub02_wide, voxel, low, high, quantiles = c(0, 0.9))
  checkmate::expect_numeric(remaining$Threshold, len = 206, any.missing = FALSE, lower = 0, upper = 1)

  # can also cross by a second group
  testthat::expect_silent(cross_threshold(sub02_wide, voxel, low, high, participant = sub))
  testthat::expect_silent(cross_threshold(sub02_wide, voxel, low, high, participant = "sub"))
  testthat::expect_silent(cross_threshold(sub02_wide, c(voxel, run), low, high, participant = sub))
  testthat::expect_equal(
    cross_threshold(sub02_wide, voxel, low, high, participant = sub),
    cross_threshold(sub02_wide, voxel, low, high, participant = "sub")
  )

  remaining <- cross_threshold(sub02_wide, voxel, low, high, participant = sub)
  testthat::expect_length(remaining, 3)
})
