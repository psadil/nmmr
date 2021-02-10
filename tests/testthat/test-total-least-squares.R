test_that("sums of squares performs checks", {
  # nothing missing
  testthat::expect_error(sum_squares(c(1, 2), c(1, NA)))
  # same length
  testthat::expect_error(sum_squares(c(1, 2), c(1)))
  # no nulls
  testthat::expect_error(sum_squares(1, NULL))
})

test_that("get pretty accurate slope", {
  n <- 1e5
  z <- rnorm(n)
  x <- rnorm(n, z)

  # with intercept
  y <- rnorm(n, 2 * z + 1)
  testthat::expect_equal(get_slope(x, y), 2, tolerance = 0.01)

  # no intercept
  y <- rnorm(n, 2 * z)
  testthat::expect_equal(get_slope(x, y), 2, tolerance = 0.01)

  # negative
  y <- rnorm(n, -2 * z)
  testthat::expect_equal(get_slope(x, y), -2, tolerance = 0.01)
})

test_that("Can calculate slopes in groups", {
  sub02_wide <- sub02 %>%
    tidyr::pivot_wider(names_from = contrast, values_from = y)

  # can run with bare and quoted names
  testthat::expect_silent(get_slope_by_group(sub02_wide, voxel, low, high))
  testthat::expect_silent(get_slope_by_group(sub02_wide, "voxel", low, high))
  testthat::expect_silent(get_slope_by_group(sub02_wide, voxel, "low", high))
  testthat::expect_silent(get_slope_by_group(sub02_wide, "voxel", "low", "high"))

  # one slope per voxel
  slopes <- get_slope_by_group(sub02_wide, voxel, low, high)
  testthat::expect_length(slopes, 2)
  testthat::expect_vector(slopes$slope, ptype = numeric(), size = dplyr::n_distinct(sub02$voxel))

  # can also cross by a second group
  testthat::expect_silent(get_slope_by_group(sub02_wide, c(voxel, sub), low, high))
  testthat::expect_silent(get_slope_by_group(sub02_wide, c("voxel", sub), low, high))
  slopes <- get_slope_by_group(sub02_wide, c(voxel, sub), low, high)
  testthat::expect_length(slopes, 3)
})
