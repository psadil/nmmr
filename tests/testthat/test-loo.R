test_that("loo runs", {
  # data("sub02")
  small <- sub02 %>%
    dplyr::filter(forcats::fct_match(voxel, c("191852","197706"))) %>%
    dplyr::mutate(voxel = forcats::fct_drop(voxel))

  #' not nearly enough samples to avoid warnings
  suppressWarnings(
    fit_m <- vtf(
      model = "multiplicative",
      d = small,
      chains = 2,
      iter = 10,
      warmup = 1,
      refresh = 0))

  suppressWarnings(
    fit_a <- vtf(
      model = "additive",
      d = small,
      chains = 2,
      iter = 10,
      warmup = 1,
      refresh = 0))

  suppressWarnings(elpd_m <- loo(fit_m))
  suppressWarnings(elpd_a <- loo(fit_a))

  expect_s3_class(elpd_m, "psis_loo")
  expect_s3_class(elpd_a, "psis_loo")
})
