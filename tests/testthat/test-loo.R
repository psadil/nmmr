small <- sub02 %>%
  dplyr::filter(forcats::fct_match(voxel, c("191852","197706"))) %>%
  dplyr::mutate(voxel = forcats::fct_drop(voxel))

test_that("loo runs", {

  m <- Model$new(form = "multiplicative")
  a <- Model$new(form = "additive")

  #' not nearly enough samples to avoid warnings
  suppressWarnings(
    fit_a <- a$sample(
      d=small,
      chains = 1,
      iter = 10,
      warmup = 1,
      refresh = 0))

  suppressWarnings(
    fit_m <- m$sample(
      d=small,
      chains = 1,
      iter = 10,
      warmup = 1,
      refresh = 0))

  suppressWarnings(elpd_m <- fit_m$loo(cores=1))
  suppressWarnings(elpd_a <- fit_a$loo(cores=1))

  expect_s3_class(elpd_m, "psis_loo")
  expect_s3_class(elpd_a, "psis_loo")
})
