small <- sub02 %>%
  dplyr::filter(forcats::fct_match(voxel, c("191852","197706"))) %>%
  dplyr::mutate(voxel = forcats::fct_drop(voxel))

test_that("loo runs", {

  m <- make_standata(small, form = "multiplicative")
  a <- make_standata(small, form = "additive")

  suppressMessages(
    f_m <- stanmodels$vtf$sample(
      data = m,
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 2,
      refresh = 0,
      show_messages = FALSE)
  )

  suppressMessages(
    f_a <- stanmodels$vtf$sample(
      data = a,
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 2,
      refresh = 0,
      show_messages = FALSE)
  )

  suppressWarnings(elpd_m <- f_m$loo(cores=1))
  suppressWarnings(elpd_a <- f_a$loo(cores=1))

  expect_s3_class(elpd_m, "psis_loo")
  expect_s3_class(elpd_a, "psis_loo")
})
