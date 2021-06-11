small <- sub02 |>
dplyr::filter(forcats::fct_match(voxel, c("191852", "197706"))) |>
dplyr::mutate(voxel = forcats::fct_drop(voxel))

test_that("loo runs", {
  m <- Model$new(small, form = "multiplicative", id_var = voxel)
  a <- Model$new(small, form = "additive", id_var = voxel)

  testthat::capture_output({
    suppressMessages(
      f_m <- m$sample(
        iter_warmup = 5,
        iter_sampling = 5,
        chains = 2,
        refresh = 0,
        show_messages = FALSE
      )
    )
  })

  testthat::capture_output({
    suppressMessages(
      f_a <- a$sample(
        iter_warmup = 5,
        iter_sampling = 5,
        chains = 2,
        refresh = 0,
        show_messages = FALSE
      )
    )
  })

  suppressWarnings(elpd_m <- f_m$loo(cores = 1))
  suppressWarnings(elpd_a <- f_a$loo(cores = 1))

  expect_s3_class(elpd_m, "psis_loo")
  expect_s3_class(elpd_a, "psis_loo")
})
