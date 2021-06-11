small <- sub02 |>
dplyr::filter(forcats::fct_match(voxel, c("191852", "197706"))) |>
dplyr::mutate(voxel = forcats::fct_drop(voxel))

m <- Model$new(small, form = "multiplicative", id_var = voxel)

test_that("model is available", {
  checkmate::expect_r6(m$cmdstanmodel, classes = "CmdStanModel")
  expect_type(m$cmdstanmodel$code(), "character")
})

test_that("read-only fields cannot be modified", {
  testthat::expect_error(m$form <- "new")
  testthat::expect_error(m$prior <- Prior$new())
  testthat::expect_error(m$standata <- list())
  testthat::expect_error(m$cmdstanmodel <- NULL)
})

testthat::capture_output({
  suppressMessages(f <- m$sample(
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 2,
    refresh = 0,
    show_messages = FALSE
  ))
})

test_that("multiplicative model runs", {
  checkmate::expect_r6(f, classes = c("ModelMCMC"))
})

test_that("new data can be passed during sampling", {
  testthat::capture_output({
    suppressMessages(f2 <- m$sample(
      data = m$make_standata(small, id_var = voxel),
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 2,
      refresh = 0,
      show_messages = FALSE
    ))
  })
  checkmate::expect_r6(f2, classes = c("ModelMCMC"))
  testthat::expect_identical(f$standata, f2$standata)
})

test_that("ModelFit object contains the data", {
  expect_identical(m$standata, f$standata)
})

test_that("Prior is accessible", {
  expect_equal(m$prior, Prior$new())
})
