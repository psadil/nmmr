small <- sub02 %>%
  dplyr::filter(forcats::fct_match(voxel, c("191852", "197706"))) %>%
  dplyr::mutate(
    voxel = forcats::fct_drop(voxel),
    orientation = factor(orientation)) %>%
  tidyr::pivot_wider(names_from = contrast, values_from = y)

m <- Deming$new(small, low, high, orientation, voxel)

test_that("model is available", {
  checkmate::expect_r6(m$cmdstanmodel, classes = "CmdStanModel")
  expect_type(m$cmdstanmodel$code(), "character")
})

test_that("ModelFit object contains data", {
  expect_type(m$standata, "list")
})

test_that("read-only fields cannot be modified", {
  expect_error(m$form <- "new")
  expect_error(m$standata <- list())
  expect_error(m$cmdstanmodel <- NULL)
  testthat::expect_error(m$prior <- list())
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

test_that("The fit method just returns the CmdStanMCMC", {
  checkmate::expect_r6(f, classes = c("CmdStanMCMC"))
  testthat::expect_s3_class(f$summary(), "draws_summary")
})

test_that("Prior is accessible", {
  expect_equal(m$prior, DemingPrior$new())
})
