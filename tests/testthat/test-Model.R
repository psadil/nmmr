small <- sub02 %>%
  dplyr::filter(forcats::fct_match(voxel, c("191852","197706"))) %>%
  dplyr::mutate(voxel = forcats::fct_drop(voxel))

m <- Model$new(small, form = "multiplicative")

{
  sink("/dev/null")
  suppressMessages(
    f <- m$sample(
      iter_warmup = 5,
      iter_sampling = 5,
      chains = 2,
      refresh = 0,
      show_messages = FALSE))
  sink()
}

test_that("multiplicative model runs", {
  checkmate::expect_r6(f, classes = c("ModelMCMC"))
})

test_that("new data can be passed during sampling", {
  {
    sink("/dev/null")
    suppressMessages(
      f2 <- m$sample(
        data = small,
        iter_warmup = 5,
        iter_sampling = 5,
        chains = 2,
        refresh = 0,
        show_messages = FALSE))
    sink()
    }
  checkmate::expect_r6(f2, classes = c("ModelMCMC"))
  testthat::expect_identical(f$standata, f2$standata)
})

test_that("ModelFit object contains the data", {
  expect_identical(m$standata, f$standata)
})
