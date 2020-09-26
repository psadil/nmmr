small <- sub02 %>%
  dplyr::filter(forcats::fct_match(voxel, c("191852","197706"))) %>%
  dplyr::mutate(voxel = forcats::fct_drop(voxel))

standata <- make_standata(sub02, "multiplicative")

suppressMessages(
  f <- stanmodels$vtf$sample(
    data = standata,
    iter_warmup = 5,
    iter_sampling = 5,
    chains = 2,
    refresh = 0,
    show_messages = FALSE)
)

test_that("multiplicative model runs", {
  checkmate::expect_r6(f, classes = c("ModelMCMC"))
})

test_that("ModelFit object contains the data", {
  expect_identical(standata, f$standata)
})
