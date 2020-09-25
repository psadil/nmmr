small <- sub02 %>%
  dplyr::filter(forcats::fct_match(voxel, c("191852","197706"))) %>%
  dplyr::mutate(voxel = forcats::fct_drop(voxel))

m <- Model$new(form = "multiplicative")

suppressWarnings(
  fit1 <- m$sample(
  d=small,
  chains = 1,
  iter = 2,
  warmup = 1,
  refresh = 0))

suppressWarnings(
  fit2 <- m$sample(
    d=small,
    chains = 1,
    iter = 2,
    warmup = 1,
    refresh = 0))

fit <- bind_fits(list(fit1, fit2))

test_that("multiplicative model ran", {

  checkmate::expect_r6(fit1, classes = c("ModelFit"))
  checkmate::expect_r6(fit, classes = c("ModelFit"))

})


test_that("ModelFit object contains the original data", {

  expect_identical(small, fit$rawdata)

})
