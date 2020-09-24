small <- sub02 %>%
  dplyr::filter(forcats::fct_match(voxel, c("191852","197706"))) %>%
  dplyr::mutate(voxel = forcats::fct_drop(voxel))

suppressWarnings(
  fit1 <- vtf(
    model = "multiplicative",
    d = small,
    chains = 1,
    iter = 2,
    warmup = 1,
    refresh = 0))

suppressWarnings(
  fit2 <- vtf(
    model = "multiplicative",
    d = small,
    chains = 1,
    iter = 2,
    warmup = 1,
    refresh = 0))

fit <- bind_fits(list(fit1, fit2))

test_that("multiplicative model runs", {

  expect_s4_class(fit1, "VtuneFit")
  expect_s4_class(fit, "VtuneFit")

})

test_that("VtuneFit object contains the original data", {

  s <- make_standata(
    small,
    "multiplicative")

  expect_identical(small, rawdata(fit))
  expect_identical(s, standata(fit))

})
