test_that("stanmodels contains the right models", {
  expect_identical(names(stan_code), c("vtf", "deming"))
  expect_type(stan_code$deming, "character")
  expect_type(stan_code$vtf, "character")
})
