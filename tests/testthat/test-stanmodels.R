test_that("stanmodels contains the right models", {
  expect_identical(names(stanmodels), c("vtf","orthogonal"))
})