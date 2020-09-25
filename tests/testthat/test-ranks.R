
qs <- c(0, 0.9)
ranks <- build_ranks(sub02, quantiles = qs)

test_that("ranks can be built", {
  expect_equal(names(ranks), c("rank", "quantile", "low", "high"))
  expect_identical(unique(ranks$quantile), qs)
})

test_that("built ranks can be plotted", {
  p <- visualize_ranks(ranks)
  expect_s3_class(p, "gg")
})

