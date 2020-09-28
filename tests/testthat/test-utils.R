test_that("angle conversion works", {
  expect_equal(deg(-pi), -180)
  expect_equal(deg(pi), 180)
  expect_equal(deg(0), 0)
  expect_equal(rad(-180), -pi)
  expect_equal(rad(180), pi)
  expect_equal(rad(0), 0)
})
