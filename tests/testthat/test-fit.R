
test_that("standata has all necessary elments", {
  utils::data("sub02")
  standata <- make_standata(sub02, "additive", rep(0, 23))
  expect_identical(
    names(standata),
    c("sub", "n_sub", "voxel", "n_voxel", "contrast", "n_contrast",
      "y", "n", "priors", "n_unique_orientations",
      "unique_orientations", "n_unique_orientations_vox", "ori_by_vox",
      "X", "sub_by_vox", "ntfp_min", "modulation"))
})
