
#' Prepare data for running model
#'
#' @param d dataframe from which to make standata. The output of this can be passed to [vtf()].
#' @param form Form of the neuromodulation (additive or multiplicative)
#' @param prior A [`Prior`]
#' @return named list
#' @export
make_standata = function(d, form, prior = Prior$new()){

  checkmate::assert_data_frame(d, any.missing = FALSE)
  checkmate::assert_subset(c("sub", "voxel", "contrast", "orientation", "y"), names(d))
  checkmate::assert_numeric(d$orientation, lower = -pi, upper = pi)
  checkmate::assert_class(prior, "Prior")

  checkmate::assert_choice(form, c("additive","multiplicative"))

  if(form == "additive"){
    ntfp_min <- 0
    modulation <- 0
  }else if(form =="multiplicative"){
    ntfp_min <- 1
    modulation <- 1
  }

  tmp <- d %>%
    dplyr::mutate(
      orientation_tested = as.numeric(factor(round(.data$orientation,3)))) %>%
    dplyr::arrange(.data$voxel, .data$contrast, .data$orientation)

  sub_by_vox <- tmp %>%
    dplyr::distinct(.data$sub, .data$voxel) %>%
    magrittr::use_series("sub") %>%
    as.numeric()

  n_unique_orientations_vox <- tmp %>%
    dplyr::group_by(.data$voxel) %>%
    dplyr::summarise(n = dplyr::n_distinct(.data$orientation_tested)) %>%
    magrittr::use_series("n")

  unique_orientations <- sort(unique(tmp$orientation))

  ori_by_vox <- matrix(
    0,
    nrow = dplyr::n_distinct(tmp$voxel),
    ncol = length(unique_orientations))

  ori_by_vox0 <- tmp %>%
    dplyr::distinct(.data$voxel, .data$orientation_tested) %>%
    dplyr::group_nest(.data$voxel)
  for(v in 1:nrow(ori_by_vox0))
    ori_by_vox[v,1:n_unique_orientations_vox[v]] <- ori_by_vox0$data[[v]]$orientation_tested

  # voxels must already be differentiated by ses or sub (if relevant)
  X <- interaction(tmp$voxel, tmp$contrast, tmp$orientation_tested, lex.order = TRUE, drop = TRUE) %>%
    as.numeric()

  stan_data <- tmp %>%
    tidybayes::compose_data() %>%
    c(.data,
      n_unique_orientations = length(unique_orientations),
      unique_orientations = list(unique_orientations),
      n_unique_orientations_vox = list(n_unique_orientations_vox),
      ori_by_vox = list(ori_by_vox),
      X = list(X),
      sub_by_vox = list(sub_by_vox),
      ntfp_min = ntfp_min,
      modulation = modulation)

  stan_data$orientation_tested <- NULL
  stan_data$orientation <- NULL

  return( c(stan_data, prior$as_list()) )
}
