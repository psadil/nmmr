#' Run One of the Stan Models
#'
#' @param model Form of neuromodulation. One of "multiplicative" or "additive".
#' @param priors named list. see output of [set_priors()].
#' @param d dataframe from which to make standata. The output of this can be passed to [vtf()].
#' @param standata data to pass directly to stan. Can be produced with [make_standata()]. If present,
#'      this data will be used rather than the data stored in `d`.
#' @param ... further arguments passed to [rstan::sampling()].
#'
#' @seealso
#' [rstan::sampling()]
#'
#' @return [VtuneFit]
#' @export
vtf <- function(model,
                d = NULL,
                standata = NULL,
                priors = set_priors(),
                ...){

  if(is.null(standata)){
    standata <- make_standata(
      d = d,
      priors = priors,
      model = model)
  }

  stanfit <- rstan::sampling(
    object = stanmodels[["vtf"]],
    data = standata,
    ...
  )

  methods::new("VtuneFit", stanfit, rawdata=d, standata=standata)
}




#' Prepare data for running model
#'
#' @param d dataframe from which to make standata. The output of this can be passed to [vtf()].
#' @param model Form of neuromodulation. One of "multiplicative" or "additive".
#' @param priors named list. see output of [set_priors()]
#'
#' @export
make_standata <- function(
  d,
  model,
  priors = set_priors()){

  checkmate::assert_choice(model, c("additive","multiplicative"))
  checkmate::assert_data_frame(
    d,
    any.missing = FALSE
  )
  checkmate::assert_subset(c("sub", "voxel", "contrast", "orientation", "y"), names(d))
  checkmate::assert_numeric(d$orientation, lower = -pi, upper = pi)

  if(model == "additive"){
    ntfp_min <- 0
    modulation <- 0
  }else if(model =="multiplicative"){
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

  return( c(stan_data, priors) )
}


#' Set priors for model
#'
#' @param prior_sigma_loc vector of length 2
#' @param prior_sigma_scale vector of length 2
#' @param prior_gamma_loc real
#' @param prior_gamma_scale vector of length 2
#' @param prior_kappa_loc vector of length 2
#' @param prior_kappa_scale vector of length 2
#' @param prior_alpha_loc vector of length 2
#' @param prior_alpha_scale vector of length 2
#' @param prior_ntfp_loc real
#' @param prior_ntfp_scale vector of length 2
#'
#'
#' @details
#'   all values must be greater than 0
#'
#'   \eqn{\sigma_loc ~ gamma(prior_sigma_loc[1], prior_sigma_loc[2])}
#'
#'   \eqn{\sigma_scale ~ gamma(prior_sigma_scale[1], prior_sigma_scale[2])}
#'
#'   \eqn{\gamma_loc ~ normal(0, prior_gamma_loc[2])}
#'
#'   \eqn{\gamma_scale ~ gamma(prior_gamma_scale[1], prior_gamma_scale[2])}
#'
#'   \eqn{\kappa_loc ~ gamma(prior_kappa_loc[1], prior_kappa_loc[2])}
#'
#'   \eqn{\kappa_scale ~ gamma(prior_kappa_scale[1], prior_kappa_scale[2])}
#'
#'   \eqn{\alpha_loc ~ normal(prior_alpha_loc[1], prior_alpha_loc[2])}
#'
#'   \eqn{\alpha_scale ~ gamma(prior_alpha_scale[1], prior_alpha_scale[2])}
#'
#'   \eqn{ntfp_loc ~ normal(prior_ntfp_loc[1], prior_ntfp_loc[2])}
#'
#'   \eqn{ntfp_scale ~ gamma(prior_ntfp_scale[1], prior_ntfp_scale[2])}
#'
#' @return named list
#' @export
#'
#' @examples
#'
#' set_priors()
#' set_priors(prior_sigma_loc = c(2, 1))
set_priors <- function(
  prior_sigma_loc = c(2, 1/2),
  prior_sigma_scale = c(2, 1/2),
  prior_gamma_loc = 5,
  prior_gamma_scale = c(2, 1/20),
  prior_kappa_loc = c(3, 1),
  prior_kappa_scale = c(3, 1),
  prior_alpha_loc = c(0, 5),
  prior_alpha_scale = c(2, 1/2),
  prior_ntfp_loc = 0.5,
  prior_ntfp_scale = c(2, 3)){

  checkmate::assert_numeric(prior_sigma_loc, lower = 0, len = 2)
  checkmate::assert_numeric(prior_sigma_scale, lower = 0, len = 2)
  checkmate::assert_numeric(prior_gamma_loc, lower = 0, len = 1)
  checkmate::assert_numeric(prior_gamma_scale, lower = 0, len = 2)
  checkmate::assert_numeric(prior_kappa_loc, lower = 0, len = 2)
  checkmate::assert_numeric(prior_kappa_scale, lower = 0, len = 2)
  checkmate::assert_numeric(prior_alpha_loc, lower = 0, len = 2)
  checkmate::assert_numeric(prior_alpha_scale, lower = 0, len = 2)
  checkmate::assert_numeric(prior_ntfp_loc, lower = 0, len = 1)
  checkmate::assert_numeric(prior_ntfp_scale, lower = 0, len = 2)

  list(
    prior_sigma_loc = prior_sigma_loc,
    prior_sigma_scale = prior_sigma_scale,
    prior_gamma_loc = prior_gamma_loc,
    prior_gamma_scale = prior_gamma_scale,
    prior_kappa_loc = prior_kappa_loc,
    prior_kappa_scale = prior_kappa_scale,
    prior_alpha_loc = prior_alpha_loc,
    prior_alpha_scale = prior_alpha_scale,
    prior_ntfp_loc = prior_ntfp_loc,
    prior_ntfp_scale = prior_ntfp_scale)
}
