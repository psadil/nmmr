#' Run One of the Stan Models
#'
#' @param model Form of neuromodulation. One of "multiplicative" or "additive"
#' @param priors Vector of doubles.
#' @param standata data to pass directly to stan.
#' @param d dataframe from which to make standata. Will supersede standata. The function [make_standata]
#'        will be called with this as input
#' @param ... see [rstan::sampling()]
#'
#' @seealso
#' [rstan::sampling()]
#'
#' @return
#' @export
vtf <- function(model,
                priors,
                standata = NULL,
                d = NULL,
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



#' Make list of data for fitting
#'
#' @param d tbl_df (see betas)
#' @param model name
#' @param priors vector
#'
#' @export
make_standata <- function(
  d,
  model,
  priors){

  checkmate::assert_choice(model, c("additive","multiplicative"))

  stopifnot(exprs = {
    all(c("voxel", "contrast","orientation","y","sub") %in% names(d))
    length(priors) == 23
    max(d$orientation) < pi # stanmodel will expect orientations in radians
    min(d$orientation) < 0 # from -pi to pi
  })

  if(model == "additive"){
    ntfp_min <- 0
    modulation <- 0
  }else if(model =="multiplicative"){
    ntfp_min <- 1
    modulation <- 1
  }

  tmp <- d %>%
    dplyr::mutate(
      orientation_tested = as.numeric(factor(round(orientation,3)))) %>%
    dplyr::arrange(voxel,contrast,orientation)

  sub_by_vox <- tmp %>%
    dplyr::distinct(sub, voxel) %>%
    magrittr::use_series(sub) %>%
    as.numeric()

  n_unique_orientations_vox <- tmp %>%
    dplyr::group_by(voxel) %>%
    dplyr::summarise(n = dplyr::n_distinct(orientation_tested)) %>%
    magrittr::use_series(n)

  unique_orientations <- sort(unique(tmp$orientation))

  ori_by_vox <- matrix(0,
                       nrow = dplyr::n_distinct(tmp$voxel),
                       ncol = length(unique_orientations))

  ori_by_vox0 <- tmp %>%
    dplyr::distinct(voxel, orientation_tested) %>%
    dplyr::group_nest(voxel)
  for(v in 1:nrow(ori_by_vox0))
    ori_by_vox[v,1:n_unique_orientations_vox[v]] <- ori_by_vox0$data[[v]]$orientation_tested

  # voxels must already be differentiated by ses or sub (if relevant)
  X <- interaction(tmp$voxel, tmp$contrast, tmp$orientation_tested, lex.order = TRUE, drop = TRUE) %>%
    as.numeric()

  stan_data <- tmp %>%
    tidybayes::compose_data() %>%
    c(.,
      priors = list(priors),
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

  return(stan_data)
}



