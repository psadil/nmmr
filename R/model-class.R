#' Model object, ready for sampling
#'
#' @export
Model <- R6::R6Class(
  "Model",
  public = list(
    #' @field form Form of the neuromodulation (additive or multiplicative)
    form = NULL,

    #' @description
    #' Create a new Model object.
    #' @param form Form of neuromodulation in the model.
    #' @return A new `Model` object.
    initialize = function(form) {

      checkmate::assert_choice(form, c("additive","multiplicative"))

      self$form <- form
    },

    #' @description
    #' Draw samples from the posterior of the model
    #'
    #' @param prior An object of class [`Prior`]
    #' @param d dataframe from which to make standata
    #' @param standata data to pass directly to stan. Can be produced with [make_standata()]. If present,
    #'      this data will be used rather than the data stored in `d`.
    #' @param ... further arguments passed to [rstan::sampling()].
    #'
    #' @seealso
    #' [rstan::sampling()]
    #'
    #' @return A new [`ModelFit`].
    sample = function(
      d = NULL,
      standata = NULL,
      prior = Prior$new(),
      ...){

      if(is.null(standata))
        standata <- self$make_standata(d=d, prior=prior)

      stanfit <- rstan::sampling(
        object = stanmodels[["vtf"]],
        data = standata,
        ...
      )

      modelfit <- ModelFit$new(
        rawdata = d,
        prior = prior,
        stanfit = stanfit,
        model = self
      )

      return(modelfit)
    },

    #' Prepare data for running model
    #'
    #' @param d dataframe from which to make standata. The output of this can be passed to [vtf()].
    #' @param prior A [`Prior`]
    #' @return named list
    make_standata = function(d, prior = Prior$new()){

      checkmate::assert_data_frame(d, any.missing = FALSE)
      checkmate::assert_subset(c("sub", "voxel", "contrast", "orientation", "y"), names(d))
      checkmate::assert_numeric(d$orientation, lower = -pi, upper = pi)
      checkmate::assert_class(prior, "Prior")

      if(self$form == "additive"){
        ntfp_min <- 0
        modulation <- 0
      }else if(self$form =="multiplicative"){
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
  )
)

