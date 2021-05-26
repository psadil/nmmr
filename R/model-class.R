#' Model object, ready for sampling
#'
#' @export
Model <- R6::R6Class(
  # TODO: implement prior predictive checks

  "Model",
  # inherit = cmdstanr:::CmdStanModel,
  public = list(

    #' @field cmdstanr_version Version of cmdstanr used to build models
    cmdstanr_version = packageVersion("cmdstanr"),

    #' @field cmdstan_version Version of cmdstan used to build models
    cmdstan_version = cmdstanr::cmdstan_version(),

    #' @param d dataframe from which to make standata.
    #' @param form Form of the neuromodulation (additive or multiplicative)
    #' @param prior A [`Prior`]
    initialize = function(d,
                          form,
                          prior = Prior$new()) {
      checkmate::assert_choice(form, c("additive", "multiplicative"))
      checkmate::assert_class(prior, "Prior")

      private$.cmdstanmodel <- cmdstanr::cmdstan_model(private$.write_file())
      private$.form <- form
      private$.prior <- prior
      private$.standata <- self$make_standata(d = d)
    },

    #' @description
    #' Draw samples from the posterior of the model
    #'
    #' @param data optional new data. If present, this will be used instead
    #' of the internally stored data (created when the object was initialized)
    #' @param ... arguments passed to [cmdstanr::sample()][cmdstanr::model-method-sample()].
    #'
    #' @seealso [cmdstanr::sample()][cmdstanr::model-method-sample]
    sample = function(data = NULL, ...) {
      if (!is.null(data)) {
        standata <- self$make_standata(d = data)
      } else {
        standata <- self$standata
      }
      fit <- self$cmdstanmodel$sample(data = standata, ...)
      modelfit <- ModelMCMC$new(standata = standata, cmdstanmcmc = fit)
      return(modelfit)
    },

    #' Prepare data for running model
    #'
    #' @param d dataframe from which to make standata.
    #'
    #' @return named list
    make_standata = function(d) {
      checkmate::assert_data_frame(d, any.missing = FALSE)
      checkmate::assert_subset(c("sub", "voxel", "contrast", "orientation", "y"), names(d))
      checkmate::assert_numeric(d$orientation, lower = -pi, upper = pi)

      if (self$form == "additive") {
        ntfp_min <- 0
        modulation <- 0
      } else if (self$form == "multiplicative") {
        ntfp_min <- 1
        modulation <- 1
      }

      tmp <- d |>
        dplyr::mutate(
          orientation_tested = as.numeric(factor(round(.data$orientation, 3)))
        ) |>
        dplyr::arrange(.data$voxel, .data$contrast, .data$orientation)

      sub_by_vox <- tmp |>
        dplyr::distinct(.data$sub, .data$voxel) |>
        magrittr::use_series("sub") |>
        as.numeric()

      n_unique_orientations_vox <- tmp |>
        dplyr::group_by(.data$voxel) |>
        dplyr::summarise(n = dplyr::n_distinct(.data$orientation_tested)) |>
        magrittr::use_series("n")

      unique_orientations <- sort(unique(tmp$orientation))

      ori_by_vox <- matrix(
        0,
        nrow = dplyr::n_distinct(tmp$voxel),
        ncol = length(unique_orientations)
      )

      ori_by_vox0 <- tmp |>
        dplyr::distinct(.data$voxel, .data$orientation_tested) |>
        dplyr::group_nest(.data$voxel)
      for (v in 1:nrow(ori_by_vox0)) {
        ori_by_vox[v, 1:n_unique_orientations_vox[v]] <- ori_by_vox0$data[[v]]$orientation_tested
      }

      # voxels must already be differentiated by ses or sub (if relevant)
      X <- interaction(tmp$voxel, tmp$contrast, tmp$orientation_tested, lex.order = TRUE, drop = TRUE) |>
        as.numeric()

      stan_data <- tmp |>
        tidybayes::compose_data() |>
        c(.data,
          n_unique_orientations = length(unique_orientations),
          unique_orientations = list(unique_orientations),
          n_unique_orientations_vox = list(n_unique_orientations_vox),
          ori_by_vox = list(ori_by_vox),
          X = list(X),
          sub_by_vox = list(sub_by_vox),
          ntfp_min = ntfp_min,
          modulation = modulation
        )

      stan_data$orientation_tested <- NULL
      stan_data$orientation <- NULL

      return(c(stan_data, private$.prior$as_list()))
    }
  ),
  active = list(

    #' @field standata used to fit model
    standata = function(value) {
      if (missing(value)) {
        private$.standata
      } else {
        stop("`$standata` is read only", call. = FALSE)
      }
    },

    #' @field form used to fit model
    form = function(value) {
      if (missing(value)) {
        private$.form
      } else {
        stop("`$form` is read only", call. = FALSE)
      }
    },

    #' @field prior used to fit model
    prior = function(value) {
      if (missing(value)) {
        private$.prior
      } else {
        stop("`$prior` is read only", call. = FALSE)
      }
    },

    #' @field cmdstanmodel Underlying [`cmdstanr::CmdStanModel`]
    cmdstanmodel = function(value) {
      if (missing(value)) {
        private$.cmdstanmodel
      } else {
        stop("`$cmdstanmodel` is read only", call. = FALSE)
      }
    }
  ),
  private = list(
    .standata = list(),
    .form = NA_character_,
    .prior = NULL,
    .cmdstanmodel = NULL,
    .write_file = function() cmdstanr::write_stan_file(stan_code$vtf)
  )
)
