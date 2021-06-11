#' Model object, ready for sampling
#'
#' @description `Deming` facilitates a non-parametric check of neuromodulation.
#'  This class is a wrapper around a [`cmdstanr::CmdStanModel`] class.
#'
#' @export
Deming <- R6::R6Class(

  "Deming",
  public = list(

    #' @field cmdstanr_version Version of cmdstanr used to build models
    cmdstanr_version = packageVersion("cmdstanr"),

    #' @field cmdstan_version Version of cmdstan used to build models
    cmdstan_version = cmdstanr::cmdstan_version(),

    #' @description
    #' Initialize new instance of class Deming
    #'
    #' @param d dataframe from which to make standata.
    #' @param x,y Names of columns in d which contain the x and y values
    #' @param tuning_var Name of column across which there was testing
    #' @param id_var Name of column indexing ids
    #' @param prior A [`DemingPrior`]
    #' @examples
    #' m <- sub02 |>
    #'      tidyr::pivot_wider(names_from = contrast, values_from = y) |>
    #'      dplyr::mutate(orientation = factor(orientation)) |>
    #'      Deming$new(low, high, tuning_var = orientation, id_var = voxel)
    #' m
    #'
    #' m$cmdstanmodel
    initialize = function(d,
                          x,
                          y,
                          tuning_var,
                          id_var,
                          prior = DemingPrior$new()) {
      checkmate::assert_class(prior, "DemingPrior")

      private$.prior <- prior
      private$.standata <- self$make_standata(d, {{ x }}, {{ y }}, {{ tuning_var }}, id_var = {{ id_var }})
    },

    #' @description
    #' Prepare data for running model
    #'
    #' @param d dataframe from which to make standata.
    #' @param x,y Names of columns in d which contain the x and y values.
    #' @param tuning_var Name of column across which there was testing. Column must be a factor.
    #' @param id_var Name of column indexing units (e.g., voxels, cells). Column must be a factor.
    #'
    #' @return named list
    make_standata = function(d, x, y, tuning_var, id_var) {
      x_name <- as_name(enquo(x))
      y_name <- as_name(enquo(y))
      tuning_name <- as_name(enquo(tuning_var))
      id_name <- as_name(enquo(id_var))

      checkmate::assert_data_frame(d)
      checkmate::assert_subset(x_name, names(d))
      checkmate::assert_subset(y_name, names(d))
      checkmate::assert_subset(tuning_name, names(d))
      checkmate::assert_subset(id_name, names(d))

      checkmate::assert_factor(d[[tuning_name]])
      checkmate::assert_factor(d[[id_name]])

      stan_data <- d |>
      dplyr::arrange({{ id_var }}, {{ tuning_var }}) |>
      dplyr::mutate(id_tuning = interaction({{ id_var }}, {{ tuning_var }}, lex.order = TRUE)) |>
      dplyr::rename(x = {{ x }}, y = {{ y }}, "tuning" = {{ tuning_var }}, "id" = {{ id_var }}) |>
      tidybayes::compose_data()

      return(c(stan_data, private$.prior$as_list()))
    },

    #' @description
    #' Draw samples from the posterior of the model
    #'
    #' @param ... arguments passed to [cmdstanr::sample()][cmdstanr::model-method-sample()].
    #'
    #' @return An object of class [`cmdstanr::CmdStanMCMC`]
    sample = function(...) {
      fit <- self$cmdstanmodel$sample(data = self$standata, ...)
      return(fit)
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
        get_stanmodel("deming")
        # private$.cmdstanmodel
      } else {
        stop("`$cmdstanmodel` is read only", call. = FALSE)
      }
    }
  ),
  private = list(
    .standata = list(),
    # .cmdstanmodel = NULL,
    .prior = NULL
  )
)
