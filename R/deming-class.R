#' Model object, ready for sampling
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
    #' Initialize new instance of class
    #'
    #' @param d dataframe from which to make standata.
    #' @param x,y Names of columns in d which contain the x and y values
    #' @param tuning_var Name of column across which there was testing
    #' @param voxel_var Name of column indexing voxels
    #'
    #' @examples
    #' sub02 %>%
    #'   tidyr::pivot_wider(names_from = contrast, values_from = y) %>%
    #'   dplyr::mutate(orientation = factor(orientation)) %>%
    #'   Deming$new(low, high, tuning_var = orientation, voxel_var = voxel)
    initialize = function(d, x, y, tuning_var, voxel_var = "voxel"){

      private$.cmdstanmodel <- stanmodels$deming
      private$.standata <- self$make_standata(d, {{x}}, {{y}}, {{tuning_var}}, {{voxel_var}})
    },

    #' @description
    #' Prepare data for running model
    #'
    #' @param d dataframe from which to make standata.
    #' @param x,y Names of columns in d which contain the x and y values.
    #' @param tuning_var Name of column across which there was testing. Column must be a factor.
    #' @param voxel_var Name of column indexing voxels. Column must be a factor.
    #'
    #' @return named list
    make_standata = function(d, x, y, tuning_var, voxel_var = "voxel"){

      x_name <- as_name(enquo(x))
      y_name <- as_name(enquo(y))
      tuning_name <- as_name(enquo(tuning_var))
      voxel_name <- as_name(enquo(voxel_var))

      checkmate::assert_data_frame(d)
      checkmate::assert_subset(x_name, names(d))
      checkmate::assert_subset(y_name, names(d))
      checkmate::assert_subset(tuning_name, names(d))
      checkmate::assert_subset(voxel_name, names(d))

      checkmate::assert_factor(d[[tuning_name]])
      checkmate::assert_factor(d[[voxel_name]])

      stan_data <- d %>%
        dplyr::arrange({{voxel_name}}, {{tuning_var}}) %>%
        dplyr::mutate(voxel_tuning = interaction({{voxel_name}}, {{tuning_var}}, lex.order = TRUE)) %>%
        dplyr::rename(x = {{x}}, y = {{y}}, "tuning" = {{tuning_var}}) %>%
        tidybayes::compose_data()

      return(stan_data)
    },

    #' @description
    #' Draw samples from the posterior of the model
    #'
    #' @param ... arguments passed to [cmdstanr::sample()][cmdstanr::model-method-sample()].
    sample = function(...){
      fit <- self$cmdstanmodel$sample(data = self$standata, ...)
      return(fit)
    }

  ),
  active = list(

    #' @field standata used to fit model
    standata = function(value){
      if (missing(value)) {
        private$.standata
      } else {
        stop("`$standata` is read only", call. = FALSE)
      }
    },

    #' @field cmdstanmodel Underlying [`cmdstanr::CmdStanModel`]
    cmdstanmodel = function(value){
      if (missing(value)) {
        private$.cmdstanmodel
      } else {
        stop("`$cmdstanmodel` is read only", call. = FALSE)
      }
    }
  ),
  private = list(
    .standata = list(),
    .cmdstanmodel = NULL
  )
)

