#' Model object, ready for sampling
#'
#' @export
Model <- R6::R6Class(
  "Model",
  inherit = cmdstanr:::CmdStanModel,
  public = list(

    #' @field cmdstanr_version Version of cmdstanr used to build models
    cmdstanr_version = packageVersion("cmdstanr"),

    #' @field cmdstan_version Version of cmdstan used to build models
    cmdstan_version = cmdstanr::cmdstan_version(),

    #' @description
    #' Draw samples from the posterior of the model
    #'
    #' @param data data to pass directly to stan. Can be produced with [make_standata()].
    #' @param ... further arguments passed to [`cmdstanr::model-method-sample()`].
    #'
    #' @seealso
    #' [make_standata()]
    sample = function(data, ...){
      fit <- super$sample(data = data, ...)
      modelfit <- ModelMCMC$new(standata = data, cmdstanmcmc=fit)
      return(modelfit)
    }
  )
)

