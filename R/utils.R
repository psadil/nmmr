#' ModelFit Bind list of [`ModelFit`] objects into single fit
#'
#' @param ModelFits list of ModelFit objects
#'
#' @details
#' thin wrapper around [rstan::sflist2stanfit()]
bind_fits <- function(ModelFits){

  sfitlist <- vector(mode = "list", length = length(ModelFits))
  for(fit in 1:length(ModelFits)) sfitlist[[fit]] <- ModelFits[[fit]]$stanfit

  stanfit <- rstan::sflist2stanfit(sfitlist)

  modelfit <- ModelFit$new(
    rawdata = ModelFits[[1]]$rawdata,
    model = ModelFits[[1]]$model,
    prior = ModelFits[[1]]$prior,
    stanfit = stanfit)

  return(modelfit)
}
