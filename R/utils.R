#' ModelFit Bind list of [`ModelFit`] objects into single fit
#'
#' @param ModelFits list of ModelFit objects
#'
#' @details
#' thin wrapper around [rstan::sflist2stanfit()]
#'
#' @return [deg()] converts radian to degree, [rad()] converts degress to radian
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


#' Convert between angles and radians
#' @aliases rad deg
#'
#' @param radian,degree angles with different units
#' @name angles
NULL

#' @rdname angles
#' @export
deg <- function(radian){
  (radian * 180) / pi
}

#' @rdname angles
#' @export
rad <- function(degree){
  (degree * pi) / 180
}


