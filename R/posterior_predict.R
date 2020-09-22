
# TODO: this should return N x n_draws matrix
posterior_predict.VtuneFit <- function(object,
                                       draws = NULL,
                                       seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  sigma <- as.matrix(object, pars = "sigma")
  mu <- as.matrix(object, pars = "mu")
  if(!is.null(draws)){
    mu <- mu[sample(nrow(mu), size=draws, replace=FALSE), ]
  }

  y_rep <- apply(mu,
                 MARGIN = 1,
                 .ppfun,
                 sigma = sigma)

  return(y_rep)
}

#' @describeIn VtuneFit sample from posterior predictive distribution
#' @importFrom rstantools posterior_predict
#'
#' @param object VtuneFit object
#' @param draws number of draws from the ppd.
#'   maximum is the number of samples in object. defaults to all samples
#' @param seed optional seed, passed to [base::set.seed()]
#' @export
setMethod("posterior_predict", "VtuneFit", posterior_predict.VtuneFit)

.ppfun <- function(mu, sigma){
  n <- length(sigma)
  stats::rnorm(n, mu, sigma)
}
