#' List of priors
#'
#' @export
Prior <- R6::R6Class(
  "Prior",
  public = list(

    #' @field sigma_loc \eqn{\sigma_loc ~ gamma(sigma_loc[1], sigma_loc[2])}
    sigma_loc = NULL,

    #' @field sigma_scale  \eqn{\sigma_scale ~ gamma(sigma_scale[1], sigma_scale[2])}
    sigma_scale = NULL,

    #' @field gamma_loc  \eqn{\gamma_loc ~ normal(0, gamma_loc[2])}
    gamma_loc = NULL,

    #' @field gamma_scale  \eqn{\gamma_scale ~ gamma(gamma_scale[1], gamma_scale[2])}
    gamma_scale = NULL,

    #' @field kappa_loc  \eqn{\kappa_loc ~ gamma(kappa_loc[1], kappa_loc[2])}
    kappa_loc = NULL,

    #' @field kappa_scale \eqn{\kappa_scale ~ gamma(kappa_scale[1], kappa_scale[2])}
    kappa_scale = NULL,

    #' @field alpha_loc \eqn{\alpha_loc ~ normal(alpha_loc[1], alpha_loc[2])}
    alpha_loc = NULL,

    #' @field alpha_scale \eqn{\alpha_scale ~ gamma(alpha_scale[1], alpha_scale[2])}
    alpha_scale = NULL,

    #' @field ntfp_loc \eqn{ntfp_loc ~ normal(ntfp_loc[1], ntfp_loc[2])}
    ntfp_loc = NULL,

    #' @field ntfp_scale \eqn{ntfp_scale ~ gamma(ntfp_scale[1], ntfp_scale[2])}
    ntfp_scale = NULL,

    #' @description
    #'   Create a new Model object.
    #'   All values must be greater than 0.
    #'
    #' @param sigma_loc vector of length 2
    #' @param sigma_scale vector of length 2
    #' @param gamma_loc real
    #' @param gamma_scale vector of length 2
    #' @param kappa_loc vector of length 2
    #' @param kappa_scale vector of length 2
    #' @param alpha_loc vector of length 2
    #' @param alpha_scale vector of length 2
    #' @param ntfp_loc real
    #' @param ntfp_scale vector of length 2
    #'
    #' @return A new `Prior` object.
    initialize = function(
      sigma_loc = c(2, 1/2),
      sigma_scale = c(2, 1/2),
      gamma_loc = 5,
      gamma_scale = c(2, 1/20),
      kappa_loc = c(3, 1),
      kappa_scale = c(3, 1),
      alpha_loc = c(0, 5),
      alpha_scale = c(2, 1/2),
      ntfp_loc = 0.5,
      ntfp_scale = c(2, 3)) {

      checkmate::assert_numeric(sigma_loc, lower = 0, len = 2)
      checkmate::assert_numeric(sigma_scale, lower = 0, len = 2)
      checkmate::assert_numeric(gamma_loc, lower = 0, len = 1)
      checkmate::assert_numeric(gamma_scale, lower = 0, len = 2)
      checkmate::assert_numeric(kappa_loc, lower = 0, len = 2)
      checkmate::assert_numeric(kappa_scale, lower = 0, len = 2)
      checkmate::assert_numeric(alpha_loc, lower = 0, len = 2)
      checkmate::assert_numeric(alpha_scale, lower = 0, len = 2)
      checkmate::assert_numeric(ntfp_loc, lower = 0, len = 1)
      checkmate::assert_numeric(ntfp_scale, lower = 0, len = 2)

      self$sigma_loc <- sigma_loc
      self$sigma_scale <- sigma_scale
      self$gamma_loc <- gamma_loc
      self$gamma_scale <- gamma_scale
      self$kappa_loc <- kappa_loc
      self$kappa_scale <- kappa_scale
      self$alpha_loc <- alpha_loc
      self$alpha_scale <- alpha_scale
      self$ntfp_loc <- ntfp_loc
      self$ntfp_scale <- ntfp_scale
    },

    #' @description Convert object to a list.
    #' This is useful when passing data to Stan
    as_list = function(){
      list(
        prior_sigma_loc = self$sigma_loc,
        prior_sigma_scale = self$sigma_scale,
        prior_gamma_loc = self$gamma_loc,
        prior_gamma_scale = self$gamma_scale,
        prior_kappa_loc = self$kappa_loc,
        prior_kappa_scale = self$kappa_scale,
        prior_alpha_loc = self$alpha_loc,
        prior_alpha_scale = self$alpha_scale,
        prior_ntfp_loc = self$ntfp_loc,
        prior_ntfp_scale = self$ntfp_scale)
    }
  )
)
