#' List of priors
#'
#' @export
Prior <- R6::R6Class(
  "Prior",
  public = list(

    #' @field sigma_loc \eqn{\sigma_{loc}} ~ `gamma(sigma_loc[1], sigma_loc[2])`
    sigma_loc = NULL,

    #' @field sigma_scale  \eqn{\sigma_{scale}} ~ `gamma(sigma_scale[1], sigma_scale[2])`
    sigma_scale = NULL,

    #' @field gamma_loc  \eqn{\gamma_{loc}} ~ `normal(0, gamma_loc[2])`
    gamma_loc = NULL,

    #' @field gamma_scale  \eqn{\gamma_{scale}} ~ `gamma(gamma_scale[1], gamma_scale[2])`
    gamma_scale = NULL,

    #' @field kappa_loc  \eqn{\kappa_{loc}} ~ `gamma(kappa_loc[1], kappa_loc[2])`
    kappa_loc = NULL,

    #' @field kappa_scale \eqn{\kappa_{scale}} ~ `gamma(kappa_scale[1], kappa_scale[2])`
    kappa_scale = NULL,

    #' @field alpha_loc \eqn{\alpha_{loc}} ~ `normal(alpha_loc[1], alpha_loc[2])`
    alpha_loc = NULL,

    #' @field alpha_scale \eqn{\alpha_{scale}} ~ `gamma(alpha_scale[1], alpha_scale[2])`
    alpha_scale = NULL,

    #' @field ntfp_loc \eqn{ntfp_{loc}} ~ `normal(ntfp_loc[1], ntfp_loc[2])`
    ntfp_loc = NULL,

    #' @field ntfp_scale \eqn{ntfp_{scale}} ~ `gamma(ntfp_scale[1], ntfp_scale[2])`
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
                          sigma_loc = c(2, 1 / 2),
                          sigma_scale = c(2, 1 / 2),
                          gamma_loc = 5,
                          gamma_scale = c(2, 1 / 20),
                          kappa_loc = c(3, 1),
                          kappa_scale = c(3, 1),
                          alpha_loc = c(0, 5),
                          alpha_scale = c(2, 1 / 2),
                          ntfp_loc = 0.5,
                          ntfp_scale = c(2, 3)) {
      checkmate::assert_numeric(sigma_loc, lower = 0, len = 2)
      checkmate::assert_numeric(sigma_scale, lower = 0, len = 2)
      checkmate::assert_number(gamma_loc, lower = 0)
      checkmate::assert_numeric(gamma_scale, lower = 0, len = 2)
      checkmate::assert_numeric(kappa_loc, lower = 0, len = 2)
      checkmate::assert_numeric(kappa_scale, lower = 0, len = 2)
      checkmate::assert_numeric(alpha_loc, lower = 0, len = 2)
      checkmate::assert_numeric(alpha_scale, lower = 0, len = 2)
      checkmate::assert_number(ntfp_loc, lower = 0)
      checkmate::assert_numeric(ntfp_scale, lower = 0, len = 2)

      purrr::walk2(rlang::fn_fmls_names(), rlang::fn_fmls_syms(), function(x, y) self |> magrittr::inset2(x, eval(y)))
    },

    #' @description Convert object to a list.
    #' This is useful when passing data to Stan
    as_list = function() {
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
        prior_ntfp_scale = self$ntfp_scale
      )
    }
  )
)

#' List of priors
#'
#' @export
DemingPrior <- R6::R6Class(
  "DemingPrior",
  public = list(

    #' @field z_mu_mu \eqn{\mu^{\mu^z}} ~ `normal(z_mu_mu[1], z_mu_mu[2])`
    z_mu_mu = NULL,

    #' @field z_mu_sigma \eqn{\sigma^{\mu^z}} ~ `normal(z_mu_sigma[1], z_mu_sigma[2])`
    z_mu_sigma = NULL,

    #' @field z_sigma_mu \eqn{\mu^{\sigma^z}} ~ `normal(z_sigma_mu[1], z_sigma_mu[2])`
    z_sigma_mu = NULL,

    #' @field z_sigma_sigma \eqn{\sigma^{\sigma^z}} ~ `normal(z_sigma_sigma[1], z_sigma_sigma[2])`
    z_sigma_sigma = NULL,

    #' @field x_sigma_mu \eqn{\mu^{\sigma^x}} ~ `normal(x_sigma_mu[1], x_sigma_mu[2])`
    x_sigma_mu = NULL,

    #' @field x_sigma_sigma \eqn{\sigma^{\sigma^x}} ~ `normal(x_sigma_sigma[1], x_sigma_sigma[2])`
    x_sigma_sigma = NULL,

    #' @field y_sigma_mu \eqn{\mu^{\sigma^y}} ~ `normal(y_sigma_mu[1], y_sigma_mu[2])`
    y_sigma_mu = NULL,

    #' @field y_sigma_sigma \eqn{\sigma^{\sigma^y}} ~ `normal(y_sigma_sigma[1], y_sigma_sigma[2])`
    y_sigma_sigma = NULL,

    #' @field g_mu \eqn{\mu^g} ~ `normal(g_mu[1], g_mu[2])`
    g_mu = NULL,

    #' @field g_sigma \eqn{\sigma^g} ~ `normal(g_sigma[1], g_sigma[2])`
    g_sigma = NULL,

    #' @field a_mu \eqn{\mu^a} ~ `normal(a_mu[1], a_mu[2])`
    a_mu = NULL,

    #' @field a_sigma \eqn{\sigma^a} ~ `normal(a_sigma[1], a_sigma[2])`
    a_sigma = NULL,

    #' @description
    #'   Create a new Model object.
    #'   All values must be greater than 0.
    #'
    #' @param z_mu_mu vector of length 2
    #' @param z_mu_sigma vector of length 2
    #' @param z_sigma_mu vector of length 2
    #' @param z_sigma_sigma vector of length 2
    #' @param x_sigma_mu vector of length 2
    #' @param x_sigma_sigma vector of length 2
    #' @param y_sigma_mu vector of length 2
    #' @param y_sigma_sigma vector of length 2
    #' @param g_mu vector of length 2
    #' @param g_sigma vector of length 2
    #' @param a_mu vector of length 2
    #' @param a_sigma vector of length 2
    #'
    #' @return A new `Prior` object.
    initialize = function(
                          z_mu_mu = c(0, 10),
                          z_mu_sigma = c(0, 10),
                          z_sigma_mu = c(0, 10),
                          z_sigma_sigma = c(0, 10),
                          x_sigma_mu = c(0, 10),
                          x_sigma_sigma = c(0, 10),
                          y_sigma_mu = c(0, 10),
                          y_sigma_sigma = c(0, 10),
                          g_mu = c(0, 10),
                          g_sigma = c(0, 10),
                          a_mu = c(0, 10),
                          a_sigma = c(0, 10)) {
      purrr::walk(rlang::fn_fmls_syms(), ~ checkmate::assert_numeric(eval(.x), len = 2))
      purrr::walk2(rlang::fn_fmls_names(), rlang::fn_fmls_syms(), function(x, y) self[[x]] <- eval(y))
    },

    #' @description Convert object to a list.
    #' This is useful when passing data to Stan
    as_list = function() {
      list(
        prior_z_mu_mu = self$z_mu_mu,
        prior_z_mu_sigma = self$z_mu_sigma,
        prior_z_sigma_mu = self$z_sigma_mu,
        prior_z_sigma_sigma = self$z_sigma_sigma,
        prior_x_sigma_mu = self$x_sigma_mu,
        prior_x_sigma_sigma = self$x_sigma_sigma,
        prior_y_sigma_mu = self$y_sigma_mu,
        prior_y_sigma_sigma = self$y_sigma_sigma,
        prior_g_mu = self$g_mu,
        prior_g_sigma = self$g_sigma,
        prior_a_mu = self$a_mu,
        prior_a_sigma = self$a_sigma
      )
    }
  )
)
