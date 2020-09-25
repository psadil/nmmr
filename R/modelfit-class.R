#' ModelFit object, output from sampling
#'
#' @export
#' @seealso [bind_fits()]
ModelFit <- R6::R6Class(
  "ModelFit",
  public = list(

    #' @field rawdata The original data that was used to prepare the model
    rawdata = NULL,

    #' @field prior The [`Prior`] that was used to fit the model
    prior = NULL,

    #' @field stanfit [`rstan::stanfit-class`] output
    stanfit = NULL,

    #' @field model [`Model`] object used to generate the fit
    model = NULL,

    #' @description
    #' Create a new ModelFit
    #' @param rawdata dataframe from which to make standata
    #' @param prior [`Prior`] that was used to fit the model
    #' @param model [`Model`] used to draw samples
    #' @param stanfit [`rstan::stanfit-class`] output by model
    #' @return A new `ModelFit` object.
    initialize = function(
      rawdata,
      prior,
      model,
      stanfit) {

      checkmate::assert_class(model, "Model")

      self$rawdata <- rawdata
      self$prior <- prior
      self$model <- model
      self$stanfit <- stanfit
    },

    #' @description Reconstruct the list of data that was passed to Stan
    #' @return A named list
    standata = function(){
      return(self$model$make_standata(d = self$rawdata, prior = self$prior))
    },

    #' Estimate the LOO score
    #' @param cores int
    #' @return A `loo::psis_loo` object
    loo = function(cores = 1){
      checkmate::assert_integerish(cores, lower=1)

      vtf0 <- self$make_vtf0(cores = cores)
      stand <- self$standata()
      data_ <- data.frame(
        y = stand$y,
        X = stand$X,
        voxel = stand$voxel)

      sigma <- rstan::extract(self$stanfit, pars = "sigma", permuted = FALSE)
      n_draw <- dim(sigma)[1]
      n_chain <- dim(sigma)[2]

      draws <- list(
        vtf0 = do.call(
          rbind,
          lapply(1:n_chain, function(i) vtf0[,i,])),
        sigma = do.call(
          rbind,
          lapply(1:n_chain, function(i) sigma[,i,])))
      chain_id <- rep(1:n_chain, each = n_draw)

      rm(sigma, vtf0, x, stand)

      r_eff <- loo::relative_eff(
        self$.lfun,
        chain_id = chain_id,
        data = data_,
        draws = draws,
        cores = cores)

      psis_loo <- loo::loo(
        self$.llfun,
        data = data_,
        draws = draws,
        r_eff = r_eff,
        cores = cores)

      return(psis_loo)
    },


    #' create voxel tuning function posterior
    #'
    #' @param cores integer number of cores over which to parallelize
    #'
    #' @return array n_draws x n_chains x n_unique obs. for use in [ModelFit$loo()]
    make_vtf0 = function(cores = 1){
      stand <- self$standata()

      x <- rstan::As.mcmc.list(
        self$stanfit,
        pars = c("v_gamma", "v_kappa", "v_alpha", "meanAngle", "v_ntfp")) %>%
        posterior::as_draws_df() %>%
        tidyr::pivot_longer(
          cols = c(-.data$.iteration, -.data$.chain, -.data$.draw),
          names_to = c(".variable","voxel"),
          names_pattern = "(.*)\\[(.*)\\]",
          values_to = ".estimate") %>%
        dplyr::mutate(voxel = as.numeric(.data$voxel)) %>%
        tidyr::pivot_wider(names_from = ".variable", values_from = ".estimate") %>%
        dplyr::group_nest(.data$.iteration)

      n_chain <- dplyr::n_distinct(x$data[[1]]$.chain)
      n_iter <- dplyr::n_distinct(x$.iteration)
      vtf <- array(dim = c(n_iter, n_chain, max(stand$X) ))

      xx <- parallel::mclapply(
        1:nrow(x),
        FUN = function(i) private$.make_vtf0_iter(x$data[[i]], stand),
        mc.cores = cores)

      for(i in 1:n_iter){
        vtf[i,,] <- xx[[i]]
      }

      return(vtf)
    },


    #' Likelihood function
    #'
    #' @param data_i single row of data
    #' @param draws posterior
    #'
    #' @return The likelihood of a single observation
    .lfun = function(data_i, draws){
      # each time called internally within loo the arguments will be equal to:
      # data_i: ith row of fake_data (fake_data[i,, drop=FALSE])
      # draws: entire fake_posterior matrix
      ll <- stats::dnorm(data_i$y, mean = draws$vtf0[,data_i$X], sd = draws$sigma[,data_i$voxel])
      return(ll)
    },

    #' Log-Likelihood function
    #'
    #' @param data_i single row of data
    #' @param draws posterior
    #'
    #' @return The log-likelihood of a single observation
    .llfun = function(data_i, draws){
      # each time called internally within loo the arguments will be equal to:
      # data_i: ith row of fake_data (fake_data[i,, drop=FALSE])
      # draws: entire fake_posterior matrix
      ll <- stats::dnorm(data_i$y, mean = draws$vtf0[,data_i$X], sd = draws$sigma[,data_i$voxel], log = TRUE)
      return(ll)
    }
  ),
  private = list(
    .make_vtf0_iter = function(xx, stand){

      d0 <- xx %>%
        dplyr::mutate(
          ori = purrr::map(
            .data$voxel,
            ~stand$unique_orientations[stand$ori_by_vox[.x, 1:stand$n_unique_orientations_vox[.x]]])) %>%
        tidyr::unnest(.data$ori) %>%
        dplyr::mutate(resp_to_ori = exp(.data$v_kappa * cos(.data$ori - .data$meanAngle))) %>%
        dplyr::select(-.data$meanAngle, -.data$v_kappa) %>%
        dplyr::group_by(.data$voxel, .data$.chain) %>%
        dplyr::mutate(resp_to_ori = .data$v_gamma * (.data$resp_to_ori / sum(.data$resp_to_ori))) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.data$v_gamma) %>%
        tidyr::crossing(contrast = factor(c("low", "high"), levels = c("low", "high")))

      if(stand$modulation == 0){
        d2 <- d0 %>%
          dplyr::mutate(
            vtf0 = dplyr::if_else(
              forcats::fct_match(.data$contrast, "low"),
              .data$resp_to_ori,
              .data$resp_to_ori + .data$v_ntfp),
            vtf0 = .data$vtf0 + .data$v_alpha)
      }else if(stand$modulation == 1){
        d2 <- d0 %>%
          dplyr::mutate(
            vtf0 = dplyr::if_else(
              forcats::fct_match(.data$contrast, "low"),
              .data$resp_to_ori,
              .data$resp_to_ori * .data$v_ntfp),
            vtf0 = .data$vtf0 + .data$v_alpha)
      }

      out <- d2 %>%
        dplyr::mutate(idx = interaction(.data$ori, .data$contrast, .data$voxel)) %>%
        dplyr::arrange(.data$.chain, .data$idx) %>%
        dplyr::select(.data$idx, .data$.chain, .data$vtf0) %>%
        tidyr::pivot_wider(names_from = "idx", values_from = "vtf0") %>%
        dplyr::select(-.data$.chain) %>%
        as.matrix()

      out
    }
  )
)
