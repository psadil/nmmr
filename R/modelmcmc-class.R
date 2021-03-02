#' ModelFit object, output from sampling
#'
#' @export
ModelMCMC <- R6::R6Class(
  "ModelMCMC",
  public = list(

    #' @field standata The original data that was used to prepare the model
    standata = NULL,

    #' @field cmdstanmcmc [`CmdStanMCMC`][cmdstanr::CmdStanMCMC] object
    cmdstanmcmc = NULL,

    #' @description
    #' Create a new ModelFit
    #' You probably don't want to call this
    #' @param standata Data that were used to generate the samples
    #' @param cmdstanmcmc [`cmdstanr::CmdStanMCMC`] output by model
    #' @return A new `ModelFit` object.
    initialize = function(standata, cmdstanmcmc) {
      self$cmdstanmcmc <- cmdstanmcmc
      self$standata <- standata
    },

    #' Estimate the LOO score
    #' @param cores int
    #' @return A `loo::psis_loo` object
    loo = function(cores = 1) {
      checkmate::assert_integerish(cores, lower = 1)

      vtf0 <- self$make_vtf0(cores = cores)

      draws <- list(
        vtf0 = posterior::as_draws_matrix(vtf0),
        sigma = self$cmdstanmcmc$draws(variables = "sigma") %>%
          posterior::as_draws_matrix()
      )

      chain_id <- rep(
        1:self$cmdstanmcmc$num_chains(),
        each = self$cmdstanmcmc$metadata()$iter_sampling
      )

      rm(vtf0)

      data_ <- data.frame(
        y = self$standata$y,
        X = self$standata$X,
        voxel = self$standata$voxel
      )

      r_eff <- loo::relative_eff(
        self$.lfun,
        chain_id = chain_id,
        data = data_,
        draws = draws,
        cores = cores
      )

      psis_loo <- loo::loo(
        self$.llfun,
        data = data_,
        draws = draws,
        r_eff = r_eff,
        cores = cores
      )

      return(psis_loo)
    },


    #' create voxel tuning function posterior
    #'
    #' @param cores integer number of cores over which to parallelize
    #'
    #' @return [`posterior::draws_array`],
    make_vtf0 = function(cores = 1) {
      x <- self$cmdstanmcmc$draws(variables = c("v_gamma", "v_kappa", "v_alpha", "meanAngle", "v_ntfp")) %>%
        posterior::as_draws_df() %>%
        tibble::as_tibble() %>%
        tidyr::pivot_longer(
          cols = c(-.data$.iteration, -.data$.chain, -.data$.draw),
          names_to = c(".variable", "voxel"),
          names_pattern = "(.*)\\[(.*)\\]",
          values_to = ".estimate"
        ) %>%
        dplyr::mutate(voxel = as.numeric(.data$voxel)) %>%
        tidyr::pivot_wider(names_from = ".variable", values_from = ".estimate") %>%
        dplyr::group_nest(.data$.iteration)

      n_chain <- dplyr::n_distinct(x$data[[1]]$.chain)
      n_iter <- dplyr::n_distinct(x$.iteration)
      vtf <- array(dim = c(n_iter, n_chain, max(self$standata$X)))

      if (!checkmate::test_os("windows")) {
        xx <- parallel::mclapply(
          1:nrow(x),
          FUN = function(i) private$.make_vtf0_iter(x$data[[i]]),
          mc.cores = cores
        )
      } else {
        cl <- parallel::makePSOCKcluster(cores)
        on.exit(parallel::stopCluster(cl))
        xx <- parallel::parLapply(
          cl = cl,
          X = 1:nrow(x),
          fun = function(i) private$.make_vtf0_iter(x$data[[i]])
        )
      }

      for (i in 1:n_iter) {
        vtf[i, , ] <- xx[[i]]
      }

      return(posterior::as_draws(vtf))
    },


    #' Likelihood function
    #'
    #' @param data_i single row of data
    #' @param draws posterior
    #'
    #' @return The likelihood of a single observation
    .lfun = function(data_i, draws) {
      # each time called internally within loo the arguments will be equal to:
      # data_i: ith row of fake_data (fake_data[i,, drop=FALSE])
      # draws: entire fake_posterior matrix
      ll <- stats::dnorm(data_i$y, mean = draws$vtf0[, data_i$X], sd = draws$sigma[, data_i$voxel])
      return(ll)
    },

    #' Log-Likelihood function
    #'
    #' @param data_i single row of data
    #' @param draws posterior
    #'
    #' @return The log-likelihood of a single observation
    .llfun = function(data_i, draws) {
      # each time called internally within loo the arguments will be equal to:
      # data_i: ith row of fake_data (fake_data[i,, drop=FALSE])
      # draws: entire fake_posterior matrix
      ll <- stats::dnorm(data_i$y, mean = draws$vtf0[, data_i$X], sd = draws$sigma[, data_i$voxel], log = TRUE)
      return(ll)
    }
  ),
  private = list(
    .make_vtf0_iter = function(xx) {
      d0 <- xx %>%
        dplyr::mutate(
          ori = purrr::map(
            .data$voxel,
            ~ self$standata$unique_orientations[
              self$standata$ori_by_vox[.x, 1:self$standata$n_unique_orientations_vox[.x]]
            ]
          )
        ) %>%
        tidyr::unnest(.data$ori) %>%
        dplyr::mutate(resp_to_ori = exp(.data$v_kappa * cos(.data$ori - .data$meanAngle))) %>%
        dplyr::select(-.data$meanAngle, -.data$v_kappa) %>%
        dplyr::group_by(.data$voxel, .data$.chain) %>%
        dplyr::mutate(resp_to_ori = .data$v_gamma * (.data$resp_to_ori / sum(.data$resp_to_ori))) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.data$v_gamma) %>%
        tidyr::crossing(contrast = factor(c("low", "high"), levels = c("low", "high")))

      if (self$standata$modulation == 0) {
        d2 <- d0 %>%
          dplyr::mutate(
            vtf0 = dplyr::if_else(
              forcats::fct_match(.data$contrast, "low"),
              .data$resp_to_ori,
              .data$resp_to_ori + .data$v_ntfp
            ),
            vtf0 = .data$vtf0 + .data$v_alpha
          )
      } else if (self$standata$modulation == 1) {
        d2 <- d0 %>%
          dplyr::mutate(
            vtf0 = dplyr::if_else(
              forcats::fct_match(.data$contrast, "low"),
              .data$resp_to_ori,
              .data$resp_to_ori * .data$v_ntfp
            ),
            vtf0 = .data$vtf0 + .data$v_alpha
          )
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
