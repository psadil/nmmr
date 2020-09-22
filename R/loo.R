loo.VtuneFit <- function(x,
                         ...,
                         cores = 1,
                         vtf0 = NULL) {

  checkmate::assert_integer(cores, lower=1)

  if (is.null(vtf0)) vtf0 <- .make_vtf0(x, cores = cores)
  stand <- standata(x)
  data_ <- data.frame(y = stand$y, X = stand$X)
  if("voxel" %in% names(stand)) data_$voxel <- stand$voxel
  else data_$voxel <- 1

  sigma <- rstan::extract(x, pars = "sigma", permuted = FALSE)
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
    .lfun,
    chain_id = chain_id,
    data = data_,
    draws = draws,
    cores = cores)

  out <- loo::loo(
    .llfun,
    data = data_,
    draws = draws,
    r_eff = r_eff,
    cores = cores)

  return(out)
}

#' @describeIn VtuneFit Leave-one-out cross-validation
#'
#' @param cores int
#' @param x VtuneFit object, containing parameters "mu" and "sigma"
#'
#' @export
#' @importFrom loo loo
setMethod("loo", "VtuneFit", loo.VtuneFit)

.lfun <- function(data_i, draws){
  # each time called internally within loo the arguments will be equal to:
  # data_i: ith row of fake_data (fake_data[i,, drop=FALSE])
  # draws: entire fake_posterior matrix
  ll <- stats::dnorm(data_i$y, mean = draws$vtf0[,data_i$X], sd = draws$sigma[,data_i$voxel])
  return(ll)
}

.llfun <- function(data_i, draws){
  # each time called internally within loo the arguments will be equal to:
  # data_i: ith row of fake_data (fake_data[i,, drop=FALSE])
  # draws: entire fake_posterior matrix
  ll <- stats::dnorm(data_i$y, mean = draws$vtf0[,data_i$X], sd = draws$sigma[,data_i$voxel], log = TRUE)
  return(ll)
}

.make_vtf0_iter <- function(xx, stand){

  d0 <- xx %>%
    dplyr::mutate(
      ori = purrr::map(voxel, ~stand$unique_orientations[stand$ori_by_vox[.x, 1:stand$n_unique_orientations_vox[.x]]])) %>%
    tidyr::unnest(ori) %>%
    dplyr::mutate(resp_to_ori = exp(v_kappa * cos(ori - meanAngle))) %>%
    dplyr::select(-meanAngle, -v_kappa) %>%
    dplyr::group_by(voxel, .chain) %>%
    dplyr::mutate(resp_to_ori = v_gamma * (resp_to_ori / sum(resp_to_ori))) %>%
    dplyr::ungroup() %>%
    dplyr::select(-v_gamma) %>%
    tidyr::crossing(contrast = factor(c("low", "high"), levels = c("low", "high")))

  if(stand$modulation == 0){
    d2 <- d0 %>%
      dplyr::mutate(
        vtf0 = dplyr::if_else(
          forcats::fct_match(contrast, "low"),
          resp_to_ori,
          resp_to_ori + v_ntfp),
        vtf0 = vtf0 + v_alpha)
  }else if(stand$modulation == 1){
    d2 <- d0 %>%
      dplyr::mutate(
        vtf0 = dplyr::if_else(
          forcats::fct_match(contrast, "low"),
          resp_to_ori,
          resp_to_ori * v_ntfp),
        vtf0 = vtf0 + v_alpha)
  }

  out <- d2 %>%
    dplyr::mutate(idx = interaction(ori, contrast, voxel)) %>%
    dplyr::arrange(.chain, idx) %>%
    dplyr::select(idx, .chain, vtf0) %>%
    tidyr::pivot_wider(names_from = "idx", values_from = "vtf0") %>%
    dplyr::select(-.chain) %>%
    as.matrix()
  out
}


#' create voxel tuning function posterior
#'
#' @param post output of model
#' @param cores integer number of cores over which to parallelize
#'
#' @return array n_draws x n_chains x n_unique obs. for use in loo.VtuneFit
#' @export
.make_vtf0 <- function(post, cores = 1){
  stand <- standata(post)

  x <- rstan::As.mcmc.list(
    post,
    pars = c("v_gamma", "v_kappa", "v_alpha", "meanAngle", "v_ntfp")) %>%
    posterior::as_draws_df() %>%
    tidyr::pivot_longer(
      cols = c(-.iteration, -.chain, -.draw),
      names_to = c(".variable","voxel"),
      names_pattern = "(.*)\\[(.*)\\]",
      values_to = ".estimate") %>%
    dplyr::mutate(voxel = as.numeric(voxel)) %>%
    tidyr::pivot_wider(names_from = ".variable", values_from = ".estimate") %>%
    dplyr::group_nest(.iteration)

  n_chain <- dplyr::n_distinct(x$data[[1]]$.chain)
  n_iter <- dplyr::n_distinct(x$.iteration)
  vtf <- array(dim = c(n_iter, n_chain, max(stand$X) ))

  xx <- parallel::mclapply(
    1:nrow(x),
    FUN = function(i) .make_vtf0_iter(x$data[[i]], stand),
    mc.cores = cores)

  for(i in 1:n_iter){
    vtf[i,,] <- xx[[i]]
  }

  return(vtf)
}
