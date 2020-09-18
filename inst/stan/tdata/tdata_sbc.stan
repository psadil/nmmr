  vector[n_channels] alpha = rep_vector(1, n_channels);
  int K = n_contrast * n_unique_orientations;
  // ---- parameters ----
  // sigma, s0, v_base_loc, v_base_sigma, ntfp_loc, ntfp_scale, v_weights
  int n_pars = 6;
  real<lower = 0> sigma_ = gamma_rng(2, priors[1]);
  real<lower = 0> s0_ = gamma_rng(priors[2], priors[3]);
  real<lower=0> v_base_loc_ = normal_lb_rng(0, 1, 0);
  real<lower=0> v_base_scale_ = gamma_rng(2, priors[6]);
  real<lower=0> ntfp_loc_ = normal_lb_rng(0, priors[4], 0);
  real<lower=0> ntfp_scale_ = gamma_rng(2, priors[5]);
  vector[n_channels] v_weights_[n_voxel];
  vector[n] y;

  for (v in 1:n_voxel){
      v_weights_[v] = dirichlet_rng(alpha);
  }

  {
    // ---- parameters not kept ----
    vector[n_voxel] v_base;
    vector[n_voxel] ntfp;
    matrix[n_channels, n_unique_orientations] channel_resp_log;
    matrix[n_unique_orientations, n_channels] channel_resp;
    matrix[n_obs_per_vox, n_voxel] y_centered;

    for(v in 1:n_voxel){
      v_base[v] = normal_lb_rng(v_base_loc_, v_base_scale_, 0);
      ntfp[v] = normal_lb_rng(ntfp_loc_, ntfp_scale_, 0);
    }

    for(ori in 1:n_unique_orientations){
      for(ch in 1:n_channels){
        channel_resp_log[ch,ori] = von_mises_lpdf(unique_orientations[ori] | channel_ori[ch], s0_);
      }
    }
    channel_resp = exp(channel_resp_log)';

    for(v in 1:n_voxel){
      matrix[n_unique_orientations, n_contrast] resp_to_ori;
      vector[K] vtf0;
      vector[n_obs_per_vox] y0;
      resp_to_ori[, 1] = channel_resp * v_weights_[v];
      resp_to_ori[, 2] = resp_to_ori[, 1] * ntfp[v];
      vtf0 = to_vector(resp_to_ori)*v_base[v];
      y0 = to_vector(normal_rng(vtf0[X[,v]], sigma_));
      y_centered[,v] = y0 - mean(y0);
    }
    y = to_vector(y_centered);
  }


