data {
  int n;
  int n_voxel; // total voxels across all subs
  int n_sub;
  int sub_by_vox[n_voxel]; // locates the voxel's sub
  int n_unique_orientations;
  vector<lower = -pi(), upper = pi()>[n_unique_orientations] unique_orientations;
  int n_unique_orientations_vox[n_voxel];
  int<lower = 0, upper = n_unique_orientations> ori_by_vox[n_voxel, n_unique_orientations];
  vector[n] y;
  int X[n];
  int<lower = 1, upper = n_sub> sub[n];
  real<lower = 0> ntfp_min;
  int<lower = 0, upper = 1> modulation;
  int<lower = 1, upper = n_voxel> voxel[n];
  // priors from here on
  vector<lower = 0>[2] prior_sigma_loc;
  vector<lower = 0>[2] prior_sigma_scale;
  real<lower = 0> prior_gamma_loc;
  vector<lower = 0>[2] prior_gamma_scale;
  vector<lower = 0>[2] prior_kappa_loc;
  vector<lower = 0>[2] prior_kappa_scale;
  vector<lower = 0>[2] prior_alpha_loc;
  vector<lower = 0>[2] prior_alpha_scale;
  real<lower = 0> prior_ntfp_loc;
  vector<lower = 0>[2] prior_ntfp_scale;
}
transformed data{
  int maxX = max(X);
}
parameters {
  // real<lower = 0> sigma;
  real<lower=0> sigma_loc;
  real<lower=0> sigma_scale;
  vector<lower = 0>[n_voxel] sigma;
  matrix[n_voxel, 2] meanAngleVector;

  // --- gamma multiplier --
  real<lower = 0> s_gamma_loc;
  real<lower = 0> s_gamma_scale;
  vector<lower = 0>[n_voxel] v_gamma;

  // -- concentration --
  real<lower = 0> v_kappa_loc;
  real<lower = 0> v_kappa_scale;
  vector<lower = (-v_kappa_loc)/v_kappa_scale>[n_voxel] v_kappa_raw;

  // -- additive offset
  real s_alpha_loc;
  real<lower = 0> s_alpha_scale;
  vector[n_voxel] v_alpha;

  // -- NTFP --
  real<lower=ntfp_min> s_ntfp_loc;
  real<lower = 0> s_ntfp_scale;
  vector<lower=ntfp_min>[n_voxel] v_ntfp;
}
transformed parameters{
  vector<lower=0>[n_voxel] v_kappa = v_kappa_loc + v_kappa_raw*v_kappa_scale;
  vector<lower=0>[n_voxel] lengthOfMeanAngleVector = sqrt(rows_dot_self(meanAngleVector));
  matrix[n_voxel, 2] meanAngleUnitVector = append_col(meanAngleVector[,1] ./ lengthOfMeanAngleVector,
  meanAngleVector[,2] ./ lengthOfMeanAngleVector);
  vector<lower = -pi(), upper = pi()>[n_voxel] meanAngle;

  for(v in 1:n_voxel) meanAngle[v] = atan2(meanAngleUnitVector[v,2], meanAngleUnitVector[v,1]);
}
model{
  lengthOfMeanAngleVector ~ normal(1, .1);

  sigma_loc ~ gamma(prior_sigma_loc[1], prior_sigma_loc[2]);
  sigma_scale ~ gamma(prior_sigma_scale[1], prior_sigma_scale[2]);
  sigma ~ normal(sigma_loc, sigma_scale);
  target += -normal_lccdf(0 | sigma_loc, sigma_scale) * n_voxel;

  // --- gamma multiplier --
  s_gamma_loc ~ normal(0, prior_gamma_loc);
  s_gamma_scale ~ gamma(prior_gamma_scale[1], prior_gamma_scale[2]);
  v_gamma ~ normal(s_gamma_loc, s_gamma_scale);
  target += -normal_lccdf(0 | s_gamma_loc, s_gamma_scale) * n_voxel;

  // -- concentration --
  v_kappa_loc ~ gamma(prior_kappa_loc[1], prior_kappa_loc[2]);
  v_kappa_scale ~ gamma(prior_kappa_scale[1], prior_kappa_scale[2]);
  v_kappa_raw ~ std_normal();
  target += -normal_lccdf((-v_kappa_loc)/v_kappa_scale | 0, 1) * n_voxel;

  // -- additive offset
  s_alpha_loc ~ normal(prior_alpha_loc[1], prior_alpha_loc[2]);
  s_alpha_scale ~ gamma(prior_alpha_scale[1], prior_alpha_scale[2]);
  v_alpha ~ normal(s_alpha_loc, s_alpha_scale);

  // -- NTFP --
  s_ntfp_loc ~ normal(ntfp_min, prior_ntfp_loc);
  s_ntfp_scale ~ gamma(prior_ntfp_scale[1], prior_ntfp_scale[2]);
  v_ntfp ~ normal(s_ntfp_loc, s_ntfp_scale);
  target += -normal_lccdf(ntfp_min | s_ntfp_loc, s_ntfp_scale) * n_voxel;

  {
    // unless all voxels were tested with the same number of orientations,
    // not every element of vtf should be filled. The int array X should
    // end up plucking out just those values which are filled
    vector[n_unique_orientations * 2 * n_voxel] vtf;
    int i = 1;

    for(v in 1:n_voxel){
      int no = n_unique_orientations_vox[v];
      int no2 = no * 2;
      int up = i + no2 - 1;
      vector[no] resp_to_ori = exp(v_kappa[v] * cos(unique_orientations[ori_by_vox[v, 1:no]] - meanAngle[v]));
      if (up > maxX) reject("index should not exceed elements of X. Found up = ", up);
      resp_to_ori /= sum(resp_to_ori);

      resp_to_ori *= v_gamma[v];
      if(modulation == 0){
        vtf[i:up] = v_alpha[v] + append_row(resp_to_ori, resp_to_ori + v_ntfp[v]);
      }else if(modulation == 1){
        vtf[i:up] = v_alpha[v] + append_row(resp_to_ori, resp_to_ori * v_ntfp[v]);
      }
      i += no2;
    }
    y ~ normal(vtf[X], sigma[voxel]);
  }
}
