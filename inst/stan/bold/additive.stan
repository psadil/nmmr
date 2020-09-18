data {
  int n_voxel;
  int n_contrast;
  int n_unique_orientations;
  int n_obs_per_vox;
  vector[14] priors;
  vector[n_unique_orientations] unique_orientations;
  matrix[n_obs_per_vox, n_voxel] y;
  matrix[n_obs_per_vox, n_contrast*n_unique_orientations] X[n_voxel];
}
transformed data {
  int K = n_contrast * n_unique_orientations;
}
parameters {
  real<lower = 0> sigma;
  matrix[n_voxel, 2] meanAngleVector;

  real alpha_loc;
  real<lower=0> alpha_scale;
  vector<offset = alpha_loc, multiplier = alpha_scale>[n_voxel] alpha;

  real<lower = 0> v_base_loc;
  real<lower = 0> v_base_scale;
  vector<lower = 0>[n_voxel] v_base;

  real<lower = 0> v_kappa_loc;
  real<lower = 0> v_kappa_scale;
  vector<lower = (-v_kappa_loc)/v_kappa_scale>[n_voxel] v_kappa_raw;

  real<lower = 0> v_int_scale;
  real v_int_loc;
  vector<offset = v_int_loc, multiplier = v_int_scale>[n_voxel] v_int;

  real<lower=0> ntfp_scale;
  real<lower=1> ntfp_loc;
  vector<lower=(1-ntfp_loc)/ntfp_scale>[n_voxel] ntfp_raw;
}
transformed parameters{
  vector<lower=0>[n_voxel] ntfp = ntfp_loc + ntfp_raw*ntfp_scale;
  vector<lower=0>[n_voxel] v_kappa = v_kappa_loc + v_kappa_raw*v_kappa_scale;
  matrix[K, n_voxel] vtf0;
  vector<lower=0>[n_voxel] lengthOfMeanAngleVector = sqrt(rows_dot_self(meanAngleVector));
  matrix[n_voxel, 2] meanAngleUnitVector = append_col(meanAngleVector[,1] ./ lengthOfMeanAngleVector,
  meanAngleVector[,2] ./ lengthOfMeanAngleVector);
  vector<lower = -pi(), upper = pi()>[n_voxel] meanAngle;
  for(v in 1:n_voxel) meanAngle[v] = atan2(meanAngleUnitVector[v,2], meanAngleUnitVector[v,1]);

  {
    for(v in 1:n_voxel){
      vector[n_unique_orientations] resp_to_ori_log;
      vector[n_unique_orientations] resp_to_ori;
      for(ori in 1:n_unique_orientations){
        resp_to_ori_log[ori] = von_mises_lpdf(unique_orientations[ori] | meanAngle[v], v_kappa[v]);
      }
      resp_to_ori = exp(resp_to_ori_log)*v_base[v];
      vtf0[,v] = v_int[v] + append_row(resp_to_ori, resp_to_ori+ntfp[v]);
    }
  }
}
model{
  sigma ~ gamma(3, priors[1]);
  lengthOfMeanAngleVector ~ normal(1, .1);
  ntfp_scale ~ gamma(priors[2], priors[3]);

  alpha_loc ~ normal(0, 10);
  alpha_scale ~ gamma(2, 1.0/3.0);
  alpha ~ normal(alpha_loc, alpha_scale);

  v_base_loc ~ normal(0, priors[4]);
  v_base_scale ~ gamma(priors[5], priors[6]);
  for(v in 1:n_voxel) v_base[v] ~ normal(v_base_loc, v_base_scale) T[(-v_base_loc)/v_base_scale,];

  v_kappa_loc ~ gamma(priors[7], priors[8]);
  v_kappa_scale ~ gamma(priors[9], priors[10]);
  for(v in 1:n_voxel) v_kappa_raw[v] ~ normal(0, 1) T[(-v_kappa_loc)/v_kappa_scale,];

  v_int_loc ~ normal(0, priors[11]);
  v_int_scale ~ gamma(priors[12], priors[13]);
  v_int ~ normal(v_int_loc, v_int_scale);

  ntfp_loc ~ normal(1, priors[14]);
  for(v in 1:n_voxel) ntfp_raw[v] ~ normal(0, 1) T[(1-ntfp_loc)/ntfp_scale,];

  for(v in 1:n_voxel){
    y[,v] ~ normal_id_glm(X[v], alpha[v], vtf0[, v], sigma);
  }
}
