data {
#include data/data.stan
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
#include tparameters/donut.stan
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

#include model/likelihood.stan
}
