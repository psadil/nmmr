data {
  int<lower=1> n;  // total number of observations
  int n_voxel;
  int<lower=1, upper=n_voxel> voxel[n];
  int n_orientation;
  int<lower=1, upper=n_orientation> orientation[n];
  int<lower=1, upper=n_orientation*n_voxel> voxel_orientation[n]; // index to pick out from matrix of orientation x voxel
  vector[n] y;  // response variable (high)
  vector[n] x;  // noisy values (low)
}
parameters {
  real<lower=0> beta_sigma;
  real beta_mu;
  vector<multiplier=beta_sigma, offset=beta_mu>[n_voxel] slope;
  real<lower=0> alpha_sigma;
  real alpha_mu;
  vector<multiplier=alpha_sigma, offset=alpha_mu>[n_voxel] alpha;
  real<lower=0> sigma_b;
  real mu_mu;
  row_vector[n_voxel] mu;
  matrix[n_orientation, n_voxel] z_raw;
  real<lower=0> sigma_z_sigma;
  real<lower=0> sigma_z_mu;
  vector<lower=-sigma_z_mu/sigma_z_sigma>[n_voxel] sigma_z_raw;
  real<lower=0> sigma_x_sigma;
  real<lower=0> sigma_x_mu;
  vector<lower=0>[n_voxel] sigma_x;
  real<lower=0> sigma_y_sigma;
  real<lower=0> sigma_y_mu;
  vector<lower=0>[n_voxel] sigma_y;
}
transformed parameters{
  vector[n_orientation*n_voxel] zz;

  {
    matrix[n_orientation, n_voxel] z;
    vector[n_voxel] sigma_z = sigma_z_mu + sigma_z_raw * sigma_z_sigma;
    for (v in 1:n_voxel) z[,v] = sigma_z[v] * z_raw[,v] + mu[v];
    zz = to_vector(z);
  }

}
model {
  mu_mu ~ normal(0, 10);
  sigma_b ~ normal(0, 10);
  mu ~ normal(mu_mu, sigma_b);

  to_vector(z_raw) ~ std_normal();

  sigma_z_mu ~ normal(0, 10);
  sigma_z_sigma ~ normal(0, 10);
  sigma_z_raw ~ std_normal();
  target += -normal_lccdf(-sigma_z_mu/sigma_z_sigma | 0, 1)*n_voxel;

  sigma_x_mu ~ normal(0, 10);
  sigma_x_sigma ~ normal(0, 10);
  sigma_x ~ normal(sigma_x_mu, sigma_x_sigma);
  target += -normal_lccdf(0 | sigma_x_mu, sigma_x_sigma) * n_voxel;

  sigma_y_mu ~ normal(0, 10);
  sigma_y_sigma ~ normal(0, 10);
  sigma_y ~ normal(sigma_y_mu, sigma_y_sigma);
  target += -normal_lccdf(0 | sigma_y_mu, sigma_y_sigma) * n_voxel;

  beta_mu ~ normal(0, 10);
  beta_sigma ~ normal(0, 10);
  slope ~ normal(beta_mu, beta_sigma);

  alpha_mu ~ normal(0, 10);
  alpha_sigma ~ normal(0, 10);
  alpha ~ normal(alpha_mu, alpha_sigma);

  {
    x ~ normal(zz[voxel_orientation], sigma_x[voxel]);
    y ~ normal(alpha[voxel] + zz[voxel_orientation] .* slope[voxel], sigma_y[voxel]);
  }
}
