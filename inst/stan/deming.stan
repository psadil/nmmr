data {
  int<lower=1> n;  // total number of observations
  int n_voxel;
  int<lower=1, upper=n_voxel> voxel[n];
  int n_tuning;
  int<lower=1, upper=n_tuning> tuning[n];
  int<lower=1, upper=n_tuning*n_voxel> voxel_tuning[n]; // index to pick out from matrix of tuning x voxel
  vector[n] y;  // response variable (high)
  vector[n] x;  // noisy values (low)
}
parameters {
  real<lower=0> g_sigma;
  real g_mu;
  vector<multiplier=g_sigma, offset=g_mu>[n_voxel] g;
  real<lower=0> a_sigma;
  real a_mu;
  vector<multiplier=a_sigma, offset=a_mu>[n_voxel] a;
  real<lower=0> z_mu_sigma;
  real z_mu_mu;
  row_vector[n_voxel] z_mu;
  matrix[n_tuning, n_voxel] z_raw;
  real<lower=0> z_sigma_sigma;
  real<lower=0> z_sigma_mu;
  vector<lower=-z_sigma_mu/z_sigma_sigma>[n_voxel] z_sigma_raw;
  real<lower=0> x_sigma_sigma;
  real<lower=0> x_sigma_mu;
  vector<lower=0>[n_voxel] x_sigma;
  real<lower=0> y_sigma_sigma;
  real<lower=0> y_sigma_mu;
  vector<lower=0>[n_voxel] y_sigma;
}
transformed parameters{
  vector[n_tuning*n_voxel] zeta;

  {
    matrix[n_tuning, n_voxel] z;
    vector[n_voxel] z_sigma = z_sigma_mu + z_sigma_raw * z_sigma_sigma;
    for (v in 1:n_voxel) z[,v] = z_sigma[v] * z_raw[,v] + z_mu[v];
    zeta = to_vector(z);
  }

}
model {
  z_mu_mu ~ normal(0, 10);
  z_mu_sigma ~ normal(0, 10);
  z_mu ~ normal(z_mu_mu, z_mu_sigma);

  to_vector(z_raw) ~ std_normal();

  z_sigma_mu ~ normal(0, 10);
  z_sigma_sigma ~ normal(0, 10);
  z_sigma_raw ~ std_normal();
  target += -normal_lccdf(-z_sigma_mu/z_sigma_sigma | 0, 1)*n_voxel;

  x_sigma_mu ~ normal(0, 10);
  x_sigma_sigma ~ normal(0, 10);
  x_sigma ~ normal(x_sigma_mu, x_sigma_sigma);
  target += -normal_lccdf(0 | x_sigma_mu, x_sigma_sigma) * n_voxel;

  y_sigma_mu ~ normal(0, 10);
  y_sigma_sigma ~ normal(0, 10);
  y_sigma ~ normal(y_sigma_mu, y_sigma_sigma);
  target += -normal_lccdf(0 | y_sigma_mu, y_sigma_sigma) * n_voxel;

  g_mu ~ normal(0, 10);
  g_sigma ~ normal(0, 10);
  g ~ normal(g_mu, g_sigma);

  a_mu ~ normal(0, 10);
  a_sigma ~ normal(0, 10);
  a ~ normal(a_mu, a_sigma);

  // likelihood
  x ~ normal(zeta[voxel_tuning], x_sigma[voxel]);
  y ~ normal(a[voxel] + zeta[voxel_tuning] .* g[voxel], y_sigma[voxel]);
}
