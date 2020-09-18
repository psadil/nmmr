  real<lower = 0> sigma;
  real<lower = 0> s0;
  simplex[n_channels] v_weights[n_voxel];
  real<lower = 0>v_base_loc;
  real <lower = 0> v_base_scale;
  vector<lower = (-v_base_loc)/v_base_scale>[n_voxel] v_base_raw;
