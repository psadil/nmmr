// real<lower = 0> sigma_loc;
// real<lower = 0> sigma_scale;
// vector<lower = 0>[n_sub] sigma;
real<lower = 0> sigma;
matrix[n_voxel, 2] meanAngleVector;

// --- gamma multiplier --
real<lower = 0> s_gammaloc_loc;
real<lower = 0> s_gammaloc_scale;
vector<lower = (-s_gammaloc_loc)/s_gammaloc_scale>[n_sub] s_gamma_loc_raw;

real<lower = 0> s_gammascale_loc;
real<lower = 0> s_gammascale_scale;
vector<lower = (-s_gammascale_loc)/s_gammascale_scale>[n_sub] s_gamma_scale_raw;

vector<lower = 0>[n_voxel] v_gamma;

// -- concentration --
real<lower = 0> v_kappa_loc;
real<lower = 0> v_kappa_scale;
vector<lower = (-v_kappa_loc)/v_kappa_scale>[n_voxel] v_kappa_raw;


// -- additive offset
real<lower = 0> s_alphaloc_scale;
real<lower=0> s_alphaloc_loc;
vector<lower=(-s_alphaloc_loc)/s_alphaloc_scale>[n_sub] v_alpha_loc_raw;

real<lower = 0> s_alphascale_scale;
real<lower=0> s_alphascale_loc;
vector<lower=(-s_alphascale_loc)/s_alphascale_scale>[n_sub] v_alpha_scale_raw;

vector<lower=0>[n_voxel] v_alpha;

// -- NTFP --
vector<lower=ntfp_min>[n_sub] s_ntfploc_loc;
vector<lower=0>[n_sub] s_ntfploc_scale;
vector<lower=(ntfp_min-s_ntfploc_loc)/s_ntfploc_scale>[n_voxel] s_ntfp_loc_raw;

real<lower = 0> s_ntfpscale_loc;
real<lower = 0> s_ntfpscale_scale;
vector<lower=(-s_ntfpscale_loc)/s_ntfpscale_scale>[n_sub] s_ntfp_scale_raw;


