sigma ~ gamma(3, priors[1]);
lengthOfMeanAngleVector ~ normal(1, .1);


// --- gamma multiplier --
s_gammaloc_loc ~ normal(0, priors[4]);
s_gammaloc_scale ~ gamma(priors[5], priors[6]);
s_gamma_loc_raw ~ std_normal();
target += -normal_lccdf((-s_gammaloc_loc)/s_gammaloc_scale | 0, 1) * n_sub;

s_gammascale_loc ~ normal(0, priors[4]);
s_gammascale_scale ~ gamma(priors[5], priors[6]);
s_gamma_scale_raw ~ std_normal();
target += -normal_lccdf((-s_gammascale_loc)/s_gammascale_scale | 0, 1) * n_sub;

v_gamma ~ normal(s_gamma_loc[sub_by_vox], s_gamma_scale[sub_by_vox]);
target += -normal_lccdf(0 | s_gamma_loc[sub_by_vox], s_gamma_scale[sub_by_vox]);


// -- concentration --
v_kappa_loc ~ gamma(priors[7], priors[8]);
v_kappa_scale ~ gamma(priors[9], priors[10]);
v_kappa_raw ~ std_normal();
target += -normal_lccdf((-v_kappa_loc)/v_kappa_scale | 0, 1) * n_voxel;


// -- additive offset
s_alphaloc_loc ~ normal(0, priors[11]);
s_alphaloc_scale ~ gamma(priors[12], priors[13]);
s_alpha_loc_raw ~ std_normal();
target += -normal_lccdf((-s_alphaloc_loc)/s_alphaloc_scale | 0, 1) * n_sub;

s_alphascale_loc ~ normal(0, priors[11]);
s_alphascale_scale ~ gamma(priors[12], priors[13]);
s_alpha_scale_raw ~ std_normal();
target += -normal_lccdf((-s_alphascale_loc)/s_alphascale_scale | 0, 1) * n_sub;

v_alpha ~ normal(s_alpha_loc[sub_by_vox], s_alpha_scale[sub_by_vox]);
target += -normal_lccdf(0 | s_alpha_loc[sub_by_vox], s_alpha_scale[sub_by_vox]);

// -- NTFP --
s_ntfploc_loc ~ normal(ntfp_min, priors[14]);
s_ntfploc_scale ~ gamma(priors[12], priors[13]);
s_ntfp_loc_raw ~ std_normal();
target += -normal_lccdf((ntfp_min-s_ntfploc_loc)/s_ntfploc_scale | 0, 1) * n_sub;

s_ntfpscale_loc ~ normal(0, priors[14]);
s_ntfpscale_scale ~ gamma(priors[12], priors[13]);
s_ntfp_scale_raw ~ std_normal();
target += -normal_lccdf((-s_ntfpscale_loc)/s_ntfpscale_scale | 0, 1) * n_sub;


