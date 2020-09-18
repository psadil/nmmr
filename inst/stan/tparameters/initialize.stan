  matrix[n_unique_orientations, n_voxel] vtf0;

  vector<lower=0>[n_voxel] v_kappa = v_kappa_loc + v_kappa_raw*v_kappa_scale;

  vector<lower=0>[n_sub] s_gamma_loc = s_gammaloc_loc + s_gamma_loc_raw*s_gammaloc_scale;
  vector<lower=0>[n_sub] s_gamma_scale = s_gammascale_loc + s_gamma_scale_raw*s_gammascale_scale;

  vector<lower = 0>[n_sub]s_alpha_loc = s_alphaloc_loc + s_alpha_loc_raw*s_alphaloc_scale;
  vector<lower = 0>[n_sub]s_alpha_scale = s_alphascale_loc + s_alpha_scale_raw*s_alphascale_scale;

  vector<lower=0>[n_sub] s_ntfp_scale = s_ntfpscale_loc + s_ntfp_scale_raw*s_ntfpscale_scale;
  vector<lower=ntfp_min>[n_sub] s_ntfp_loc = s_ntfploc_loc + s_ntfploc_raw*s_ntfploc_scale;


  vector<lower=0>[n_voxel] lengthOfMeanAngleVector = sqrt(rows_dot_self(meanAngleVector));
  matrix[n_voxel, 2] meanAngleUnitVector = append_col(meanAngleVector[,1] ./ lengthOfMeanAngleVector,
                                                      meanAngleVector[,2] ./ lengthOfMeanAngleVector);
  vector<lower = -pi(), upper = pi()>[n_voxel] meanAngle;
  for(v in 1:n_voxel) meanAngle[v] = atan2(meanAngleUnitVector[v,2], meanAngleUnitVector[v,1]);
