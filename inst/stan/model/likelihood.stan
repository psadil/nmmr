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
