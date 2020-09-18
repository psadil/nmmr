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
  vector[23] priors;
  int<lower = 1, upper = n_voxel> voxel[n];

