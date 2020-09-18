  int n;
  int n_voxel;
  int n_contrast;
  int n_unique_orientations;
  int n_channels;
  vector[6] priors;
  vector[n_unique_orientations] unique_orientations;
  vector[n_channels] channel_ori; // preferred orientation of each channel
  int n_obs_per_vox;
  int X[n_obs_per_vox, n_voxel]; // contains indicator variable (1:(n_contrast*n_voxel)) with the trial type on an observation
