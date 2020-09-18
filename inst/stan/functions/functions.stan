
// Directional Statistics, pg 44
real dconv2vm(real x, real ori_pref, real kappa_weights, real kappa_channel){
  return modified_bessel_first_kind(0, sqrt(square(kappa_weights) + square(kappa_channel) + 2 * kappa_weights * kappa_channel * cos(x - ori_pref))) /
  (2 * pi() * modified_bessel_first_kind(0, kappa_weights) * modified_bessel_first_kind(0, kappa_channel));
}


/**
* Density function of convolution of w von Mises distributions
* Note, f*(mg+a) = f*(mg) + (f*a) = m(f*g) + a(f*1) = m(f*g) + a, which involves
* f*g := the convolution of two von mises,
* f*1 := the convolution of a von mises and uniform => addition of 1
*
* @param x vector of orientations at which to evaluate pdf
* @param ori_pref center of von mises (channel's preferred orientation)
* @param kappa_weights concentration of weights
* @param kappa_channel concentration of NTF
* @return density vector (response of voxel to ori_tested)
*/
real dconv2vm_pdf(real x, real ori_pref, real kappa_weights, real kappa_channel, real gain, real additive){
  real first = gain * dconv2vm(x, ori_pref, kappa_weights, kappa_channel);
  return first + additive;
}
