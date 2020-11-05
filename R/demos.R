

#' @param modulation subset of c("Additive", "Multiplicative", "Bandwidth", "Shift")
#' @param x radians over which to cross
#'
#' @return
#' @export
#'
#' @examples
make_d0 <- function(modulation,
                   x = seq(-pi, pi-2*pi/8, length.out = 8)) {

  modulations <- c("Additive", "Multiplicative", "Bandwidth", "Shift")
  checkmate::assert_subset(modulation, modulations, empty.ok = FALSE)

  d <- tibble::tibble(raw = x) %>%
    tidyr::crossing(
      Contrast = factor(c("Low", "High"), levels = c("Low", "High")),
      VTF = factor(modulation, levels = modulations))

  return(d)
}
