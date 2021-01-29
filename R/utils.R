
#' Convert between angles and radians
#' @aliases rad deg
#'
#' @param radian,degree angles with different units
#' @name angles
NULL

#' @rdname angles
#' @export
deg <- function(radian){
  checkmate::assert_numeric(radian, lower=-2*pi, upper=2*pi)
  (radian * 180) / pi
}

#' @rdname angles
#' @export
rad <- function(degree){
  checkmate::assert_numeric(degree, lower=-360, upper=360)
  (degree * pi) / 180
}
