
#' Sum of Squared Error
#'
#' @param x numeric vector
#' @param y optional, numeric vector, same length as `x`
#'
#' @return
#' either the sum of squared errors for `x` (if `y` is left out),
#' or the cross-product of two vectors `x` with `y`
#' @export
#'
#' @details
#'
#' The sum of squared errors for a length \eqn{n} vector \eqn{x} will equal
#'
#' \deqn{S_{xx} = \sum_i^n(x - \overline{x})^2,}
#'
#' where \eqn{\overline{x}} is the mean of \eqn{x}.
#'
#' The cross-product is
#'
#' \deqn{S_{xy} = \sum_i^n (x - \overline{x})(y - \overline{y})}
#'
#' @examples
#' # the sum of squared errors for x
#' x <- rnorm(10)
#' sum_squares(x)
#'
#' # a cross-product
#' y <- rnorm(10)
#' sum_squares(x,y)
sum_squares <- function(x, y=x){
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 2, null.ok = FALSE)
  checkmate::assert_numeric(y, any.missing = FALSE, min.len = 2, null.ok = FALSE, len = length(x))

  sum((x-mean(x)) * (y-mean(y)) )
}

#' Return slope in total least squares problem
#'
#' @param x vector with independent variable
#' @param y vector with dependent variable
#'
#' @return the slope of the line that minimizes the total least squared error
#' @export
#'
#' @details
#' Whereas ordinary least squares finds the line that minimizes the sum of squared
#' vertical distance, total least squares finds the line that minimizes the sum
#' of vertical and horizontal differences.
#'
#' @seealso
#' For a more general function (including one that returns the intercept), see [pracma::odregress()]
#'
#' @examples
#' n <- 1000
#' z <- rnorm(n)
#' x <- rnorm(n, z)
#' y <- rnorm(n, 2*x + 1)
#' get_slope(x,y)
#'
get_slope <- function(x,y){
  checkmate::assert_numeric(x, any.missing = FALSE, min.len = 2, null.ok = FALSE)
  checkmate::assert_numeric(y, any.missing = FALSE, min.len = 2, null.ok = FALSE, len = length(x))

  s_xx <- sum_squares(x)
  s_yy <- sum_squares(y)
  s_xy <- sum_squares(x,y)

  (-(s_xx - s_yy) + sqrt((s_xx - s_yy)^2 + 4*s_xy^2)) / (2*s_xy)
}

