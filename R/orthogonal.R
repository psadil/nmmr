
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
  checkmate::assert_numeric(x, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_numeric(y, any.missing = FALSE, null.ok = FALSE, len = length(x))

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
#' [sum_squares()], [get_slope_by_group()]
#' For a more general function (including one that returns the intercept), see [pracma::odregress()]
#'
#' @examples
#' n <- 1000
#' z <- rnorm(n)
#' x <- rnorm(n, z)
#' y <- rnorm(n, 2*z + 1)
#' get_slope(x,y)
get_slope <- function(x,y){

  # checks performed in sum_squares function
  s_xx <- sum_squares(x)
  s_yy <- sum_squares(y)
  s_xy <- sum_squares(x,y)

  (-(s_xx - s_yy) + sqrt((s_xx - s_yy)^2 + 4*s_xy^2)) / (2*s_xy)
}


#' Calculate slopes by groups
#'
#' @param d wide,dataframe containing the
#' @param group name of column (bare only)
#' @param x,y names of column (bare or quoted) in d with which to do the regression
#'
#' @return
#' a dataframe with two columns, one containing the group id and
#' one containing the estimated slope for that group
#' @seealso [get_slope()], [cross_threshold()]
#'
#' @examples
#' sub02 %>%
#'   tidyr::pivot_wider(names_from = contrast, values_from = y) %>%
#'   get_slope_by_group(voxel, low, high)
#'
#' # create group with intersection of variables
#' sub02 %>%
#'   tidyr::pivot_wider(names_from = contrast, values_from = y) %>%
#'   get_slope_by_group(c(voxel, sub), low, high)
#'
#' @export
#' @importFrom rlang .data
get_slope_by_group <- function(d, group, x, y) {

  x_name <- as_name(enquo(x))
  y_name <- as_name(enquo(y))

  checkmate::assert_data_frame(d, any.missing = FALSE)
  checkmate::assert_subset(x_name, names(d))
  checkmate::assert_subset(y_name, names(d))

  d %>%
    dplyr::group_nest(dplyr::across({{group}})) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      slope = get_slope(.data$data[[x_name]], .data$data[[y_name]])) %>%
    dplyr::ungroup() %>%
    dplyr::select({{group}}, .data$slope)

}


