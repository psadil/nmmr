
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
#' [sum_squares()], [get_slope_by_group()]
#' For a more general function (including one that returns the intercept), see [pracma::odregress()]
#'
#' @examples
#' n <- 1000
#' z <- rnorm(n)
#' x <- rnorm(n, z)
#' y <- rnorm(n, 2*z + 1)
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
#' @export
#' @importFrom rlang .data
get_slope_by_group <- function(d, group, x, y) {

  x_name <- as_name(enquo(x))
  y_name <- as_name(enquo(y))

  checkmate::assert_data_frame(d)
  checkmate::assert_subset(x_name, names(d))
  checkmate::assert_subset(y_name, names(d))

  d %>%
    dplyr::group_nest(across({{group}})) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      slope = get_slope(.data$data[[x_name]], .data$data[[y_name]])) %>%
    dplyr::ungroup() %>%
    dplyr::select({{group}}, .data$slope)

}


#' Title
#'
#' @param d wide,dataframe containing the
#' @param group name of column (bare only)
#' @param x,y names of column (bare or quoted) in d with which to do the regression
#' @param quantiles threshold to apply.
#' @param participant optional secondary grouping column. Can be used to nest thresholding
#'       (e.g., calculate the quantiles within each participant separately)
#'
#' @return
#'   data frame object with one column for the `group`, potentially another index
#'   column for `participant`, and a new column called Threshold. A group will be
#'   repeated as many times as it passes the threshold.
#' @seealso [get_slope()], [get_slope_by_group()]
#' @examples
#' sub02 %>%
#'   tidyr::pivot_wider(names_from = contrast, values_from = y) %>%
#'   cross_threshold(voxel, low, high)
#'
#' @export
#' @importFrom rlang .data
cross_threshold <- function(d, group, x, y, quantiles = c(0, 0.9), participant = NULL) {

  checkmate::assert_numeric(quantiles, lower=0, upper = 1, any.missing = FALSE, min.len = 1, unique = TRUE)
  checkmate::assert_data_frame(d)
  checkmate::assert_subset(as_name(enquo(x)), names(d))
  checkmate::assert_subset(as_name(enquo(y)), names(d))

  d %>%
    dplyr::group_by(across({{group}}), {{participant}}) %>%
    dplyr::summarise(
      y = mean({{y}}),
      x = mean({{x}}),
      .groups = "drop") %>%
    dplyr::mutate(di = .data$y - .data$x) %>%
    tidyr::crossing(Threshold = quantiles) %>%
    dplyr::group_by(.data$Threshold, {{participant}}) %>%
    dplyr::filter(di >= quantile(.data$di, .data$Threshold)) %>%
    dplyr::select({{group}}, Threshold, {{participant}}) %>%
    dplyr::ungroup()
}

