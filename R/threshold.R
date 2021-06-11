#' Get Thresholds
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
#'   column for `participant`, and a new column called `Threshold.` The `Threshold`
#'   column is crossed with the original dataset, but for each crossing, members
#'   of `group` are excluded if they do not cross the threshold.
#' @seealso [get_slope()], [get_slope_by_group()]
#' @examples
#' sub02 |>
#' tidyr::pivot_wider(names_from = contrast, values_from = y) |>
#' cross_threshold(voxel, low, high)
#'
#' # can also calculate within groups
#' sub02 |>
#' tidyr::pivot_wider(names_from = contrast, values_from = y) |>
#' cross_threshold(c(voxel, run), low, high, participant = sub)
#' @export
#' @importFrom rlang .data
cross_threshold <- function(d, group, x, y, quantiles = c(0, 0.9), participant = NULL) {
  checkmate::assert_numeric(quantiles, lower = 0, upper = 1, any.missing = FALSE, min.len = 1, unique = TRUE)
  checkmate::assert_data_frame(d)
  checkmate::assert_subset(as_name(enquo(x)), names(d))
  checkmate::assert_subset(as_name(enquo(y)), names(d))

  d |>
  dplyr::group_by(dplyr::across({{ group }}), dplyr::across({{ participant }})) |>
  dplyr::summarise(
    dplyr::across({{ x }}, mean, .names = "x"),
    dplyr::across({{ y }}, mean, .names = "y"),
    .groups = "drop"
  ) |>
  dplyr::mutate(di = .data$y - .data$x) |>
  tidyr::crossing(Threshold = quantiles) |>
  dplyr::group_by(.data$Threshold, dplyr::across({{ participant }})) |>
  dplyr::filter(.data$di >= stats::quantile(.data$di, .data$Threshold)) |>
  dplyr::select({{ group }}, .data$Threshold, {{ participant }}) |>
  dplyr::ungroup()
}
