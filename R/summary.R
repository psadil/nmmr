#' WISEsummmary
#'
#' Within-subject Error Summary
#'
#' @param data A data frame
#' @param dependentvars Character vector giving the dependent variable
#' @param betweenvars Character vector giving the between subject variables
#' @param withinvars Character vector giving the within subject variables
#' @param idvar Character vector giving the name of the column holding subject
#' identifiers
#' @param CI_width Numeric vector giving the confidence level for computing the
#' confidence interval boundaries. Must be between 0 and 1, non-inclusive.
#' @param na.rm a logical value indicating whether NA values should be removed from the Dependent Variables.
#'
#' @return A data frame with summary statistics
#'
#' @references
#' [WISESummary](http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/),
#' [Morey (2008)](http://tqmp.org/Content/vol04-2/p061/p061.html)
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' WISEsummary(sub02, y, withinvars = c(contrast, orientation), idvar = voxel)
#'
WISEsummary <- function(data, dependentvars, betweenvars=NULL, withinvars=NULL,
                        idvar=NULL, CI_width=.95, na.rm=FALSE) {

  checkmate::assert_logical(na.rm, min.len = 1, max.len = 1)
  checkmate::assert_number(CI_width, lower = 0, upper = 1)
  checkmate::assert_data_frame(data)
  checkmate::assert_names(names(data), must.include = rlang::as_name(enquo(dependentvars)))

  # Norm each subject's data so that each subject's mean is equal to the mean
  # of the between subject condition they are in
  #
  # To do this, we get each subject's mean, join it with the raw data,
  # then center the observations from each subject around the grand mean
  # by subtracting off the individual mean for each subject, and then add
  # the grand mean
  #
  # Then we use this re-centered data as the new "raw" data, to calculate
  # means, sd, and sem as usual

  # Reshape the data into a long format that combines values from different DV's into
  # one column. This makes the operations that calculate different means, SEMs, and CI
  # widths for different DVs simple column-wise operations on data frames grouped by
  # the DV variable name.

  # Get the averages in each condition (grouping by within and between variables,
  # ignoring the subjects. Standard 'unnormed' means.

  by_dv <- data %>%
    tidyr::pivot_longer(cols = {{dependentvars}}, names_to = "DV")

  cell_means <- by_dv %>%
    dplyr::group_by(.data$DV, dplyr::across({{betweenvars}}), dplyr::across({{withinvars}})) %>%
    dplyr::summarise(
      dplyr::across(
        .data$value,
        .fns = ~mean(.x, na.rm = na.rm),
        .names = "mean"),
      .groups = "drop")

  nCells <- nrow(dplyr::distinct(cell_means, dplyr::across({{withinvars}})))
  correction <- sqrt((nCells/(nCells - 1)))

  recentered <- by_dv %>%
    dplyr::group_by(.data$DV, dplyr::across({{idvar}})) %>%
    dplyr::mutate(subject_avg = mean(.data$value)) %>%
    dplyr::group_by(.data$DV) %>%
    dplyr::mutate(recentered_value = .data$value - .data$subject_avg + mean(.data$value)) %>%
    dplyr::group_by(.data$DV, dplyr::across({{withinvars}}), dplyr::across({{betweenvars}})) %>%
    dplyr::summarise(
      dplyr::across(
        .data$recentered_value,
        .fns = list(recentered_mean = mean, sem = sem, n = length),
        .names = "{.fn}"),
      .groups = "drop")


  dplyr::left_join(cell_means, recentered) %>%
    dplyr::mutate(
      sem = .data$sem * correction,
      CI = stats::qt((1 - CI_width)/2, df = .data$n-1, lower.tail = FALSE) * .data$sem,
      CI_lower = .data$mean - .data$CI,
      CI_upper = .data$mean + .data$CI) %>%
    dplyr::select(-.data$CI) %>%
    tidyr::pivot_wider(
      names_from = .data$DV,
      values_from = c(.data$mean, .data$recentered_mean, .data$sem, .data$n, .data$CI_lower, .data$CI_upper),
      names_glue = "{DV}_{.value}") %>%
    suppressMessages()

}


#' Standard Error of the Mean
#'
#' Calculates the standard error of the mean statistic, an estimate of the variability
#'  of the sampling distribution of the mean. See "details" for equation.
#'
#' @param x A numeric or logical atomic vector
#' @param na.rm a logical value indicating whether NA values should be removed from the input.
#'
#' @return A scalar numeric vector
#' @export
#'
#' @details
#' Uses the following forumula:
#'
#' \deqn{S.E.M. = \sqrt{\frac{Var(x)}{N}}}{S.E.M. =  sqrt(Var(x)/N)}
#'
#' @examples
#'
#' x <- runif(30, 5, 2)
#' sem(x)
#'
sem <- function(x, na.rm = FALSE) {

  checkmate::assert_numeric(x)
  check <- checkmate::check_numeric(x, all.missing = FALSE, any.missing = na.rm)
  if(rlang::is_character(check)) warning(check)

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  ## Standard error of the mean calculation
  sqrt(stats::var(x) / length(x))
}

