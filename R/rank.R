
#' Prepare and visualize rank-ordered beta values
#'
#' @name ranks
#' @return
#'
#' [build_ranks()] constructs a data.frame which can be passed on to [visualize_ranks()]
#'
#' [visualize_ranks()] uses that output to make a basic [`ggplot2`][`ggplot2::ggplot2-package`] figure
#'
NULL


#' @rdname ranks
#'
#' @param d data.frame, like [sub02]
#' @param quantiles Values at which to filter voxels
#'
#' @export
build_ranks <- function(d, quantiles = c(0, 0.5, 0.75, 0.9)){

  rawd <- d %>%
    dplyr::mutate(
      orientation = (deg(.data$orientation) + 180)/2,
      orientation = round(.data$orientation, digits = 2))

  ranks <- rawd %>%
    dplyr::group_by(.data$voxel, .data$orientation, .data$contrast) %>%
    dplyr::summarise(
      avg0 = mean(.data$y),
      .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$contrast, values_from = .data$avg0) %>%
    dplyr::mutate(di = .data$high - .data$low) %>%
    tidyr::crossing(Quantile = quantiles) %>%
    dplyr::group_by(.data$Quantile) %>%
    dplyr::filter(.data$di > stats::quantile(.data$di, .data$Quantile)) %>%
    dplyr::select(-.data$di) %>%
    tidyr::pivot_longer(c(.data$low, .data$high), names_to = "contrast", values_to = "y") %>%
    dplyr::group_by(.data$voxel, .data$orientation, .data$Quantile) %>%
    dplyr::summarise(
      avg = mean(.data$y),
      .groups = "drop") %>%
    dplyr::group_by(.data$voxel, .data$Quantile) %>%
    dplyr::mutate(
      pref_o = .data$orientation[which.max(.data$avg)],
      pref = rad(.data$pref_o*2 - 180),
      rad = rad(.data$orientation*2 - 180),
      sep = abs(atan2(sin(.data$rad - .data$pref), cos(.data$rad - .data$pref))),
      sep = round(deg(.data$sep)/2, digits = 2),
      rank = rank(.data$avg)) %>%
    dplyr::select(.data$voxel, .data$orientation, .data$sep, .data$rank, .data$Quantile)


  out <- rawd %>%
    dplyr::inner_join(ranks, by = c("voxel","orientation")) %>%
    dplyr::group_by(.data$rank, .data$contrast, .data$Quantile) %>%
    dplyr::summarise(
      y = mean(.data$y),
      .groups = "drop") %>%
    tidyr::pivot_wider(names_from = .data$contrast, values_from = .data$y) %>%
    dplyr::rename(quantile = .data$Quantile)

  return(out)
}

#' @rdname ranks
#' @param ranks As output from [build_ranks()]
#'
#' @export
visualize_ranks <- function(ranks){
  ranks %>%
    ggplot2::ggplot(ggplot2::aes(x=.data$low, y=.data$high)) +
    ggplot2::facet_wrap(
      ~quantile,
      labeller = "label_both",
      nrow = 1) +
    ggplot2::coord_fixed() +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::scale_x_continuous(
      name = expression(paste("Observed Low Contrast ", beta))) +
    ggplot2::scale_y_continuous(
      name = expression(paste("Observed High Contrast ", beta))) +
    ggplot2::geom_line(
      formula = y~x,
      stat = "smooth",
      method = "lm",
      se = FALSE,
      color = "black",
      alpha = 0.5) +
    ggplot2::geom_point() +
    ggplot2::theme_classic() +
    ggplot2::theme(rect = ggplot2::element_blank()) +
    ggplot2::annotate("segment", x = 3, xend = 15.5, y=Inf, yend = Inf)
}
