
library("ggplot2")

#' Discrete analogue to violin plots for ggplot2
#'
#' Plot a centered vertical histrograms (a discrete analogue to violin plots)
#' for data for integer-values y-values. Will throw an error for non-integer data.
#'
#' @param width maximum bar width, from zero to one
#' @param height bar height, from zero to one
#' @param scale whether to scale bars so that bars for each unique x value have the same width (scale = "width") or so that x groups with fewer data points have proportionall narrower bars (scale = "count")
#' @param position Currently, only "identity" is supported
#'
#' @inheritParams ggplot2::layer
#'
#' @export
#' @rdname geom_violin_discrete
#'
#' @examples
#'
#' ## Simulate Poisson data
#' library("dplyr")
#'
#' data <- tibble(
#'   x = sample(letters[1:4], 200, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
#'   y = rpois(200, as.numeric(as.factor(x)))
#' )
#'
#' # Plot distribution, with equal width for each category
#' data |> ggplot(aes(x, y)) + geom_violin_discrete()
#'
#' # Equivalently
#' data |> ggplot(aes(x, y)) + geom_rect(stat = "ycount")
#'
#' # Plot distribution, scaling width by sample size
#' data |> ggplot(aes(x, y)) + geom_violin_discrete(scale = "count")
#'
stat_ycount <- function(mapping = NULL, data = NULL,
                       geom = "rect", position = "identity",
                       width = 0.9, height = 0.8, na.rm = TRUE,
                       scale = "width", show.legend = NA,
                       inherit.aes = TRUE, ...) {

  layer(
    data = data,
    mapping = mapping,
    stat = StatYcount,
    geom = geom_rect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      width = width,
      height = height,
      scale = scale,
      ...
    )
  )
}

#' @rdname geom_violin_discrete
#' @export
StatYcount <- ggproto("StatYcount", Stat,

                      required_aes = c("x", "y"),

                      setup_params = function(data, params) {
                        params
                      },

                      extra_params = c("na.rm", "scale", "width", "height"),

                      compute_panel = function(data, scales, na.rm = TRUE, width = 0.9, height = 0.8, scale = "width") {

                        if(!all(data$y == as.integer(data$y))) stop("stat_ycount requires integer y-values")

                        if (na.rm) data <- data |> na.omit()

                        if (width < 0 | width > 1) stop("please specify a width between 0 and 1")
                        if (height < 0 | height > 1) stop("please specify a height between 0 and 1")

                        if(is.numeric(data$x)) width <- width * resolution(data$x)

                        distinct <- dplyr::distinct(data, x, y, .keep_all = TRUE)

                        data <- scale |> switch(

                          width = data |>
                            dplyr::count(x, y) |>
                            dplyr::group_by(x) |>
                            dplyr::mutate(
                              rel_width = n / max(n),
                              rel_width = rel_width / max(rel_width)
                            ) |>
                            dplyr::ungroup(),

                          count = data |>
                            dplyr::count(x, y) |>
                            dplyr::mutate(
                              rel_width = n / max(n),
                              rel_width = rel_width / max(rel_width)
                            ) |>
                            dplyr::ungroup()

                        )

                        data <- data |> dplyr::mutate(
                          xmin = x - width * (rel_width / 2),
                          xmax = x + width * (rel_width / 2),
                          ymin = y - (height / 2),
                          ymax = y + (height / 2)
                        ) |> dplyr::select(-rel_width)

                        data <- dplyr::left_join(data, distinct, by = c("x", "y"))

                        data

                      }

)

#' @export
#' @rdname geom_violin_discrete
geom_violin_discrete = function(mapping = NULL, data = NULL, position = "identity",
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                                width = 0.9, height = 0.8, scale = "width", ...) {
  layer(
    geom = GeomRect, mapping = mapping, data = data, stat = "ycount",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, width = width, height = height, scale = scale,...)
  )
}
