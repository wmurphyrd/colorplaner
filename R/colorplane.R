# Wikipedia page on converions:
# https://en.wikipedia.org/wiki/YUV

#' @name color_projections
#'
#' @title Color Space Projections
#'
#' @description Functions to define how variables are mapped into color space.
#'   Custom functions can also be defined following this signature.
#'
#' @details Color space projection functions take two numeric vectors and return
#'   a single character vector of the same length that specifies the colors to
#'   plot in the form "#rrggbb", as returned by \code{\link[grDevices]{rgb}}.
#'   Additional projection functions can be defined following the same signature
#'   and passed to the \code{color_projection} arugment of the
#'   \code{scale_*_colorplane} scale constructors. When writing custom
#'   projection functions, expect two arguments that are numeric vectors scaled
#'   to a range of 0 to 1 and that do not contain missing values. Custom
#'   projections can accept additional arguments that are passed through from
#'   the \code{...} of \code{scale_*_colorplane}.
#'
#' @param x,y numeric vectors of equal length containing the values to be mapped
#'   to the horizontal and vertical axes of the colorplane.
#' @param Y numeric value for the fixed lumosity level in YUV projections.
#' @seealso \code{\link{scale_color_colorplane}},
#'   \code{\link{scale_fill_colorplane}}
NULL

#' @export
#' @rdname color_projections
#' @references YUV conversion matrix from \url{https://en.wikipedia.org/wiki/YUV}
YUV_projection <- function(x, y, Y = .35) {
  YUV <- cbind(Y,
               scales::rescale(x, to = c(-0.436, .436), from = c(0, 1)),
               scales::rescale(y, to = c(-0.615, 0.615), from = c(0, 1)))
  out <- t(matrix(c(1, 0, 1.13983,
                    1, -0.39465, -0.58060,
                    1, 2.03211, 0), ncol = 3, byrow = TRUE) %*%
             t(YUV))
  out <- apply(out, 2, function(x)pmax(0, pmin(1, x)))
  grDevices::rgb(out, maxColorValue = 1)
}

#' @export
#' @rdname color_projections
red_blue_projection <- function(x, y) {
  x <- scales::rescale(x, to = c(0, 255), from = c(0,1))
  y <- scales::rescale(y, to = c(0, 255), from = c(0,1))
  r <- 255 - pmax(0, y - x)
  b <- 255 - pmax(0, x - y)
  g <- 255 - (x + y) / 2
  rgb_mat <- cbind(r, g, b)
  grDevices::rgb(rgb_mat, maxColorValue = 255)
}

get_projection <- function(x) {
  if (mode(x) == "function") return(x)
  get(paste0(x, "_projection"), mode = "function")
}
