# Wikipedia page on converions:
# https://en.wikipedia.org/wiki/YUV

#' @name color_projections
#'
#' @title Color Space Projections
#'
#' @description Functions to define how variables are mapped into color space.
#'   Used for the \code{color_projection} argument to
#'   \code{\link{scale_color_colorplane}} and
#'   \code{\link{scale_fill_colorplane}}. Custom functions can also be defined
#'   following this signature.
#'
#' @details Color space projection functions take two numeric vectors and return
#'   a single character vector of the same length that specifies the colors to
#'   plot in the form "#rrggbb", as returned by \code{\link[grDevices]{rgb}}.
#'   Additional projection functions can be defined following the same signature
#'   and passed to the \code{color_projection} argument of the
#'   \code{scale_*_colorplane} scale constructors. When writing custom
#'   projection functions, expect two arguments that are numeric vectors scaled
#'   to a range of 0 to 1 and that do not contain missing values. Custom
#'   projections can accept additional arguments that are passed through from
#'   the \code{...} of \code{scale_*_colorplane}.
#' @return Character vector of colors of the same length as \code{x} and
#'   \code{y}.
#'
#' @param x,y numeric vectors of equal length containing the values to be mapped
#'   to the horizontal and vertical axes of the colorplane.
#' @param Y numeric value in range 0 to 1 for the fixed luminosity level in YUV
#'   projections.
#' @seealso \code{\link{scale_color_colorplane}},
#'   \code{\link{scale_fill_colorplane}}
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg, color = disp, colour2 = hp)) +
#'   geom_point() +
#'   scale_color_colorplane(color_projection = interpolate_projection,
#'                          zero_color = "darkorange2",
#'                          horizontal_color = "mediumspringgreen",
#'                          vertical_color = "magenta")
NULL

#' @export
#' @rdname color_projections
#' @references YUV conversion matrix from
#' \url{https://en.wikipedia.org/wiki/YUV}. UV limits sourced from
#' \href{http://downloads.bbc.co.uk/rd/pubs/reports/1987-22.pdf}{Deveroux VG. Limiting of YUV Video Signals. British Broadcasting System. 1987}
#' .
YUV_projection <- function(x, y, Y = .3) {
  if(length(Y) > 1) {
    warning("YUV_projection: Y argument length > 1, using first element")
    Y <- Y[1]
  }
  if(!(is.numeric(Y) && Y <= 1 && Y >= 0)) {
    stop("YUV_projection: Invalid Y specification. Needs numeric in [0,1]")
  }
  YUV <- cbind(Y,
               scales::rescale(x, to = c(-0.886, .886), from = c(0, 1)),
               scales::rescale(y, to = c(-0.701, 0.701), from = c(0, 1)))
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
  interpolate_projection(x, y, "white", "red", "blue")
}

#' @export
#' @rdname color_projections
#' @param zero_color,horizontal_color,vertical_color Character strings
#'   specifying R colors to use in interpolation projections. See Details.
#'
#' @details For \code{interpolate_projection}, a color space is created via
#'   linear RGB-space interpolation for each axis and then blending by averages.
#'   \code{zero_color} is the base color when both \code{x} and \code{y} are
#'   minimal. \code{horizontal_color} specified the color to interpolate towards
#'   for increasing \code{x} values and \code{vertical_color} for \code{y}. The
#'   plotted color will be a simple average of the interpolated \code{x} and
#'   \code{y} values.
#'
interpolate_projection <- function(x, y,
                                   zero_color,
                                   horizontal_color,
                                   vertical_color) {
  xc <- grDevices::colorRamp(c(zero_color, horizontal_color))(x)
  yc <- grDevices::colorRamp(c(zero_color, vertical_color))(y)
  grDevices::rgb((xc + yc) / 2, maxColorValue = 255)
}

get_projection <- function(x) {
  if (mode(x) == "function") return(x)
  get(paste0(x, "_projection"), mode = "function")
}
