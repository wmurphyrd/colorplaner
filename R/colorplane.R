# Wikipedia page on converions:
# https://en.wikipedia.org/wiki/YUV

#input range (0,1), (-.043599,0.536), (-0.615, 0.615)
#output range #000000,#ffffff
YUV2grDeviceRGB <- function(YUV) {
  out <- t(matrix(c(1, 0, 1.13983,
                    1, -0.39465, -0.58060,
                    1, 2.03211, 0), ncol = 3, byrow = TRUE) %*%
             t(YUV))
  out <- apply(out, 2, function(x)pmax(0, pmin(1, x)))
  grDevices::rgb(out, maxColorValue = 1)
}

#input range (0,1), (0,1), (0,1)
#output range (0,1), (-.043599,0.536), (-0.615, 0.615)
RGB2YUV <- function(x) {
  t(matrix(c(0.299, 0.587, 0.114,
           -0.14713, -0.28886, 0.436,
           0.615, -0.51499, -0.10001), nrow = 3, byrow = TRUE) %*%
    t(x))
}

#' @name color_projections
#'
#' @title Color Space Projections
#'
#' @description Functions to define how variables are mapped into color space. Custom
#' functions can also be defined following this signature.
#'
#' @details Color space projection functions take two numeric vectors and return a single
#' character vector of the same length that specifies the colors to plot in the
#' form "#rrggbb", as is returned by \code{\link[grDevices]{rgb}}.
#' Additional projection functions can be defined following the same signature
#' and passed to the \code{color_projection} arugment of the scale_*_colorplane
#' scale constructors.
#'
#' @param x,y numeric vectors of equal length containing the values to be mapped
#'   to the horizontal and vertical axes of the colorplane.
#' @param Y numeric value for the fixed lumosity level in YUV projections.
#' @param xRange,yRange numeric vectors of length 2 specifying the lower and
#'   upper bounds of the scale for each dimension. Defaults to the range of
#'   \code{x} and \code{y}.
#' @param naColor character string specifying a color to use when either
#'   \code{x} or \code{y} are \code{NA}. Accepts standard \code{R} color
#'   specifications.
NULL

#' @export
#' @rdname color_projections
YUV_projection <- function(x, y, Y = .35,
                       xRange = range(x, na.rm = TRUE, finite = TRUE),
                       yRange = range(y, na.rm = TRUE, finite = TRUE),
                       naColor = "black") {
  naColor <- RGB2YUV(t(grDevices::col2rgb(naColor[1])))
  u <- scales::rescale(x, to = c(-0.43599, .436), from = xRange)
  v <- scales::rescale(y, to = c(-0.615, 0.615), from = yRange)
  # Y <- scales::rescale(sqrt((x - mean(xRange))^2 + (y - mean(yRange))^2),
  #                      to = c(0, 511))
  YUV <- as.matrix(cbind(Y, u, v))
  if(anyNA(x) || anyNA(y)) {
    naIndices <- cbind(c(rep(which(is.na(x)), 3), rep(which(is.na(y)), 3)), 1:3)
    YUV[naIndices] <- naColor
  }

  YUV2grDeviceRGB(YUV)
}


#' @export
#' @rdname color_projections
red_blue_projection <- function(x, y, Y = NULL,
                           xRange = range(x, na.rm = TRUE, finite = TRUE),
                           yRange = range(y, na.rm = TRUE, finite = TRUE),
                           naColor = "black") {
  naColor <- t(grDevices::col2rgb(naColor[1]))
  x <- scales::rescale(x, to = c(0, 255), from = xRange)
  y <- scales::rescale(y, to = c(0, 255), from = yRange)
  r <- 255 - pmax(0, y - x)
  b <- 255 - pmax(0, x - y)
  g <- 255 - (x + y) / 2
  rgb_mat <- cbind(r, g, b)
  if(anyNA(x) || anyNA(y)) {
    naIndices <- cbind(c(rep(which(is.na(x)), 3), rep(which(is.na(y)), 3)), 1:3)
    rgb_mat[naIndices] <- naColor
  }
  grDevices::rgb(rgb_mat, maxColorValue = 255)
}

get_projection <- function(x) {
  if (mode(x) == "function") return(x)
  get(paste0(x, "_projection"), mode = "function")
}
