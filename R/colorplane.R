# TODO: consider removing depency on colorscience due to its importing of Hmisc
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

colorplane <- function(x, y, Y = .35,
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

