# TODO: consider removing depency on colorscience due to its importing of Hmisc
# MSDN page on conversion formula:
# https://msdn.microsoft.com/en-us/library/ms893078.aspx

YUV2grDeviceRGB <- function(YUV) {
  out <- colorscience::YUV2RGB(YUV)
  out <- round(out)
  out <- pmax(out, 0)
  out <- pmin(out, 255)
  grDevices::rgb(out, maxColorValue = 255)
}

colorplane <- function(x, y, Y = 128,
                       xRange = range(x, na.rm = TRUE, finite = TRUE),
                       yRange = range(y, na.rm = TRUE, finite = TRUE),
                       naColor = "black") {
  naColor <- colorscience::RGB2YUV(t(grDevices::col2rgb(naColor[1])))
  u <- scales::rescale(x, to = c(-255, 255), from = xRange)
  v <- scales::rescale(y, to = c(-255, 255), from = yRange)
  # Y <- scales::rescale(sqrt((x - mean(xRange))^2 + (y - mean(yRange))^2),
  #                      to = c(0, 511))
  YUV <- as.matrix(cbind(Y, u, v))
  if(anyNA(x) || anyNA(y)) {
    naIndices <- cbind(c(rep(which(is.na(x)), 3), rep(which(is.na(y)), 3)), 1:3)
    YUV[naIndices] <- naColor
  }

  YUV2grDeviceRGB(YUV)
}

