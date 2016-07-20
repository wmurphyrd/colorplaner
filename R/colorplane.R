#library(colorscience)
#library(ggplot2)

scaleUV <- function(x) {
  x <- scales::rescale(x, to = c(-255, 255))
}


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
                       naColor = "#f7f7f2") {
  u <- scales::rescale(x, to = c(-255, 255), from = xRange)
  v <- scales::rescale(y, to = c(-255, 255), from = yRange)
  # Y <- scales::rescale(sqrt((x - mean(xRange))^2 + (y - mean(yRange))^2),
  #                      to = c(0, 511))
  YUV <- as.matrix(cbind(Y, u, v))
  YUV[is.na(x) | is.na(y), ]
  out <- YUV2grDeviceRGB(YUV)
  ifelse(is.na(out), naColor, out)
}



# legendPlot <- function(x, y, xlab, ylab, Y = 128,
#                        xRange = range(x, na.rm = TRUE, finite = TRUE),
#                        yRange = range(y, na.rm = TRUE, finite = TRUE)) {
#   dat <- expand.grid(do.call(seq,c(as.list(xRange), length.out = 100)),
#                      do.call(seq,c(as.list(yRange), length.out = 100)))
#   dat$colors = colorPlane(dat[[1]], dat[[2]], Y = Y,
#                       xRange = xRange, yRange = yRange)
#   ggplot(dat, aes(x = dat[[1]], y = dat[[2]], color = dat$colors)) +
#     geom_point(size = 4) +
#     scale_color_identity() +
#     labs(x = xlab, y = ylab) +
#     scale_x_continuous(expand = c(0,0)) +
#     scale_y_continuous(expand = c(0,0)) +
#     theme_classic() + theme(axis.line = element_blank())
# }
#
#
# # code below for alternate color space projection; deprecated
# testPlotYCbCr <- function() {
#   dat <- seq(1:500) %>% expand.grid(., .) %>% as.matrix()
#   plot(x = dat[, 1], y = dat[, 2], type = "p",
#        col = colorPlaneYCbCr(dat[ , 1], dat[, 2]), pch = 16)
# }
#
#
# colorPlaneYCbCr <- function(x, y, Y = 28141,
#                             xRange = range(x, na.rm = TRUE, finite = TRUE),
#                             yRange = range(y, na.rm = TRUE, finite = TRUE)) {
#   u <- scales::rescale(x, to = c(-32768, 32768), from = xRange)
#   v <- scales::rescale(y, to = c(-32768, 32768), from = yRange)
#   YCbCr2grDeviceRGB(as.matrix(cbind(Y, u, v)))
# }
#
# YCbCr2grDeviceRGB <- function(YCbCr) {
#   out <- t(colorscience::YCbCr2RGB(YCbCr))
#   out <- round(out)
#   out <- pmax(out, 0)
#   out <- pmin(out, 255)
#   grDevices::rgb(out, maxColorValue = 255)
# }
#
#
#
