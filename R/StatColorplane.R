StatColorplane <- ggplot2::ggproto("StatColorplane", ggplot2::Stat,
#   setup_params = function(data, params) {
#     #params$color2 <- lazyeval::lazy_eval(lazyeval::lazy(params$color2))
#     #params$color2 <- lazyeval::lazy_eval(params$color2, data)
#   },
  compute_group = function(data, scales, color2) {
    browser()
    data$colour <- colorplane(data$colour, data$colour2)
    data
  },
  required_aes = c("colour", "colour2")
)

stat_colorplane <- function(mapping = NULL, data = NULL, geom = "point",
                            position = "identity", show.legend = FALSE,
                            inherit.aes = TRUE, ...) {
  layer(
    stat = StatColorplane, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = F, ...)
  )
}
