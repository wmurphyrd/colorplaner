ScaleColorPlane <- ggproto("ScaleColorPlane", ScaleContinuous,
  map_df = function(self, df, i = NULL) {
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return()

    aesthetics <- sort(intersect(self$aesthetics, names(df)))
    names(aesthetics) <- aesthetics
    if (length(aesthetics) == 0) return()
    if (length(aesthetics) != 2) {
     message("Number of aesthetics not equal to 2:", aesthetics)
    }

    df[[aesthetics[1]]] <- colorplane(df[[aesthetics[1]]],
                                     df[[aesthetics[2]]])
    # This handling for optional paramter i is in the default method for Scale
    # proto, but the method is only ever called from ggplot_build without it

    #    if (is.null(i)) {
    #      lapply(aesthetics, function(j) self$map(df[[j]]))
    #    } else {
    #      lapply(aesthetics, function(j) self$map(df[[j]][i]))
    #    }
    df
  },
  train_df = function(self, df) {
    browser()
    self$super$train_df(df)
  }
)

scale_color_colorplane <- function(name = waiver(),
                                   breaks = waiver(), minor_breaks = waiver(),
                                   labels = waiver(), limits = NULL,
                                   rescaler = scales::rescale,
                                   oob = scales::censor, na.value = NA_real_,
                                   guide = "none") {

  ggplot2:::check_breaks_labels(breaks, labels)

  if (is.null(breaks) && guide != "none") {
    guide <- "none"
  }

  trans <- scales::identity_trans()
  if (!is.null(limits)) {
    limits <- trans$transform(limits)
  }

  ggproto(NULL, ScaleColorPlane,
          call = match.call(),

          aesthetics = c("colour", "colour2"),
          scale_name = "colorplane",
          palette = scales::identity_pal(),
          range = ggplot2:::continuous_range(),

          limits = limits,
          trans = trans,
          na.value = na.value,
          expand = function(range, ...) {range},
          rescaler = rescaler,  # Used by diverging and n colour gradients
          oob = oob,

          name = name,
          breaks = breaks,
          minor_breaks = minor_breaks,

          labels = labels,
          guide = guide
  )
}
