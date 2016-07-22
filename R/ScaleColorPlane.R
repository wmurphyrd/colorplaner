ScaleColorPlane <- ggproto("ScaleColorPlane", ScaleContinuous,
  limits_y = NULL,
  breaks_y = waiver(),
  labels_y = waiver(),
  range = ggproto(NULL, ggplot2:::RangeContinuous),
  range_y = ggproto(NULL, ggplot2:::RangeContinuous),
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
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return()
    aesthetics <- sort(intersect(self$aesthetics, names(df)))
    names(aesthetics) <- aesthetics
    if (length(aesthetics) == 0) return()
    if (length(aesthetics) != 2) {
      message("Number of aesthetics not equal to 2:", aesthetics)
    }
    self$range$train(df[[aesthetics[1]]])
    self$range_y$train(df[[aesthetics[2]]])
  },
  get_range = function(self, dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    if(dir == "horizontal") self$range$range else self$range_y$range
  },
  get_limits = function(self, dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    if (self$is_empty()) return(c(0, 1))

    if(dir == "horizontal") {
      if (!is.null(self$limits)) {
        ifelse(!is.na(self$limits), self$limits, self$get_range(dir))
      } else {
        self$get_range(dir)
      }
    } else {
      if (!is.null(self$limits_y)) {
        ifelse(!is.na(self$limits_y), self$limits_y, self$get_range(dir))
      } else {
        self$get_range(dir)
      }
    }
  },
  get_breaks = function(self, limits = self$get_limits(dir),
                        dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    if (self$is_empty()) return(numeric())
    if(dir == "horizontal") breaks <- self$breaks else breaks <- self$breaks_y
    # Limits in transformed space need to be converted back to data space
    limits <- self$trans$inverse(limits)

    if (is.null(breaks)) {
      return(NULL)
    } else if (identical(breaks, NA)) {
      stop("Invalid breaks specification. Use NULL, not NA")
    } else if (scales::zero_range(as.numeric(limits))) {
      breaks <- limits[1]
    } else if (ggplot2:::is.waive(breaks)) {
      breaks <- self$trans$breaks(limits)
    } else if (is.function(breaks)) {
      breaks <- breaks(limits)
    }

    # Breaks in data space need to be converted back to transformed space
    # And any breaks outside the dimensions need to be flagged as missing
    breaks <- scales::censor(self$trans$transform(breaks),
                             self$trans$transform(limits),
                             only.finite = FALSE)
    if (length(breaks) == 0) {
      stop("Zero breaks in scale for ", paste(self$aesthetics, collapse = "/"),
           call. = FALSE)
    }
    breaks
  },

  get_breaks_minor = function(self, n = 2, b = self$break_positions(),
                              limits = self$get_limits()) {
#     if (zero_range(as.numeric(limits))) {
#       return()
#     }
#
#     if (is.null(self$minor_breaks)) {
#       return(NULL)
#     } else if (identical(self$minor_breaks, NA)) {
#       stop("Invalid minor_breaks specification. Use NULL, not NA", call. = FALSE)
#     } else if (is.waive(self$minor_breaks)) {
#       if (is.null(b)) {
#         breaks <- NULL
#       } else {
#         b <- b[!is.na(b)]
#         if (length(b) < 2) return()
#
#         bd <- diff(b)[1]
#         if (min(limits) < min(b)) b <- c(b[1] - bd, b)
#         if (max(limits) > max(b)) b <- c(b, b[length(b)] + bd)
#         breaks <- unique(unlist(mapply(seq, b[-length(b)], b[-1], length.out = n + 1,
#                                        SIMPLIFY = FALSE)))
#       }
#     } else if (is.function(self$minor_breaks)) {
#       # Find breaks in data space, and convert to numeric
#       breaks <- self$minor_breaks(self$trans$inverse(limits))
#       breaks <- self$trans$transform(breaks)
#     } else {
#       breaks <- self$trans$transform(self$minor_breaks)
#     }
#
#     # Any minor breaks outside the dimensions need to be thrown away
#     discard(breaks, limits)
    return()
  },
  get_labels = function(self, breaks = self$get_breaks(dir),
                        dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    if (is.null(breaks)) return(NULL)

    breaks <- self$trans$inverse(breaks)

    if (dir == "horizontal") labels <- self$labels else labels <- self$labels_y

    if (is.null(labels)) {
      return(NULL)
    } else if (identical(labels, NA)) {
      stop("Invalid labels specification. Use NULL, not NA", call. = FALSE)
    } else if (ggplot2:::is.waive(labels)) {
      labels <- self$trans$format(breaks)
    } else if (is.function(labels)) {
      labels <- labels(breaks)
    }
    if (length(labels) != length(breaks)) {
      stop("Breaks and labels are different lengths")
    }
    labels
  },
  map = function(self, x, limits = self$get_limits(dir),
                 dir = c("horizontal", "vertical")) {
    dir <- match.arg(dir)
    x <- self$oob(self$rescaler(x, from = limits))

    uniq <- unique(x)
    pal <- self$palette(uniq)
    scaled <- pal[match(x, uniq)]

    ifelse(!is.na(scaled), scaled, self$na.value)
  }
)

scale_color_colorplane <- function(name = waiver(),
                                   breaks = waiver(),
                                   breaks_y = waiver(),
                                   minor_breaks = waiver(),
                                   minor_breaks_y = waiver(),
                                   labels = waiver(),
                                   labels_y = waiver(),
                                   limits = NULL,
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
          range = ggproto(NULL, ggplot2:::RangeContinuous),

          limits = limits,
          trans = trans,
          na.value = na.value,
          expand = function(range, ...) {range},
          rescaler = rescaler,  # Used by diverging and n colour gradients
          oob = oob,

          name = name,
          breaks = breaks,
          breaks_y = breaks_y,
          minor_breaks = minor_breaks,
          minor_breaks_y = minor_breaks_y,

          labels = labels,
          labels_y = labels_y,
          guide = guide
  )
}
