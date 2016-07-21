guide_colorplane <- function(

  # title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = NULL,
  title.vjust = NULL,

  # label
  label = TRUE,
  label.position = NULL,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,

  # plane
  planewidth = NULL,
  planeheight = NULL,
  nbin = 20,
  #raster = TRUE,

  # ticks
  ticks = TRUE,
  draw.ulim= TRUE,
  draw.llim = TRUE,

  # general
  #direction = NULL,
  default.unit = "line",
  #reverse = FALSE,
  order = 0,

  ...) {

  if (!is.null(planewidth) && !is.unit(planewidth)) planewidth <-
      unit(planewidth, default.unit)
  if (!is.null(planeheight) && !is.unit(planeheight)) planeheight <-
      unit(planeheight, default.unit)

  structure(list(
    # title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    # label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,

    # plane
    planewidth = planewidth,
    planeheight = planeheight,
    nbin = nbin,
    #raster = raster,

    # ticks
    ticks = ticks,
    draw.ulim = draw.ulim,
    draw.llim = draw.llim,

    # general
    #direction = direction,
    default.unit = default.unit,
    #reverse = reverse,
    order = order,

    # parameter
    available_aes = c("colour", "color", "fill", "color2", "colour2", "fill2"),
    ..., name = "colorplane"),
    class = c("guide", "colorplane")
  )
}

#' @export
guide_train.colorplane <- function(guide, scale) {

  # do nothing if scale are inappropriate
  if (!is(scale, "ScaleColorPlane")) {
    warning("colorplane guide needs scale_color_colorplane or scale_fill_colorplane.")
    return(NULL)
  }
  if (scale$is_discrete()) {
    warning("colorbar guide needs continuous scales.")
    return(NULL)
  }


  # create data frame for tick display
  breaks <- scale$get_breaks(dir = "horizontal")
  if (length(breaks) == 0 || all(is.na(breaks)))
    return()
  #TODO split ticks into two data frames for horix & vert due to difering lengths
  ticks <- data.frame(scale$map(breaks, dir = "horizontal"),
                      stringsAsFactors = FALSE)
  names(ticks) <- scale$aesthetics[1]
  ticks$.value <- breaks
  ticks$.label <- scale$get_labels(breaks, dir = "horizontal")

  breaks_y <- scale$get_breaks(dir = "vertical")
  if (length(breaks_y) == 0 || all(is.na(breaks_y)))
    return()
  ticks_y <- data.frame(scale$map(breaks_y, dir = "vertical"),
                        stringsAsFactors = FALSE)
  names(ticks_y) <- scale$aesthetics[1]
  ticks_y$.value <- breaks_y
  ticks_y$.label <- scale$get_labels(breaks_y, dir = "vertical")

  guide$key <- ticks
  guide$key_y <- ticks_y

  # bar specification (number of divs etc)
  .limits <- scale$get_limits(dir = "horizontal")
  .bar <- seq(.limits[1], .limits[2], length.out = guide$nbin)
  .limits_y <- scale$get_limits(dir = "vertical")
  .bar_y <- seq(.limits_y[1], .limits_y[2], length.out = guide$nbin)

  if (length(.bar) == 0) {
    .bar <- unique(.limits)
  }
  if (length(.bar_y) == 0) {
    .bar_y <- unique(.limits_y)
  }
  browser()
  .plane <- expand.grid(.bar, .bar_y)
  names(.plane) <- scale$aesthetics
  .plane <- matrix(scale$map_df(.plane)[[scale$aesthetics[1]]],
                   nrow = guide$nbin)
  attr(.plane, "aesthetic") <- scale$aesthetics[1]
  attr(.plane, "value") <- .bar
  attr(.plane, "value_y") <- .bar_y
  guide$plane <- .plane
#   if (guide$reverse) {
#     guide$key <- guide$key[nrow(guide$key):1, ]
#     guide$bar <- guide$bar[nrow(guide$bar):1, ]
#   }
  guide$hash <- with(guide, digest::digest(list(title, key$.label, key_y$.label,
                                                plane, name)))
  guide
}

# simply discards the new guide
#' @export
guide_merge.colorbar <- function(guide, new_guide) {
  guide
}

# this guide is not geom-based.
#' @export
guide_geom.colorbar <- function(guide, ...) {
  guide
}

#' @export
guide_gengrob.colorbar <- function(guide, theme) {

  # settings of location and size
#   switch(guide$direction,
#          "horizontal" = {
#            label.position <- guide$label.position %||% "bottom"
#            if (!label.position %in% c("top", "bottom")) stop("label position \"", label.position, "\" is invalid")
#
#            barwidth <- convertWidth(guide$barwidth %||% (theme$legend.key.width * 5), "mm")
#            barheight <- convertHeight(guide$barheight %||% theme$legend.key.height, "mm")
#          },
#          "vertical" = {
#            label.position <- guide$label.position %||% "right"
#            if (!label.position %in% c("left", "right")) stop("label position \"", label.position, "\" is invalid")
#
#            barwidth <- convertWidth(guide$barwidth %||% theme$legend.key.width, "mm")
#            barheight <- convertHeight(guide$barheight %||% (theme$legend.key.height * 5), "mm")
#          })
  planewidth <- grid::convertWidth(guide$barwidth %||%
                                     (theme$legend.key.width * 5), "mm")
  planeheight <- grid::convertHeight(guide$barheight %||%
                                       (theme$legend.key.height * 5), "mm")

  planewidth.c <- c(barwidth)
  planeheight.c <- c(barheight)
  #barlength.c <- switch(guide$direction, "horizontal" = barwidth.c, "vertical" = barheight.c)
  nbreak <- nrow(guide$key)
  nbreak_y <- nrow(guide$key_y)

  # gap between keys etc
  hgap <- c(grid::convertWidth(unit(0.3, "lines"), "mm"))
  vgap <- hgap

  grob.bar <-
    rasterGrob(guide$plane, width = planewidth.c, height = planeheight.c,
               default.units = "mm", gp = gpar(col = NA), interpolate = TRUE)
#     if (guide$raster) {
#       image <- switch(guide$direction, horizontal = t(guide$bar$colour), vertical = rev(guide$bar$colour))
#       rasterGrob(image = image, width = barwidth.c, height = barheight.c, default.units = "mm", gp = gpar(col = NA), interpolate = TRUE)
#     } else {
#       switch(guide$direction,
#              horizontal = {
#                bw <- barwidth.c / nrow(guide$bar)
#                bx <- (seq(nrow(guide$bar)) - 1) * bw
#                rectGrob(x = bx, y = 0, vjust = 0, hjust = 0, width = bw, height = barheight.c, default.units = "mm",
#                         gp = gpar(col = NA, fill = guide$bar$colour))
#              },
#              vertical = {
#                bh <- barheight.c / nrow(guide$bar)
#                by <- (seq(nrow(guide$bar)) - 1) * bh
#                rectGrob(x = 0, y = by, vjust = 0, hjust = 0, width = barwidth.c, height = bh, default.units = "mm",
#                         gp = gpar(col = NA, fill = guide$bar$colour))
#              })
#     }

  # tick and label position
  tic_pos.c <- scales::rescale(guide$key$.value, c(0.5, guide$nbin - 0.5),
                               range(attr(guide$plane, "value"),
                                     na.rm = T, finite = T)) *
    planewidth.c / guide$nbin
  label_pos <- unit(tic_pos.c, "mm")
  if (!guide$draw.ulim) tic_pos.c <- tic_pos.c[-1]
  if (!guide$draw.llim) tic_pos.c <- tic_pos.c[-length(tic_pos.c)]

  tic_pos_y.c <- scales::rescale(guide$key_y$.value, c(0.5, guide$nbin - 0.5),
                               range(attr(guide$plane, "value_y"),
                                     na.rm = T, finite = T)) *
    planeheight.c / guide$nbin
  label_pos_y <- unit(tic_pos_y.c, "mm")
  if (!guide$draw.ulim) tic_pos_y.c <- tic_pos_y.c[-1]
  if (!guide$draw.llim) tic_pos_y.c <- tic_pos_y.c[-length(tic_pos_y.c)]

  # title
  grob.title <- ggplot2:::ggname(
    "guide.title",
    ggplot2::element_grob(
      guide$title.theme %||% ggplot2::calc_element("legend.title", theme),
      label = guide$title,
      hjust = guide$title.hjust %||% theme$legend.title.align %||% 0,
      vjust = guide$title.vjust %||% 0.5
    )
  )


  title_width <- grid::convertWidth(grid::grobWidth(grob.title), "mm")
  title_width.c <- c(title_width)
  title_height <- grid::convertHeight(grid::grobHeight(grob.title), "mm")
  title_height.c <- c(title_height)

  # label
  label.theme <- guide$label.theme %||%
    ggplot2::calc_element("legend.text", theme)
  grob.label <- {
    if (!guide$label)
      ggplot2::zeroGrob()
    else {
      hjust <- x <- guide$label.hjust %||% theme$legend.text.align %||%
        if (any(is.expression(guide$key$.label))) 1 else switch(guide$direction, horizontal = 0.5, vertical = 0)
      vjust <- y <- guide$label.vjust %||% 0.5
      switch(guide$direction, horizontal = {x <- label_pos; y <- vjust}, "vertical" = {x <- hjust; y <- label_pos})

      label <- guide$key$.label

      # If any of the labels are quoted language objects, convert them
      # to expressions. Labels from formatter functions can return these
      if (any(vapply(label, is.call, logical(1)))) {
        label <- lapply(label, function(l) {
          if (is.call(l)) substitute(expression(x), list(x = l))
          else l
        })
        label <- do.call(c, label)
      }
      g <- element_grob(element = label.theme, label = label,
                        x = x, y = y, hjust = hjust, vjust = vjust)
      ggname("guide.label", g)
    }
  }

  label_width <- convertWidth(grobWidth(grob.label), "mm")
  label_width.c <- c(label_width)
  label_height <- convertHeight(grobHeight(grob.label), "mm")
  label_height.c <- c(label_height)

  # ticks
  grob.ticks <-
    if (!guide$ticks) ggplot2::zeroGrob()
  else {
    switch(guide$direction,
           "horizontal" = {
             x0 = rep(tic_pos.c, 2)
             y0 = c(rep(0, nbreak), rep(barheight.c * (4/5), nbreak))
             x1 = rep(tic_pos.c, 2)
             y1 = c(rep(barheight.c * (1/5), nbreak), rep(barheight.c, nbreak))
           },
           "vertical" = {
             x0 = c(rep(0, nbreak), rep(barwidth.c * (4/5), nbreak))
             y0 = rep(tic_pos.c, 2)
             x1 = c(rep(barwidth.c * (1/5), nbreak), rep(barwidth.c, nbreak))
             y1 = rep(tic_pos.c, 2)
           })
    grid::segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                       default.units = "mm", gp = gpar(col = "white", lwd = 0.5, lineend = "butt"))
  }

  # layout of bar and label
  switch(guide$direction,
         "horizontal" = {
           switch(label.position,
                  "top" = {
                    bl_widths <- barwidth.c
                    bl_heights <- c(label_height.c, vgap, barheight.c)
                    vps <- list(bar.row = 3, bar.col = 1,
                                label.row = 1, label.col = 1)
                  },
                  "bottom" = {
                    bl_widths <- barwidth.c
                    bl_heights <- c(barheight.c, vgap, label_height.c)
                    vps <- list(bar.row = 1, bar.col = 1,
                                label.row = 3, label.col = 1)
                  })
         },
         "vertical" = {
           switch(label.position,
                  "left" = {
                    bl_widths <- c(label_width.c, vgap, barwidth.c)
                    bl_heights <- barheight.c
                    vps <- list(bar.row = 1, bar.col = 3,
                                label.row = 1, label.col = 1)
                  },
                  "right" = {
                    bl_widths <- c(barwidth.c, vgap, label_width.c)
                    bl_heights <- barheight.c
                    vps <- list(bar.row = 1, bar.col = 1,
                                label.row = 1, label.col = 3)
                  })
         })

  # layout of title and bar+label
  switch(guide$title.position,
         "top" = {
           widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
           heights <- c(title_height.c, vgap, bl_heights)
           vps <- with(vps,
                       list(bar.row = bar.row + 2, bar.col = bar.col,
                            label.row = label.row + 2, label.col = label.col,
                            title.row = 1, title.col = 1:length(widths)))
         },
         "bottom" = {
           widths <- c(bl_widths, max(0, title_width.c - sum(bl_widths)))
           heights <- c(bl_heights, vgap, title_height.c)
           vps <- with(vps,
                       list(bar.row = bar.row, bar.col = bar.col,
                            label.row = label.row, label.col = label.col,
                            title.row = length(heights), title.col = 1:length(widths)))
         },
         "left" = {
           widths <- c(title_width.c, hgap, bl_widths)
           heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
           vps <- with(vps,
                       list(bar.row = bar.row, bar.col = bar.col + 2,
                            label.row = label.row, label.col = label.col + 2,
                            title.row = 1:length(heights), title.col = 1))
         },
         "right" = {
           widths <- c(bl_widths, hgap, title_width.c)
           heights <- c(bl_heights, max(0, title_height.c - sum(bl_heights)))
           vps <- with(vps,
                       list(bar.row = bar.row, bar.col = bar.col,
                            label.row = label.row, label.col = label.col,
                            title.row = 1:length(heights), title.col = length(widths)))
         })

  # background
  grob.background <- ggplot2:::element_render(theme, "legend.background")

  # padding
  padding <- unit(1.5, "mm")
  widths <- c(padding, widths, padding)
  heights <- c(padding, heights, padding)

  gt <- gtable::gtable(widths = unit(widths, "mm"), heights = unit(heights, "mm"))
  gt <- gtable::gtable_add_grob(gt, grob.background, name = "background", clip = "off",
                        t = 1, r = -1, b = -1, l = 1)
  gt <- gtable::gtable_add_grob(gt, grob.bar, name = "bar", clip = "off",
                        t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
                        b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))
  gt <- gtable::gtable_add_grob(gt, grob.label, name = "label", clip = "off",
                        t = 1 + min(vps$label.row), r = 1 + max(vps$label.col),
                        b = 1 + max(vps$label.row), l = 1 + min(vps$label.col))
  gt <- gtable::gtable_add_grob(gt, grob.title, name = "title", clip = "off",
                        t = 1 + min(vps$title.row), r = 1 + max(vps$title.col),
                        b = 1 + max(vps$title.row), l = 1 + min(vps$title.col))
  gt <- gtable::gtable_add_grob(gt, grob.ticks, name = "ticks", clip = "off",
                        t = 1 + min(vps$bar.row), r = 1 + max(vps$bar.col),
                        b = 1 + max(vps$bar.row), l = 1 + min(vps$bar.col))

  gt
}
