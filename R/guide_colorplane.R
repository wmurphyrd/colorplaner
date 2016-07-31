#' @include other_package_compatibility.R
NULL

#' Add Guide for Colorplane
#'
#' Generates a guide to explain the colors plotted via
#' \code{\link{scale_color_colorplane}} and \code{\link{scale_fill_colorplane}}.
#'
#' The guide is similar to \code{\link[ggplot2]{guide_colorbar}}, but as a plane
#' of colors with ticks and labels for both variables in the scale.
#'
#' @param axis_title,axis_title_y Character strings or expressions inidicating
#'   the horizontal and vertical axis titles in the guide, respectively. If
#'   \code{NULL}, the title is not shown. By default (\link[ggplot2]{waiver}),
#'   the name of the scale or the name of the variable mapped to the aesthetic.
#' @param axis_title.position,axis_title_y.position Character strings indicating
#'   the positions of the axis titles. \code{axis_title.position}: one of "top"
#'   or "bottom" (default). \code{axis_title_y.position}: one of "left"
#'   (default) or "right". Not yet implemented.
#' @param axis_title.theme,axis_title_y.theme Theme objects for rendering the
#'   axis title text. Typically an \code{\link[ggplot2]{element_text}} object.
#'   When \code{NULL}, defaults to settings for \code{axis.title.x} and
#'   \code{axis.title.y} in the plot theme.
#' @param axis_title.hjust,axis_title_y.hjust Numbers specifying horizontal
#'   justification of the axis title text. When \code{NULL}, defaults to
#'   \code{axis_title.theme} or \code{axis.title.x} in plot theme.
#' @param axis_title.vjust,axis_title_y.vjust Numbers specifing veritcal
#'   justification of the axis title text. When \code{NULL}, defaults to
#'   \code{axis_title*.theme} if set or \code{axis.title.*} in plot theme.
#' @param planewidth,planeheight Numeric or \code{\link[grid]{unit}} objects
#'   specifying the width and height of the colorplane. Default values are 5
#'   times the \code{legend.key.width/height} or \code{legend.key.size} in the
#'   plot theme.
#' @param nbin Number specifying how many color pixels are generated for each
#'   dimension of the colorplane. Higher numbers increase guide color accuracy
#'   (especially for larger sized guides) at the expense of speed.
#' @param label.position,label_y.position Character strings indicating the
#'   positions of axis labels. For \code{label.position}, "top" or "bottom"
#'   (default). For \code{label_y.position}, "left" (default) or "right". Not
#'   yet implemented.
#' @param label.theme,label_y.theme	Theme objects for rendering axis label text.
#'   Usually the object of \code{\link[ggplot2]{element_text}} is expected. By
#'   default, the theme is specified by \code{axis.text.*} in the plot theme.
#' @param label.hjust,label_y.hjust Numerics specifying horizontal justification
#'   of the axis label text. Defauls to value in \code{label.theme} /
#'   \code{label_y.theme} if set or \code{axis.text.*} in the plot theme.
#' @param label.vjust,label_y.vjust Numerics specifying vertical justification
#'   of the axis label text. Defauls to value in \code{label.theme} /
#'   \code{label_y.theme} if set or \code{axis.text.*} in the plot theme.
#'
#' @inheritParams ggplot2::guide_colorbar
#' @examples
#' if(requireNamespace("mapproj")) {
#'   crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#'   states_map <- map_data("state")
#'   ggplot(crimes,
#'          aes(map_id = state, fill = Murder, fill2 = UrbanPop)) +
#'     geom_map(map = states_map) +
#'     scale_fill_colorplane() +
#'     expand_limits(x = states_map$long, y = states_map$lat) +
#'     coord_map() +
#'     guides(fill = guide_colorplane("My Title", axis_title = "Murder Rate",
#'     axis_title_y = "Urban Population %"))
#'  }
#' @export
guide_colorplane <- function(

  # title
  title = waiver(),
  title.position = NULL,
  title.theme = NULL,
  title.hjust = 0.5,
  title.vjust = NULL,

  #axis titles
  axis_title = waiver(),
  axis_title.position = NULL,
  axis_title.theme = NULL,
  axis_title.hjust = NULL,
  axis_title.vjust = NULL,
  axis_title_y = waiver(),
  axis_title_y.position = NULL,
  axis_title_y.theme = NULL,
  axis_title_y.hjust = NULL,
  axis_title_y.vjust = NULL,


  # label
  label = TRUE,
  label.position = NULL,
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,
  label_y.position = NULL,
  label_y.theme = NULL,
  label_y.hjust = NULL,
  label_y.vjust = NULL,

  # plane
  planewidth = NULL,
  planeheight = NULL,
  nbin = 20,
  #raster = TRUE,

  # ticks
  ticks = TRUE,

  # general
  #direction = NULL,
  default.unit = "line",
  #reverse = FALSE,
  order = 0,

  ...) {

  if (!is.null(planewidth) && !grid::is.unit(planewidth)) planewidth <-
      unit(planewidth, default.unit)
  if (!is.null(planeheight) && !grid::is.unit(planeheight)) planeheight <-
      unit(planeheight, default.unit)

  structure(list(
    # title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    #axis titles
    axis_title = axis_title,
    axis_title.position = axis_title.position,
    axis_title.theme = axis_title.theme,
    axis_title.hjust = axis_title.hjust,
    axis_title.vjust = axis_title.vjust,
    axis_title_y = axis_title_y,
    axis_title_y.position = axis_title_y.position,
    axis_title_y.theme = axis_title_y.theme,
    axis_title_y.hjust = axis_title_y.hjust,
    axis_title_y.vjust = axis_title_y.vjust,

    # label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,
    label_y.position = label_y.position,
    label_y.theme = label_y.theme,
    label_y.hjust = label_y.hjust,
    label_y.vjust = label_y.vjust,

    # plane
    planewidth = planewidth,
    planeheight = planeheight,
    nbin = nbin,
    #raster = raster,

    # ticks
    ticks = ticks,

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

#' Colorplane method for guide_train
#'
#' Called by ggplot2 engine when building the plot.
#'
#' @param guide Object of class "colorplane" generate by
#'   \code{\link{guide_colorplane}}
#' @param scale ggproto object instance of \code{\link{ScaleColorPlane}}
#'
#' @export
guide_train.colorplane <- function(guide, scale) {
  # do nothing if scale inappropriate
  if (!is(scale, "ScaleColorPlane")) {
    warning("colorplane guide needs scale_color_colorplane or scale_fill_colorplane.")
    return(NULL)
  }
  if (scale$is_discrete()) {
    warning("colorplane guide needs continuous scales.")
    return(NULL)
  }
  # determine axis labels
  if(is.waive(guide$axis_title)) guide$axis_title <-
      scale$axis_title
  if(is.waive(guide$axis_title_y)) guide$axis_title_y <-
      scale$axis_title_y
  # create data frames for tick display
  breaks <- scale$get_breaks(dir = "horizontal")
  if (length(breaks) == 0 || all(is.na(breaks)))
    return()
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
  # plan dimension specification (number of divs etc)
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
  .plane <- expand.grid(.bar, rev(.bar_y))
  names(.plane) <- scale$aesthetics
  .plane <- matrix(scale$map_df(.plane)[[scale$aesthetics[1]]], byrow = T,
                   nrow = guide$nbin)
  attr(.plane, "aesthetic") <- scale$aesthetics[1]
  attr(.plane, "value") <- .bar
  attr(.plane, "value_y") <- .bar_y
  guide$plane <- .plane
#   if (guide$reverse) {
#     guide$key <- guide$key[nrow(guide$key):1, ]
#     guide$bar <- guide$bar[nrow(guide$bar):1, ]
#   }
  guide$hash <- with(guide, digest::digest(list(title, axis_title, axis_title_y,
                                                key$.label, key_y$.label,
                                                plane, name)))
  guide
}

#' Colorplane method for guide_merge
#'
#' Called by ggplot2 engine. Ignores attempts to merge guides.
#'
#' @inheritParams guide_train.colorplane
#' @param new_guide New guide object
#'
#' @export
guide_merge.colorplane <- function(guide, new_guide) {
  guide
}

#' Colorplane method for guide_geom
#'
#' Called by ggplot2 engine. Takes no action as this guide is not geom-based.
#'
#' @inheritParams guide_train.colorplane
#' @param ... Not used
#' @export
guide_geom.colorplane <- function(guide, ...) {
  guide
}

#' Colorplane method for guide_gengrob
#'
#' Called by ggplot2 engine to create graphical objects for the guide.
#'
#' @inheritParams guide_train.colorplane
#' @param theme Plot theme object
#'
#' @seealso \code{\link{scale_color_colorplane}}
#' @export
guide_gengrob.colorplane <- function(guide, theme) {
# TODO: implement label and axis title position options
#   switch(guide$direction,
#          "horizontal" = {
#            label.position <- guide$label.position %||% "bottom"
#            if (!label.position %in% c("top", "bottom")) stop("label position \"", label.position, "\" is invalid")
#          "vertical" = {
#            label.position <- guide$label.position %||% "right"
#            if (!label.position %in% c("left", "right")) stop("label position \"", label.position, "\" is invalid")
#          })
  planewidth <- grid::convertWidth(guide$planewidth %||%
                                     (theme$legend.key.width * 5), "mm")
  planeheight <- grid::convertHeight(guide$planeheight %||%
                                       (theme$legend.key.height * 5), "mm")

  planewidth.c <- c(planewidth)
  planeheight.c <- c(planeheight)

  nbreak <- nrow(guide$key)
  nbreak_y <- nrow(guide$key_y)

  # gap between keys etc
  hgap <- c(grid::convertWidth(unit(0.3, "lines"), "mm"))
  vgap <- hgap

  # only raster rendering supported at present; won't work on graphics devices
  # without raster support
  grob.plane <-
    grid::rasterGrob(guide$plane, width = planewidth.c, height = planeheight.c,
               default.units = "mm", gp = grid::gpar(col = NA),
               interpolate = TRUE)

  # tick and label position
  tic_pos.c <- scales::rescale(guide$key$.value, c(0.5, guide$nbin - 0.5),
                               range(attr(guide$plane, "value"),
                                     na.rm = T, finite = T)
                               ) * planewidth.c / guide$nbin
  label_pos <- unit(tic_pos.c, "mm")

  tic_pos_y.c <- scales::rescale(guide$key_y$.value, c(0.5, guide$nbin - 0.5),
                                 range(attr(guide$plane, "value_y"),
                                       na.rm = T, finite = T)
                                 ) * planeheight.c / guide$nbin
  label_pos_y <- unit(tic_pos_y.c, "mm")

  # title
  grob.title <- ggname(
    "guide.title",
    ggplot2::element_grob(
      guide$title.theme %||% ggplot2::calc_element("legend.title", theme),
      label = guide$title,
      hjust = guide$title.hjust %||% theme$legend.title.align %||%
        calc_element("legend.title", theme)$hjust %||% 0.5,
      vjust = guide$title.vjust %||%
        calc_element("legend.title", theme)$hjust %||% 0.5
    )
  )

  title_width <- grid::convertWidth(grid::grobWidth(grob.title), "mm")
  title_width.c <- c(title_width)
  title_height <- grid::convertHeight(grid::grobHeight(grob.title), "mm")
  title_height.c <- c(title_height)

  #axis titles
  grob.axis_title <- ggname(
    "guide.axis_title",
    ggplot2::element_grob(
      guide$axis_title.theme %||% ggplot2::calc_element("axis.title.x", theme),
      label = guide$axis_title,
      hjust = guide$axis_title.hjust %||%
        calc_element("axis.title.x", theme)$hjust %||% 0.5,
      vjust = guide$title.vjust %||%
        calc_element("axis.title.x", theme)$vjust %||% 0.5
    )
  )

  axis_title_width <- grid::convertWidth(grid::grobWidth(grob.axis_title), "mm")
  axis_title_width.c <- c(axis_title_width)
  axis_title_height <-
    grid::convertHeight(grid::grobHeight(grob.axis_title), "mm")
  axis_title_height.c <- c(axis_title_height)

  grob.axis_title_y <- ggname(
    "guide.axis_title_y",
    ggplot2::element_grob(
      guide$axis_title_y.theme %||%
        ggplot2::calc_element("axis.title.y", theme),
      label = guide$axis_title_y,
      hjust = guide$axis_title_y.hjust %||%
        calc_element("axis.title.y", theme)$hjust %||% 0.5,
      vjust = guide$title_y.vjust %||%
        calc_element("axis.title.y", theme)$vjust %||% 0.5
    )
  )

  axis_title_y_width <-
    grid::convertWidth(grid::grobWidth(grob.axis_title_y), "mm")
  axis_title_y_width.c <- c(axis_title_y_width)
  axis_title_y_height <-
    grid::convertHeight(grid::grobHeight(grob.axis_title_y), "mm")
  axis_title_y_height.c <- c(axis_title_y_height)


  # label
  label.theme <- guide$label.theme %||%
    ggplot2::calc_element("axis.text.x", theme)
  grob.label <- ggplot2::zeroGrob()
  if (guide$label) {
    hjust <- guide$label.hjust %||% label.theme$hjust %||%
      if (any(is.expression(guide$key$.label))) 1 else 0.5
    vjust <- y <- guide$label.vjust %||% label.theme$vjust %||% 0.5
    x <- label_pos

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
    g <- ggplot2::element_grob(element = label.theme, label = label,
                               x = x, y = y, hjust = hjust, vjust = vjust)
    grob.label <- ggname("guide.label", g)
  }


  label_width <- grid::convertWidth(grid::grobWidth(grob.label), "mm")
  label_width.c <- c(label_width)
  label_height <- grid::convertHeight(grid::grobHeight(grob.label), "mm")
  label_height.c <- c(label_height)

  label_y.theme <- guide$label_y.theme %||%
    ggplot2::calc_element("axis.text.y", theme)
  grob.label_y <- ggplot2::zeroGrob()
  if (guide$label) {
    hjust <- x <- guide$label_y.hjust %||% label_y.theme$hjust %||%
      if (any(is.expression(guide$key_y$.label))) 1 else 0.5
    vjust <- guide$label_y.vjust %||% label_y.theme$vjust %||% 0.5
    y <- label_pos_y

    label <- guide$key_y$.label

    # If any of the labels are quoted language objects, convert them
    # to expressions. Labels from formatter functions can return these
    if (any(vapply(label, is.call, logical(1)))) {
      label <- lapply(label, function(l) {
        if (is.call(l)) substitute(expression(x), list(x = l))
        else l
      })
      label <- do.call(c, label)
    }
    g <- ggplot2::element_grob(element = label_y.theme, label = label,
                      x = x, y = y, hjust = hjust, vjust = vjust)
    grob.label_y <- ggname("guide.label_y", g)
  }

  label_width_y <- grid::convertWidth(grid::grobWidth(grob.label_y), "mm")
  label_width_y.c <- c(label_width_y)
  label_height_y <- grid::convertHeight(grid::grobHeight(grob.label_y), "mm")
  label_height_y.c <- c(label_height_y)

  # ticks - horiz
  grob.ticks <- ggplot2::zeroGrob()
  if (guide$ticks) {
      x0 = rep(tic_pos.c, 2)
      y0 = c(rep(0, nbreak), rep(planeheight.c * (9/10), nbreak))
      x1 = rep(tic_pos.c, 2)
      y1 = c(rep(planeheight.c * (1/10), nbreak), rep(planeheight.c, nbreak))
      grob.ticks <- grid::segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                                       default.units = "mm",
                                       gp = grid::gpar(col = "white", lwd = 0.5,
                                                       lineend = "butt")
                                       )
  }

  # ticks - vertical
  grob.ticks_y <- ggplot2::zeroGrob()
  if (guide$ticks) {
    x0 = c(rep(0, nbreak_y), rep(planewidth.c * (9/10), nbreak_y))
    y0 = rep(tic_pos_y.c, 2)
    x1 = c(rep(planewidth.c * (1/10), nbreak_y), rep(planewidth.c, nbreak_y))
    y1 = rep(tic_pos_y.c, 2)
    grob.ticks_y <- grid::segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1,
                                     default.units = "mm",
                                     gp = grid::gpar(col = "white", lwd = 0.5,
                                                     lineend = "butt")
                                     )
  }

  # layout of bar and label
  pl_widths <- c(label_width_y.c, hgap, planewidth.c)
  pl_heights <- c(planeheight.c, vgap, label_height.c)
  vps <- list(plane.row = 1, plane.col = 3,
              label.row = 3, label.col = 3,
              label_y.row = 1, label_y.col = 1)
  # add in axis titles
  # vertical
  widths <- c(axis_title_y_width.c, hgap, pl_widths)
  heights <- c(max(0, axis_title_y_height.c - sum(pl_heights)), pl_heights)
  vps <- with(vps,
              list(plane.row = plane.row + 1, plane.col = plane.col + 2,
                   label.row = label.row + 1, label.col = label.col + 2,
                   label_y.row = label_y.row + 1, label_y.col = label_y.col + 2,
                   axis_title_y.row = 1:2, axis_title_y.col = 1))
  # horizontal
  widths <- c(widths, max(0, axis_title_width.c - sum(widths)))
  heights <- c(heights, vgap, axis_title_height.c)
  vps <- c(vps, list(axis_title.row = length(heights),
                     axis_title.col = vps$plane.col:length(widths)))

  # layout with title
  widths <- c(widths, max(0, title_width.c - sum(widths)))
  heights <- c(title_height.c, vgap, heights)
  vps <- with(vps, list(
    plane.row = plane.row + 2, plane.col = plane.col,
    label.row = label.row + 2, label.col = label.col,
    label_y.row = label_y.row + 2, label_y.col = label_y.col,
    axis_title.row = axis_title.row + 2, axis_title.col = axis_title.col,
    axis_title_y.row = axis_title_y.row + 2, axis_title_y.col = axis_title_y.col,
    title.row = 1, title.col = plane.col:length(widths)
    ))

  # background
  grob.background <- element_render(theme, "legend.background")

  # padding
  padding <- unit(1.5, "mm")
  widths <- c(padding, widths, padding)
  heights <- c(padding, heights, padding)

  gt <- gtable::gtable(widths = unit(widths, "mm"),
                       heights = unit(heights, "mm"))
  gt <- gtable::gtable_add_grob(gt, grob.background,
                                name = "background", clip = "off",
                                t = 1, r = -1, b = -1, l = 1)
  gt <- gtable::gtable_add_grob(gt, grob.plane,
                                name = "plane", clip = "off",
                                t = 1 + min(vps$plane.row),
                                r = 1 + max(vps$plane.col),
                                b = 1 + max(vps$plane.row),
                                l = 1 + min(vps$plane.col))
  gt <- gtable::gtable_add_grob(gt, grob.label,
                                name = "label", clip = "off",
                                t = 1 + min(vps$label.row),
                                r = 1 + max(vps$label.col),
                                b = 1 + max(vps$label.row),
                                l = 1 + min(vps$label.col))
  gt <- gtable::gtable_add_grob(gt, grob.label_y,
                                name = "label_y", clip = "off",
                                t = 1 + min(vps$label_y.row),
                                r = 1 + max(vps$label_y.col),
                                b = 1 + max(vps$label_y.row),
                                l = 1 + min(vps$label_y.col))
  gt <- gtable::gtable_add_grob(gt, grob.axis_title,
                                name = "axis_title", clip = "off",
                                t = 1 + min(vps$axis_title.row),
                                r = 1 + max(vps$axis_title.col),
                                b = 1 + max(vps$axis_title.row),
                                l = 1 + min(vps$axis_title.col))
  gt <- gtable::gtable_add_grob(gt, grob.axis_title_y,
                                name = "axis_title_y", clip = "off",
                                t = 1 + min(vps$axis_title_y.row),
                                r = 1 + max(vps$axis_title_y.col),
                                b = 1 + max(vps$axis_title_y.row),
                                l = 1 + min(vps$axis_title_y.col))
  gt <- gtable::gtable_add_grob(gt, grob.title,
                                name = "title", clip = "off",
                                t = 1 + min(vps$title.row),
                                r = 1 + max(vps$title.col),
                                b = 1 + max(vps$title.row),
                                l = 1 + min(vps$title.col))
  gt <- gtable::gtable_add_grob(gt, grob.ticks,
                                name = "ticks", clip = "off",
                                t = 1 + min(vps$plane.row),
                                r = 1 + max(vps$plane.col),
                                b = 1 + max(vps$plane.row),
                                l = 1 + min(vps$plane.col))
  gt <- gtable::gtable_add_grob(gt, grob.ticks_y,
                                name = "ticks_y", clip = "off",
                                t = 1 + min(vps$plane.row),
                                r = 1 + max(vps$plane.col),
                                b = 1 + max(vps$plane.row),
                                l = 1 + min(vps$plane.col))
  gt
}

#' @export
#' @rdname guide_colorplane
guide_colourplane <- guide_colorplane
