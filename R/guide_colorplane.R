# This file is part of colorplaner. Code in this file is derived from modifying
# guide_colorbar in ggplot2 (copyright RStudio 2016), 2016-09-18.
#
# colorplaner is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2.
#
# colorplaner is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with colorplaner.  If not, see <http://www.gnu.org/licenses/>.

#' @include other_package_compatibility.R
#' @importFrom ggplot2 guide_train guide_gengrob guide_geom guide_merge
NULL

#' Add Guide for Colorplane
#'
#' Generates a guide to explain the colors plotted via
#' \code{\link{scale_color_colorplane}} and \code{\link{scale_fill_colorplane}}.
#'
#' NOTE: ggplot2 now attempts to draw guides for all new aesthetics. This
#' can result in duplicate colorplane guides. To avoid this, set the guide
#' for the secondary dimension (color2/fill2) to FALSE, as in the example below.
#' The guide is based on \code{\link[ggplot2]{guide_colorbar}}, but extended to
#' be a plane of colors with ticks and labels for both variables in the scale.
#' All \code{*.theme} arguments accept two types of arguments: a complete theme
#' object (e.g. the object returned by \code{\link[ggplot2]{theme_grey}}) or an
#' \code{\link[ggplot2]{element_text}}. If a theme is given, the related element
#' will be extracted from the theme and used as-is. If an element is given, any
#' missing parameters will be inherited from the plot's theme before use. If not
#' specified, \code{*.hjust} and \code{*.vjust} parameters will draw from the
#' corresponding \code{*.theme} argument, the plot's theme, or a default of 0.5
#' (centered). One exception is \code{title.hjust} which has been given a
#' default value of 0.5 to override a undesirable default value in the default
#' ggplot theme. Specify \code{title.hjust = NULL} to restore normal inheritance
#' if needed.
#'
#' @param axis.title,axis.title.y Character strings or expressions indicating
#'   the horizontal and vertical axis titles in the guide, respectively. If
#'   \code{NULL}, the title is not shown. By default (\link[ggplot2]{waiver}),
#'   the name of the scale or the name of the variable mapped to the aesthetic.
#' @param axis.title.position,axis.title.y.position Character vectors indicating
#'   the position(s) of axis titles. \code{axis.title.position}: "top" and/or
#'   "bottom" (default). \code{axis.title.y.position}: "left" (default) and/or
#'   "right".
#' @param axis.title.theme,axis.title.y.theme Theme objects for rendering the
#'   axis title text. Typically an \code{\link[ggplot2]{element_text}} object.
#'   When \code{NULL}, defaults to settings for \code{axis.title.x} and
#'   \code{axis.title.y} in the plot theme.
#' @param
#'   axis.title.hjust,axis.title.vjust,axis.title.y.vjust,axis.title.y.hjust
#'   Numerics specifying the horizontal (\code{hjust}) and vertical
#'   (\code{vjust}) justifications of the horizontal (\code{axis.title}) and
#'   vertical (\code{axis.title.y}) axis title text.
#' @param planewidth,planeheight Numeric or \code{\link[grid]{unit}} objects
#'   specifying the width and height of the colorplane. Default values are 5
#'   times the \code{legend.key.width/height} or \code{legend.key.size} in the
#'   plot theme.
#' @param nbin Number specifying how many color pixels are generated for each
#'   dimension of the colorplane. Higher numbers increase guide color accuracy
#'   (especially for larger sized guides) at the expense of speed.
#' @param label.position,label.y.position Character vectors indicating the
#'   position(s) of axis labels. For \code{label.position}, "top" and/or
#'   "bottom" (default). For \code{label.y.position}, "left" (default) and/or
#'   "right".
#' @param label.theme,label.y.theme	Theme objects for rendering axis label text.
#'   Usually the object of \code{\link[ggplot2]{element_text}} is expected. By
#'   default, the theme is specified by \code{axis.text.*} in the plot theme.
#' @param label.hjust,label.vjust,label.y.hjust,label.y.vjust Numerics
#'   specifying the horizontal (\code{hjust}) and vertical (\code{vjust})
#'   justifications of the horizontal (\code{label}) and vertical
#'   (\code{label.y}) axis label text.
#' @param title.position Character string indicating position for the main
#'   title. One of "top" (default) or "bottom".
#' @param default.unit A character string indicating unit for \code{planewidth}
#' and \code{planeheight}.
#'
#' @inheritParams ggplot2::guide_colorbar
#' @examples
#' if(requireNamespace("mapproj")) {
#'   library(ggplot2)
#'   crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#'   states_map <- map_data("state")
#'   ggplot(crimes,
#'          aes(map_id = state, fill = Murder, fill2 = UrbanPop)) +
#'     geom_map(map = states_map) +
#'     scale_fill_colorplane() +
#'     expand_limits(x = states_map$long, y = states_map$lat) +
#'     coord_map() +
#'     guides(fill = guide_colorplane("My Title", axis.title = "Murder Rate",
#'     axis.title.y = "Urban Population %", label.position = c("top", "bottom"),
#'     label.y.position = c("left", "right")), fill2 = FALSE)
#'  }
#' @export
#' @aliases guide_colourplane
#' @references Based on \code{\link[ggplot2]{guide_colorbar}}, modified 2016.
guide_colorplane <- function(

  # title
  title = waiver(),
  title.position = c("top", "bottom"),
  title.theme = NULL,
  title.hjust = 0.5,
  title.vjust = NULL,

  #axis titles
  axis.title = waiver(),
  axis.title.position = c("bottom", "top"),
  axis.title.theme = NULL,
  axis.title.hjust = NULL,
  axis.title.vjust = NULL,
  axis.title.y = waiver(),
  axis.title.y.position = c("left", "right"),
  axis.title.y.theme = NULL,
  axis.title.y.hjust = NULL,
  axis.title.y.vjust = NULL,


  # label
  label = TRUE,
  label.position = c("bottom", "top"),
  label.theme = NULL,
  label.hjust = NULL,
  label.vjust = NULL,
  label.y.position = c("left", "right"),
  label.y.theme = NULL,
  label.y.hjust = NULL,
  label.y.vjust = NULL,

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
  # make defaults one-sided labeling, while allowing for double labeling when
  # specified
  if (missing(axis.title.position)) axis.title.position <- "bottom"
  if (missing(axis.title.y.position)) axis.title.y.position <- "left"
  if (missing(label.position)) label.position <- "bottom"
  if (missing(label.y.position)) label.y.position <- "left"
  title.position <- match.arg(title.position)
  axis.title.position <- match.arg(axis.title.position, several.ok = TRUE)
  axis.title.y.position <- match.arg(axis.title.y.position, several.ok = TRUE)
  label.position <- match.arg(label.position, several.ok = TRUE)
  label.y.position <- match.arg(label.y.position, several.ok = TRUE)

  structure(list(
    # title
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    title.hjust = title.hjust,
    title.vjust = title.vjust,

    #axis titles
    axis.title = axis.title,
    axis.title.position = axis.title.position,
    axis.title.theme = axis.title.theme,
    axis.title.hjust = axis.title.hjust,
    axis.title.vjust = axis.title.vjust,
    axis.title.y = axis.title.y,
    axis.title.y.position = axis.title.y.position,
    axis.title.y.theme = axis.title.y.theme,
    axis.title.y.hjust = axis.title.y.hjust,
    axis.title.y.vjust = axis.title.y.vjust,

    # label
    label = label,
    label.position = label.position,
    label.theme = label.theme,
    label.hjust = label.hjust,
    label.vjust = label.vjust,
    label.y.position = label.y.position,
    label.y.theme = label.y.theme,
    label.y.hjust = label.y.hjust,
    label.y.vjust = label.y.vjust,

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
#' @export
#' @keywords internal
guide_train.colorplane <- function(guide, scale, aesthetic = NULL) {
  # do nothing if scale inappropriate
  if (!inherits(scale, "ScaleColorPlane")) {
    warning("colorplane guide needs scale_color_colorplane or scale_fill_colorplane.")
    return(NULL)
  }
  if (scale$is_discrete()) {
    warning("colorplane guide needs continuous scales.")
    return(NULL)
  }
  # determine axis labels
  if(is.waive(guide$axis.title)) guide$axis.title <-
      scale$axis_title
  if(is.waive(guide$axis.title.y)) guide$axis.title.y <-
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
  guide$hash <- with(guide, digest::digest(list(title, axis.title, axis.title.y,
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
#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
guide_gengrob.colorplane <- function(guide, theme) {
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

  #helper for dealing with incomplete theme objects passed as options
  complete_theme_item <- function(item, id) {
    if (ggplot2::is.theme(item)) {
      ggplot2::calc_element(id, item)
    } else if (inherits(item, "element")) {
      if (!ggplot2::is.theme(theme)) {
        # the theme argument is passed by the ggplot engine as a plain list
        # without the theme attributes, preventing the use of theme_add
        attributes(theme) <- list(class = c("theme", "gg"),
                                  complete = TRUE, validate = TRUE,
                                  names = names(theme))
      }
      # using an incomplete element_text would cause errors in element_grob;
      # fill in any missing specs using plot theme
      ggplot2::calc_element(
        id,
        theme + do.call(ggplot2::theme, stats::setNames(list(item), id))
      )
    } else {
      ggplot2::calc_element(id, theme)
    }
  }
  # title
  grob.title <- ggname(
    "guide.title",
    ggplot2::element_grob(
      complete_theme_item(guide$title.theme, "legend.title"),
      label = guide$title,
      hjust = guide$title.hjust %||% theme$legend.title.align %||%
        calc_element("legend.title", theme)$hjust %||% 0.5,
      vjust = guide$title.vjust %||%
        calc_element("legend.title", theme)$hjust %||% 0.5
    )
  )

  #axis titles
  grob.axis.title <- ggname(
    "guide.axis.title",
    ggplot2::element_grob(
      complete_theme_item(guide$axis.title.theme, "axis.title.x"),
      label = guide$axis.title,
      hjust = guide$axis.title.hjust %||%
        calc_element("axis.title.x", theme)$hjust %||% 0.5,
      vjust = guide$title.vjust %||%
        calc_element("axis.title.x", theme)$vjust %||% 0.5
    )
  )

  grob.axis.title.y <- ggname(
    "guide.axis.title.y",
    ggplot2::element_grob(
      complete_theme_item(guide$axis.title.y.theme, "axis.title.y"),
      label = guide$axis.title.y,
      hjust = guide$axis.title.y.hjust %||%
        calc_element("axis.title.y", theme)$hjust %||% 0.5,
      vjust = guide$title_y.vjust %||%
        calc_element("axis.title.y", theme)$vjust %||% 0.5
    )
  )

  # label
  label.theme <- complete_theme_item(guide$label.theme, "axis.text.x")
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

  label.y.theme <- complete_theme_item(guide$label.y.theme, "axis.text.y")
  grob.label.y <- ggplot2::zeroGrob()
  if (guide$label) {
    hjust <- x <- guide$label.y.hjust %||% label.y.theme$hjust %||%
      if (any(is.expression(guide$key_y$.label))) 1 else 0.5
    vjust <- guide$label.y.vjust %||% label.y.theme$vjust %||% 0.5
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
    g <- ggplot2::element_grob(element = label.y.theme, label = label,
                      x = x, y = y, hjust = hjust, vjust = vjust)
    grob.label.y <- ggname("guide.label.y", g)
  }

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

  #setup layout
  lay <- list(
    widths = planewidth.c,
    heights = planeheight.c,
    rows = list(1),
    cols = list(1),
    grobs = list(grob.plane)
  )

  add_to_layout <- function(lay, grobside) {
    grob <- grobside[[1]]
    side <- grobside[[2]]
    # accumulates all list positions used when adding to multiple sides
    all_pos <- length(lay$grobs) + 1
    if ("overlay" %in% side) {
      pos <- all_pos[length(all_pos)]
      lay$rows[[pos]] <- seq_along(lay$heights)
      lay$cols[[pos]] <- seq_along(lay$widths)
      all_pos <- c(all_pos, pos + 1)
    } else {
      # avoid calculating grobsize when side == overlay because attempting to do
      # so on the segmentsGrobs for the ticks generates an error in some
      # instances (likely related to missing values in tick positions)
      w <- grid::convertWidth(grid::grobWidth(grob), "mm", valueOnly = TRUE)
      h <- grid::convertHeight(grid::grobHeight(grob), "mm", valueOnly = TRUE)
    }
    if ("top" %in% side) {
      pos <- all_pos[length(all_pos)]
      lay$heights <- c(h, vgap, lay$heights)
      lay$rows <- lapply(lay$rows, function(x){x + 2})
      lay$rows[[pos]] <- 1
      lay$cols[[pos]] <- lay$cols[[1]]
      if (w > sum(lay$widths)) {
        lay$widths <- c(lay$widths, w - sum(lay$widths))
        lay$cols[[pos]] <- seq_along(lay$widths)
      }
      all_pos <- c(all_pos, pos + 1)
    }
    if ("bottom" %in% side) {
      pos <- all_pos[length(all_pos)]
      lay$heights <- c(lay$heights, vgap, h)
      lay$rows[[pos]] <- length(lay$heights)
      lay$cols[[pos]] <- lay$cols[[1]]
      if(w > sum(lay$widths)) {
        lay$widths <- c(lay$widths, w - sum(lay$widths))
        lay$cols[[pos]] <- seq_along(lay$widths)

      }
      all_pos <- c(all_pos, pos + 1)
    }
    if ("left" %in% side) {
      pos <- all_pos[length(all_pos)]
      lay$widths <- c(w, hgap, lay$widths)
      lay$cols <- lapply(lay$cols, function(x){x + 2})
      lay$cols[[pos]] <- 1
      lay$rows[[pos]] <- lay$rows[[1]]
      if(h > sum(lay$heights)) {
        lay$heights <- c(lay$heights, h - sum(lay$heights))
        lay$rows[[pos]] <- seq_along(lay$heights)
      }
      all_pos <- c(all_pos, pos + 1)
    }
    if ("right" %in% side) {
      pos <- all_pos[length(all_pos)]
      lay$widths <- c(lay$widths, hgap, w)
      lay$cols[[pos]] <- length(lay$widths)
      lay$rows[[pos]] <- lay$rows[[1]]
      if(h > sum(lay$heights)) {
        lay$heights <- c(lay$heights, h - sum(lay$heights))
        lay$rows[[pos]] <- seq_along(lay$heights)
      }
      all_pos <- c(all_pos, pos + 1)
    }
    # add the grob to the list multiple times when multiple sides used
    all_pos <- all_pos[-length(all_pos)]
    lay$grobs <- c(lay$grobs, rep(list(grob), length(all_pos)))
    lay
  }

  lay <- Reduce(add_to_layout, init = lay,
                list(
                  list(grob.ticks, "overlay"),
                  list(grob.ticks_y, "overlay"),
                  list(grob.label, guide$label.position),
                  list(grob.label.y, guide$label.y.position),
                  list(grob.axis.title, guide$axis.title.position),
                  list(grob.axis.title.y, guide$axis.title.y.position),
                  list(grob.title, guide$title.position),
                  list(ggplot2::zeroGrob(), c("top", "left", "bottom", "right"))
                ))
  # background
  grob.background <- element_render(theme, "legend.background")

  gt <- gtable::gtable(widths = unit(lay$widths, "mm"),
                       heights = unit(lay$heights, "mm"))
  gt <- gtable::gtable_add_grob(gt, grob.background,
                                name = "background", clip = "off",
                                t = 1, r = -1, b = -1, l = 1)
  Reduce(
    f = function(gt, obj) {
      gtable::gtable_add_grob(gt, obj$grob, clip = "off",
                              t = min(obj$row), b = max(obj$row),
                              l = min(obj$col), r = max(obj$col),
                              name = obj$grob$name)
    },
    # grob and position lists need to be transposed for Reduce to traverse them
    x = mapply(lay$grobs, lay$rows, lay$cols, SIMPLIFY = FALSE,
           FUN = function(grob, row, col){
             list(grob = grob, row = row, col = col)
           }),
    init = gt
  )
}

#' @export
guide_colourplane <- guide_colorplane
